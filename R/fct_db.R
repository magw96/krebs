#' Database connection pool and query helpers.
#'
#' One pool per app. RLS context (`app.current_hospital`, `app.is_super_admin`)
#' is set per-checkout via a `pool::poolWithTransaction`-aware helper, so each
#' Shiny session sees only its tenant's rows.
#'
#' All write operations go through `db_insert()` / `db_update()` which also
#' write an audit_log row in the same transaction.

#' Build the pool. Called once from app_server.R.
#' @export
db_pool <- function() {
  url <- Sys.getenv("KREBS_DB_URL", "")
  if (!nzchar(url)) {
    stop("KREBS_DB_URL is not set. Copy .Renviron.example to ~/.Renviron and fill it in.",
         call. = FALSE)
  }
  u <- httr::parse_url(url)
  pool::dbPool(
    drv         = RPostgres::Postgres(),
    host        = u$hostname,
    port        = as.integer(u$port %||% 5432),
    dbname      = sub("^/", "", u$path %||% "postgres"),
    user        = u$username,
    password    = utils::URLdecode(u$password %||% ""),
    sslmode     = "require",
    bigint      = "integer64",
    minSize     = 1,
    maxSize     = 8,
    idleTimeout = 600 * 1000
  )
}

#' Run a function inside a transaction with the session's tenant context set.
#' This is the only way write operations should touch the DB.
#'
#' @param pool   The pool created by db_pool().
#' @param user   A list with $user_id, $hospital_id, $role, $username.
#' @param fn     A function(con) {...} that performs DB work and returns a value.
with_tenant <- function(pool, user, fn) {
  pool::poolWithTransaction(pool, function(con) {
    if (isTRUE(user$role == "super_admin")) {
      DBI::dbExecute(con, "SET LOCAL app.is_super_admin = 'true'")
      DBI::dbExecute(con, "SET LOCAL app.current_hospital = ''")
    } else {
      DBI::dbExecute(con, "SET LOCAL app.is_super_admin = 'false'")
      DBI::dbExecute(con, glue::glue_sql(
        "SET LOCAL app.current_hospital = {as.character(user$hospital_id)}",
        .con = con
      ))
    }
    fn(con)
  })
}

#' Read-only helper. Sets tenant context for the duration of one query.
db_read <- function(pool, user, sql, params = list()) {
  with_tenant(pool, user, function(con) {
    if (length(params) == 0L) {
      DBI::dbGetQuery(con, sql)
    } else {
      DBI::dbGetQuery(con, sql, params = params)
    }
  })
}

#' Insert one row into a table and write an audit_log row in the same txn.
#' @param row  Named list of columns.
db_insert <- function(pool, user, table, row, target_id = NULL) {
  with_tenant(pool, user, function(con) {
    cols <- names(row)
    placeholders <- paste0("$", seq_along(row))
    sql <- glue::glue_sql(
      "INSERT INTO {`table`} ({`cols`*}) VALUES ({placeholders*}) RETURNING *",
      table = table, cols = cols, placeholders = DBI::SQL(placeholders),
      .con = con
    )
    res <- DBI::dbGetQuery(con, sql, params = unname(row))
    audit_write(con, user, action = "INSERT", target_table = table,
                target_id = target_id %||% as.character(res[[1L]][1L]),
                diff = list(after = row))
    res
  })
}

#' Update one row by primary key (composite or simple).
#' @param where  Named list of WHERE conditions ANDed together.
#' @param set    Named list of columns to set.
db_update <- function(pool, user, table, where, set) {
  with_tenant(pool, user, function(con) {
    # Capture before:
    where_sql <- paste(
      vapply(names(where), function(k) {
        glue::glue_sql("{`k`} = {where[[k]]}", k = k, .con = con)
      }, character(1)),
      collapse = " AND "
    )
    before <- DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT * FROM {`table`} WHERE {DBI::SQL(where_sql)}",
      table = table, .con = con
    ))
    set_sql <- paste(
      vapply(names(set), function(k) {
        glue::glue_sql("{`k`} = {set[[k]]}", k = k, .con = con)
      }, character(1)),
      collapse = ", "
    )
    n <- DBI::dbExecute(con, glue::glue_sql(
      "UPDATE {`table`} SET {DBI::SQL(set_sql)} WHERE {DBI::SQL(where_sql)}",
      table = table, .con = con
    ))
    audit_write(con, user, action = "UPDATE", target_table = table,
                target_id = paste(unlist(where), collapse = ":"),
                diff = list(before = as.list(before), after = set))
    n
  })
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[[1]])) a else b

#' Apply a clinician-justified amendment to a patient identity field or an
#' encounter field. Wraps the actual UPDATE plus the patient_amendments INSERT
#' plus the audit_log row in ONE transaction so partial writes are impossible.
#'
#' @param pool         pool object
#' @param user         list with $user_id, $hospital_id, $role
#' @param target_table 'patient_identifiers' or 'encounters'
#' @param target_id    encounter_id (UUID as text), or 'identity' for patient
#' @param mrn          patient mrn (denormalised here so we can index)
#' @param changes      named list of new values (col_name = new_value)
#' @param motivo       clinician-supplied reason (required, >=5 chars)
#'
#' @return integer number of rows updated (1 on success).
record_amendment <- function(pool, user, target_table, target_id, mrn,
                             changes, motivo) {
  stopifnot(target_table %in% c("patient_identifiers","encounters"),
            length(changes) > 0L,
            nchar(trimws(motivo %||% "")) >= 5L)

  with_tenant(pool, user, function(con) {
    # ---- Build WHERE clause for UPDATE -------------------------------------
    where_sql <- if (target_table == "patient_identifiers") {
      glue::glue_sql(
        "hospital_id = {as.integer(user$hospital_id)} AND mrn = {mrn}",
        .con = con)
    } else {
      glue::glue_sql("encounter_id = {target_id}::uuid", .con = con)
    }

    # ---- Snapshot current values for the affected fields -------------------
    cols_sql <- DBI::SQL(paste(sprintf("%s::text AS %s", names(changes), names(changes)),
                               collapse = ", "))
    before <- DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT {cols_sql} FROM {`target_table`} WHERE {DBI::SQL(where_sql)}",
      target_table = target_table, cols_sql = cols_sql, .con = con
    ))
    if (nrow(before) == 0L) {
      stop("record_amendment: target row not found")
    }

    # ---- UPDATE the underlying row -----------------------------------------
    set_sql <- paste(
      vapply(names(changes), function(k) {
        glue::glue_sql("{`k`} = {changes[[k]]}", k = k, .con = con)
      }, character(1)),
      collapse = ", "
    )
    n <- DBI::dbExecute(con, glue::glue_sql(
      "UPDATE {`target_table`} SET {DBI::SQL(set_sql)} WHERE {DBI::SQL(where_sql)}",
      target_table = target_table, .con = con
    ))

    # ---- Insert one row per changed field into patient_amendments ----------
    for (k in names(changes)) {
      old_v <- as.character(before[[k]][1])
      new_v <- if (is.null(changes[[k]])) NA_character_ else as.character(changes[[k]])
      if (identical(old_v, new_v)) next   # skip no-op edits
      DBI::dbExecute(con,
        "INSERT INTO patient_amendments
           (hospital_id, mrn, target_table, target_id, field_name,
            old_value, new_value, motivo, amended_by)
         VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)",
        params = list(
          as.integer(user$hospital_id %||% NA_integer_),
          as.character(mrn),
          target_table,
          as.character(target_id),
          k,
          old_v, new_v,
          trimws(motivo),
          as.integer(user$user_id)
        ))
    }

    # ---- Mirror to audit_log so the global activity feed shows it too ------
    audit_write(con, user, action = "UPDATE",
                target_table = target_table,
                target_id    = as.character(target_id),
                diff         = list(before = as.list(before),
                                    after  = changes,
                                    motivo = motivo))
    n
  })
}

#' One-time bootstrap on first launch. Creates a default 'admin' user if no
#' users exist, using the password from $KREBS_ADMIN_BOOTSTRAP_PWD. Idempotent.
#' Also creates a default hospital row if `hospitals` is empty.
db_bootstrap <- function(pool) {
  con <- pool::poolCheckout(pool); on.exit(pool::poolReturn(con))

  # Default hospital
  n_hosp <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM hospitals")$n
  if (n_hosp == 0) {
    DBI::dbExecute(con, "INSERT INTO hospitals(code, name) VALUES ('DEFAULT','Default Hospital')")
    message("[bootstrap] Created default hospital. Edit it in the admin UI.")
  }

  # Default super_admin user
  n_users <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM users")$n
  if (n_users == 0) {
    pwd <- Sys.getenv("KREBS_ADMIN_BOOTSTRAP_PWD", "")
    if (!nzchar(pwd)) {
      warning("KREBS_ADMIN_BOOTSTRAP_PWD not set; skipping admin bootstrap. ",
              "You will not be able to log in.", call. = FALSE)
      return(invisible(NULL))
    }
    hash <- scrypt::hashPassword(pwd)
    DBI::dbExecute(con,
      "INSERT INTO users(username, full_name, pwd_hash, role, hospital_id)
       VALUES ('admin','Bootstrap Admin', $1, 'super_admin', NULL)",
      params = list(hash)
    )
    message("[bootstrap] Created super_admin user 'admin'. CHANGE THE PASSWORD ON FIRST LOGIN.")
  }
  invisible(NULL)
}
