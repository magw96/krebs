#' Audit log writer.
#'
#' Called from inside db_insert / db_update / explicit code paths
#' (LOGIN, LOGOUT, EXPORT). Always writes inside an existing transaction so
#' the audit row is rolled back if the underlying op fails.

#' @param con   Existing DBI connection (inside a transaction).
#' @param user  list(user_id, username, hospital_id, role).
#' @param action  One of: INSERT, UPDATE, DELETE, EXPORT, LOGIN, LOGOUT.
#' @param target_table  Optional table name.
#' @param target_id     Optional row identifier (string).
#' @param diff   Optional list with $before and/or $after, serialised as JSONB.
audit_write <- function(con, user, action,
                        target_table = NA_character_,
                        target_id    = NA_character_,
                        diff         = NULL,
                        ip           = NA_character_) {
  diff_json <- if (is.null(diff)) NA_character_ else jsonlite::toJSON(diff, auto_unbox = TRUE, null = "null")
  DBI::dbExecute(con,
    "INSERT INTO audit_log
       (actor_user_id, actor_name, hospital_id, action, target_table, target_id, diff, ip)
     VALUES ($1, $2, $3, $4, $5, $6, $7::jsonb, $8::inet)",
    params = list(
      user$user_id %||% NA_integer_,
      user$username %||% "system",
      user$hospital_id %||% NA_integer_,
      action,
      target_table,
      target_id,
      diff_json,
      ip
    )
  )
  invisible(NULL)
}

#' Convenience wrapper for events that aren't a DB write (LOGIN/LOGOUT/EXPORT).
audit_event <- function(pool, user, action, target_table = NA, target_id = NA, ip = NA) {
  con <- pool::poolCheckout(pool); on.exit(pool::poolReturn(con))
  DBI::dbWithTransaction(con, {
    audit_write(con, user, action, target_table, target_id, ip = ip)
  })
}
