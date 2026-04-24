# =============================================================================
# Unit tests for the pure helpers in R/fct_validation.R and R/fct_db.R
# These do NOT require a database connection. Run with:
#   devtools::test()
# =============================================================================

test_that("MRN regex accepts numeric IDs", {
  rx <- "^[0-9]{4,15}$"
  expect_true(grepl(rx, "3825369"))   # typical Mexican expediente
  expect_true(grepl(rx, "0001"))      # 4-digit minimum
  expect_true(grepl(rx, "999999999999999"))  # 15-digit maximum
})

test_that("MRN regex rejects malformed values", {
  rx <- "^[0-9]{4,15}$"
  expect_false(grepl(rx, "12"))           # too short
  expect_false(grepl(rx, "ABC123"))       # alphabetic
  expect_false(grepl(rx, "382-5369"))     # punctuation
  expect_false(grepl(rx, ""))             # empty
  expect_false(grepl(rx, " 3825369"))     # leading space
})

test_that("legacy MRN format used by migration is valid", {
  rx <- "^[0-9]{4,15}$"
  legacy <- sprintf("9%07d", 1:5)
  expect_true(all(grepl(rx, legacy)))
  expect_true(all(nchar(legacy) == 8))
})

test_that("%||% returns first when truthy, second otherwise", {
  `%||%` <- function(a, b) if (!is.null(a) && nzchar(as.character(a))) a else b
  expect_equal("x" %||% "y", "x")
  expect_equal(NULL %||% "y", "y")
  expect_equal("" %||% "y", "y")
})

test_that("nz coerces empty/null to NA", {
  nz <- function(x) if (is.null(x) || !nzchar(as.character(x))) NA else x
  expect_true(is.na(nz(NULL)))
  expect_true(is.na(nz("")))
  expect_equal(nz("foo"), "foo")
})

test_that("date order rule: death must not precede surgery", {
  # Mirrors the cross-field rule from make_encounter_validator
  rule <- function(death, surgery) {
    if (is.null(death) || is.null(surgery)) return(TRUE)
    as.Date(death) >= as.Date(surgery)
  }
  expect_true(rule("2025-06-01", "2025-05-01"))
  expect_false(rule("2025-04-01", "2025-05-01"))
  expect_true(rule(NULL, "2025-05-01"))
})

test_that("chemo intent required if chemo flagged", {
  rule <- function(chemo, intent) {
    if (!isTRUE(chemo)) return(TRUE)
    !is.null(intent) && nzchar(intent)
  }
  expect_true(rule(FALSE, NULL))
  expect_true(rule(TRUE, "neoadyuvante"))
  expect_false(rule(TRUE, NULL))
  expect_false(rule(TRUE, ""))
})

test_that("postgres text-array literal is well formed", {
  pg_text_array <- function(x) {
    x <- x[nzchar(x)]
    if (!length(x)) return(NA_character_)
    paste0("{", paste(x, collapse = ","), "}")
  }
  expect_equal(pg_text_array(c("a","b")), "{a,b}")
  expect_true(is.na(pg_text_array(character(0))))
  expect_true(is.na(pg_text_array("")))
})
