#!/usr/bin/env Rscript
# One-time helper: download the four S3 lookup files used by V0.1 into
# inst/extdata/, so the deployed app no longer needs S3 at runtime.
#
# Usage (from project root):
#   Rscript bin/fetch_lookups.R
#
# Reads AWS creds from environment (AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY).
# Falls back to a `Sys.setenv()` block at the top if you prefer to hardcode
# locally — but DO NOT commit your keys.

if (!nzchar(Sys.getenv("AWS_ACCESS_KEY_ID"))) {
  stop("Set AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY in your env first.")
}
if (!nzchar(Sys.getenv("AWS_DEFAULT_REGION"))) {
  Sys.setenv(AWS_DEFAULT_REGION = "us-east-2")
}

if (!requireNamespace("aws.s3", quietly = TRUE)) {
  install.packages("aws.s3", repos = "https://cloud.r-project.org")
}

dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)

files <- c("icd_codes.csv", "icdo3.csv", "ctps.csv", "fda_active_filtered.csv")
for (f in files) {
  out <- file.path("inst/extdata", f)
  message("downloading ", f, " -> ", out)
  raw <- aws.s3::get_object(f, bucket = "cptcode")
  writeBin(raw, out)
}
message("done. ", length(files), " files saved to inst/extdata/")
