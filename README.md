# Krebs V0.2 — Multi-tenant Oncology Cancer Registry

Shiny app for capturing and following oncology patients across multiple
institutions. Implements WHO-aligned data capture (TNM, ICD-O-3, CPT),
longitudinal follow-up with recurrence tracking, role-based access control,
and interactive dashboards.

## Stack

- R / Shiny + golem package layout
- bs4Dash for the UI shell
- PostgreSQL (Supabase, RDS, or local) with row-level security for tenant isolation
- shinymanager for auth (bcrypt-hashed passwords)
- DBI + pool + RPostgres for DB access (no full-table scans, no S3 walks)

## Quickstart (local development)

### 1. PostgreSQL

Sign up for a free Supabase project (https://supabase.com), then in the SQL editor
run the contents of `inst/sql/schema.sql`. This creates all tables, indexes, and
row-level security policies.

Get your connection string from **Project Settings → Database → URI**. It looks
like `postgresql://postgres:PWD@db.PROJECT.supabase.co:5432/postgres`.

### 2. Environment variables

Copy `.Renviron.example` to `~/.Renviron` (your home directory, **not** the
project folder), fill in the values, then restart R.

```bash
cp .Renviron.example ~/.Renviron
# edit ~/.Renviron, set KREBS_DB_URL and KREBS_ADMIN_BOOTSTRAP_PWD
```

### 3. Install dependencies

```r
install.packages("renv")
renv::restore()       # if renv.lock exists
# or, first time:
renv::init()
install.packages(c("bs4Dash","DBI","RPostgres","pool","shinymanager",
                   "shinyvalidate","shinyWidgets","shinyjs","timevis",
                   "DT","plotly","ggplot2","survival","survminer",
                   "dplyr","tidyr","lubridate","glue","uuid","golem","config"))
```

### 4. Run

```r
shiny::runApp()
# first launch creates a default admin user with username "admin"
# and password = $KREBS_ADMIN_BOOTSTRAP_PWD. Change it immediately after login.
```

## Deploy to shinyapps.io

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name="<account>", token="<token>", secret="<secret>")
rsconnect::deployApp(
  appDir  = ".",
  envVars = c("KREBS_DB_URL",
              "KREBS_ADMIN_BOOTSTRAP_PWD",
              "R_CONFIG_ACTIVE"),
  appName = "krebs"
)
```

`envVars=` is the key step that fixed the original problem of `.Renviron`
being ignored on shinyapps.io. The variables are encrypted at deploy time
and injected as real environment variables in the runtime container.

## Multi-tenant model

Every PHI and clinical row carries a `hospital_id`. Postgres row-level
security enforces that a clinician at hospital A can never read rows from
hospital B, even if a query bug tries to. After login, the app sets a
session GUC (`SET app.current_hospital = ...`) which the RLS policies use.

To onboard a new hospital:

```sql
INSERT INTO hospitals(code, name, country) VALUES ('HGM','Hospital General de Mexico','MX');
-- Then add a user mapped to that hospital_id via the admin UI.
```

## Roles

| Role | Read | Write | Export | Admin |
|------|------|-------|--------|-------|
| viewer | own hospital | — | — | — |
| clinician | own hospital | own hospital | — | — |
| researcher | de-identified, own hospital | — | de-identified | — |
| admin | own hospital | own hospital | own hospital | own hospital users |
| super_admin | all hospitals | all hospitals | all hospitals | all hospitals |

## Migrating from V0.1

See `inst/migrations/001_v01_to_v02.R`. One-time ETL from your S3 CSV bucket
into the new Postgres schema. **Take an S3 backup first.**

## License

MIT.
