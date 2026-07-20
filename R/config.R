##################################################################
#####  config.R  --  project paths                           #####
##################################################################
# Sourced by every script in R/. Paths resolve from the repository
# root, so the pipeline runs from R/, from the repository root, or
# under Rscript without modification.

##################################################################
#####  1. Locate the project root                            #####
##################################################################
# Walk upward from the running script (or the working directory)
# until a repository marker is found.

.find_proj_root <- function(start = NULL) {
  if (is.null(start)) {
    a <- commandArgs(trailingOnly = FALSE)
    f <- sub("^--file=", "", a[grep("^--file=", a)])
    start <- if (length(f)) dirname(normalizePath(f[1], mustWork = FALSE)) else getwd()
  }
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  markers <- c(".here", "ntis-cancer-rd.Rproj")
  repeat {
    if (any(file.exists(file.path(d, markers)))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) {
      stop("PROJ_ROOT not found: no .here or .Rproj marker at or above '", start,
           "'.\nRun scripts from inside the repository, or set the working ",
           "directory to the repository root.")
    }
    d <- parent
  }
}

PROJ_ROOT <- .find_proj_root()

##################################################################
#####  2. Directories                                        #####
##################################################################
DATA_RAW    <- file.path(PROJ_ROOT, "data", "raw")      # NTIS API census lands here
EPI_DIR     <- file.path(PROJ_ROOT, "data", "epi")      # published national cancer statistics
LEXICON_DIR <- file.path(PROJ_ROOT, "lexicon")          # cancer lexicon + keyword translation
OUT_TABLES  <- file.path(PROJ_ROOT, "output", "tables")
OUT_FIGURES <- file.path(PROJ_ROOT, "output", "figures")

for (.d in c(DATA_RAW, OUT_TABLES, OUT_FIGURES)) {
  if (!dir.exists(.d)) dir.create(.d, recursive = TRUE, showWarnings = FALSE)
}
rm(.d)

##################################################################
#####  3. Input census                                       #####
##################################################################
# Built by tools/ntis_api_census.py. Set NTIS_CENSUS_CSV to read a
# census held outside the repository.

CENSUS_CSV <- Sys.getenv(
  "NTIS_CENSUS_CSV",
  unset = file.path(DATA_RAW, "ntis_api_census_260713.csv")
)

##################################################################
#####  4. Shared helpers                                     #####
##################################################################
# Newest file matching a pattern. Intermediates in output/tables are
# date-stamped, so no step refers to a particular run date.
latest_file <- function(dir, pattern) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    stop("No file matching '", pattern, "' in ", dir,
         "\nDid an earlier step in R/run_all.R fail?")
  }
  files[which.max(file.mtime(files))]
}

cat("=== config.R ===\n")
cat("PROJ_ROOT  :", PROJ_ROOT, "\n")
cat("CENSUS_CSV :", CENSUS_CSV,
    if (file.exists(CENSUS_CSV)) "(found)" else "(NOT FOUND - see README Quick start)", "\n")
