##################################################################
#####  run_all.R  --  full analysis pipeline                 #####
##################################################################
#
#   Rscript R/run_all.R
#
# Requires the NTIS census CSV. Build it with
# tools/ntis_api_census.py, or set NTIS_CENSUS_CSV to an existing copy.
#
# Steps pass data through date-stamped intermediates in output/tables/
# and each picks up the newest match, so the pipeline can be restarted
# from any step. Allow 40-60 minutes; step 01 dominates.

source("config.R")

STEPS <- c(
  "00_build_classification.R",  # census -> project-year classification (C0/C1/C2)
  "01_filter.R",                # Figure 1, Table S1, Figure 2 / Table S2 inputs
  "02_analysis.R",              # representative texts per period-project
  "03_keyword_flow.R",          # Figure 3, Table S3
  "04_cancer_types.R",          # site-specific yearly series (feeds Figure 4)
  "05_compare_epi.R",           # Figure 4
  "06_inst_pay.R",              # Table 1
  "07_sensitivity_C2only.R",    # Table S4 (C2-only sensitivity)
  "08_figS1.R"                  # Supplementary Figure S1 / Figure R1
)

if (!file.exists(CENSUS_CSV)) {
  stop("Census not found: ", CENSUS_CSV,
       "\nBuild it with:  python tools/ntis_api_census.py",
       "\nor set NTIS_CENSUS_CSV to an existing copy. See README.md.")
}

t0 <- Sys.time()
failed <- character(0)

for (s in STEPS) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("=== ", s, "  (", format(Sys.time(), "%H:%M:%S"), ")\n", sep = "")
  cat(strrep("=", 70), "\n", sep = "")
  step_t0 <- Sys.time()
  ok <- tryCatch({
    source(file.path(PROJ_ROOT, "R", s), local = new.env(), echo = FALSE)
    TRUE
  }, error = function(e) {
    cat("--- FAILED:", s, "---\n", conditionMessage(e), "\n")
    FALSE
  })
  if (!ok) {
    failed <- c(failed, s)
    break  # later steps consume this step's output; stop rather than mislead
  }
  cat("--- done in ",
      round(as.numeric(difftime(Sys.time(), step_t0, units = "mins")), 1), " min\n", sep = "")
}

cat("\n", strrep("=", 70), "\n", sep = "")
if (length(failed)) {
  cat("PIPELINE FAILED at: ", paste(failed, collapse = ", "), "\n", sep = "")
} else {
  cat("PIPELINE COMPLETE\n")
  cat("tables : ", OUT_TABLES, "\n", sep = "")
  cat("figures: ", OUT_FIGURES, "\n", sep = "")
}
cat("total elapsed: ",
    round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min\n", sep = "")
