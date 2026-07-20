# =============================================================================
# Sensitivity analysis restricted to definite cancer-related projects.
#
# Repeats the annual project and funding series using C2 projects only, to show
# how much the primary findings depend on the inclusion of C1 (possible
# cancer-related) projects.
#
# Input :  NTIS_cancer_classification_2006-2024_*.RData,
#          ntis2_c0_c1_c2_classification_*
# Output:  tableS4_sensitivity_C2only_*  -> Table S4
# =============================================================================

if (!exists("PROJ_ROOT")) source("config.R")

library(dplyr)
library(data.table)
library(openxlsx)

output_dir <- OUT_TABLES
timestamp  <- format(Sys.time(), "%Y%m%d")

cat("=== 07_sensitivity_C2only ===\n")

# =============================================================================
# 1. Load step 00 classification (project-year level)
# =============================================================================
cat("Step 1: Loading step 00 classification data...\n")

rdata_files <- list.files(output_dir,
  pattern = "NTIS_cancer_classification_.*\\.RData$", full.names = TRUE)

if (length(rdata_files) == 0) {
  stop("No step 00 classification RData found. Run 00_build_classification.R first.")
}
latest_rdata <- rdata_files[order(file.info(rdata_files)$mtime, decreasing = TRUE)[1]]
cat("Loading:", basename(latest_rdata), "\n")
load(latest_rdata)   # loads object 'data'
cat("Loaded:", nrow(data), "project-year rows\n")

# Ensure Year and MoneyC are numeric
data <- as.data.table(data)
data[, Year     := as.numeric(Year)]
data[, MoneyC   := as.numeric(as.character(MoneyC))]
data[is.na(MoneyC), MoneyC := 0]

# =============================================================================
# 2. Load C0/C1/C2 classification (project-level, hybrid MAX+MEAN logic)
# =============================================================================
cat("Step 2: Loading C0/C1/C2 classification...\n")

c1c2_files <- list.files(output_dir,
  pattern = "ntis2_c0_c1_c2_classification_.*\\.xlsx$", full.names = TRUE)

if (length(c1c2_files) == 0) {
  # Fall back: reconstruct from step 00 data directly
  cat("No C0/C1/C2 file found — reconstructing from step 00 data.\n")
  proj_class <- data[, .(
    cancer_total_hits_max  = max(cancer_total_hits, na.rm = TRUE),
    cancer_total_hits_mean = mean(cancer_total_hits, na.rm = TRUE)
  ), by = .(filenum, NO_base)]
  proj_class[, classification := ifelse(
    cancer_total_hits_max == 0, "C0",
    ifelse(cancer_total_hits_mean >= 1, "C2", "C1")
  )]
} else {
  latest_c1c2 <- c1c2_files[order(file.info(c1c2_files)$mtime, decreasing = TRUE)[1]]
  cat("Loading:", basename(latest_c1c2), "\n")
  proj_class <- as.data.table(read.xlsx(latest_c1c2))
  # Keep only needed columns
  proj_class <- proj_class[, .(filenum, NO_base, classification)]
}

# Count C0/C1/C2
cat("Classification distribution:\n")
print(proj_class[, .N, by = classification])

# =============================================================================
# 3. Merge classification onto project-year data
# =============================================================================
cat("Step 3: Merging classification onto project-year data...\n")

# Ensure key types match
proj_class[, filenum  := as.numeric(filenum)]
proj_class[, NO_base  := as.character(NO_base)]
data[, filenum  := as.numeric(filenum)]
data[, NO_base  := as.character(NO_base)]

data_classified <- merge(data, proj_class[, .(filenum, NO_base, classification)],
                         by = c("filenum", "NO_base"), all.x = TRUE)
data_classified[is.na(classification), classification := "C0"]

cat("Rows after merge:", nrow(data_classified), "\n")

# =============================================================================
# 4. Compute annual statistics: Primary (C1+C2) vs Sensitivity (C2 only)
#
# COUNTING METHOD — must match how the paper's primary analysis was computed:
#   Primary   : rows where cancer_related == TRUE (year-level hit flag from step 00)
#               → matches ntis2_yearly_total_stats cancer_related_projects (e.g. 4,008 in 2021)
#   Sensitivity: rows where cancer_related == TRUE AND classification == "C2"
#               → removes C1 projects from the primary count
#
# This is the correct comparison: "what if we excluded possible-cancer (C1) projects?"
# =============================================================================
cat("Step 4: Computing annual statistics...\n")

years_range <- 2006:2024

# -- 4a. All NTIS (denominator)
all_ntis <- data_classified[Year %in% years_range,
  .(
    total_projects = .N,
    total_money_B  = sum(MoneyC, na.rm = TRUE) / 1e9
  ), by = Year]

# -- 4b. Primary analysis: year-level cancer_related == TRUE (C1 + C2)
#         Replicates step 01's cancer_related_projects count
primary <- data_classified[cancer_related == TRUE & Year %in% years_range,
  .(
    primary_projects = .N,
    primary_money_B  = sum(MoneyC, na.rm = TRUE) / 1e9
  ), by = Year]

# -- 4c. Sensitivity: year-level cancer hit AND project classified as C2
sensitivity <- data_classified[cancer_related == TRUE & classification == "C2" & Year %in% years_range,
  .(
    c2only_projects = .N,
    c2only_money_B  = sum(MoneyC, na.rm = TRUE) / 1e9
  ), by = Year]

# -- 4d. C1-only contribution (primary minus sensitivity)
# Not saved separately but used in summary

# -- 4c. All NTIS (denominator)
all_ntis <- data_classified[Year %in% years_range,
  .(
    total_projects = .N,
    total_money_B  = sum(MoneyC, na.rm = TRUE) / 1e9
  ), by = Year]

# -- Merge all NTIS + primary + sensitivity
result <- merge(all_ntis, primary,     by = "Year", all.x = TRUE)
result <- merge(result,   sensitivity, by = "Year", all.x = TRUE)
setorder(result, Year)

# C1-only: primary minus sensitivity
result[, c1only_projects := primary_projects - c2only_projects]
result[, c1only_money_B  := primary_money_B  - c2only_money_B]

# Fill NAs with 0
for (col in c("primary_projects","primary_money_B","c2only_projects","c2only_money_B")) {
  result[is.na(get(col)), (col) := 0]
}

# Proportions
result[, primary_pct_projects := round(primary_projects / total_projects * 100, 2)]
result[, c2only_pct_projects  := round(c2only_projects  / total_projects * 100, 2)]
result[, primary_pct_money    := round(primary_money_B  / total_money_B  * 100, 2)]
result[, c2only_pct_money     := round(c2only_money_B   / total_money_B  * 100, 2)]

# Peak year and % decline from peak
peak_row_primary    <- result[which.max(primary_projects)]
peak_row_c2only     <- result[which.max(c2only_projects)]
peak_year_primary   <- peak_row_primary$Year
peak_year_c2only    <- peak_row_c2only$Year

val_2023_primary    <- result[Year == 2023, primary_projects]
val_2023_c2only     <- result[Year == 2023, c2only_projects]
val_2024_primary    <- result[Year == 2024, primary_projects]
val_2024_c2only     <- result[Year == 2024, c2only_projects]

pct_decline_primary_23 <- round((val_2023_primary - peak_row_primary$primary_projects) /
                                  peak_row_primary$primary_projects * 100, 1)
pct_decline_c2only_23  <- round((val_2023_c2only  - peak_row_c2only$c2only_projects) /
                                  peak_row_c2only$c2only_projects   * 100, 1)

# Funding peaks
peak_funding_primary <- result[which.max(primary_money_B)]
peak_funding_c2only  <- result[which.max(c2only_money_B)]
val_2023_money_primary <- result[Year == 2023, primary_money_B]
val_2023_money_c2only  <- result[Year == 2023, c2only_money_B]
pct_decline_money_primary_23 <- round((val_2023_money_primary - peak_funding_primary$primary_money_B) /
                                        peak_funding_primary$primary_money_B * 100, 1)
pct_decline_money_c2only_23  <- round((val_2023_money_c2only  - peak_funding_c2only$c2only_money_B)  /
                                        peak_funding_c2only$c2only_money_B   * 100, 1)

# C1 project count and share
total_c1_rows <- result[, sum(c1only_projects, na.rm=TRUE)]
total_c2_rows <- result[, sum(c2only_projects, na.rm=TRUE)]
c1_share_pct  <- round(total_c1_rows / (total_c1_rows + total_c2_rows) * 100, 1)

cat("\n=== SUMMARY ===\n")
cat("Primary (C1+C2, year-level hit): peak", peak_year_primary, "=",
    peak_row_primary$primary_projects, "projects\n")
cat("Sensitivity (C2 only, year-level hit): peak", peak_year_c2only, "=",
    peak_row_c2only$c2only_projects, "projects\n")
cat("Projects decline to 2023 — Primary:", pct_decline_primary_23,
    "% | C2-only:", pct_decline_c2only_23, "%\n")
cat("Funding peak — Primary:", round(peak_funding_primary$primary_money_B, 0), "B KRW (",
    peak_funding_primary$Year, ") | C2-only:", round(peak_funding_c2only$c2only_money_B, 0),
    "B KRW (", peak_funding_c2only$Year, ")\n")
cat("Funding decline to 2023 — Primary:", pct_decline_money_primary_23,
    "% | C2-only:", pct_decline_money_c2only_23, "%\n")
cat("C1 share of cancer-related project-years:", c1_share_pct, "%\n")

# =============================================================================
# 5. Comparison table (for Supplementary Table S4)
# =============================================================================
tableS4 <- result[, .(
  Year,
  `Total NTIS Projects`           = total_projects,
  `Primary Projects (C1+C2)`      = primary_projects,
  `C1-only Projects`              = c1only_projects,
  `C2-only Projects`              = c2only_projects,
  `C2-only % of Primary`          = round(c2only_projects / primary_projects * 100, 1),
  `Primary % of Total NTIS`       = primary_pct_projects,
  `C2-only % of Total NTIS`       = c2only_pct_projects,
  `Primary Funding (B KRW)`       = round(primary_money_B, 1),
  `C2-only Funding (B KRW)`       = round(c2only_money_B, 1),
  `C2-only % of Primary Funding`  = round(c2only_money_B / primary_money_B * 100, 1),
  `Primary Funding % of NTIS`     = primary_pct_money,
  `C2-only Funding % of NTIS`     = c2only_pct_money
)]

# =============================================================================
# 6. Save
# =============================================================================
out_file <- file.path(output_dir, paste0("tableS4_sensitivity_C2only_", timestamp, ".xlsx"))
wb <- createWorkbook()
addWorksheet(wb, "TableS4_C2only")
writeData(wb, "TableS4_C2only", tableS4)

# Add summary sheet
summary_data <- data.frame(
  Item = c(
    "PRIMARY ANALYSIS (C1+C2, year-level hit)",
    "  Peak year (projects)",
    "  Peak project count",
    "  2023 project count",
    "  % decline peak → 2023",
    "  2024 project count (provisional)",
    "  Peak year (funding)",
    "  Peak funding (B KRW)",
    "  2023 funding (B KRW)",
    "  % decline peak → 2023 (funding)",
    "SENSITIVITY ANALYSIS (C2 only, year-level hit)",
    "  Peak year (projects)",
    "  Peak project count",
    "  2023 project count",
    "  % decline peak → 2023",
    "  2024 project count (provisional)",
    "  Peak year (funding)",
    "  Peak funding (B KRW)",
    "  2023 funding (B KRW)",
    "  % decline peak → 2023 (funding)",
    "C1 PROJECT SHARE",
    "  C1 project-years as % of cancer-related project-years",
    "NOTE",
    "  2024 data"
  ),
  Value = c(
    "",
    peak_year_primary,
    peak_row_primary$primary_projects,
    val_2023_primary,
    paste0(pct_decline_primary_23, "%"),
    val_2024_primary,
    peak_funding_primary$Year,
    round(peak_funding_primary$primary_money_B, 0),
    round(val_2023_money_primary, 0),
    paste0(pct_decline_money_primary_23, "%"),
    "",
    peak_year_c2only,
    peak_row_c2only$c2only_projects,
    val_2023_c2only,
    paste0(pct_decline_c2only_23, "%"),
    val_2024_c2only,
    peak_funding_c2only$Year,
    round(peak_funding_c2only$c2only_money_B, 0),
    round(val_2023_money_c2only, 0),
    paste0(pct_decline_money_c2only_23, "%"),
    "",
    paste0(c1_share_pct, "%"),
    "",
    "Provisional; likely undercount due to reporting lags"
  )
)
addWorksheet(wb, "Summary")
writeData(wb, "Summary", summary_data)

saveWorkbook(wb, out_file, overwrite = TRUE)
cat("Saved:", out_file, "\n")
cat("=== NTIS_sensitivity_C2only.R COMPLETE ===\n")

