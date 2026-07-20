# =============================================================================
# Project-level classification, period reshaping, and the temporal tables.
#
# Assigns each project to C0 (no cancer-term match in any year), C2 (maximum
# matched-field count > 0 and mean >= 1) or C1 (maximum > 0, mean < 1), reshapes
# the project-year records into the five study periods, and derives the annual
# project and funding series used throughout the paper. Also draws the
# stratified samples used for physician validation.
#
# Input :  NTIS_cancer_classification_2006-2024_*.RData
# Output:  ntis2_period_reshape_{cancer,all}_*, ntis2_yearly_total_stats_*,
#          ntis2_c0_c1_c2_classification_*, ntis2_all_summary_tables_*
#          -> Figure 1 and Table S1
# =============================================================================

if (!exists("PROJ_ROOT")) source("config.R")

required_packages <- c("readxl", "data.table", "stringr", "writexl", "openxlsx", "ggplot2")
invisible(lapply(required_packages, library, character.only = TRUE))


cat("=== 01_filter STARTING ===\n")
start_time <- Sys.time()

# =============================================================================
# 2. READ DATA FROM THE CLASSIFICATION STEP
# =============================================================================

cat("Step 1: Reading output from 00_build_classification.R...\n")

# Find the most recent classification file (prefer RData, fallback to CSV)
output_dir_tables <- OUT_TABLES
ntis_rdata_files <- list.files(path = output_dir_tables, pattern = "NTIS.*classification.*\\.RData$", full.names = TRUE)
ntis_csv_files <- list.files(path = output_dir_tables, pattern = "NTIS.*classification.*\\.csv$", full.names = TRUE)

# Prefer RData, fallback to CSV
if (length(ntis_rdata_files) > 0) {
  file_info <- file.info(ntis_rdata_files)
  ntis_rdata_files <- ntis_rdata_files[order(file_info$mtime, decreasing = TRUE)]
  input_file <- ntis_rdata_files[1]
  cat("Loading RData file:", basename(input_file), "\n")
  load(input_file)
  cat("Original data loaded from RData:", nrow(data), "rows x", ncol(data), "columns\n")
} else if (length(ntis_csv_files) > 0) {
  file_info <- file.info(ntis_csv_files)
  ntis_csv_files <- ntis_csv_files[order(file_info$mtime, decreasing = TRUE)]
  input_file <- ntis_csv_files[1]
  cat("Loading CSV file:", basename(input_file), " (RData not found, using CSV)\n")
  library(data.table)
  data <- fread(input_file, encoding = "UTF-8")
  cat("Original data loaded from CSV:", nrow(data), "rows x", ncol(data), "columns\n")
} else {
  stop("No classification results file found in output/tables. Please run 00_build_classification.R first.")
}

# Verify Korean text encoding
cat("=== VERIFYING KOREAN TEXT ENCODING ===\n")
korean_cols <- c("Text1", "Text2", "Key1", "Key2")
for (col in korean_cols) {
  if (col %in% colnames(data)) {
    # Check for Korean characters in the first few rows
    sample_texts <- head(data[[col]], 5)
    korean_chars <- sum(grepl("[가-힣]", sample_texts, useBytes = TRUE), na.rm = TRUE)
    cat(sprintf("%s: %d Korean characters found in sample\n", col, korean_chars))

    # Show sample Korean text
    korean_samples <- sample_texts[grepl("[가-힣]", sample_texts, useBytes = TRUE)]
    if (length(korean_samples) > 0) {
      cat(sprintf("  Sample Korean text: %s\n", paste(head(korean_samples, 2), collapse = " | ")))
    }
  }
}

# =============================================================================
# 3. CHANGE DATA COLUMN TYPES
# =============================================================================

cat("Step 2: Converting to data.table and column types...\n")

# Ensure data is a data.table
if (!is.data.table(data)) {
  data <- as.data.table(data)
  cat("Data converted to data.table\n")
}

# Convert column types as specified
data[, `:=`(
  filenum = as.numeric(filenum),
  NO = as.character(NO),
  Year = as.numeric(Year),
  cancer_total_hits = as.numeric(cancer_total_hits)
)]

cat("Column types converted successfully\n")

# =============================================================================
# 4. PROCESS DATA - CREATE NO_BASE
# =============================================================================

cat("Step 3: Checking/creating NO_base column...\n")

# Use NO_base from input if present, otherwise create it
if (!"NO_base" %in% names(data)) {
  data[, NO_base := str_replace(NO, "-.*$", "")]
  cat("NO_base column created (computed from NO)\n")
} else {
  cat("NO_base column found in input, using as-is\n")
}

# === PERIOD-BASED RESHAPING FOR ALL PROJECTS ===
cat("\n=== PERIOD-BASED RESHAPING FOR ALL PROJECTS ===\n")

# Define periods and years
periods <- list(
  '1' = 2006:2010,
  '2' = 2011:2015,
  '3' = 2016:2020,
  '4' = 2021:2024
)

# Keep relevant columns for melting (ALL projects, not just cancer-related)
# Include classification verification columns
keep_cols <- c("filenum", "NO_base", "Year", "Text1", "Text2", "Key1", "Key2", "cancer_related",
               "cancer_total_hits", "Text1_cancer_hit", "Text2_cancer_hit", "Key1_cancer_hit", "Key2_cancer_hit")
long_data_all <- data[, ..keep_cols]

cat("Processing ALL projects for period-based reshaping:", nrow(long_data_all), "rows\n")

# For each project and period, create one row with year-tagged columns (ALL projects)
library(data.table)
period_rows_all <- list()
for (p in names(periods)) {
  yrs <- periods[[p]]
  # Subset to years in this period
  period_dt <- long_data_all[Year %in% yrs]
  if (nrow(period_dt) == 0) next
  period_dt[, period := as.integer(p)]
  # Remove duplicates for the merge keys
  period_dt <- unique(period_dt, by = c("filenum", "NO_base", "period", "Year"))
  # Wide reshape, one column per year-tagged field
  # First, create cancer_related columns
  cancer_wide <- dcast(
    period_dt,
    filenum + NO_base + period ~ Year,
    value.var = "cancer_related",
    fun.aggregate = function(x) {
      if (length(x) == 0) return(FALSE)
      return(any(x, na.rm = TRUE))
    }
  )

  # Rename cancer columns to avoid conflicts
  cancer_cols <- grep("^\\d{4}$", names(cancer_wide), value = TRUE)
  setnames(cancer_wide, cancer_cols, paste0("cancer_related_", cancer_cols))

  # Create text columns for each text field
  text_wide_list <- list()
  for (col_base in c("Text1", "Text2", "Key1", "Key2")) {
    text_wide <- dcast(
      period_dt,
      filenum + NO_base + period ~ Year,
      value.var = col_base,
      fun.aggregate = function(x) {
        if (length(x) == 0) return(NA_character_)
        non_na_vals <- x[!is.na(x) & x != ""]
        if (length(non_na_vals) > 0) {
          return(non_na_vals[1])
        } else {
          return(NA_character_)
        }
      }
    )

    # Rename text columns to avoid conflicts
    text_cols <- grep("^\\d{4}$", names(text_wide), value = TRUE)
    setnames(text_wide, text_cols, paste0(col_base, "_", text_cols))

    text_wide_list[[col_base]] <- text_wide
  }

  # Merge all text columns with cancer columns
  period_wide <- cancer_wide
  for (text_wide in text_wide_list) {
    if (nrow(period_wide) > 0 && nrow(text_wide) > 0) {
      period_wide <- merge(period_wide, text_wide, by = c("filenum", "NO_base", "period"), all.x = TRUE)
    }
  }
  # Add Year_YYYY columns (TRUE if present, else FALSE)
  for (y in yrs) {
    period_wide[[paste0("Year_", y)]] <- !is.na(period_wide[[paste0("Text1_", y)]])
  }
  # Add cancer_any_in_period column
  cancer_cols <- paste0("cancer_related_", yrs)
  # Calculate cancer_any_in_period - if any year has cancer_related = TRUE, then TRUE
  period_wide[, cancer_any_in_period := any(.SD == TRUE, na.rm = TRUE), .SDcols = cancer_cols]
  # Reorder columns: filenum, NO_base, period, Year_YYYY..., all year-tagged columns, cancer_any_in_period
  year_cols <- unlist(lapply(yrs, function(y) paste0("Year_", y)))
  tag_cols <- unlist(lapply(yrs, function(y) c(paste0("Text1_", y), paste0("Text2_", y), paste0("Key1_", y), paste0("Key2_", y), paste0("cancer_related_", y))))
  setcolorder(period_wide, c("filenum", "NO_base", "period", year_cols, tag_cols, "cancer_any_in_period"))
  period_rows_all[[p]] <- period_wide
}
# Combine all period rows for ALL projects
dt_all_projects <- rbindlist(period_rows_all, use.names = TRUE, fill = TRUE)

cat("Period-based reshaping completed for ALL projects:", nrow(dt_all_projects), "rows\n")

# === FILTER TO CANCER-RELATED PROJECTS ===
cat("\n=== FILTERING TO CANCER-RELATED PROJECTS ===\n")

# Debug: Check cancer_any_in_period values
cat("Debug: cancer_any_in_period TRUE count:", sum(dt_all_projects$cancer_any_in_period == TRUE, na.rm = TRUE), "\n")
cat("Debug: cancer_any_in_period FALSE count:", sum(dt_all_projects$cancer_any_in_period == FALSE, na.rm = TRUE), "\n")
cat("Debug: cancer_any_in_period NA count:", sum(is.na(dt_all_projects$cancer_any_in_period)), "\n")

# Check if filtering is working properly
cancer_cols_all <- grep("^cancer_related_\\d{4}$", names(dt_all_projects), value = TRUE)
cat("Debug: Found cancer columns:", length(cancer_cols_all), "\n")

# Create a more reliable cancer_any_in_period column
dt_all_projects[, cancer_any_in_period_reliable := any(.SD == TRUE, na.rm = TRUE), .SDcols = cancer_cols_all]
cat("Debug: cancer_any_in_period_reliable TRUE count:", sum(dt_all_projects$cancer_any_in_period_reliable == TRUE, na.rm = TRUE), "\n")

# Filter to only cancer-related projects for final output
dt_final <- dt_all_projects[cancer_any_in_period_reliable == TRUE]
cat("Filtered to cancer-related projects only:", nrow(dt_final), "rows\n")

# === STRATIFIED SAMPLING AND SUMMARY ===
cat("\n=== STRATIFIED SAMPLING AND SUMMARY ===\n")

strat_start_time <- Sys.time()

# Classify every project, cancer-related or not
cat("Step 1: Classifying all projects...\n")

# Create unique projects classification for ALL projects (cancer and non-cancer)
# IMPORTANT: Keep original logic using first(cancer_total_hits) for backward compatibility
# Add new mean-based calculation for C0/C1/C2 classification
unique_projects_all <- data[, .(
  filenum = first(filenum),
  NO_base = first(NO_base),
  cancer_total_hits = first(cancer_total_hits),  # ORIGINAL LOGIC: first value (for backward compatibility)
  cancer_total_hits_mean = mean(cancer_total_hits, na.rm = TRUE),  # NEW: MEAN count across all years (for C0/C1/C2)
  cancer_total_hits_min = min(cancer_total_hits, na.rm = TRUE),
  cancer_total_hits_max = max(cancer_total_hits, na.rm = TRUE),
  total_years = .N,
  # Combine all text fields across all years for cancer type classification
  Text1_combined = paste(na.omit(unique(Text1[!is.na(Text1) & Text1 != ""])), collapse = " "),
  Text2_combined = paste(na.omit(unique(Text2[!is.na(Text2) & Text2 != ""])), collapse = " "),
  Key1_combined = paste(na.omit(unique(Key1[!is.na(Key1) & Key1 != ""])), collapse = " "),
  Key2_combined = paste(na.omit(unique(Key2[!is.na(Key2) & Key2 != ""])), collapse = " "),
  # Cancer-related: if any year is cancer-related, then TRUE
  cancer_related = any(cancer_related == TRUE, na.rm = TRUE)
), by = .(filenum, NO_base)]

# ORIGINAL LOGIC: Keep cancer_category based on first(cancer_total_hits) - DO NOT CHANGE
unique_projects_all[, cancer_category := ifelse(cancer_total_hits == 0, "unrelated",
                                               ifelse(cancer_total_hits <= 1, "uncertain", "certain"))]

# NEW: Create 3-tier classification system for C0/C1/C2 using HYBRID logic:
# - Step 1 (base cancer-related vs non-cancer): use MAX cancer_total_hits across years
#       -> C0 if cancer_total_hits_max == 0 (never cancer-related in any year)
# - Step 2 (C1 vs C2 among cancer-related projects): use MEAN cancer_total_hits across years
#       -> C2 if mean >= 1 (definite cancer-related)
#       -> C1 if 0 < mean < 1 (possible/probable cancer-related)
# IMPORTANT:
#   - cancer_category continues to use first(cancer_total_hits) for backward compatibility
#   - classification uses HYBRID (max + mean) logic for C0/C1/C2
unique_projects_all[, classification := ifelse(
  cancer_total_hits_max == 0, "C0",                                    # never cancer-related
  ifelse(cancer_total_hits_mean >= 1, "C2", "C1")                      # cancer-related: mean-based split
)]

cat("Unique projects classification:\n")
cat("  ORIGINAL (based on first cancer_total_hits):\n")
cat("    Cancer unrelated (cancer_total_hits = 0):", sum(unique_projects_all$cancer_category == "unrelated"), "\n")
cat("    Cancer related but uncertain (cancer_total_hits <= 1):", sum(unique_projects_all$cancer_category == "uncertain"), "\n")
cat("    Cancer related and certain (cancer_total_hits > 1):", sum(unique_projects_all$cancer_category == "certain"), "\n")
cat("  NEW C0/C1/C2 (HYBRID: MAX for cancer vs non-cancer, MEAN for C1 vs C2):\n")
cat("    C0 (Never cancer-related in any year, max = 0):", sum(unique_projects_all$classification == "C0"), "\n")
cat("    C1 (Possible cancer-related: max > 0 & 0 < mean < 1):", sum(unique_projects_all$classification == "C1"), "\n")
cat("    C2 (Definite cancer-related: max > 0 & mean ≥ 1):", sum(unique_projects_all$classification == "C2"), "\n")

# =============================================================================
# CANCER TYPE CLASSIFICATION (for reference, not for C0/C1/C2)
# =============================================================================

cat("\n=== PERFORMING CANCER TYPE CLASSIFICATION FOR UNIQUE PROJECTS (for reference) ===\n")

# Load cancer type keywords function (same as step 04)
get_cancer_type_keywords <- function() {
  cancer_types <- list(
    "갑상선암" = list(
      korean = c("갑상선암", "갑상선세포암", "갑상선세포 암", "갑상선유두암", "갑상선여포암",
                 "유두상갑상선암", "여포성갑상선암", "갑상선유두상암", "갑상선여포성암",
                 "갑상선종양", "갑상선 악성종양"),
      english = c("thyroid cancer", "thyroid carcinoma", "papillary thyroid carcinoma",
                  "follicular thyroid carcinoma", "papillary thyroid", "follicular thyroid",
                  "thyroid tumor", "thyroid neoplasm", "ptc", "ftc", "\\bptc\\b", "\\bftc\\b")
    ),
    "대장암" = list(
      korean = c("대장암", "대장세포암", "대장세포 암", "결장암", "결장세포암", "직장암", "직장세포암",
                 "항문암", "항문세포암", "항문세포 암",
                 "대장선암", "결장선암", "직장선암", "항문선암", "대장선암종", "결장선암종", "직장선암종", "항문선암종",
                 "대장종양", "결장종양", "직장종양", "항문종양", "대장 악성종양", "결장 악성종양", "직장 악성종양", "항문 악성종양"),
      english = c("colon cancer", "colorectal cancer", "rectum cancer", "rectal cancer", "anal cancer", "anus cancer",
                  "colon carcinoma", "colorectal carcinoma", "rectal carcinoma", "anal carcinoma",
                  "adenocarcinoma of the colon", "adenocarcinoma of the rectum", "adenocarcinoma of the anus",
                  "colorectal adenocarcinoma", "colon adenocarcinoma", "rectal adenocarcinoma", "anal adenocarcinoma",
                  "colon tumor", "colorectal tumor", "rectal tumor", "anal tumor", "colon neoplasm", "colorectal neoplasm", "anal neoplasm")
    ),
    "폐암" = list(
      korean = c("폐암", "폐세포암", "폐세포 암", "비소세포폐암", "소세포폐암",
                 "비소세포 폐암", "소세포 폐암", "폐 비소세포암", "폐 소세포암",
                 "기관암", "기관세포암", "기관세포 암", "기관지암", "기관지세포암", "기관지세포 암",
                 "폐종양", "폐 악성종양", "폐편평세포암", "폐 편평세포암", "기관종양", "기관지종양"),
      english = c("lung cancer", "pulmonary cancer", "non.?small cell lung cancer",
                  "small cell lung cancer", "nsclc", "sclc", "\\bnsclc\\b", "\\bsclc\\b",
                  "tracheal cancer", "trachea cancer", "bronchial cancer", "bronchus cancer",
                  "adenocarcinoma of the lung", "squamous cell carcinoma of the lung",
                  "large cell carcinoma of the lung", "lung adenocarcinoma",
                  "pulmonary adenocarcinoma", "lung carcinoma", "pulmonary carcinoma",
                  "tracheal carcinoma", "bronchial carcinoma", "trachea carcinoma", "bronchus carcinoma",
                  "lung tumor", "pulmonary tumor", "lung neoplasm", "tracheal tumor", "bronchial tumor", "tracheal neoplasm", "bronchial neoplasm")
    ),
    "유방암" = list(
      korean = c("유방암", "유방세포암", "유방세포 암", "삼중음성유방암", "삼중음성 유방암",
                 "her2양성유방암", "her2 양성 유방암", "her2\\+유방암", "her2\\+ 유방암",
                 "침윤성유관암", "침윤성 유관암", "침윤성유방암", "침윤성 유방암",
                 "소엽암", "유관암", "유방소엽암", "유방유관암",
                 "유방종양", "유방 악성종양"),
      english = c("breast cancer", "brca", "\\bbrca\\b", "triple.?negative breast cancer",
                  "tnbc", "\\btnbc\\b", "triple negative breast", "her2.?positive breast cancer",
                  "her2\\+ breast", "her2 positive breast", "invasive ductal carcinoma",
                  "invasive lobular carcinoma", "lobular carcinoma",
                  "breast carcinoma", "mammary carcinoma", "ductal carcinoma",
                  "invasive breast cancer", "breast tumor", "mammary tumor", "breast neoplasm")
    ),
    "위암" = list(
      korean = c("위암", "위세포암", "위세포 암", "위장암", "위장세포암", "위선암",
                 "위식도접합부암", "위식도 접합부 암", "위식도접합부 암",
                 "위종양", "위장종양", "위 악성종양", "위장 악성종양"),
      english = c("stomach cancer", "gastric cancer", "gastric adenocarcinoma",
                  "gastric carcinoma", "stomach adenocarcinoma", "stomach carcinoma",
                  "gastroesophageal junction cancer", "gej cancer", "\\bgej\\b",
                  "gastric tumor", "stomach tumor", "gastric neoplasm", "stomach neoplasm")
    ),
    "전립선암" = list(
      korean = c("전립선암", "전립선세포암", "전립선세포 암", "전립선선암",
                 "전립선종양", "전립선 악성종양"),
      english = c("prostate cancer", "prostatic carcinoma", "prostate carcinoma",
                  "adenocarcinoma of the prostate", "prostatic adenocarcinoma",
                  "prostate adenocarcinoma", "prostate tumor", "prostatic tumor",
                  "prostate neoplasm", "prostatic neoplasm")
    ),
    "간암" = list(
      korean = c("간암", "간세포암", "간세포 암", "간장암", "간장세포암", "간선암",
                 "간종양", "간장종양", "간 악성종양", "간장 악성종양", "간세포암종", "간세포암종"),
      english = c("liver cancer", "hepatic cancer", "hepatocellular carcinoma", "hcc", "\\bhcc\\b",
                  "liver carcinoma", "hepatic carcinoma", "liver adenocarcinoma", "hepatic adenocarcinoma",
                  "liver tumor", "hepatic tumor", "liver neoplasm", "hepatic neoplasm")
    ),
    "췌장암" = list(
      korean = c("췌장암", "췌장세포암", "췌장세포 암", "췌선암", "췌선암종",
                 "췌장종양", "췌장 악성종양"),
      english = c("pancreatic cancer", "pancreas cancer", "pancreatic adenocarcinoma",
                  "pancreatic carcinoma", "pancreas carcinoma", "pancreatic ductal adenocarcinoma", "pdac", "\\bpdac\\b",
                  "pancreatic tumor", "pancreas tumor", "pancreatic neoplasm", "pancreas neoplasm")
    ),
    "신장암" = list(
      korean = c("신장암", "신세포암", "신세포 암", "신장세포암", "신장세포 암",
                 "신장선암", "신장선암종", "신장종양", "신장 악성종양"),
      english = c("kidney cancer", "renal cancer", "renal cell carcinoma", "rcc", "\\brcc\\b",
                  "kidney carcinoma", "renal carcinoma", "kidney adenocarcinoma", "renal adenocarcinoma",
                  "kidney tumor", "renal tumor", "kidney neoplasm", "renal neoplasm")
    ),
    "자궁경부암" = list(
      korean = c("자궁경부암", "자궁경부세포암", "자궁경부세포 암", "경부암", "경부세포암", "경부세포 암",
                 "자궁경부선암", "경부선암", "자궁경부종양", "경부종양", "자궁경부 악성종양", "경부 악성종양"),
      english = c("cervical cancer", "cervix cancer", "cervical carcinoma", "cervix carcinoma",
                  "cervical adenocarcinoma", "cervical squamous cell carcinoma",
                  "cervical tumor", "cervix tumor", "cervical neoplasm", "cervix neoplasm",
                  "cervical intraepithelial neoplasia", "cin", "\\bcin\\b")
    ),
    "자궁내막암" = list(
      korean = c("자궁내막암", "자궁내막세포암", "자궁내막세포 암", "내막암", "내막세포암", "내막세포 암",
                 "자궁내막선암", "내막선암", "자궁내막종양", "내막종양", "자궁내막 악성종양", "내막 악성종양"),
      english = c("endometrial cancer", "endometrium cancer", "endometrial carcinoma", "endometrium carcinoma",
                  "endometrial adenocarcinoma", "endometrial adenocarcinoma",
                  "endometrial tumor", "endometrium tumor", "endometrial neoplasm", "endometrium neoplasm")
    ),
    "난소암" = list(
      korean = c("난소암", "난소세포암", "난소세포 암", "난소선암", "난소선암종",
                 "난소종양", "난소 악성종양"),
      english = c("ovarian cancer", "ovary cancer", "ovarian carcinoma", "ovary carcinoma",
                  "ovarian adenocarcinoma", "ovarian serous carcinoma", "ovarian mucinous carcinoma",
                  "ovarian tumor", "ovary tumor", "ovarian neoplasm", "ovary neoplasm")
    )
  )
  return(cancer_types)
}

# Function to check if text contains any of the keywords for a cancer type
check_cancer_type <- function(texts, keywords) {
  if (length(texts) == 0 || all(is.na(texts))) return(logical(0))
  texts <- as.character(texts)
  texts[is.na(texts)] <- ""
  matches <- logical(length(texts))
  for (keyword in keywords) {
    matches <- matches | grepl(keyword, texts, ignore.case = TRUE, perl = TRUE)
  }
  return(matches)
}

# Classify each cancer type on unique projects
cancer_types_list <- get_cancer_type_keywords()

for (cancer_type in names(cancer_types_list)) {
  keyword_list <- cancer_types_list[[cancer_type]]
  korean_keywords <- keyword_list$korean
  english_keywords <- keyword_list$english

  # Initialize matches
  type_matches <- logical(nrow(unique_projects_all))

  # Search Korean keywords in combined Text1 and Key1
  if (length(korean_keywords) > 0) {
    if ("Text1_combined" %in% colnames(unique_projects_all)) {
      korean_matches_text1 <- check_cancer_type(unique_projects_all[["Text1_combined"]], korean_keywords)
      type_matches <- type_matches | korean_matches_text1
    }
    if ("Key1_combined" %in% colnames(unique_projects_all)) {
      korean_matches_key1 <- check_cancer_type(unique_projects_all[["Key1_combined"]], korean_keywords)
      type_matches <- type_matches | korean_matches_key1
    }
  }

  # Search English keywords in combined Text2 and Key2
  if (length(english_keywords) > 0) {
    if ("Text2_combined" %in% colnames(unique_projects_all)) {
      english_matches_text2 <- check_cancer_type(unique_projects_all[["Text2_combined"]], english_keywords)
      type_matches <- type_matches | english_matches_text2
    }
    if ("Key2_combined" %in% colnames(unique_projects_all)) {
      english_matches_key2 <- check_cancer_type(unique_projects_all[["Key2_combined"]], english_keywords)
      type_matches <- type_matches | english_matches_key2
    }
  }

  unique_projects_all[[paste0("type_", cancer_type)]] <- type_matches
}

# Calculate number of cancer types matched (for reference, separate from C0/C1/C2 classification)
cancer_type_cols <- grep("^type_", colnames(unique_projects_all), value = TRUE)
if (length(cancer_type_cols) > 0) {
  unique_projects_all[, num_cancer_types := rowSums(.SD, na.rm = TRUE), .SDcols = cancer_type_cols]
  cat("Cancer type classification completed (for reference):\n")
  cat("  Projects with 0 cancer types:", sum(unique_projects_all$num_cancer_types == 0), "\n")
  cat("  Projects with 1 cancer type:", sum(unique_projects_all$num_cancer_types == 1), "\n")
  cat("  Projects with 2+ cancer types:", sum(unique_projects_all$num_cancer_types >= 2), "\n")
} else {
  unique_projects_all[, num_cancer_types := 0]
}

# Keep the original cancer_uncertain for backward compatibility (only for cancer-related projects)
unique_projects_cancer <- unique_projects_all[cancer_category != "unrelated"]
unique_projects_cancer[, cancer_uncertain := ifelse(cancer_category == "uncertain", 1, 0)]

# Apply the classification to the period-based data (cancer-related projects only)
cat("Debug: Before merge - dt_final rows:", nrow(dt_final), "\n")
dt_final <- merge(dt_final, unique_projects_all[, .(filenum, NO_base, cancer_category, cancer_total_hits_mean, classification)],
                  by = c("filenum", "NO_base"), all.x = FALSE)
cat("Debug: After first merge - dt_final rows:", nrow(dt_final), "\n")

# Also add the original cancer_uncertain for backward compatibility
dt_final <- merge(dt_final, unique_projects_cancer[, .(filenum, NO_base, cancer_uncertain)],
                  by = c("filenum", "NO_base"), all.x = FALSE)
cat("Debug: After second merge - dt_final rows:", nrow(dt_final), "\n")

# Debug: Check cancer category distribution (cancer-related projects only)
cat("Debug: cancer_category distribution (cancer-related projects only):\n")
cat("  uncertain:", sum(dt_final$cancer_category == "uncertain", na.rm = TRUE), "\n")
cat("  certain:", sum(dt_final$cancer_category == "certain", na.rm = TRUE), "\n")
cat("  NA (no classification):", sum(is.na(dt_final$cancer_category)), "\n")

# Identify cancer relevance columns for this period
cancer_cols <- grep("^cancer_related_\\d{4}$", names(dt_final), value = TRUE)
# Calculate average cancer count per row (per project/period)
dt_final[, cancer_count := rowSums(.SD, na.rm = TRUE), .SDcols = cancer_cols]
dt_final[, years_in_period := rowSums(!is.na(.SD)), .SDcols = cancer_cols]
dt_final[, avg_cancer_count := ifelse(years_in_period > 0, cancer_count / years_in_period, NA)]
# Cancer uncertainty already calculated from merged project data above

cat("Final period-based reshaped data with cancer relevance:", nrow(dt_final), "rows\n")

# Verify Korean text is preserved after processing
cat("\n=== VERIFYING KOREAN TEXT AFTER PROCESSING ===\n")
korean_cols_processed <- grep("^(Text1|Text2|Key1|Key2)_\\d{4}$", names(dt_final), value = TRUE)
cat("Found", length(korean_cols_processed), "Korean text columns after processing\n")

# Check a few sample columns
sample_cols <- head(korean_cols_processed, 4)
for (col in sample_cols) {
  if (col %in% names(dt_final)) {
    sample_texts <- head(dt_final[[col]], 3)
    korean_chars <- sum(grepl("[가-힣]", sample_texts, useBytes = TRUE), na.rm = TRUE)
    cat(sprintf("%s: %d Korean characters found in sample\n", col, korean_chars))

    # Show sample Korean text
    korean_samples <- sample_texts[grepl("[가-힣]", sample_texts, useBytes = TRUE)]
    if (length(korean_samples) > 0) {
      cat(sprintf("  Sample: %s\n", paste(head(korean_samples, 1), collapse = " | ")))
    }
  }
}

# =============================================================================
# OUTPUT FILES WITH UTF-8 ENCODING
# =============================================================================

cat("\n=== CREATING OUTPUT FILES WITH UTF-8 ENCODING ===\n")

# Create output directories
output_dir_tables <- OUT_TABLES
if (!dir.exists(output_dir_tables)) dir.create(output_dir_tables, recursive = TRUE)

# 1. CSV - Cancer-related projects only (UTF-8) - MAIN OUTPUT
timestamp <- format(Sys.time(), "%Y%m%d")
period_csv_file <- file.path(output_dir_tables, paste0("ntis2_period_reshape_cancer_", timestamp, ".csv"))
fwrite(dt_final, period_csv_file, encoding = "UTF-8")
cat("1) Cancer-related projects CSV saved to:", period_csv_file, " (UTF-8 encoding)\n")

# 1b. RData - Cancer-related projects (for faster loading)
period_rdata_file <- file.path(output_dir_tables, paste0("ntis2_period_reshape_cancer_", timestamp, ".RData"))
save(dt_final, file = period_rdata_file)
cat("1b) Cancer-related projects RData saved to:", period_rdata_file, " (for faster loading)\n")

# 2. CSV - All projects (UTF-8) - MAIN OUTPUT
all_projects_csv_file <- file.path(output_dir_tables, paste0("ntis2_period_reshape_all_", timestamp, ".csv"))
fwrite(dt_all_projects, all_projects_csv_file, encoding = "UTF-8")
cat("2) All projects CSV saved to:", all_projects_csv_file, " (UTF-8 encoding)\n")

# 2b. RData - All projects (for faster loading)
all_projects_rdata_file <- file.path(output_dir_tables, paste0("ntis2_period_reshape_all_", timestamp, ".RData"))
save(dt_all_projects, file = all_projects_rdata_file)
cat("2b) All projects RData saved to:", all_projects_rdata_file, " (for faster loading)\n")

# Sampling based on cancer category (cancer-related projects only)
set.seed(123)

# Sample for each cancer category
sample_uncertain <- dt_final[cancer_category == "uncertain"]
if (nrow(sample_uncertain) > 0) {
  sample_uncertain <- sample_uncertain[sample(.N, ceiling(.N * 0.05))] # 5% sample
}

sample_certain <- dt_final[cancer_category == "certain"]
if (nrow(sample_certain) > 0) {
  sample_certain <- sample_certain[sample(.N, ceiling(.N * 0.005))] # 0.5% sample
}

cat("Sampling completed (cancer-related projects only):\n")
cat("  Uncertain sample:", nrow(sample_uncertain), "rows\n")
cat("  Certain sample:", nrow(sample_certain), "rows\n")

# 3. Excel - Cancer uncertain sample with specific columns
if (nrow(sample_uncertain) > 0) {
  # Create representative columns from year-tagged columns
  # Get the most recent year for each period to create representative text
  sample_uncertain[, Text1_representative := NA_character_]
  sample_uncertain[, Text2_representative := NA_character_]
  sample_uncertain[, Key1_representative := NA_character_]
  sample_uncertain[, Key2_representative := NA_character_]

  # Fill representative columns with the most recent non-NA value for each period
  for (i in 1:nrow(sample_uncertain)) {
    period <- sample_uncertain$period[i]
    if (period == 1) years <- 2006:2010
    else if (period == 2) years <- 2011:2015
    else if (period == 3) years <- 2016:2020
    else if (period == 4) years <- 2021:2024

    # Get the most recent non-NA value for each text column
    for (col_base in c("Text1", "Text2", "Key1", "Key2")) {
      col_values <- c()
      for (year in rev(years)) {  # Start from most recent year
        col_name <- paste0(col_base, "_", year)
        if (col_name %in% names(sample_uncertain)) {
          value <- sample_uncertain[[col_name]][i]
          if (!is.na(value) && value != "") {
            col_values <- c(col_values, value)
          }
        }
      }
      if (length(col_values) > 0) {
        sample_uncertain[[paste0(col_base, "_representative")]][i] <- col_values[1]  # Use first (most recent) value
      }
    }
  }

  # Select specific columns for Excel output - include ALL Korean text columns and classification verification
  # Get all year-tagged Korean text columns
  korean_text_cols <- grep("^(Text1|Text2|Key1|Key2)_\\d{4}$", names(sample_uncertain), value = TRUE)

  # Get classification verification columns
  classification_cols <- c("cancer_total_hits", "Text1_cancer_hit", "Text2_cancer_hit", "Key1_cancer_hit", "Key2_cancer_hit")
  available_classification_cols <- classification_cols[classification_cols %in% names(sample_uncertain)]

  excel_columns_uncertain <- c("filenum", "NO_base", "period",
                               korean_text_cols,  # ALL year-tagged Korean text columns
                               "Text1_representative", "Text2_representative",
                               "Key1_representative", "Key2_representative",
                               available_classification_cols,  # Classification verification columns
                               "cancer_related", "cancer_count",
                               "cancer_any_in_period", "cancer_uncertain", "cancer_category")

  # Check which columns exist in the data
  available_columns_uncertain <- excel_columns_uncertain[excel_columns_uncertain %in% names(sample_uncertain)]
  missing_columns_uncertain <- excel_columns_uncertain[!excel_columns_uncertain %in% names(sample_uncertain)]

  if (length(missing_columns_uncertain) > 0) {
    cat("Warning: Some requested columns not found in uncertain sample:", paste(missing_columns_uncertain, collapse = ", "), "\n")
  }

  # Create Excel sample with available columns
  excel_sample_uncertain <- sample_uncertain[, ..available_columns_uncertain]

  # Verify Korean text in ALL columns
  cat("=== VERIFYING KOREAN TEXT IN ALL COLUMNS (UNCERTAIN) ===\n")

  # Check representative columns
  rep_cols <- c("Text1_representative", "Text2_representative", "Key1_representative", "Key2_representative")
  for (col in rep_cols) {
    if (col %in% names(excel_sample_uncertain)) {
      sample_texts <- head(excel_sample_uncertain[[col]], 3)
      korean_chars <- sum(grepl("[가-힣]", sample_texts, useBytes = TRUE), na.rm = TRUE)
      cat(sprintf("%s: %d Korean characters found in sample\n", col, korean_chars))

      # Show sample Korean text
      korean_samples <- sample_texts[grepl("[가-힣]", sample_texts, useBytes = TRUE)]
      if (length(korean_samples) > 0) {
        cat(sprintf("  Sample: %s\n", paste(head(korean_samples, 1), collapse = " | ")))
      }
    }
  }

  # Check year-tagged columns
  year_cols <- grep("^(Text1|Text2|Key1|Key2)_\\d{4}$", names(excel_sample_uncertain), value = TRUE)
  cat(sprintf("\nFound %d year-tagged Korean text columns: %s\n", length(year_cols), paste(head(year_cols, 4), collapse = ", ")))

  # Check a few sample year-tagged columns
  sample_year_cols <- head(year_cols, 4)
  for (col in sample_year_cols) {
    if (col %in% names(excel_sample_uncertain)) {
      sample_texts <- head(excel_sample_uncertain[[col]], 3)
      korean_chars <- sum(grepl("[가-힣]", sample_texts, useBytes = TRUE), na.rm = TRUE)
      cat(sprintf("%s: %d Korean characters found in sample\n", col, korean_chars))

      # Show sample Korean text
      korean_samples <- sample_texts[grepl("[가-힣]", sample_texts, useBytes = TRUE)]
      if (length(korean_samples) > 0) {
        cat(sprintf("  Sample: %s\n", paste(head(korean_samples, 1), collapse = " | ")))
      }
    }
  }

  # Verify classification columns
  cat("\n=== VERIFYING CLASSIFICATION COLUMNS ===\n")
  classification_cols <- c("cancer_total_hits", "Text1_cancer_hit", "Text2_cancer_hit", "Key1_cancer_hit", "Key2_cancer_hit", "cancer_category")
  for (col in classification_cols) {
    if (col %in% names(excel_sample_uncertain)) {
      sample_values <- head(excel_sample_uncertain[[col]], 5)
      cat(sprintf("%s: %s\n", col, paste(sample_values, collapse = " | ")))
    }
  }

  # Create Excel workbook for uncertain sample
  tryCatch({
    wb_uncertain <- createWorkbook()
    addWorksheet(wb_uncertain, "Cancer_Uncertain_Sample")
    writeData(wb_uncertain, sheet = "Cancer_Uncertain_Sample", excel_sample_uncertain)

    # Save Excel file for uncertain sample
    excel_file_uncertain <- file.path(output_dir_tables, paste0("ntis2_cancer_uncertain_sample_", format(Sys.time(), "%Y%m%d"), ".xlsx"))
    saveWorkbook(wb_uncertain, excel_file_uncertain, overwrite = TRUE)
    cat("3) Cancer uncertain sample Excel saved to:", excel_file_uncertain, " (columns: ", paste(available_columns_uncertain, collapse = ", "), ")\n")
  }, error = function(e) {
    cat("Warning: Failed to create Excel file for uncertain sample. Error:", e$message, "\n")
    cat("3) Excel creation failed - continuing with other outputs\n")
  })
} else {
  cat("3) No cancer uncertain projects found for Excel output\n")
}

# 4. Excel - Cancer certain sample with specific columns
if (nrow(sample_certain) > 0) {
  # Create representative columns from year-tagged columns
  # Get the most recent year for each period to create representative text
  sample_certain[, Text1_representative := NA_character_]
  sample_certain[, Text2_representative := NA_character_]
  sample_certain[, Key1_representative := NA_character_]
  sample_certain[, Key2_representative := NA_character_]

  # Fill representative columns with the most recent non-NA value for each period
  for (i in 1:nrow(sample_certain)) {
    period <- sample_certain$period[i]
    if (period == 1) years <- 2006:2010
    else if (period == 2) years <- 2011:2015
    else if (period == 3) years <- 2016:2020
    else if (period == 4) years <- 2021:2024

    # Get the most recent non-NA value for each text column
    for (col_base in c("Text1", "Text2", "Key1", "Key2")) {
      col_values <- c()
      for (year in rev(years)) {  # Start from most recent year
        col_name <- paste0(col_base, "_", year)
        if (col_name %in% names(sample_certain)) {
          value <- sample_certain[[col_name]][i]
          if (!is.na(value) && value != "") {
            col_values <- c(col_values, value)
          }
        }
      }
      if (length(col_values) > 0) {
        sample_certain[[paste0(col_base, "_representative")]][i] <- col_values[1]  # Use first (most recent) value
      }
    }
  }

  # Select specific columns for Excel output - include ALL Korean text columns and classification verification
  # Get all year-tagged Korean text columns
  korean_text_cols <- grep("^(Text1|Text2|Key1|Key2)_\\d{4}$", names(sample_certain), value = TRUE)

  # Get classification verification columns
  classification_cols <- c("cancer_total_hits", "Text1_cancer_hit", "Text2_cancer_hit", "Key1_cancer_hit", "Key2_cancer_hit")
  available_classification_cols <- classification_cols[classification_cols %in% names(sample_certain)]

  excel_columns_certain <- c("filenum", "NO_base", "period",
                             korean_text_cols,  # ALL year-tagged Korean text columns
                             "Text1_representative", "Text2_representative",
                             "Key1_representative", "Key2_representative",
                             available_classification_cols,  # Classification verification columns
                             "cancer_related", "cancer_count",
                             "cancer_any_in_period", "cancer_uncertain", "cancer_category")

  # Check which columns exist in the data
  available_columns_certain <- excel_columns_certain[excel_columns_certain %in% names(sample_certain)]
  missing_columns_certain <- excel_columns_certain[!excel_columns_certain %in% names(sample_certain)]

  if (length(missing_columns_certain) > 0) {
    cat("Warning: Some requested columns not found in certain sample:", paste(missing_columns_certain, collapse = ", "), "\n")
  }

  # Create Excel sample with available columns
  excel_sample_certain <- sample_certain[, ..available_columns_certain]

  # Verify Korean text in ALL columns
  cat("=== VERIFYING KOREAN TEXT IN ALL COLUMNS (CERTAIN) ===\n")

  # Check representative columns
  rep_cols <- c("Text1_representative", "Text2_representative", "Key1_representative", "Key2_representative")
  for (col in rep_cols) {
    if (col %in% names(excel_sample_certain)) {
      sample_texts <- head(excel_sample_certain[[col]], 3)
      korean_chars <- sum(grepl("[가-힣]", sample_texts, useBytes = TRUE), na.rm = TRUE)
      cat(sprintf("%s: %d Korean characters found in sample\n", col, korean_chars))

      # Show sample Korean text
      korean_samples <- sample_texts[grepl("[가-힣]", sample_texts, useBytes = TRUE)]
      if (length(korean_samples) > 0) {
        cat(sprintf("  Sample: %s\n", paste(head(korean_samples, 1), collapse = " | ")))
      }
    }
  }

  # Check year-tagged columns
  year_cols <- grep("^(Text1|Text2|Key1|Key2)_\\d{4}$", names(excel_sample_certain), value = TRUE)
  cat(sprintf("\nFound %d year-tagged Korean text columns: %s\n", length(year_cols), paste(head(year_cols, 4), collapse = ", ")))

  # Check a few sample year-tagged columns
  sample_year_cols <- head(year_cols, 4)
  for (col in sample_year_cols) {
    if (col %in% names(excel_sample_certain)) {
      sample_texts <- head(excel_sample_certain[[col]], 3)
      korean_chars <- sum(grepl("[가-힣]", sample_texts, useBytes = TRUE), na.rm = TRUE)
      cat(sprintf("%s: %d Korean characters found in sample\n", col, korean_chars))

      # Show sample Korean text
      korean_samples <- sample_texts[grepl("[가-힣]", sample_texts, useBytes = TRUE)]
      if (length(korean_samples) > 0) {
        cat(sprintf("  Sample: %s\n", paste(head(korean_samples, 1), collapse = " | ")))
      }
    }
  }

  # Verify classification columns
  cat("\n=== VERIFYING CLASSIFICATION COLUMNS ===\n")
  classification_cols <- c("cancer_total_hits", "Text1_cancer_hit", "Text2_cancer_hit", "Key1_cancer_hit", "Key2_cancer_hit", "cancer_category")
  for (col in classification_cols) {
    if (col %in% names(excel_sample_certain)) {
      sample_values <- head(excel_sample_certain[[col]], 5)
      cat(sprintf("%s: %s\n", col, paste(sample_values, collapse = " | ")))
    }
  }

  # Create Excel workbook for certain sample
  tryCatch({
    wb_certain <- createWorkbook()
    addWorksheet(wb_certain, "Cancer_Certain_Sample")
    writeData(wb_certain, sheet = "Cancer_Certain_Sample", excel_sample_certain)

    # Save Excel file for certain sample
    excel_file_certain <- file.path(output_dir_tables, paste0("ntis2_cancer_certain_sample_", format(Sys.time(), "%Y%m%d"), ".xlsx"))
    saveWorkbook(wb_certain, excel_file_certain, overwrite = TRUE)
    cat("4) Cancer certain sample Excel saved to:", excel_file_certain, " (columns: ", paste(available_columns_certain, collapse = ", "), ")\n")
  }, error = function(e) {
    cat("Warning: Failed to create Excel file for certain sample. Error:", e$message, "\n")
    cat("4) Excel creation failed - continuing with other outputs\n")
  })
} else {
  cat("4) No cancer certain projects found for Excel output\n")
}

# Summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")

# Calculate unique projects statistics
cat("=== UNIQUE PROJECTS SUMMARY ===\n")
# 1. Unique projects of total projects
total_unique_projects <- unique(data[, .(filenum, NO_base)])
cat("1) Unique projects of total projects:", nrow(total_unique_projects), "\n")

# 2. Unique projects of cancer related projects
cancer_unique_projects <- unique(data[cancer_related == TRUE, .(filenum, NO_base)])
cat("2) Unique projects of cancer related projects:", nrow(cancer_unique_projects), "\n")

# 2b. Unique projects breakdown by cancer category
cat("2b) Unique projects breakdown by cancer category:\n")
cat("   - Cancer unrelated:", sum(unique_projects_all$cancer_category == "unrelated"), "\n")
cat("   - Cancer related but uncertain:", sum(unique_projects_all$cancer_category == "uncertain"), "\n")
cat("   - Cancer related and certain:", sum(unique_projects_all$cancer_category == "certain"), "\n")
cat("   - Total cancer related (uncertain + certain):", sum(unique_projects_all$cancer_category != "unrelated"), "\n")

# Calculate period-based projects statistics
cat("\n=== PERIOD-BASED PROJECTS SUMMARY ===\n")
# 3. Period-based projects of total projects (sum of unique projects per period)
total_period_projects <- dt_all_projects[, .(unique_projects = length(unique(paste(filenum, NO_base)))), by = period][, sum(unique_projects)]
cat("3) Period-based projects of total projects (sum across periods):", total_period_projects, "\n")

# 4. Period-based projects of cancer related projects (sum of unique projects per period)
cancer_period_projects <- dt_final[, .(unique_projects = length(unique(paste(filenum, NO_base)))), by = period][, sum(unique_projects)]
cat("4) Period-based projects of cancer related projects (sum across periods):", cancer_period_projects, "\n")

# 5. Period-based cancer-related projects by period
cat("5) Period-based cancer-related projects by period:\n")
period_summary <- dt_final[, .(
  total_projects = length(unique(paste(filenum, NO_base))),
  uncertain_projects = length(unique(paste(filenum, NO_base)[cancer_category == "uncertain"])),
  certain_projects = length(unique(paste(filenum, NO_base)[cancer_category == "certain"]))
), by = period]
print(period_summary)

# Debug: Check if filtering is working properly
cat("Debug: dt_all_projects rows:", nrow(dt_all_projects), "\n")
cat("Debug: dt_final rows:", nrow(dt_final), "\n")
cat("Debug: cancer_any_in_period_reliable TRUE count in dt_all_projects:", sum(dt_all_projects$cancer_any_in_period_reliable == TRUE, na.rm = TRUE), "\n")

# Original stratified sampling summary
cat("\n=== STRATIFIED SAMPLING SUMMARY ===\n")
cat("Cancer classification system:\n")
cat("  - unrelated: cancer_total_hits = 0\n")
cat("  - uncertain: cancer_total_hits <= 1\n")
cat("  - certain: cancer_total_hits > 1\n")

# Summary by cancer category (cancer-related projects only)
cat("\n=== CANCER CATEGORY SUMMARY (Cancer-Related Projects Only) ===\n")
sumtab_category <- dt_final[, .(
  N = .N,
  mean_cancer_count = mean(cancer_count, na.rm = TRUE),
  mean_avg_cancer_count = mean(avg_cancer_count, na.rm = TRUE),
  mean_cancer_total_hits_mean = if ("cancer_total_hits_mean" %in% colnames(dt_final)) {
    mean(cancer_total_hits_mean, na.rm = TRUE)
  } else {
    NA_real_
  }
), by = cancer_category]
print(sumtab_category)

# Original summary by cancer_uncertain (for backward compatibility)
cat("\n=== CANCER UNCERTAINTY SUMMARY (Backward Compatibility) ===\n")
sumtab_uncertain <- dt_final[, .(
  N = .N,
  mean_cancer_count = mean(cancer_count, na.rm = TRUE),
  mean_avg_cancer_count = mean(avg_cancer_count, na.rm = TRUE)
), by = cancer_uncertain]
print(sumtab_uncertain)
cat("\nOverall N:", nrow(dt_final), "\n")
cat("Overall mean cancer_count:", mean(dt_final$cancer_count, na.rm = TRUE), "\n")
cat("Overall mean avg_cancer_count:", mean(dt_final$avg_cancer_count, na.rm = TRUE), "\n")

strat_end_time <- Sys.time()
cat("\nStratified sampling and summary computation time:", round(difftime(strat_end_time, strat_start_time, units = "secs"), 2), "seconds\n")

# =============================================================================
# CREATE CONCISE OUTPUT SUMMARY AS TEXT FILE
# =============================================================================

cat("\n=== CREATING CONCISE OUTPUT SUMMARY ===\n")

# Create summary text
summary_text <- paste0(
  "01_filter - OUTPUT SUMMARY\n",
  "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
  "=", strrep("=", 50), "\n\n",

  "1. UNIQUE PROJECTS SUMMARY\n",
  "-", strrep("-", 30), "\n",
  "Total unique projects: ", nrow(total_unique_projects), "\n",
  "Cancer-related unique projects: ", nrow(cancer_unique_projects), "\n",
  "Cancer unrelated: ", sum(unique_projects_all$cancer_category == "unrelated"), "\n",
  "Cancer uncertain: ", sum(unique_projects_all$cancer_category == "uncertain"), "\n",
  "Cancer certain: ", sum(unique_projects_all$cancer_category == "certain"), "\n\n",

  "2. PERIOD-BASED PROJECTS SUMMARY\n",
  "-", strrep("-", 30), "\n",
  "Total period-based projects: ", total_period_projects, "\n",
  "Cancer-related period-based projects: ", cancer_period_projects, "\n\n",

  "3. PERIOD-BY-PERIOD CANCER PROJECTS\n",
  "-", strrep("-", 30), "\n"
)

# Add period summary
for (i in 1:nrow(period_summary)) {
  p <- period_summary[i]
  summary_text <- paste0(summary_text,
    "Period ", p$period, " (",
    ifelse(p$period == 1, "2006-2010",
           ifelse(p$period == 2, "2011-2015",
                  ifelse(p$period == 3, "2016-2020", "2021-2024"))), "): ",
    p$total_projects, " total (", p$uncertain_projects, " uncertain, ", p$certain_projects, " certain)\n"
  )
}

# Add sampling summary
summary_text <- paste0(summary_text, "\n",
  "4. SAMPLING SUMMARY\n",
  "-", strrep("-", 30), "\n",
  "Uncertain sample: ", nrow(sample_uncertain), " rows (5% of ", sum(dt_final$cancer_category == "uncertain", na.rm = TRUE), ")\n",
  "Certain sample: ", nrow(sample_certain), " rows (0.5% of ", sum(dt_final$cancer_category == "certain", na.rm = TRUE), ")\n\n",

  "5. CANCER CLASSIFICATION CRITERIA\n",
  "-", strrep("-", 30), "\n",
  "Unrelated: cancer_total_hits = 0\n",
  "Uncertain: cancer_total_hits <= 1\n",
  "Certain: cancer_total_hits > 1\n\n",

  "6. OUTPUT FILES (UTF-8 ENCODING)\n",
  "-", strrep("-", 30), "\n",
  "1) Cancer-related projects: ", period_csv_file, "\n",
  "2) All projects: ", all_projects_csv_file, "\n",
  "3) Cancer uncertain sample Excel: ntis2_cancer_uncertain_sample_", format(Sys.time(), "%Y%m%d"), ".xlsx\n",
  "4) Cancer certain sample Excel: ntis2_cancer_certain_sample_", format(Sys.time(), "%Y%m%d"), ".xlsx\n\n",

  "7. SUMMARY ANALYSIS TABLES (EXCEL FORMAT - BETTER FOR KOREAN TEXT)\n",
  "-", strrep("-", 30), "\n",
  "All 6 tables saved in single Excel file: ntis2_all_summary_tables_", format(Sys.time(), "%Y%m%d"), ".xlsx\n",
  "  - Table 1: Money by Year and Cancer Classification\n",
  "  - Table 2: Projects by Year and Cancer Classification\n",
  "  - Table 3: Money by Year and 6T Technological Classification\n",
  "  - Table 4: Projects by Year and 6T Technological Classification\n",
  "  - Table 5: Money by Year and R&D Period\n",
  "  - Table 6: Projects by Year and R&D Period\n\n",

  "8. PROCESSING TIME\n",
  "-", strrep("-", 30), "\n",
  "Total processing time: ", round(difftime(strat_end_time, strat_start_time, units = "secs"), 2), " seconds\n"
)

# Write summary to file
summary_file <- file.path(output_dir_tables, paste0("ntis2_summary_report_", format(Sys.time(), "%Y%m%d"), ".txt"))
writeLines(summary_text, summary_file)
cat("Concise summary saved to:", summary_file, "\n")

# =============================================================================
# CREATE 6 SUMMARY TABLES FOR ANALYSIS
# =============================================================================

cat("\n=== CREATING 6 SUMMARY TABLES ===\n")

# Verify data.table is loaded for dcast function
if (!require(data.table, quietly = TRUE)) {
  stop("data.table package is required for table creation. Please install and load it.")
}

# Debug: Check data structure from step 00
cat("Debug: step 00 Data structure check:\n")
cat("  - Original data dimensions:", nrow(data), "rows x", ncol(data), "columns\n")
cat("  - Data class:", class(data), "\n")
cat("  - Key columns present:",
    paste(c("Year", "cancer_related", "cancer_total_hits", "MoneyC", "Class_D", "Class_I") %in% colnames(data), collapse = ", "), "\n")

# Verify that step 00 output columns contain actual data
cat("\n=== VERIFYING step 00 OUTPUT COLUMNS ===\n")
if ("MoneyC" %in% colnames(data)) {
  money_summary_stats <- summary(data$MoneyC)
  cat("MoneyC column summary:\n")
  print(money_summary_stats)
} else {
  cat("Warning: MoneyC column not found in step 00 output\n")
}

if ("Class_D" %in% colnames(data)) {
  class_d_counts <- table(data$Class_D, useNA = "ifany")
  cat("Class_D column distribution:\n")
  print(head(class_d_counts, 10))
} else {
  cat("Warning: Class_D column not found in step 00 output\n")
}

if ("Class_I" %in% colnames(data)) {
  class_i_counts <- table(data$Class_I, useNA = "ifany")
  cat("Class_I column distribution:\n")
  print(head(class_i_counts, 10))
} else {
  cat("Warning: Class_I column not found in step 00 output\n")
}

# =============================================================================
# TABLE 1: TOTAL MONEY (MoneyC) BY YEAR AND CANCER CLASSIFICATION
# =============================================================================

cat("Creating Table 1: Total Money (MoneyC) by Year and Cancer Classification...\n")

# Ensure MoneyC column exists and is numeric
if (!"MoneyC" %in% colnames(data)) {
  cat("Error: MoneyC column not found in step 00 output. Cannot create money summary tables.\n")
  stop("Required column MoneyC missing from step 00 output")
} else {
  # Convert MoneyC to numeric, handling any non-numeric values
  data[, MoneyC := as.numeric(as.character(MoneyC))]
  data[is.na(MoneyC), MoneyC := 0]
  cat("MoneyC column processed successfully. Range:", min(data$MoneyC, na.rm = TRUE), "to", max(data$MoneyC, na.rm = TRUE), "\n")
}

# Verify required columns exist for cancer classification
required_cols <- c("cancer_related", "cancer_total_hits")
missing_cols <- required_cols[!required_cols %in% colnames(data)]
if (length(missing_cols) > 0) {
  cat("Error: Missing required columns for cancer classification:", paste(missing_cols, collapse = ", "), "\n")
  cat("These columns should be created by 00_build_classification.R\n")
  stop("Required cancer classification columns missing from step 00 output")
} else {
  cat("All required cancer classification columns found in step 00 output\n")
}

# Create year-wise summary for money
money_summary <- data[, .(
  total_money = sum(MoneyC, na.rm = TRUE),
  total_projects = .N,
  cancer_related_money = sum(MoneyC[cancer_related == TRUE], na.rm = TRUE),
  cancer_related_projects = sum(cancer_related == TRUE, na.rm = TRUE),
  cancer_certain_money = sum(MoneyC[cancer_total_hits > 1], na.rm = TRUE),
  cancer_certain_projects = sum(cancer_total_hits > 1, na.rm = TRUE),
  cancer_uncertain_money = sum(MoneyC[cancer_total_hits == 1], na.rm = TRUE),
  cancer_uncertain_projects = sum(cancer_total_hits == 1, na.rm = TRUE)
), by = Year]

# Calculate proportions
money_summary[, `:=`(
  cancer_related_proportion = ifelse(total_projects > 0, round(cancer_related_projects / total_projects * 100, 2), 0),
  cancer_certain_proportion = ifelse(total_projects > 0, round(cancer_certain_projects / total_projects * 100, 2), 0),
  cancer_uncertain_proportion = ifelse(total_projects > 0, round(cancer_uncertain_projects / total_projects * 100, 2), 0)
)]

# Reorder columns for Table 1
table1_money <- money_summary[, .(
  Year,
  All_Projects_Money = total_money,
  Cancer_Related_Money = cancer_related_money,
  Cancer_Related_Proportion = cancer_related_proportion,
  Cancer_Certain_Money = cancer_certain_money,
  Cancer_Certain_Proportion = cancer_certain_proportion,
  Cancer_Uncertain_Money = cancer_uncertain_money,
  Cancer_Uncertain_Proportion = cancer_uncertain_proportion
)]

# Sort by year
setorder(table1_money, Year)

# =============================================================================
# TABLE 2: TOTAL NUMBER OF PROJECTS BY YEAR AND CANCER CLASSIFICATION
# =============================================================================

cat("Creating Table 2: Total Number of Projects by Year and Cancer Classification...\n")

# Create year-wise summary for project counts
project_summary <- data[, .(
  total_projects = .N,
  cancer_related_projects = sum(cancer_related == TRUE, na.rm = TRUE),
  cancer_certain_projects = sum(cancer_total_hits > 1, na.rm = TRUE),
  cancer_uncertain_projects = sum(cancer_total_hits == 1, na.rm = TRUE)
), by = Year]

# Calculate proportions
project_summary[, `:=`(
  cancer_related_proportion = ifelse(total_projects > 0, round(cancer_related_projects / total_projects * 100, 2), 0),
  cancer_certain_proportion = ifelse(total_projects > 0, round(cancer_certain_projects / total_projects * 100, 2), 0),
  cancer_uncertain_proportion = ifelse(total_projects > 0, round(cancer_uncertain_projects / total_projects * 100, 2), 0)
)]

# Reorder columns for Table 2
table2_projects <- project_summary[, .(
  Year,
  All_Projects_Count = total_projects,
  Cancer_Related_Count = cancer_related_projects,
  Cancer_Related_Proportion = cancer_related_proportion,
  Cancer_Certain_Count = cancer_certain_projects,
  Cancer_Certain_Proportion = cancer_certain_proportion,
  Cancer_Uncertain_Count = cancer_uncertain_projects,
  Cancer_Uncertain_Proportion = cancer_uncertain_proportion
)]

# Sort by year
setorder(table2_projects, Year)

# =============================================================================
# TABLE 3: TOTAL MONEY (MoneyC) BY YEAR AND 6T TECHNOLOGICAL CLASSIFICATION (Class_D)
# =============================================================================

cat("Creating Table 3: Total Money (MoneyC) by Year and 6T Technological Classification (Cancer-Related Projects Only)...\n")

# Check if Class_D column exists
if (!"Class_D" %in% colnames(data)) {
  cat("Error: Class_D column not found in step 00 output. Cannot create 6T technological classification tables.\n")
  stop("Required column Class_D missing from step 00 output")
}

# Get unique 6T classifications and clean them (cancer-related projects only)
cancer_data <- data[cancer_related == TRUE]
class_d_values <- unique(cancer_data$Class_D)
class_d_values <- class_d_values[!is.na(class_d_values) & class_d_values != ""]
cat("Found 6T technological classifications in cancer-related projects:", paste(class_d_values, collapse = ", "), "\n")

# Clean Class_D values - replace NA/empty with "기타" (cancer-related projects only)
cancer_data[, Class_D_clean := ifelse(is.na(Class_D) | Class_D == "", "기타", as.character(Class_D))]

# Create year-wise summary for money by Class_D (cancer-related projects only)
money_class_d <- cancer_data[, .(
  total_money = sum(MoneyC, na.rm = TRUE)
), by = .(Year, Class_D_clean)]

# Handle case where no data exists
if (nrow(money_class_d) == 0) {
  cat("Error: No data available for Class_D analysis. Check if Class_D column has valid values.\n")
  stop("Cannot create Class_D money table - no valid data")
} else {
  # Reshape to wide format
  tryCatch({
    table3_money_class_d <- dcast(money_class_d, Year ~ Class_D_clean, value.var = "total_money", fill = 0)
    cat("Class_D money table created successfully with", ncol(table3_money_class_d)-1, "classifications\n")
  }, error = function(e) {
    cat("Error in dcast operation for Class_D:", e$message, "\n")
    stop("Failed to create Class_D money table")
  })
}

# Sort by year (only if table has data)
if (nrow(table3_money_class_d) > 0) {
  setorder(table3_money_class_d, Year)
}

# =============================================================================
# TABLE 4: TOTAL NUMBER OF PROJECTS BY YEAR AND 6T TECHNOLOGICAL CLASSIFICATION (Class_D)
# =============================================================================

cat("Creating Table 4: Total Number of Projects by Year and 6T Technological Classification (Cancer-Related Projects Only)...\n")

# Create year-wise summary for project counts by Class_D (cancer-related projects only)
projects_class_d <- cancer_data[, .(
  total_projects = .N
), by = .(Year, Class_D_clean)]

# Handle case where no data exists
if (nrow(projects_class_d) == 0) {
  cat("Error: No data available for Class_D project analysis. Check if Class_D column has valid values.\n")
  stop("Cannot create Class_D projects table - no valid data")
} else {
  # Reshape to wide format
  tryCatch({
    table4_projects_class_d <- dcast(projects_class_d, Year ~ Class_D_clean, value.var = "total_projects", fill = 0)
    cat("Class_D projects table created successfully with", ncol(table4_projects_class_d)-1, "classifications\n")
  }, error = function(e) {
    cat("Error in dcast operation for Class_D projects:", e$message, "\n")
    stop("Failed to create Class_D projects table")
  })
}

# Sort by year (only if table has data)
if (nrow(table4_projects_class_d) > 0) {
  setorder(table4_projects_class_d, Year)
}

# =============================================================================
# TABLE 5: TOTAL MONEY (MoneyC) BY YEAR AND R&D PERIOD (Class_I)
# =============================================================================

cat("Creating Table 5: Total Money (MoneyC) by Year and R&D Period (Cancer-Related Projects Only)...\n")

# Check if Class_I column exists
if (!"Class_I" %in% colnames(data)) {
  cat("Error: Class_I column not found in step 00 output. Cannot create R&D period tables.\n")
  stop("Required column Class_I missing from step 00 output")
}

# Get unique R&D period classifications and clean them (cancer-related projects only)
class_i_values <- unique(cancer_data$Class_I)
class_i_values <- class_i_values[!is.na(class_i_values) & class_i_values != ""]
cat("Found R&D period classifications in cancer-related projects:", paste(class_i_values, collapse = ", "), "\n")

# Clean Class_I values - replace NA/empty with "기타" (cancer-related projects only)
cancer_data[, Class_I_clean := ifelse(is.na(Class_I) | is.null(Class_I) | Class_I == "", "기타", as.character(Class_I))]

# Create year-wise summary for money by Class_I (cancer-related projects only)
money_class_i <- cancer_data[, .(
  total_money = sum(MoneyC, na.rm = TRUE)
), by = .(Year, Class_I_clean)]

# Handle case where no data exists
if (nrow(money_class_i) == 0) {
  cat("Error: No data available for Class_I money analysis. Check if Class_I column has valid values.\n")
  stop("Cannot create Class_I money table - no valid data")
} else {
  # Reshape to wide format
  tryCatch({
    table5_money_class_i <- dcast(money_class_i, Year ~ Class_I_clean, value.var = "total_money", fill = 0)
    cat("Class_I money table created successfully with", ncol(table5_money_class_i)-1, "periods\n")
  }, error = function(e) {
    cat("Error in dcast operation for Class_I money:", e$message, "\n")
    stop("Failed to create Class_I money table")
  })
}

# Sort by year (only if table has data)
if (nrow(table5_money_class_i) > 0) {
  setorder(table5_money_class_i, Year)
}

# =============================================================================
# TABLE 6: TOTAL NUMBER OF PROJECTS BY YEAR AND R&D PERIOD (Class_I)
# =============================================================================

cat("Creating Table 6: Total Number of Projects by Year and R&D Period (Cancer-Related Projects Only)...\n")

# Create year-wise summary for project counts by Class_I (cancer-related projects only)
projects_class_i <- cancer_data[, .(
  total_projects = .N
), by = .(Year, Class_I_clean)]

# Handle case where no data exists
if (nrow(projects_class_i) == 0) {
  cat("Error: No data available for Class_I project analysis. Check if Class_I column has valid values.\n")
  stop("Cannot create Class_I projects table - no valid data")
} else {
  # Reshape to wide format
  tryCatch({
    table6_projects_class_i <- dcast(projects_class_i, Year ~ Class_I_clean, value.var = "total_projects", fill = 0)
    cat("Class_I projects table created successfully with", ncol(table6_projects_class_i)-1, "periods\n")
  }, error = function(e) {
    cat("Error in dcast operation for Class_I projects:", e$message, "\n")
    stop("Failed to create Class_I projects table")
  })
}

# Sort by year (only if table has data)
if (nrow(table6_projects_class_i) > 0) {
  setorder(table6_projects_class_i, Year)
}

# =============================================================================
# SAVE ALL TABLES TO EXCEL FILES (BETTER FOR KOREAN TEXT)
# =============================================================================

cat("\n=== SAVING SUMMARY TABLES TO EXCEL ===\n")

# Create a single Excel workbook with all 6 tables
tryCatch({
  wb_summary <- createWorkbook()

  # Add Table 1: Money by Year and Cancer Classification
  addWorksheet(wb_summary, "Table1_Money_Cancer")
  writeData(wb_summary, sheet = "Table1_Money_Cancer", table1_money)
  cat("1) Money by Year and Cancer Classification added to Excel\n")

  # Add Table 2: Projects by Year and Cancer Classification
  addWorksheet(wb_summary, "Table2_Projects_Cancer")
  writeData(wb_summary, sheet = "Table2_Projects_Cancer", table2_projects)
  cat("2) Projects by Year and Cancer Classification added to Excel\n")

  # Add Table 3: Money by Year and 6T Technological Classification
  addWorksheet(wb_summary, "Table3_Money_6T")
  writeData(wb_summary, sheet = "Table3_Money_6T", table3_money_class_d)
  cat("3) Money by Year and 6T Technological Classification added to Excel\n")

  # Add Table 4 and 5 to workbook (needed for pipeline: step 02 reads Table5 for Table 2 / Figure 2; not used in paper)
  addWorksheet(wb_summary, "Table4_Projects_6T")
  writeData(wb_summary, sheet = "Table4_Projects_6T", table4_projects_class_d)
  addWorksheet(wb_summary, "Table5_Money_RnD")
  writeData(wb_summary, sheet = "Table5_Money_RnD", table5_money_class_i)

  # Add Table 6: Projects by Year and R&D Period
  addWorksheet(wb_summary, "Table6_Projects_RnD")
  writeData(wb_summary, sheet = "Table6_Projects_RnD", table6_projects_class_i)
  cat("6) Projects by Year and R&D Period added to Excel\n")

  # Save the Excel workbook
  excel_summary_file <- file.path(output_dir_tables, paste0("ntis2_all_summary_tables_", format(Sys.time(), "%Y%m%d"), ".xlsx"))
  saveWorkbook(wb_summary, excel_summary_file, overwrite = TRUE)
  cat("Summary tables (1–6) saved to Excel file:", excel_summary_file, "\n")
  cat("  (Table 4 and 5 in workbook for pipeline only; not used in paper.)\n")

}, error = function(e) {
  cat("Warning: Failed to create Excel summary file. Error:", e$message, "\n")
  cat("Falling back to CSV files...\n")

  # Fallback to CSV if Excel fails
  table1_file <- file.path(output_dir_tables, paste0("ntis2_table1_money_cancer_by_year_", format(Sys.time(), "%Y%m%d"), ".csv"))
  fwrite(table1_money, table1_file, encoding = "UTF-8")
  cat("1) Money by Year and Cancer Classification saved to CSV:", table1_file, "\n")

  table2_file <- file.path(output_dir_tables, paste0("ntis2_table2_projects_cancer_by_year_", format(Sys.time(), "%Y%m%d"), ".csv"))
  fwrite(table2_projects, table2_file, encoding = "UTF-8")
  cat("2) Projects by Year and Cancer Classification saved to CSV:", table2_file, "\n")

  table3_file <- file.path(output_dir_tables, paste0("ntis2_table3_money_6T_by_year_", format(Sys.time(), "%Y%m%d"), ".csv"))
  fwrite(table3_money_class_d, table3_file, encoding = "UTF-8")
  cat("3) Money by Year and 6T Technological Classification saved to CSV:", table3_file, "\n")

  # (Table 4 and Table 5 not used in paper - not saved.)

  table6_file <- file.path(output_dir_tables, paste0("ntis2_table6_projects_RnD_by_year_", format(Sys.time(), "%Y%m%d"), ".csv"))
  fwrite(table6_projects_class_i, table6_file, encoding = "UTF-8")
  cat("6) Projects by Year and R&D Period saved to CSV:", table6_file, "\n")
})

# =============================================================================
# DISPLAY SUMMARY OF TABLES
# =============================================================================

# =============================================================================
# SAVE YEARLY STATISTICS
# =============================================================================

cat("\n=== SAVING YEARLY STATISTICS FOR step 04 ===\n")

# Create yearly statistics summary for step 04
yearly_total_stats <- data.frame(
  Year = table2_projects$Year,
  total_projects = table2_projects$All_Projects_Count,
  total_cancer_projects = table2_projects$Cancer_Related_Count,
  total_money = if ("MoneyC" %in% colnames(data)) table1_money$All_Projects_Money else NA,
  total_cancer_money = if ("MoneyC" %in% colnames(data)) table1_money$Cancer_Related_Money else NA
)

# Save yearly statistics as RData
yearly_stats_rdata_file <- file.path(output_dir_tables, paste0("ntis2_yearly_total_stats_", timestamp, ".RData"))
save(yearly_total_stats, file = yearly_stats_rdata_file)
cat("Yearly total statistics saved to:", yearly_stats_rdata_file, "\n")

# Also save as CSV for reference
yearly_stats_csv_file <- file.path(output_dir_tables, paste0("ntis2_yearly_total_stats_", timestamp, ".csv"))
fwrite(yearly_total_stats, yearly_stats_csv_file, encoding = "UTF-8")
cat("Yearly total statistics also saved to:", yearly_stats_csv_file, " (CSV format)\n")

# =============================================================================
# CREATE FIGURE 1 & FIGURE 2 (MAIN PAPER FIGURES FROM step 01 OUTPUT)
# =============================================================================

cat("\n=== CREATING FIGURE 1 & FIGURE 2 FROM step 01 OUTPUT ===\n")

# Ensure figures output directory exists
output_dir_figures <- OUT_FIGURES
if (!dir.exists(output_dir_figures)) dir.create(output_dir_figures, recursive = TRUE)

## ---------------------------------------------------------------------------
## FIGURE 1: Cancer-Related Funding (Trillion KRW) & Proportion (%), by Year
## ---------------------------------------------------------------------------

if ("total_cancer_money" %in% colnames(yearly_total_stats) &&
    "total_money" %in% colnames(yearly_total_stats)) {

  fig1_data <- yearly_total_stats

  # Remove years with missing totals
  fig1_data <- fig1_data[!is.na(fig1_data$total_money) & fig1_data$total_money > 0, ]

  if (nrow(fig1_data) > 0) {
    # Funding in trillion KRW
    fig1_data$cancer_funding_trillion <- fig1_data$total_cancer_money / 1e12
    # Proportion (%)
    fig1_data$cancer_funding_prop <- (fig1_data$total_cancer_money / fig1_data$total_money) * 100

    # For dual y-axis scaling, map proportion to roughly same numeric range
    max_funding <- max(fig1_data$cancer_funding_trillion, na.rm = TRUE)
    max_prop <- max(fig1_data$cancer_funding_prop, na.rm = TRUE)
    scale_factor <- if (!is.na(max_prop) && max_prop > 0) max_funding / max_prop else 1

    fig1_plot <- ggplot(fig1_data, aes(x = Year)) +
      # Left axis: Cancer-related funding (Trillion KRW)
      geom_line(aes(y = cancer_funding_trillion, color = "Funding"), linewidth = 1.1) +
      geom_point(aes(y = cancer_funding_trillion, color = "Funding"), size = 2.5) +
      # Right axis: Cancer-related proportion (%), scaled
      geom_line(aes(y = cancer_funding_prop * scale_factor, color = "Proportion"),
                linewidth = 1.1, linetype = "longdash") +
      geom_point(aes(y = cancer_funding_prop * scale_factor, color = "Proportion"),
                 size = 2.5, shape = 17) +
      # Text labels for proportion (%)
      geom_text(
        aes(y = cancer_funding_prop * scale_factor,
            label = sprintf("%.1f%%", cancer_funding_prop),
            color = "Proportion"),
        vjust = -0.8, size = 3
      ) +
      scale_x_continuous(breaks = seq(min(fig1_data$Year, na.rm = TRUE),
                                      max(fig1_data$Year, na.rm = TRUE), by = 2)) +
      scale_y_continuous(
        name = "Cancer-Related Funding (Trillion KRW)",
        limits = c(0, NA),
        sec.axis = sec_axis(~ . / scale_factor,
                            name = "Cancer-Related Proportion (%)",
                            labels = function(x) sprintf("%.1f", x))
      ) +
      scale_color_manual(
        name = NULL,
        values = c("Funding" = "#2484B8", "Proportion" = "#C03B8E"),
        labels = c("Cancer-Related Funding", "Cancer-Related Proportion")
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold", color = "#2484B8"),
        axis.title.y.right = element_text(size = 11, face = "bold", color = "#C03B8E"),
        axis.text = element_text(size = 9),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
      ) +
      labs(
        title = "Cancer-Related Funding and Proportion by Year",
        x = "Year"
      )

    fig1_file <- file.path(output_dir_figures,
                           paste0("ntis2_cancer_related_funding_", timestamp, ".png"))
    ggsave(fig1_file, fig1_plot, width = 9, height = 6, dpi = 300)
    cat("step 01 funding plot (not Figure 1) saved to:", fig1_file, "\n")
  } else {
    cat("Figure 1 not created: yearly_total_stats has no valid rows.\n")
  }
} else {
  cat("Figure 1 not created: yearly_total_stats missing total_money / total_cancer_money.\n")
}

## ---------------------------------------------------------------------------
## FIGURE 2: R&D Funding Composition by Year (Basic / Applied / Development / Others)
## ---------------------------------------------------------------------------

if (exists("table5_money_class_i") && nrow(table5_money_class_i) > 0) {
  fig2_data <- as.data.table(table5_money_class_i)

  # Identify R&D period columns (exclude Year)
  rd_cols <- setdiff(colnames(fig2_data), "Year")

  if (length(rd_cols) >= 2) {
    # Melt to long format
    fig2_long <- melt(
      fig2_data,
      id.vars = "Year",
      measure.vars = rd_cols,
      variable.name = "RD_Type",
      value.name = "Money"
    )

    # Replace NA with 0
    fig2_long[is.na(Money), Money := 0]

    # Compute proportion within each year
    fig2_long[, Year_Total := sum(Money), by = Year]
    fig2_long[, Prop := ifelse(Year_Total > 0, Money / Year_Total * 100, 0)]

    # Map RD_Type to English labels where possible
    rd_labels_map <- c(
      "기초연구" = "Basic Research",
      "응용연구" = "Applied Research",
      "개발연구" = "Development Research",
      "기타"     = "Others"
    )
    fig2_long[, RD_Type_Eng := ifelse(RD_Type %in% names(rd_labels_map),
                                      rd_labels_map[RD_Type],
                                      as.character(RD_Type))]

    fig2_plot <- ggplot(fig2_long, aes(x = Year, y = Prop, fill = RD_Type_Eng)) +
      geom_col(width = 0.8, position = "stack", color = NA) +
      geom_text(
        aes(label = ifelse(Prop > 0, sprintf("%.0f", Prop), "")),
        position = position_stack(vjust = 0.5),
        size = 3,
        color = "white",
        fontface = "bold"
      ) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0), limits = c(0, 100)) +
      scale_x_continuous(breaks = seq(min(fig2_long$Year, na.rm = TRUE),
                                      max(fig2_long$Year, na.rm = TRUE), by = 2)) +
      scale_fill_manual(
        name = NULL,
        values = c(
          "Basic Research" = "#2C9DCB",
          "Applied Research" = "#F5A623",
          "Development Research" = "#C03B8E",
          "Others" = "#8B572A"
        )
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 9),
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank()
      ) +
      labs(
        title = "R&D Funding Composition for Cancer-Related Projects",
        x = "Year",
        y = "R&D Funding (%)"
      )

    fig2_file <- file.path(output_dir_figures,
                           paste0("ntis2_rd_funding_composition_", timestamp, ".png"))
    ggsave(fig2_file, fig2_plot, width = 11, height = 7, dpi = 300)
    cat("step 01 R&D composition plot (not Figure 2) saved to:", fig2_file, "\n")
  } else {
    cat("Figure 2 not created: table5_money_class_i has insufficient R&D period columns.\n")
  }
} else {
  cat("Figure 2 not created: table5_money_class_i not available.\n")
}

# =============================================================================
# SAVE C0/C1/C2 CLASSIFICATION TO EXCEL
# =============================================================================

cat("\n=== SAVING C0/C1/C2 CLASSIFICATION TO EXCEL ===\n")

tryCatch({
  # Create Excel workbook for C0/C1/C2 classification
  wb_classification <- createWorkbook()

  # 1. All unique projects (full period) - C0/C1/C2
  # Select relevant columns for output
  output_cols_all <- c("filenum", "NO_base", "cancer_related",
                       "cancer_total_hits", "cancer_total_hits_mean", "cancer_total_hits_min", "cancer_total_hits_max",
                       "cancer_category", "classification", "num_cancer_types", "total_years")
  # Add cancer type columns
  if (exists("cancer_type_cols") && length(cancer_type_cols) > 0) {
    output_cols_all <- c(output_cols_all, cancer_type_cols)
  }
  available_cols_all <- output_cols_all[output_cols_all %in% colnames(unique_projects_all)]

  unique_projects_output_all <- unique_projects_all[, ..available_cols_all]

  addWorksheet(wb_classification, "All_Unique_Projects")
  writeData(wb_classification, sheet = "All_Unique_Projects", unique_projects_output_all)
  cat("1) All unique projects (full period) C0/C1/C2 saved to Excel\n")

  # 2. Epoch-based unique projects (by period) - C0/C1/C2
  # For each period, create unique projects
  cat("\nCalculating epoch-based (period-based) unique projects...\n")

  epoch_projects_list <- list()
  for (p in names(periods)) {
    yrs <- periods[[p]]
    # Subset data to this period
    period_data <- data[Year %in% yrs]

    if (nrow(period_data) > 0) {
      # Create unique projects for this period using ORIGINAL LOGIC (first cancer_total_hits)
      epoch_unique <- period_data[, .(
        filenum = first(filenum),
        NO_base = first(NO_base),
        cancer_total_hits = first(cancer_total_hits),  # ORIGINAL LOGIC: first value
        cancer_total_hits_mean = mean(cancer_total_hits, na.rm = TRUE),  # For reference
        cancer_total_hits_min = min(cancer_total_hits, na.rm = TRUE),
        cancer_total_hits_max = max(cancer_total_hits, na.rm = TRUE),
        total_years = .N,
        # Combine text fields for this period
        Text1_combined = paste(na.omit(unique(Text1[!is.na(Text1) & Text1 != ""])), collapse = " "),
        Text2_combined = paste(na.omit(unique(Text2[!is.na(Text2) & Text2 != ""])), collapse = " "),
        Key1_combined = paste(na.omit(unique(Key1[!is.na(Key1) & Key1 != ""])), collapse = " "),
        Key2_combined = paste(na.omit(unique(Key2[!is.na(Key2) & Key2 != ""])), collapse = " "),
        cancer_related = any(cancer_related == TRUE, na.rm = TRUE)
      ), by = .(filenum, NO_base)]

      # Calculate C0/C1/C2 for this epoch based on MEAN cancer_total_hits:
      # C0: mean = 0, C1: 0 < mean < 1, C2: mean >= 1
      epoch_unique[, classification := ifelse(cancer_total_hits_mean == 0, "C0",
                                               ifelse(cancer_total_hits_mean >= 1, "C2", "C1"))]

      # Perform cancer type classification for this epoch (for reference, not for C0/C1/C2)
      for (cancer_type in names(cancer_types_list)) {
        keyword_list <- cancer_types_list[[cancer_type]]
        korean_keywords <- keyword_list$korean
        english_keywords <- keyword_list$english

        type_matches <- logical(nrow(epoch_unique))

        if (length(korean_keywords) > 0) {
          if ("Text1_combined" %in% colnames(epoch_unique)) {
            korean_matches_text1 <- check_cancer_type(epoch_unique[["Text1_combined"]], korean_keywords)
            type_matches <- type_matches | korean_matches_text1
          }
          if ("Key1_combined" %in% colnames(epoch_unique)) {
            korean_matches_key1 <- check_cancer_type(epoch_unique[["Key1_combined"]], korean_keywords)
            type_matches <- type_matches | korean_matches_key1
          }
        }

        if (length(english_keywords) > 0) {
          if ("Text2_combined" %in% colnames(epoch_unique)) {
            english_matches_text2 <- check_cancer_type(epoch_unique[["Text2_combined"]], english_keywords)
            type_matches <- type_matches | english_matches_text2
          }
          if ("Key2_combined" %in% colnames(epoch_unique)) {
            english_matches_key2 <- check_cancer_type(epoch_unique[["Key2_combined"]], english_keywords)
            type_matches <- type_matches | english_matches_key2
          }
        }

        epoch_unique[[paste0("type_", cancer_type)]] <- type_matches
      }

      # Calculate number of cancer types (for reference, separate from C0/C1/C2)
      epoch_cancer_type_cols <- grep("^type_", colnames(epoch_unique), value = TRUE)
      if (length(epoch_cancer_type_cols) > 0) {
        epoch_unique[, num_cancer_types := rowSums(.SD, na.rm = TRUE), .SDcols = epoch_cancer_type_cols]
      } else {
        epoch_unique[, num_cancer_types := 0]
      }

      epoch_unique[, period := as.integer(p)]
      epoch_unique[, period_years := paste(yrs, collapse = "-")]

      epoch_projects_list[[p]] <- epoch_unique

      cat(sprintf("  Period %s (%s): %d unique projects\n", p, paste(yrs, collapse = "-"), nrow(epoch_unique)))
      cat(sprintf("    C0: %d, C1: %d, C2: %d\n",
                  sum(epoch_unique$classification == "C0"),
                  sum(epoch_unique$classification == "C1"),
                  sum(epoch_unique$classification == "C2")))
    }
  }

  # Combine all epoch projects
  if (length(epoch_projects_list) > 0) {
    epoch_projects_all <- rbindlist(epoch_projects_list, use.names = TRUE, fill = TRUE)

    # Select relevant columns for epoch output
    output_cols_epoch <- c("period", "period_years", "filenum", "NO_base", "cancer_related",
                           "cancer_total_hits", "cancer_total_hits_mean", "cancer_total_hits_min", "cancer_total_hits_max",
                           "classification", "num_cancer_types", "total_years")
    if (exists("epoch_cancer_type_cols") && length(epoch_cancer_type_cols) > 0) {
      output_cols_epoch <- c(output_cols_epoch, epoch_cancer_type_cols)
    }
    available_cols_epoch <- output_cols_epoch[output_cols_epoch %in% colnames(epoch_projects_all)]

    epoch_projects_output <- epoch_projects_all[, ..available_cols_epoch]

    addWorksheet(wb_classification, "Epoch_Unique_Projects")
    writeData(wb_classification, sheet = "Epoch_Unique_Projects", epoch_projects_output)
    cat("2) Epoch-based unique projects C0/C1/C2 saved to Excel\n")
  }

  # Save Excel file
  excel_classification_file <- file.path(output_dir_tables, paste0("ntis2_c0_c1_c2_classification_", timestamp, ".xlsx"))
  saveWorkbook(wb_classification, excel_classification_file, overwrite = TRUE)
  cat("C0/C1/C2 classification Excel saved to:", excel_classification_file, "\n")

}, error = function(e) {
  cat("Warning: Failed to create C0/C1/C2 classification Excel file. Error:", e$message, "\n")
  cat("Falling back to CSV files...\n")

  # Fallback to CSV
  csv_all_file <- file.path(output_dir_tables, paste0("ntis2_all_unique_projects_c0_c1_c2_", timestamp, ".csv"))
  fwrite(unique_projects_output_all, csv_all_file, encoding = "UTF-8")
  cat("All unique projects C0/C1/C2 saved to CSV:", csv_all_file, "\n")

  if (exists("epoch_projects_output")) {
    csv_epoch_file <- file.path(output_dir_tables, paste0("ntis2_epoch_unique_projects_c0_c1_c2_", timestamp, ".csv"))
    fwrite(epoch_projects_output, csv_epoch_file, encoding = "UTF-8")
    cat("Epoch unique projects C0/C1/C2 saved to CSV:", csv_epoch_file, "\n")
  }
})

# =============================================================================
# DISPLAY SUMMARY OF TABLES
# =============================================================================

cat("\n=== SUMMARY OF CREATED TABLES ===\n")
cat("Table 1: Money by Year and Cancer Classification -", nrow(table1_money), "years\n")
cat("Table 2: Projects by Year and Cancer Classification -", nrow(table2_projects), "years\n")
cat("Table 3: Money by Year and 6T Technological Classification (Cancer-Related Projects Only) -", nrow(table3_money_class_d), "years,", ncol(table3_money_class_d)-1, "classifications\n")
# Table 4 and Table 5 not used in paper - objects kept for internal use (e.g. Figure 2 uses table5_money_class_i) but not written to output.
cat("Table 6: Projects by Year and R&D Period (Cancer-Related Projects Only) -", nrow(table6_projects_class_i), "years,", ncol(table6_projects_class_i)-1, "periods\n")

# Display sample of each table
cat("\n=== SAMPLE OF TABLE 1 (Money by Year and Cancer Classification) ===\n")
print(head(table1_money, 5))

cat("\n=== SAMPLE OF TABLE 2 (Projects by Year and Cancer Classification) ===\n")
print(head(table2_projects, 5))

cat("\n=== SAMPLE OF TABLE 3 (Money by Year and 6T Technological Classification) ===\n")
print(head(table3_money_class_d, 5))

# (Table 4 and Table 5 not used in paper - sample omitted.)

cat("\n=== SAMPLE OF TABLE 6 (Projects by Year and R&D Period) ===\n")
print(head(table6_projects_class_i, 5))


cat("\n=== INPUT STRUCTURE CHECK ===\n")
cat("Input columns:\n")
cat("  - MoneyC column:", ifelse("MoneyC" %in% colnames(data), "present", "missing"), "\n")
cat("  - Class_D column:", ifelse("Class_D" %in% colnames(data), "present", "missing"), "\n")
cat("  - Class_I column:", ifelse("Class_I" %in% colnames(data), "present", "missing"), "\n")
cat("  - cancer_related column:", ifelse("cancer_related" %in% colnames(data), "present", "missing"), "\n")
cat("  - cancer_total_hits column:", ifelse("cancer_total_hits" %in% colnames(data), "present", "missing"), "\n")

cat("\nSummary tables:\n")
cat("  - Table 1 (Money by Year and Cancer):", nrow(table1_money), "years\n")
cat("  - Table 2 (Projects by Year and Cancer):", nrow(table2_projects), "years\n")
cat("  - Table 3 (Money by 6T Classification - Cancer-Related Only):", nrow(table3_money_class_d), "years,", ncol(table3_money_class_d)-1, "classifications\n")
cat("  - Table 4 and 5 (not used in paper - omitted from output)\n")
cat("  - Table 6 (Projects by R&D Period - Cancer-Related Only):", nrow(table6_projects_class_i), "years,", ncol(table6_projects_class_i)-1, "periods\n")

cat("\nTotal processing time:", round(difftime(Sys.time(), start_time, units = "secs"), 2), "seconds\n")
cat("=== 01_filter COMPLETED ===\n")
