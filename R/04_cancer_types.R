# =============================================================================
# Site-specific cancer research trends.
#
# Classifies cancer-related projects by cancer site using site-specific keyword
# patterns and produces annual project counts and funding per site.
#
# Input :  NTIS_cancer_classification_2006-2024_*.RData,
#          ntis2_yearly_total_stats_*
# Output:  ntis6_cancer_types_yearly_summary_{count,cost}_*  -> feeds 05_compare_epi.R
# =============================================================================

if (!exists("PROJ_ROOT")) source("config.R")

required_packages <- c("readxl", "dplyr", "stringr", "ggplot2", "tidyr", "data.table")
invisible(lapply(required_packages, library, character.only = TRUE))

# Ensure data.table is loaded for fread
library(data.table)


# Create output directories
output_dir_tables <- OUT_TABLES
output_dir_figures <- OUT_FIGURES
if (!dir.exists(output_dir_tables)) dir.create(output_dir_tables, recursive = TRUE)
if (!dir.exists(output_dir_figures)) dir.create(output_dir_figures, recursive = TRUE)

# Timestamp for output files
timestamp <- format(Sys.time(), "%Y%m%d")

cat("=== 04_cancer_types.R - Cancer Type Analysis by Year ===\n")

# =============================================================================
# Section 1: Define Cancer Type Keywords
# =============================================================================

get_cancer_type_keywords <- function() {
  # Returns a list with Korean and English keywords separated
  # Structure: list(cancer_type = list(korean = c(...), english = c(...)))
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
      korean = c("간암", "간세포암", "간세포 암", "간세포암종", "간선암",
                 "간종양", "간 악성종양"),
      english = c("liver cancer", "hepatocellular carcinoma", "hcc", "\\bhcc\\b",
                  "hepatic carcinoma", "liver carcinoma", "hepatoma",
                  "primary liver cancer", "primary hepatic cancer",
                  "liver tumor", "hepatic tumor", "liver neoplasm", "hepatic neoplasm")
    ),
    "췌장암" = list(
      korean = c("췌장암", "췌장세포암", "췌장세포 암", "췌관선암", "췌장선암",
                 "췌관 선암", "췌장 선암", "이자암",
                 "췌장종양", "췌장 악성종양", "이자종양"),
      english = c("pancreatic cancer", "pancreas cancer", "pancreatic adenocarcinoma",
                  "pancreatic ductal adenocarcinoma", "pdac", "\\bpdac\\b",
                  "pancreatic carcinoma", "pancreas carcinoma", "pancreatic tumor",
                  "pancreas tumor", "pancreatic neoplasm", "pancreas neoplasm")
    ),
    "신장암" = list(
      korean = c("신장암", "신세포암", "신세포 암", "콩팥암", "신장선암",
                 "투명세포암", "유두상신세포암", "크로모포브신세포암",
                 "투명세포 신세포암", "유두상 신세포암", "크로모포브 신세포암",
                 "신장종양", "콩팥종양", "신장 악성종양", "콩팥 악성종양"),
      english = c("kidney cancer", "renal cancer", "rcc", "\\brcc\\b", "renal cell carcinoma",
                  "clear cell renal carcinoma", "clear cell renal", "ccrcc", "\\bccrcc\\b",
                  "papillary renal cell carcinoma", "prcc", "\\bprcc\\b",
                  "chromophobe renal cell carcinoma", "chromophobe rcc",
                  "renal carcinoma", "kidney carcinoma", "renal tumor", "kidney tumor",
                  "renal neoplasm", "kidney neoplasm")
    ),
    "자궁경부암" = list(
      # NOTE: "경부암" alone may match non-cervical contexts, but kept for coverage
      # NOTE: "자궁암" in project titles/keywords may cause overlap with 자궁내막암
      # NOTE: "부인암" projects may match multiple female cancer types (자궁경부암, 자궁내막암, 난소암)
      korean = c("자궁경부암", "자궁경부세포암", "자궁경부세포 암", "경부암", "경부세포암", "경부세포 암",
                 "자궁경부선암", "경부선암", "자궁경부종양", "경부종양", "자궁경부 악성종양", "경부 악성종양"),
      english = c("cervical cancer", "cervix cancer", "cervical carcinoma", "cervix carcinoma",
                  "cervical adenocarcinoma", "cervical squamous cell carcinoma",
                  "cervical tumor", "cervix tumor", "cervical neoplasm", "cervix neoplasm",
                  "cervical intraepithelial neoplasia", "cin", "\\bcin\\b")
    ),
    "자궁내막암" = list(
      # NOTE: "내막암" alone may match non-endometrial contexts, but kept for coverage
      # NOTE: "자궁암" in project titles/keywords may cause overlap with 자궁경부암
      # NOTE: "부인암" projects may match multiple female cancer types (자궁경부암, 자궁내막암, 난소암)
      korean = c("자궁내막암", "자궁내막세포암", "자궁내막세포 암", "내막암", "내막세포암", "내막세포 암",
                 "자궁내막선암", "내막선암", "자궁내막종양", "내막종양", "자궁내막 악성종양", "내막 악성종양"),
      english = c("endometrial cancer", "endometrium cancer", "endometrial carcinoma", "endometrium carcinoma",
                  "endometrial adenocarcinoma", "endometrial tumor", "endometrium tumor",
                  "endometrial neoplasm", "endometrium neoplasm", "uterine corpus cancer", "uterine body cancer")
    ),
    "난소암" = list(
      # NOTE: "부인암" projects may match multiple female cancer types (자궁경부암, 자궁내막암, 난소암)
      korean = c("난소암", "난소세포암", "난소세포 암", "난소선암", "난소종양", "난소 악성종양",
                 "난소상피암", "난소 상피암", "난소상피세포암", "난소 상피세포암"),
      english = c("ovarian cancer", "ovary cancer", "ovarian carcinoma", "ovary carcinoma",
                  "ovarian adenocarcinoma", "ovarian tumor", "ovary tumor",
                  "ovarian neoplasm", "ovary neoplasm", "epithelial ovarian cancer", "eoc", "\\beoc\\b",
                  "ovarian epithelial cancer", "ovarian epithelial carcinoma")
    )
  )
  return(cancer_types)
}

# =============================================================================
# Section 2: Read Data
# =============================================================================

cat("Step 1: Reading step 00 classification data...\n")

# Find the most recent classification file (prefer RData, fallback to CSV)
ntis_rdata_files <- list.files(path = output_dir_tables, pattern = "NTIS.*classification.*\\.RData$", full.names = TRUE)
ntis_csv_files <- list.files(path = output_dir_tables, pattern = "NTIS.*classification.*\\.csv$", full.names = TRUE)

# Prefer RData, fallback to CSV
if (length(ntis_rdata_files) > 0) {
  file_info <- file.info(ntis_rdata_files)
  ntis_rdata_files <- ntis_rdata_files[order(file_info$mtime, decreasing = TRUE)]
  input_file <- ntis_rdata_files[1]
  cat("Loading RData file:", basename(input_file), "\n")
  load(input_file)
  cat("Data loaded from RData:", nrow(data), "rows x", ncol(data), "columns\n")
} else if (length(ntis_csv_files) > 0) {
  file_info <- file.info(ntis_csv_files)
  ntis_csv_files <- ntis_csv_files[order(file_info$mtime, decreasing = TRUE)]
  input_file <- ntis_csv_files[1]
  cat("Loading CSV file:", basename(input_file), " (RData not found, using CSV)\n")
  data <- fread(input_file, encoding = "UTF-8")
  data <- as.data.frame(data)
  cat("Data loaded from CSV:", nrow(data), "rows x", ncol(data), "columns\n")
} else {
  stop("No classification results file found in output/tables. Please run 00_build_classification.R first.")
}

# Load yearly statistics from step 01 if available
yearly_stats_available <- FALSE
yearly_total_stats <- NULL
yearly_stats_rdata_files <- list.files(path = output_dir_tables, pattern = "ntis2_yearly_total_stats_.*\\.RData$", full.names = TRUE)
if (length(yearly_stats_rdata_files) > 0) {
  file_info <- file.info(yearly_stats_rdata_files)
  yearly_stats_rdata_files <- yearly_stats_rdata_files[order(file_info$mtime, decreasing = TRUE)]
  stats_file <- yearly_stats_rdata_files[1]
  cat("Loading yearly statistics from step 01:", basename(stats_file), "\n")
  load(stats_file)
  yearly_stats_available <- TRUE
  cat("Yearly statistics loaded - will use for proportion calculations\n")
} else {
  cat("Yearly statistics not found - will calculate from data\n")
}

# Filter to cancer-related projects only
if ("cancer_related" %in% colnames(data)) {
  data <- data[data$cancer_related == TRUE, ]
} else {
  cat("Warning: cancer_related column not found. Using all data.\n")
}

# Convert Year to numeric
data$Year <- as.numeric(as.character(data$Year))
data <- data[!is.na(data$Year) & data$Year >= 2006 & data$Year <= 2024, ]

# Check and process MoneyC column for cost analysis
has_money_data <- FALSE
if ("MoneyC" %in% colnames(data)) {
  data$MoneyC <- as.numeric(as.character(data$MoneyC))
  data$MoneyC[is.na(data$MoneyC)] <- 0
  has_money_data <- TRUE
  cat("MoneyC column found and processed for cost analysis\n")
  cat("MoneyC range:", min(data$MoneyC, na.rm = TRUE), "to", max(data$MoneyC, na.rm = TRUE), "\n")
} else {
  cat("Warning: MoneyC column not found. Cost analysis will be skipped.\n")
}

cat("Data loaded:", nrow(data), "cancer-related projects\n")
cat("Year range:", min(data$Year), "-", max(data$Year), "\n")

# =============================================================================
# Section 3: Classify by Cancer Type
# =============================================================================

cat("\nStep 2: Classifying projects by cancer type...\n")

cancer_types <- get_cancer_type_keywords()
target_cols <- c("Text1", "Text2", "Key1", "Key2")

# Function to check if text contains any of the keywords for a cancer type
# Uses word boundary matching for English terms to avoid false positives
check_cancer_type <- function(texts, keywords) {
  if (length(texts) == 0) return(logical(0))

  matches <- logical(length(texts))
  for (i in seq_along(texts)) {
    text <- texts[i]
    if (is.na(text) || nchar(text) < 2) next

    for (keyword in keywords) {
      # Check if keyword contains Korean characters
      has_korean <- grepl("[가-힣]", keyword, useBytes = TRUE)

      if (has_korean) {
        # For Korean keywords, use simple pattern matching
        # Korean text doesn't have clear word boundaries like English
        pattern <- keyword
      } else {
        # For English keywords, use word boundary matching for better accuracy
        # This prevents matching "breast" in "abreast" or "kidney" in "kidney-shaped"
        # But allows "breast cancer" in "triple-negative breast cancer"
        if (grepl("\\s", keyword)) {
          # Multi-word phrases: use word boundaries around the phrase
          pattern <- paste0("\\b", keyword, "\\b")
        } else {
          # Single words: use word boundaries
          pattern <- paste0("\\b", keyword, "\\b")
        }
      }

      if (grepl(pattern, text, fixed = FALSE, ignore.case = TRUE, useBytes = TRUE)) {
        matches[i] <- TRUE
        break
      }
    }
  }
  return(matches)
}

# Classify each cancer type
for (cancer_type in names(cancer_types)) {
  keyword_list <- cancer_types[[cancer_type]]
  korean_keywords <- keyword_list$korean
  english_keywords <- keyword_list$english

  cat("Processing", cancer_type, "...\n")
  cat("  Korean keywords:", length(korean_keywords), "\n")
  cat("  English keywords:", length(english_keywords), "\n")

  # Initialize matches
  type_matches <- logical(nrow(data))

  # Search Korean keywords in Text1 (Korean title) and Key1 (Korean keywords)
  if (length(korean_keywords) > 0) {
    if ("Text1" %in% colnames(data)) {
      korean_matches_text1 <- check_cancer_type(data[["Text1"]], korean_keywords)
      type_matches <- type_matches | korean_matches_text1
    }
    if ("Key1" %in% colnames(data)) {
      korean_matches_key1 <- check_cancer_type(data[["Key1"]], korean_keywords)
      type_matches <- type_matches | korean_matches_key1
    }
  }

  # Search English keywords in Text2 (English title) and Key2 (English keywords)
  if (length(english_keywords) > 0) {
    if ("Text2" %in% colnames(data)) {
      english_matches_text2 <- check_cancer_type(data[["Text2"]], english_keywords)
      type_matches <- type_matches | english_matches_text2
    }
    if ("Key2" %in% colnames(data)) {
      english_matches_key2 <- check_cancer_type(data[["Key2"]], english_keywords)
      type_matches <- type_matches | english_matches_key2
    }
  }

  data[[paste0("type_", cancer_type)]] <- type_matches
}

# =============================================================================
# Section 4: Calculate Yearly Statistics - PROJECT COUNT ANALYSIS
# =============================================================================

cat("\nStep 3: Calculating yearly statistics (PROJECT COUNT)...\n")

# Create summary by year and cancer type (PROJECT COUNT)
yearly_summary_count <- data %>%
  group_by(Year) %>%
  summarise(
    total_cancer = n(),
    갑상선암   = sum(type_갑상선암, na.rm = TRUE),
    대장암     = sum(type_대장암,   na.rm = TRUE),
    폐암       = sum(type_폐암,     na.rm = TRUE),
    유방암     = sum(type_유방암,   na.rm = TRUE),
    위암       = sum(type_위암,     na.rm = TRUE),
    전립선암   = sum(type_전립선암, na.rm = TRUE),
    간암       = sum(type_간암,     na.rm = TRUE),
    췌장암     = sum(type_췌장암,   na.rm = TRUE),
    신장암     = sum(type_신장암,   na.rm = TRUE),
    자궁경부암 = sum(type_자궁경부암, na.rm = TRUE),
    자궁내막암 = sum(type_자궁내막암, na.rm = TRUE),
    난소암     = sum(type_난소암,     na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate proportions for project count
# Use yearly_total_stats if available (from step 01), otherwise use calculated total_cancer
cancer_type_names <- c("갑상선암", "대장암", "폐암", "유방암", "위암", "전립선암", "간암", "췌장암", "신장암",
                      "자궁경부암", "자궁내막암", "난소암")
if (yearly_stats_available && !is.null(yearly_total_stats)) {
  # Merge with yearly_total_stats to get total_cancer_projects per year
  yearly_summary_count <- merge(yearly_summary_count,
                                yearly_total_stats[, c("Year", "total_cancer_projects")],
                                by = "Year", all.x = TRUE)
  # Use total_cancer_projects from stats if available, otherwise use calculated total_cancer
  for (cancer_type in cancer_type_names) {
    yearly_summary_count[[paste0(cancer_type, "_pct")]] <- round(
      yearly_summary_count[[cancer_type]] /
      ifelse(!is.na(yearly_summary_count$total_cancer_projects),
             yearly_summary_count$total_cancer_projects,
             yearly_summary_count$total_cancer) * 100, 2)
  }
} else {
  for (cancer_type in cancer_type_names) {
    yearly_summary_count[[paste0(cancer_type, "_pct")]] <- round(yearly_summary_count[[cancer_type]] / yearly_summary_count$total_cancer * 100, 2)
  }
}

# =============================================================================
# Section 4.5: Calculate Yearly Statistics - PROJECT COST ANALYSIS
# =============================================================================

if (has_money_data) {
  cat("\nStep 3.5: Calculating yearly statistics (PROJECT COST)...\n")

  # Create summary by year and cancer type (PROJECT COST)
  yearly_summary_cost <- data %>%
    group_by(Year) %>%
    summarise(
      total_cancer_cost = sum(MoneyC, na.rm = TRUE),
      갑상선암   = sum(MoneyC[type_갑상선암   == TRUE], na.rm = TRUE),
      대장암     = sum(MoneyC[type_대장암     == TRUE], na.rm = TRUE),
      폐암       = sum(MoneyC[type_폐암       == TRUE], na.rm = TRUE),
      유방암     = sum(MoneyC[type_유방암     == TRUE], na.rm = TRUE),
      위암       = sum(MoneyC[type_위암       == TRUE], na.rm = TRUE),
      전립선암   = sum(MoneyC[type_전립선암   == TRUE], na.rm = TRUE),
      간암       = sum(MoneyC[type_간암       == TRUE], na.rm = TRUE),
      췌장암     = sum(MoneyC[type_췌장암     == TRUE], na.rm = TRUE),
      신장암     = sum(MoneyC[type_신장암     == TRUE], na.rm = TRUE),
      자궁경부암 = sum(MoneyC[type_자궁경부암 == TRUE], na.rm = TRUE),
      자궁내막암 = sum(MoneyC[type_자궁내막암 == TRUE], na.rm = TRUE),
      난소암     = sum(MoneyC[type_난소암     == TRUE], na.rm = TRUE),
      .groups = 'drop'
    )

  # Calculate proportions for project cost
  # Use yearly_total_stats if available (from step 01), otherwise use calculated total_cancer_cost
  if (yearly_stats_available && !is.null(yearly_total_stats) && "total_cancer_money" %in% colnames(yearly_total_stats)) {
    # Merge with yearly_total_stats to get total_cancer_money per year
    yearly_summary_cost <- merge(yearly_summary_cost,
                                 yearly_total_stats[, c("Year", "total_cancer_money")],
                                 by = "Year", all.x = TRUE)
    # Use total_cancer_money from stats if available, otherwise use calculated total_cancer_cost
    for (cancer_type in cancer_type_names) {
      yearly_summary_cost[[paste0(cancer_type, "_pct")]] <- round(
        yearly_summary_cost[[cancer_type]] /
        ifelse(!is.na(yearly_summary_cost$total_cancer_money),
               yearly_summary_cost$total_cancer_money,
               yearly_summary_cost$total_cancer_cost) * 100, 2)
    }
  } else {
    for (cancer_type in cancer_type_names) {
      yearly_summary_cost[[paste0(cancer_type, "_pct")]] <- round(yearly_summary_cost[[cancer_type]] / yearly_summary_cost$total_cancer_cost * 100, 2)
    }
  }

  # Calculate average cost per project by cancer type
  yearly_summary_cost_avg <- data %>%
    group_by(Year) %>%
    summarise(
      갑상선암   = ifelse(sum(type_갑상선암,   na.rm = TRUE) > 0,
                        sum(MoneyC[type_갑상선암   == TRUE], na.rm = TRUE) / sum(type_갑상선암,   na.rm = TRUE), 0),
      대장암     = ifelse(sum(type_대장암,     na.rm = TRUE) > 0,
                        sum(MoneyC[type_대장암     == TRUE], na.rm = TRUE) / sum(type_대장암,     na.rm = TRUE), 0),
      폐암       = ifelse(sum(type_폐암,       na.rm = TRUE) > 0,
                        sum(MoneyC[type_폐암       == TRUE], na.rm = TRUE) / sum(type_폐암,       na.rm = TRUE), 0),
      유방암     = ifelse(sum(type_유방암,     na.rm = TRUE) > 0,
                        sum(MoneyC[type_유방암     == TRUE], na.rm = TRUE) / sum(type_유방암,     na.rm = TRUE), 0),
      위암       = ifelse(sum(type_위암,       na.rm = TRUE) > 0,
                        sum(MoneyC[type_위암       == TRUE], na.rm = TRUE) / sum(type_위암,       na.rm = TRUE), 0),
      전립선암   = ifelse(sum(type_전립선암,   na.rm = TRUE) > 0,
                        sum(MoneyC[type_전립선암   == TRUE], na.rm = TRUE) / sum(type_전립선암,   na.rm = TRUE), 0),
      간암       = ifelse(sum(type_간암,       na.rm = TRUE) > 0,
                        sum(MoneyC[type_간암       == TRUE], na.rm = TRUE) / sum(type_간암,       na.rm = TRUE), 0),
      췌장암     = ifelse(sum(type_췌장암,     na.rm = TRUE) > 0,
                        sum(MoneyC[type_췌장암     == TRUE], na.rm = TRUE) / sum(type_췌장암,     na.rm = TRUE), 0),
      신장암     = ifelse(sum(type_신장암,     na.rm = TRUE) > 0,
                        sum(MoneyC[type_신장암     == TRUE], na.rm = TRUE) / sum(type_신장암,     na.rm = TRUE), 0),
      자궁경부암 = ifelse(sum(type_자궁경부암, na.rm = TRUE) > 0,
                        sum(MoneyC[type_자궁경부암 == TRUE], na.rm = TRUE) / sum(type_자궁경부암, na.rm = TRUE), 0),
      자궁내막암 = ifelse(sum(type_자궁내막암, na.rm = TRUE) > 0,
                        sum(MoneyC[type_자궁내막암 == TRUE], na.rm = TRUE) / sum(type_자궁내막암, na.rm = TRUE), 0),
      난소암     = ifelse(sum(type_난소암,     na.rm = TRUE) > 0,
                        sum(MoneyC[type_난소암     == TRUE], na.rm = TRUE) / sum(type_난소암,     na.rm = TRUE), 0),
      .groups = 'drop'
    )
} else {
  yearly_summary_cost <- NULL
  yearly_summary_cost_avg <- NULL
}

# =============================================================================
# Section 5: Create Visualization Data - PROJECT COUNT
# =============================================================================

cat("\nStep 4: Preparing visualization data (PROJECT COUNT)...\n")

# Reshape data for plotting (PROJECT COUNT)
count_data <- yearly_summary_count %>%
  select(Year, all_of(cancer_type_names)) %>%
  pivot_longer(cols = all_of(cancer_type_names), names_to = "Cancer_Type", values_to = "Count")

pct_data <- yearly_summary_count %>%
  select(Year, ends_with("_pct")) %>%
  rename_with(~str_replace(.x, "_pct", ""), ends_with("_pct")) %>%
  pivot_longer(cols = all_of(cancer_type_names), names_to = "Cancer_Type", values_to = "Proportion")

# Combine count and proportion
plot_data_count <- count_data %>%
  left_join(pct_data, by = c("Year", "Cancer_Type"))

# =============================================================================
# Section 5.5: Create Visualization Data - PROJECT COST
# =============================================================================

if (has_money_data) {
  cat("\nStep 4.5: Preparing visualization data (PROJECT COST)...\n")

  # Reshape data for plotting (PROJECT COST - Total)
  cost_data <- yearly_summary_cost %>%
    select(Year, all_of(cancer_type_names)) %>%
    pivot_longer(cols = all_of(cancer_type_names), names_to = "Cancer_Type", values_to = "Cost")

  cost_pct_data <- yearly_summary_cost %>%
    select(Year, ends_with("_pct")) %>%
    rename_with(~str_replace(.x, "_pct", ""), ends_with("_pct")) %>%
    pivot_longer(cols = all_of(cancer_type_names), names_to = "Cancer_Type", values_to = "Cost_Proportion")

  # Combine cost and cost proportion
  plot_data_cost <- cost_data %>%
    left_join(cost_pct_data, by = c("Year", "Cancer_Type"))

  # Reshape data for plotting (PROJECT COST - Average per project)
  cost_avg_data <- yearly_summary_cost_avg %>%
    select(Year, all_of(cancer_type_names)) %>%
    pivot_longer(cols = all_of(cancer_type_names), names_to = "Cancer_Type", values_to = "Avg_Cost")

  plot_data_cost_avg <- cost_avg_data
} else {
  plot_data_cost <- NULL
  plot_data_cost_avg <- NULL
}

# =============================================================================
# Section 6: Create Graphs - PROJECT COUNT ANALYSIS
# =============================================================================

cat("\nStep 5: Creating graphs (PROJECT COUNT ANALYSIS)...\n")

# Color palette for 9 cancer types
cancer_colors <- c(
  "갑상선암" = "#FF6B6B",
  "대장암" = "#4ECDC4",
  "폐암" = "#45B7D1",
  "유방암" = "#FFA07A",
  "위암" = "#98D8C8",
  "전립선암" = "#F7DC6F",
  "간암" = "#BB8FCE",
  "췌장암" = "#85C1E2",
  "신장암" = "#F8B739"
)

# Graph 1: Count by Year (PROJECT COUNT)
g1 <- ggplot(plot_data_count, aes(x = Year, y = Count, color = Cancer_Type, group = Cancer_Type)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(values = cancer_colors) +
  scale_x_continuous(breaks = seq(2006, 2024, 2), minor_breaks = 2006:2024) +
  labs(
    title = "연도별 암 종류별 연구 프로젝트 개수",
    subtitle = "Cancer Type-Specific Research Projects by Year (Count)",
    x = "연도 (Year)",
    y = "프로젝트 개수 (Number of Projects)",
    color = "암 종류\n(Cancer Type)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.5)
  )

# Graph 2: Proportion by Year (PROJECT COUNT)
g2 <- ggplot(plot_data_count, aes(x = Year, y = Proportion, color = Cancer_Type, group = Cancer_Type)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(values = cancer_colors) +
  scale_x_continuous(breaks = seq(2006, 2024, 2), minor_breaks = 2006:2024) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "연도별 암 종류별 연구 프로젝트 비율",
    subtitle = "Cancer Type-Specific Research Projects by Year (Proportion)",
    x = "연도 (Year)",
    y = "비율 (%) (Proportion %)",
    color = "암 종류\n(Cancer Type)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.5)
  )

# Graph 3: Stacked Area Chart (Proportion - PROJECT COUNT)
g3 <- ggplot(plot_data_count, aes(x = Year, y = Proportion, fill = Cancer_Type)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_fill_manual(values = cancer_colors) +
  scale_x_continuous(breaks = seq(2006, 2024, 2), minor_breaks = 2006:2024) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "연도별 암 종류별 연구 프로젝트 비율 (누적)",
    subtitle = "Cancer Type-Specific Research Projects by Year (Stacked Proportion)",
    x = "연도 (Year)",
    y = "비율 (%) (Proportion %)",
    fill = "암 종류\n(Cancer Type)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.5)
  )

# Graph 4: Bar Chart (Count by Year - PROJECT COUNT)
g4 <- ggplot(plot_data_count, aes(x = factor(Year), y = Count, fill = Cancer_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = cancer_colors) +
  labs(
    title = "연도별 암 종류별 연구 프로젝트 개수 (막대 그래프)",
    subtitle = "Cancer Type-Specific Research Projects by Year (Bar Chart)",
    x = "연도 (Year)",
    y = "프로젝트 개수 (Number of Projects)",
    fill = "암 종류\n(Cancer Type)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", linewidth = 0.5)
  )

# =============================================================================
# Section 6.5: Create Graphs - PROJECT COST ANALYSIS
# =============================================================================

if (has_money_data) {
  cat("\nStep 5.5: Creating graphs (PROJECT COST ANALYSIS)...\n")

  # Graph 5: Total Cost by Year (PROJECT COST)
  g5 <- ggplot(plot_data_cost, aes(x = Year, y = Cost, color = Cancer_Type, group = Cancer_Type)) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    geom_point(size = 2.5) +
    scale_color_manual(values = cancer_colors) +
    scale_x_continuous(breaks = seq(2006, 2024, 2), minor_breaks = 2006:2024) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ",")) +
    labs(
      title = "연도별 암 종류별 연구 프로젝트 총 비용",
      subtitle = "Cancer Type-Specific Research Projects by Year (Total Cost)",
      x = "연도 (Year)",
      y = "총 비용 (Total Cost)",
      color = "암 종류\n(Cancer Type)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "right",
      panel.grid.minor = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.major = element_line(color = "gray80", linewidth = 0.5)
    )

  # Graph 6: Cost Proportion by Year (PROJECT COST)
  g6 <- ggplot(plot_data_cost, aes(x = Year, y = Cost_Proportion, color = Cancer_Type, group = Cancer_Type)) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    geom_point(size = 2.5) +
    scale_color_manual(values = cancer_colors) +
    scale_x_continuous(breaks = seq(2006, 2024, 2), minor_breaks = 2006:2024) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = "연도별 암 종류별 연구 프로젝트 비용 비율",
      subtitle = "Cancer Type-Specific Research Projects by Year (Cost Proportion)",
      x = "연도 (Year)",
      y = "비용 비율 (%) (Cost Proportion %)",
      color = "암 종류\n(Cancer Type)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "right",
      panel.grid.minor = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.major = element_line(color = "gray80", linewidth = 0.5)
    )

  # Graph 7: Stacked Area Chart (Cost Proportion - PROJECT COST)
  g7 <- ggplot(plot_data_cost, aes(x = Year, y = Cost_Proportion, fill = Cancer_Type)) +
    geom_area(alpha = 0.7, position = "stack") +
    scale_fill_manual(values = cancer_colors) +
    scale_x_continuous(breaks = seq(2006, 2024, 2), minor_breaks = 2006:2024) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = "연도별 암 종류별 연구 프로젝트 비용 비율 (누적)",
      subtitle = "Cancer Type-Specific Research Projects by Year (Stacked Cost Proportion)",
      x = "연도 (Year)",
      y = "비용 비율 (%) (Cost Proportion %)",
      fill = "암 종류\n(Cancer Type)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "right",
      panel.grid.minor = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.major = element_line(color = "gray80", linewidth = 0.5)
    )

  # Graph 8: Average Cost per Project by Year (PROJECT COST)
  g8 <- ggplot(plot_data_cost_avg, aes(x = Year, y = Avg_Cost, color = Cancer_Type, group = Cancer_Type)) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    geom_point(size = 2.5) +
    scale_color_manual(values = cancer_colors) +
    scale_x_continuous(breaks = seq(2006, 2024, 2), minor_breaks = 2006:2024) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ",")) +
    labs(
      title = "연도별 암 종류별 프로젝트당 평균 비용",
      subtitle = "Cancer Type-Specific Average Cost per Project by Year",
      x = "연도 (Year)",
      y = "평균 비용 (Average Cost per Project)",
      color = "암 종류\n(Cancer Type)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "right",
      panel.grid.minor = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.major = element_line(color = "gray80", linewidth = 0.5)
    )

  # Graph 9: Bar Chart (Total Cost by Year - PROJECT COST)
  g9 <- ggplot(plot_data_cost, aes(x = factor(Year), y = Cost, fill = Cancer_Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = cancer_colors) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ",")) +
    labs(
      title = "연도별 암 종류별 연구 프로젝트 총 비용 (막대 그래프)",
      subtitle = "Cancer Type-Specific Research Projects by Year (Total Cost - Bar Chart)",
      x = "연도 (Year)",
      y = "총 비용 (Total Cost)",
      fill = "암 종류\n(Cancer Type)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "right",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray80", linewidth = 0.5)
    )
}

# =============================================================================
# Section 7: Save Outputs
# =============================================================================

cat("\nStep 6: Saving outputs...\n")

# Save summary tables - PROJECT COUNT
summary_file_count <- file.path(output_dir_tables, paste0("ntis6_cancer_types_yearly_summary_count_", timestamp, ".csv"))
write.csv(yearly_summary_count, summary_file_count, row.names = FALSE, fileEncoding = "UTF-8")
cat("Summary table (PROJECT COUNT) saved to:", summary_file_count, "\n")

# Save plot data - PROJECT COUNT
plot_data_file_count <- file.path(output_dir_tables, paste0("ntis6_cancer_types_plot_data_count_", timestamp, ".csv"))
write.csv(plot_data_count, plot_data_file_count, row.names = FALSE, fileEncoding = "UTF-8")
cat("Plot data (PROJECT COUNT) saved to:", plot_data_file_count, "\n")

# Save summary tables - PROJECT COST
if (has_money_data) {
  summary_file_cost <- file.path(output_dir_tables, paste0("ntis6_cancer_types_yearly_summary_cost_", timestamp, ".csv"))
  write.csv(yearly_summary_cost, summary_file_cost, row.names = FALSE, fileEncoding = "UTF-8")
  cat("Summary table (PROJECT COST) saved to:", summary_file_cost, "\n")

  summary_file_cost_avg <- file.path(output_dir_tables, paste0("ntis6_cancer_types_yearly_summary_cost_avg_", timestamp, ".csv"))
  write.csv(yearly_summary_cost_avg, summary_file_cost_avg, row.names = FALSE, fileEncoding = "UTF-8")
  cat("Summary table (AVERAGE COST PER PROJECT) saved to:", summary_file_cost_avg, "\n")

  # Save plot data - PROJECT COST
  plot_data_file_cost <- file.path(output_dir_tables, paste0("ntis6_cancer_types_plot_data_cost_", timestamp, ".csv"))
  write.csv(plot_data_cost, plot_data_file_cost, row.names = FALSE, fileEncoding = "UTF-8")
  cat("Plot data (PROJECT COST) saved to:", plot_data_file_cost, "\n")

  plot_data_file_cost_avg <- file.path(output_dir_tables, paste0("ntis6_cancer_types_plot_data_cost_avg_", timestamp, ".csv"))
  write.csv(plot_data_cost_avg, plot_data_file_cost_avg, row.names = FALSE, fileEncoding = "UTF-8")
  cat("Plot data (AVERAGE COST PER PROJECT) saved to:", plot_data_file_cost_avg, "\n")
}

# Save graphs - PROJECT COUNT ANALYSIS
png_file1 <- file.path(output_dir_figures, paste0("ntis6_cancer_types_count_", timestamp, ".png"))
ggsave(png_file1, g1, width = 14, height = 8, dpi = 300)
cat("Graph 1 (Count - PROJECT COUNT) saved to:", png_file1, "\n")

png_file2 <- file.path(output_dir_figures, paste0("ntis6_cancer_types_proportion_", timestamp, ".png"))
ggsave(png_file2, g2, width = 14, height = 8, dpi = 300)
cat("Graph 2 (Proportion - PROJECT COUNT) saved to:", png_file2, "\n")

png_file3 <- file.path(output_dir_figures, paste0("ntis6_cancer_types_stacked_", timestamp, ".png"))
ggsave(png_file3, g3, width = 14, height = 8, dpi = 300)
cat("Graph 3 (Stacked - PROJECT COUNT) saved to:", png_file3, "\n")

png_file4 <- file.path(output_dir_figures, paste0("ntis6_cancer_types_bar_", timestamp, ".png"))
ggsave(png_file4, g4, width = 16, height = 8, dpi = 300)
cat("Graph 4 (Bar Chart - PROJECT COUNT) saved to:", png_file4, "\n")

# Save graphs - PROJECT COST ANALYSIS
if (has_money_data) {
  png_file5 <- file.path(output_dir_figures, paste0("ntis6_cancer_types_cost_total_", timestamp, ".png"))
  ggsave(png_file5, g5, width = 14, height = 8, dpi = 300)
  cat("Graph 5 (Total Cost - PROJECT COST) saved to:", png_file5, "\n")

  png_file6 <- file.path(output_dir_figures, paste0("ntis6_cancer_types_cost_proportion_", timestamp, ".png"))
  ggsave(png_file6, g6, width = 14, height = 8, dpi = 300)
  cat("Graph 6 (Cost Proportion - PROJECT COST) saved to:", png_file6, "\n")

  png_file7 <- file.path(output_dir_figures, paste0("ntis6_cancer_types_cost_stacked_", timestamp, ".png"))
  ggsave(png_file7, g7, width = 14, height = 8, dpi = 300)
  cat("Graph 7 (Stacked Cost Proportion - PROJECT COST) saved to:", png_file7, "\n")

  png_file8 <- file.path(output_dir_figures, paste0("ntis6_cancer_types_cost_avg_", timestamp, ".png"))
  ggsave(png_file8, g8, width = 14, height = 8, dpi = 300)
  cat("Graph 8 (Average Cost per Project - PROJECT COST) saved to:", png_file8, "\n")

  png_file9 <- file.path(output_dir_figures, paste0("ntis6_cancer_types_cost_bar_", timestamp, ".png"))
  ggsave(png_file9, g9, width = 16, height = 8, dpi = 300)
  cat("Graph 9 (Bar Chart Total Cost - PROJECT COST) saved to:", png_file9, "\n")
}

# =============================================================================
# Section 8: Summary Statistics
# =============================================================================

cat("\n=== SUMMARY STATISTICS ===\n")
cat("Total cancer-related projects analyzed:", nrow(data), "\n")
cat("Year range:", min(data$Year), "-", max(data$Year), "\n\n")

cat("Total projects by cancer type (2006-2024):\n")
for (cancer_type in cancer_type_names) {
  total <- sum(data[[paste0("type_", cancer_type)]], na.rm = TRUE)
  pct <- round(total / nrow(data) * 100, 2)
  cat(sprintf("  %s: %d projects (%.2f%%)\n", cancer_type, total, pct))
}

cat("\nYearly average projects by cancer type:\n")
yearly_avg_count <- yearly_summary_count %>%
  summarise(across(all_of(cancer_type_names), \(x) mean(x, na.rm = TRUE)))
for (cancer_type in cancer_type_names) {
  avg <- round(yearly_avg_count[[cancer_type]], 1)
  cat(sprintf("  %s: %.1f projects/year\n", cancer_type, avg))
}

# Cost analysis summary
if (has_money_data) {
  cat("\n=== PROJECT COST ANALYSIS SUMMARY ===\n")
  cat("Total cost by cancer type (2006-2024):\n")
  for (cancer_type in cancer_type_names) {
    total_cost <- sum(yearly_summary_cost[[cancer_type]], na.rm = TRUE)
    total_cost_pct <- round(total_cost / sum(yearly_summary_cost$total_cancer_cost, na.rm = TRUE) * 100, 2)
    cat(sprintf("  %s: %s (%.2f%%)\n", cancer_type,
                format(total_cost, scientific = FALSE, big.mark = ","), total_cost_pct))
  }

  cat("\nYearly average cost by cancer type:\n")
  yearly_avg_cost <- yearly_summary_cost %>%
    summarise(across(all_of(cancer_type_names), \(x) mean(x, na.rm = TRUE)))
  for (cancer_type in cancer_type_names) {
    avg <- yearly_avg_cost[[cancer_type]]
    cat(sprintf("  %s: %s/year\n", cancer_type,
                format(round(avg, 0), scientific = FALSE, big.mark = ",")))
  }

  cat("\nYearly average cost per project by cancer type:\n")
  yearly_avg_cost_per_project <- yearly_summary_cost_avg %>%
    summarise(across(all_of(cancer_type_names), \(x) mean(x, na.rm = TRUE)))
  for (cancer_type in cancer_type_names) {
    avg <- yearly_avg_cost_per_project[[cancer_type]]
    cat(sprintf("  %s: %s/project\n", cancer_type,
                format(round(avg, 0), scientific = FALSE, big.mark = ",")))
  }
}

cat("\n=== 04_cancer_types.R COMPLETED ===\n")


