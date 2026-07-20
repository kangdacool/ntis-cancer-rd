# =============================================================================
# Keyword composition by period.
#
# Computes term frequencies of Korean project keywords within each period,
# takes the top 50, translates them to English via lexicon/keyword_translation.csv,
# and draws the flow diagram tracking how the leading keywords shift between
# periods. Keywords with no translation entry are reported so the mapping can be
# extended.
#
# Input :  ntis3_analysis_with_representative_texts_*
#          lexicon/keyword_translation.csv
# Output:  tableS3_epoch_top50_keywords_eng_with_counts_*,
#          ntis5_flow_keywords_final_eng_*  -> Figure 3 and Table S3
# =============================================================================

if (!exists("PROJ_ROOT")) source("config.R")

library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(viridis)
library(openxlsx)


# Create output directories
output_dir_tables <- OUT_TABLES
output_dir_figures <- OUT_FIGURES
if (!dir.exists(output_dir_tables)) dir.create(output_dir_tables, recursive = TRUE)
if (!dir.exists(output_dir_figures)) dir.create(output_dir_figures, recursive = TRUE)

# Timestamp for output files
timestamp <- format(Sys.time(), "%Y%m%d")

# =============================================================================
# Korean keyword preprocessing (UTF-8) — TF, 빈도 집계, lookup 공통
# =============================================================================
preprocess_korean_tf <- function(keywords_vector) {
  keywords <- unlist(strsplit(unlist(keywords_vector), ","))
  keywords <- trimws(keywords)
  keywords <- keywords[keywords != "" & nchar(keywords) > 1]
  filler_terms <- c("및", "또는", "그리고", "또한", "미입력", "기타", "등", "등등")
  keywords <- keywords[!keywords %in% filler_terms]
  keywords <- gsub("\\s+", "", keywords)
  keywords[keywords %in% c("전이", "암전이", "종양전이")] <- "전이"
  keywords[keywords %in% c("항암제", "항암", "항암치료", "항암요법")] <- "항암제"
  keywords[keywords %in% c("암미세환경", "종양미세환경", "미세환경")] <- "미세환경"
  keywords[keywords %in% c("세포신호전달", "신호전달")] <- "신호전달"
  keywords[keywords %in% c("암치료", "암치료법")] <- "암치료"
  keywords[keywords %in% c("방사선치료", "방사선", "방사선요법")] <- "방사선치료"
  keywords[keywords %in% c("항암면역치료", "면역치료", "면역요법")] <- "면역치료"
  keywords[keywords %in% c("암줄기세포", "종양줄기세포", "줄기세포")] <- "줄기세포"
  keywords[keywords %in% c("간암", "간세포암", "간세포암종")] <- "간암"
  keywords[keywords %in% c("기계학습", "머신러닝")] <- "기계학습"
  keywords[keywords %in% c("정밀의학", "정밀의료", "정밀치료")] <- "정밀의학"
  keywords[keywords %in% c("약물전달시스템", "약물전달")] <- "약물전달시스템"
  keywords[keywords %in% c("유전자", "유전자발현", "유전자변이", "유전학")] <- "유전자"
  keywords[keywords %in% c("단백질", "단백질발현", "단백질분석", "단백체학")] <- "단백질"
  keywords[keywords %in% c("폐암", "폐세포암")] <- "폐암"
  keywords[keywords %in% c("유방암", "유방세포암")] <- "유방암"
  keywords[keywords %in% c("대장암", "대장세포암", "결장암", "결장세포암")] <- "대장암"
  keywords[keywords %in% c("위암", "위세포암", "위장암", "위장세포암")] <- "위암"
  keywords[keywords %in% c("췌장암", "췌장세포암")] <- "췌장암"
  keywords[keywords %in% c("난소암", "난소세포암")] <- "난소암"
  keywords[keywords %in% c("자궁경부암", "자궁경부세포암", "경부암")] <- "자궁경부암"
  keywords[keywords %in% c("갑상선암", "갑상선세포암")] <- "갑상선암"
  keywords[keywords %in% c("뇌종양", "뇌암", "뇌세포암")] <- "뇌종양"
  keywords[keywords %in% c("혈액암", "백혈병", "림프종", "골수종")] <- "혈액암"
  keywords[keywords %in% c("액체생검", "액상생검")] <- "액체생검"
  keywords[keywords %in% c("암예방")] <- "암예방"
  keywords[keywords %in% c("삼중음성유방암")] <- "삼중음성유방암"
  keywords <- keywords[!keywords %in% c("암", "종양", "미입력", "연구", "개발", "분석")]
  keywords
}

normalize_korean_for_lookup <- function(kor) {
  if (is.na(kor) || !nzchar(trimws(as.character(kor)))) return(kor)
  res <- preprocess_korean_tf(as.character(kor))
  if (length(res) >= 1L) return(res[[1L]])
  gsub("\\s+", "", trimws(as.character(kor)))
}

# =============================================================================
# Section 1: Data Reading and Processing
# =============================================================================

cat("=== 03_keyword_flow.R - Table S3, Figure 3 ===\n")

# ---- 1.1 Use a precomputed TF table if one is present, else compute it from step 02 ----
excel_files <- list.files(path = output_dir_tables, pattern = "ntis4_kor_analysis_.*\\.xlsx$", full.names = TRUE)
data_df <- NULL
use_precomputed_tf <- FALSE

if (length(excel_files) > 0) {
  file_info <- file.info(excel_files)
  excel_files <- excel_files[order(file_info$mtime, decreasing = TRUE)]
  excel_file <- excel_files[1]
  sheet_names <- excel_sheets(excel_file)
  target_sheet <- if ("TF_Top50" %in% sheet_names) "TF_Top50" else sheet_names[1]
  data_df <- read_excel(excel_file, sheet = target_sheet)
  required_cols <- c("rank", "TF_p1", "TF_p2", "TF_p3", "TF_p4")
  if (!any(is.na(match(required_cols, colnames(data_df))))) {
    use_precomputed_tf <- TRUE
    cat("Using precomputed TF table:", basename(excel_file), ", sheet:", target_sheet, "\n")
  }
}

if (!use_precomputed_tf) {
  cat("Computing TF Top50 from step 02 output...\n")
  load_ntis3_for_tf <- function() {
    ot <- output_dir_tables
    rdata <- list.files(path = ot, pattern = "ntis3_analysis_with_representative_texts_.*\\.RData$", full.names = TRUE)
    csvs  <- list.files(path = ot, pattern = "ntis3_analysis_with_representative_texts_.*\\.csv$", full.names = TRUE)
    if (length(rdata) > 0) {
      latest <- rdata[order(file.info(rdata)$mtime, decreasing = TRUE)[1]]
      load(latest)
      if (!exists("data")) stop("RData did not contain object 'data'")
      return(data)
    }
    if (length(csvs) > 0) {
      latest <- csvs[order(file.info(csvs)$mtime, decreasing = TRUE)[1]]
      if (!requireNamespace("data.table", quietly = TRUE)) stop("Need data.table to read CSV")
      data <- data.table::fread(latest)
      need <- c("period", "Key1_representative")
      if (!all(need %in% names(data))) stop("Missing columns: ", paste(need[!need %in% names(data)], collapse = ", "))
      return(as.data.frame(data))
    }
    stop("No step 02 analysis files (ntis3_analysis_with_representative_texts_*.RData or *.csv) in output/tables. Run 02_analysis.R first.")
  }
  # preprocess_korean_tf: shared helper defined at the top of this file
  build_tf_top50 <- function(data) {
    if (!all(1:4 %in% unique(data$period))) {
      cat("Note: step 02 periods present:", paste(sort(unique(data$period)), collapse = ", "), "\n")
    }
    top50_list <- list(p1 = rep(NA_character_, 50), p2 = rep(NA_character_, 50),
                       p3 = rep(NA_character_, 50), p4 = rep(NA_character_, 50))
    all_keywords_combined <- character(0)
    for (p in 1:4) {
      idx <- data$period == p & !is.na(data$Key1_representative) & data$Key1_representative != ""
      if (sum(idx) == 0) next
      kw <- preprocess_korean_tf(data$Key1_representative[idx])
      all_keywords_combined <- c(all_keywords_combined, kw)
      tbl <- sort(table(kw), decreasing = TRUE)
      top50 <- names(head(tbl, 50))
      if (length(top50) < 50) top50 <- c(top50, rep(NA_character_, 50 - length(top50)))
      top50_list[[paste0("p", p)]] <- top50
    }
    overall_freq <- sort(table(all_keywords_combined), decreasing = TRUE)
    overall_top50 <- names(head(overall_freq, 50))
    if (length(overall_top50) < 50) overall_top50 <- c(overall_top50, rep(NA_character_, 50 - length(overall_top50)))
    result_df <- data.frame(rank = 1:50, stringsAsFactors = FALSE)
    result_df$TF_p1 <- top50_list$p1
    result_df$TF_p2 <- top50_list$p2
    result_df$TF_p3 <- top50_list$p3
    result_df$TF_p4 <- top50_list$p4
    result_df$TF_2006_2024 <- overall_top50
    result_df
  }
  tryCatch({
    ntis3_data <- load_ntis3_for_tf()
    data_df <- build_tf_top50(ntis3_data)
    cat("TF Top50 computed from step 02 (", nrow(ntis3_data), " rows). Table S3 and Figure 3 will use this.\n", sep = "")
  }, error = function(e) {
    stop("Could not build TF Top50 from step 02: ", e$message, "\nRun 02_analysis.R first.")
  })
}

cat("Data dimensions:", nrow(data_df), "x", ncol(data_df), "\n")
required_cols <- c("rank", "TF_p1", "TF_p2", "TF_p3", "TF_p4")
missing_cols <- setdiff(required_cols, colnames(data_df))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}
cat("All required TF columns present.\n")

# =============================================================================
# Section 2: Data Preparation
# =============================================================================

prepare_period_data <- function(data, period_col, period_name) {
  clean_data <- data %>%
    filter(!is.na(!!sym(period_col)), !!sym(period_col) != "", !!sym(period_col) != "NA") %>%
    select(rank, !!sym(period_col)) %>%
    rename(keyword = !!sym(period_col)) %>%
    mutate(period = period_name) %>%
    arrange(rank)
  head(clean_data, 30)
}

periods_data <- list(
  p1 = prepare_period_data(data_df, "TF_p1", "2006-2010"),
  p2 = prepare_period_data(data_df, "TF_p2", "2011-2015"),
  p3 = prepare_period_data(data_df, "TF_p3", "2016-2020"),
  p4 = prepare_period_data(data_df, "TF_p4", "2021-2024")
)

for (nm in names(periods_data)) {
  periods_data[[nm]]$keyword <- trimws(periods_data[[nm]]$keyword)
}

# =============================================================================
# Section 2.1: Translation Mapping (KO -> EN) - prefer XLSX, fallback CSV
# =============================================================================

translation_xlsx <- file.path(LEXICON_DIR, "keyword_translation.xlsx")
translation_xlsx_alt <- file.path(output_dir_tables, "keyword_translation.xlsx")
translation_xlsx_template <- file.path(output_dir_tables, "keyword_translation_template.xlsx")
translation_csv  <- file.path(LEXICON_DIR, "keyword_translation.csv")
translation_csv_alt  <- file.path(output_dir_tables, "keyword_translation.csv")
translation_df <- NULL

read_xlsx_map <- function(path) {
  readxl::read_xlsx(path) %>%
    select(ko, en) %>%
    mutate(ko = trimws(as.character(ko)), en = trimws(as.character(en))) %>%
    mutate(en = ifelse(is.na(en) | en == "", NA_character_, en)) %>%
    distinct()
}

if (file.exists(translation_xlsx)) {
  translation_df <- tryCatch(read_xlsx_map(translation_xlsx), error = function(e) {cat("Failed to read XLSX:", e$message, "\n"); NULL})
  if (!is.null(translation_df)) cat("Loaded translations from XLSX:", nrow(translation_df), "rows\n")
} else if (file.exists(translation_xlsx_alt)) {
  translation_df <- tryCatch(read_xlsx_map(translation_xlsx_alt), error = function(e) {cat("Failed to read XLSX (alt):", e$message, "\n"); NULL})
  if (!is.null(translation_df)) cat("Loaded translations from XLSX (output/tables):", nrow(translation_df), "rows\n")
} else if (file.exists(translation_xlsx_template)) {
  translation_df <- tryCatch(read_xlsx_map(translation_xlsx_template), error = function(e) {cat("Failed to read XLSX (template):", e$message, "\n"); NULL})
  if (!is.null(translation_df)) cat("Loaded translations from XLSX template:", nrow(translation_df), "rows\n")
} else if (file.exists(translation_csv)) {
  translation_df <- tryCatch(read.csv(translation_csv, stringsAsFactors = FALSE) %>% select(ko, en) %>% mutate(ko = trimws(ko), en = trimws(en)) %>% distinct(), error = function(e) {cat("Failed to read CSV:", e$message, "\n"); NULL})
  if (!is.null(translation_df)) cat("Loaded translations from CSV:\n")
} else if (file.exists(translation_csv_alt)) {
  translation_df <- tryCatch(read.csv(translation_csv_alt, stringsAsFactors = FALSE) %>% select(ko, en) %>% mutate(ko = trimws(ko), en = trimws(en)) %>% distinct(), error = function(e) {cat("Failed to read CSV (alt):", e$message, "\n"); NULL})
  if (!is.null(translation_df)) cat("Loaded translations from CSV (output/tables)\n")
} else {
  cat("Translation file not found. Using original keywords.\n")
}

translate_vector <- function(x, map_df) {
  if (is.null(map_df)) return(x)
  tmp <- data.frame(keyword = trimws(x), stringsAsFactors = FALSE)
  tmp2 <- tmp %>% left_join(map_df, by = c("keyword" = "ko"))
  ifelse(is.na(tmp2$en) | tmp2$en == "", tmp2$keyword, tmp2$en)
}

for (nm in names(periods_data)) {
  periods_data[[nm]]$keyword <- translate_vector(periods_data[[nm]]$keyword, translation_df)
}

# Unify display: shorten / standardize labels for table and figure
normalize_display_name <- function(x) {
  x[x %in% c("Cancer Treatment", "Treatment")] <- "Treatment"
  x[x %in% c("Anticancer Drug Resistance", "Drug Resistance")] <- "Drug Resistance"
  x[x %in% c("Immuno-oncology Drug", "Immuno-oncology drug", "Immuno-oncology")] <- "Immuno-oncology"
  x[x %in% c("Triple Negative Breast Cancer", "Triple negative breast cancer")] <- "Triple negative"
  x[x %in% c("Immune Checkpoint Inhibitor", "Checkpoint Inhibitor")] <- "Checkpoint Inhibitor"
  x[x %in% c("Head and Neck Cancer", "Head&Neck Cancer")] <- "Head&Neck Cancer"
  x
}
for (nm in names(periods_data)) {
  if (!is.null(periods_data[[nm]])) periods_data[[nm]]$keyword <- normalize_display_name(periods_data[[nm]]$keyword)
}

# =============================================================================
# Section 3: Table-Style Diagram Functions (Ver2)
# =============================================================================

create_table_flow_diagram <- function(periods_data) {
  # White columns: width 0.11 each (wider for labels); gray bands between
  # Column centers: 0.095, 0.305, 0.515, 0.725
  p <- ggplot() +
    # Epoch rectangles (white columns)
    annotate("rect", xmin = 0.04, xmax = 0.15, ymin = 0.05, ymax = 0.95, fill = "white", color = "black", linewidth = 1) +
    annotate("rect", xmin = 0.25, xmax = 0.36, ymin = 0.05, ymax = 0.95, fill = "white", color = "black", linewidth = 1) +
    annotate("rect", xmin = 0.46, xmax = 0.57, ymin = 0.05, ymax = 0.95, fill = "white", color = "black", linewidth = 1) +
    annotate("rect", xmin = 0.67, xmax = 0.78, ymin = 0.05, ymax = 0.95, fill = "white", color = "black", linewidth = 1) +
    # Rank-changing gray bands
    annotate("rect", xmin = 0.15, xmax = 0.25, ymin = 0.05, ymax = 0.95, fill = "lightgray", color = "black", linewidth = 1, alpha = 0.3) +
    annotate("rect", xmin = 0.36, xmax = 0.46, ymin = 0.05, ymax = 0.95, fill = "lightgray", color = "black", linewidth = 1, alpha = 0.3) +
    annotate("rect", xmin = 0.57, xmax = 0.67, ymin = 0.05, ymax = 0.95, fill = "lightgray", color = "black", linewidth = 1, alpha = 0.3) +
    # Connections confined within bands
    add_epoch_connections(periods_data$p1, periods_data$p2, 0.15, 0.25, "E1?E2") +
    add_epoch_connections(periods_data$p2, periods_data$p3, 0.36, 0.46, "E2?E3") +
    add_epoch_connections(periods_data$p3, periods_data$p4, 0.57, 0.67, "E3?E4") +
    # Headers (centers of white columns)
    annotate("text", x = 0.095, y = 0.97, label = "2006-2010", hjust = 0.5, size = 4, fontface = "bold") +
    annotate("text", x = 0.305, y = 0.97, label = "2011-2015", hjust = 0.5, size = 4, fontface = "bold") +
    annotate("text", x = 0.515, y = 0.97, label = "2016-2020", hjust = 0.5, size = 4, fontface = "bold") +
    annotate("text", x = 0.725, y = 0.97, label = "2021-2024", hjust = 0.5, size = 4, fontface = "bold") +
    xlim(0, 0.82) +
    ylim(0, 1) +
    scale_color_identity() +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(0, 0, 0, 0, unit = "pt"))
  p
}

add_epoch_connections <- function(left_data, right_data, left_x, right_x, connection_label) {
  common_keywords <- intersect(left_data$keyword, right_data$keyword)
  disappearing_keywords <- setdiff(left_data$keyword, right_data$keyword)
  emerging_keywords <- setdiff(right_data$keyword, left_data$keyword)
  line_data <- data.frame()

  if (length(common_keywords) > 0) {
    for (kw in common_keywords) {
      left_pos <- which(left_data$keyword == kw)[1]
      right_pos <- which(right_data$keyword == kw)[1]
      rank_change <- left_pos - right_pos
      if (rank_change > 0) {
        color_intensity <- min(abs(rank_change) / 30, 1)
        red_weight <- 0.2 * (1 - color_intensity)
        green_weight <- 0.8 * (1 - color_intensity)
        blue_weight <- 0.2 + 0.8 * color_intensity
        color <- rgb(red_weight, green_weight, blue_weight)
      } else if (rank_change < 0) {
        color_intensity <- min(abs(rank_change) / 30, 1)
        red_weight <- 0.8 + 0.2 * color_intensity
        green_weight <- 0.2 + 0.8 * (1 - color_intensity)
        blue_weight <- 0.2 * (1 - color_intensity)
        color <- rgb(red_weight, green_weight, blue_weight)
      } else {
        color <- "green"
      }
      line_data <- rbind(line_data, data.frame(
        x1 = left_x,
        y1 = seq(0.9, 0.1, length.out = nrow(left_data))[left_pos],
        x2 = right_x,
        y2 = seq(0.9, 0.1, length.out = nrow(right_data))[right_pos],
        color = color
      ))
    }
  }

  if (length(disappearing_keywords) > 0) {
    for (kw in disappearing_keywords) {
      left_pos <- which(left_data$keyword == kw)[1]
      color_intensity <- (61 - left_pos) / 60
      color <- rgb(0.8 + 0.2 * color_intensity, 0, 0)
      line_data <- rbind(line_data, data.frame(
        x1 = left_x,
        y1 = seq(0.9, 0.1, length.out = nrow(left_data))[left_pos],
        x2 = right_x,
        y2 = 0.05,
        color = color
      ))
    }
  }

  if (length(emerging_keywords) > 0) {
    for (kw in emerging_keywords) {
      right_pos <- which(right_data$keyword == kw)[1]
      color_intensity <- (61 - right_pos) / 60
      color <- rgb(0, 0, 0.8 + 0.2 * color_intensity)
      line_data <- rbind(line_data, data.frame(
        x1 = left_x,
        y1 = 0.05,
        x2 = right_x,
        y2 = seq(0.9, 0.1, length.out = nrow(right_data))[right_pos],
        color = color
      ))
    }
  }

  geom_segment(data = line_data, aes(x = x1, y = y1, xend = x2, yend = y2, color = color), alpha = 0.4, linewidth = 0.8)
}

add_epoch_labels <- function(periods_data) {
  # x positions = centers of white columns (0.095, 0.305, 0.515, 0.725)
  label_layers <- list()
  label_layers[[1]] <- geom_text(data = periods_data$p1,
                                 aes(x = 0.095, y = seq(0.9, 0.1, length.out = nrow(periods_data$p1)), label = keyword),
                                 hjust = 0.5, size = 3.6, fontface = "bold", color = "black")
  label_layers[[2]] <- geom_text(data = periods_data$p2,
                                 aes(x = 0.305, y = seq(0.9, 0.1, length.out = nrow(periods_data$p2)), label = keyword),
                                 hjust = 0.5, size = 3.6, fontface = "bold", color = "black")
  label_layers[[3]] <- geom_text(data = periods_data$p3,
                                 aes(x = 0.515, y = seq(0.9, 0.1, length.out = nrow(periods_data$p3)), label = keyword),
                                 hjust = 0.5, size = 3.6, fontface = "bold", color = "black")
  label_layers[[4]] <- geom_text(data = periods_data$p4,
                                 aes(x = 0.725, y = seq(0.9, 0.1, length.out = nrow(periods_data$p4)), label = keyword),
                                 hjust = 0.5, size = 3.6, fontface = "bold", color = "black")
  # Other keywords labels (slightly smaller, gray to distinguish from main keywords)
  other_color <- "gray40"
  label_layers[[5]] <- annotate("text", x = 0.095, y = 0.07, label = "Other keywords", hjust = 0.5, size = 3.8, fontface = "bold", color = other_color)
  label_layers[[6]] <- annotate("text", x = 0.305, y = 0.07, label = "Other keywords", hjust = 0.5, size = 3.8, fontface = "bold", color = other_color)
  label_layers[[7]] <- annotate("text", x = 0.515, y = 0.07, label = "Other keywords", hjust = 0.5, size = 3.8, fontface = "bold", color = other_color)
  label_layers[[8]] <- annotate("text", x = 0.725, y = 0.07, label = "Other keywords", hjust = 0.5, size = 3.8, fontface = "bold", color = other_color)
  label_layers
}

# =============================================================================
# Section 4: Build and Save
# =============================================================================

# =============================================================================
# Section 4.1: Create Table S3 - Epoch top 50 keywords (with counts)
# =============================================================================

cat("\n=== CREATING TABLE S3: EPOCH TOP 50 KEYWORDS (WITH COUNTS) ===\n")
cat("Note: Table S3 uses English keywords (translated from Korean) with TF counts.\n")

# Prepare top 50 data for each epoch (using original Korean keywords before translation)
prepare_period_data_top50 <- function(data, period_col, period_name) {
  clean_data <- data %>%
    filter(!is.na(!!sym(period_col)), !!sym(period_col) != "", !!sym(period_col) != "NA") %>%
    select(rank, !!sym(period_col)) %>%
    rename(keyword = !!sym(period_col)) %>%
    mutate(period = period_name) %>%
    arrange(rank) %>%
    head(50)  # Top 50 instead of 30

  # Pad with NA if less than 50
  if (nrow(clean_data) < 50) {
    missing_rows <- 50 - nrow(clean_data)
    for (i in 1:missing_rows) {
      clean_data <- rbind(clean_data, data.frame(
        rank = nrow(clean_data) + i,
        keyword = NA_character_,
        period = period_name,
        stringsAsFactors = FALSE
      ))
    }
  }
  clean_data
}

# Get top 50 keywords for each epoch (before translation, using Korean keywords)
periods_data_top50 <- list(
  p1 = prepare_period_data_top50(data_df, "TF_p1", "2006-2010"),
  p2 = prepare_period_data_top50(data_df, "TF_p2", "2011-2015"),
  p3 = prepare_period_data_top50(data_df, "TF_p3", "2016-2020"),
  p4 = prepare_period_data_top50(data_df, "TF_p4", "2021-2024")
)

# Get top 50 keywords for overall period (2006-2024) if available
if ("TF_2006_2024" %in% colnames(data_df)) {
  periods_data_top50$overall <- prepare_period_data_top50(data_df, "TF_2006_2024", "2006-2024")
  cat("Found TF_2006_2024 column - will include overall period in Table S3\n")
} else {
  cat("TF_2006_2024 column not found - overall period not available\n")
  periods_data_top50$overall <- NULL
}

# Helper function to detect Korean characters
has_korean <- function(text) {
  if (is.na(text) || text == "") return(FALSE)
  # Korean Unicode range: AC00-D7AF (Hangul syllables)
  # Also check for common Korean characters in other ranges
  korean_pattern <- "[\uAC00-\uD7AF\u1100-\u11FF\u3130-\u318F]"
  grepl(korean_pattern, text, perl = TRUE)
}

# Store original Korean keywords before translation (for frequency lookup)
periods_data_top50_kor_orig <- list()
for (nm in names(periods_data_top50)) {
  if (!is.null(periods_data_top50[[nm]])) {
    periods_data_top50_kor_orig[[nm]] <- trimws(periods_data_top50[[nm]]$keyword)
  }
}

# Apply translation to top 50 keywords
for (nm in names(periods_data_top50)) {
  if (!is.null(periods_data_top50[[nm]])) {
    periods_data_top50[[nm]]$keyword <- trimws(periods_data_top50[[nm]]$keyword)
    periods_data_top50[[nm]]$keyword <- translate_vector(periods_data_top50[[nm]]$keyword, translation_df)
    periods_data_top50[[nm]]$keyword <- normalize_display_name(periods_data_top50[[nm]]$keyword)
  }
}

# Check for any remaining Korean characters in translated keywords
untranslated_keywords <- character()
for (nm in names(periods_data_top50)) {
  if (!is.null(periods_data_top50[[nm]])) {
    korean_found <- sapply(periods_data_top50[[nm]]$keyword, has_korean)
    if (any(korean_found, na.rm = TRUE)) {
      untranslated <- periods_data_top50[[nm]]$keyword[korean_found & !is.na(periods_data_top50[[nm]]$keyword)]
      untranslated_keywords <- c(untranslated_keywords, unique(untranslated))
    }
  }
}

# Report untranslated keywords
if (length(untranslated_keywords) > 0) {
  cat("\nWARNING: Found", length(unique(untranslated_keywords)), "untranslated Korean keywords in Table S3:\n")
  for (kw in unique(untranslated_keywords)) {
    cat("  -", kw, "\n")
  }
  cat("Please add translations for these keywords to the translation file.\n")
} else {
  cat("All keywords in Table S3 have been translated to English.\n")
}

# =============================================================================
# Calculate frequencies from original step 02 data
# =============================================================================

cat("\nCalculating keyword frequencies from original data...\n")

# Load step 02 data to get frequencies
ntis3_rdata_files <- list.files(path = output_dir_tables, pattern = "ntis3_analysis_with_representative_texts_.*\\.RData$", full.names = TRUE)
frequencies_available <- FALSE
keyword_frequencies <- list()

if (length(ntis3_rdata_files) > 0) {
  file_info <- file.info(ntis3_rdata_files)
  latest_ntis3_file <- ntis3_rdata_files[order(file_info$mtime, decreasing = TRUE)][1]
  cat("Loading step 02 data for frequency calculation:", basename(latest_ntis3_file), "\n")

  tryCatch({
    load(latest_ntis3_file)  # loads 'data'

    if (exists("data") && "Key1_representative" %in% colnames(data) && "period" %in% colnames(data)) {
      # 빈도 집계: 파일 상단 preprocess_korean_tf와 동일 규칙 (한글 키 기준)

      # Calculate frequencies for each period
      period_mapping <- list(
        "2006-2010" = 1,
        "2011-2015" = 2,
        "2016-2020" = 3,
        "2021-2024" = 4
      )

      # Get original Korean keywords before translation for frequency calculation
      periods_data_top50_kor <- list(
        p1 = prepare_period_data_top50(data_df, "TF_p1", "2006-2010"),
        p2 = prepare_period_data_top50(data_df, "TF_p2", "2011-2015"),
        p3 = prepare_period_data_top50(data_df, "TF_p3", "2016-2020"),
        p4 = prepare_period_data_top50(data_df, "TF_p4", "2021-2024")
      )

      if ("TF_2006_2024" %in% colnames(data_df)) {
        periods_data_top50_kor$overall <- prepare_period_data_top50(data_df, "TF_2006_2024", "2006-2024")
      }

      # Calculate frequencies for each period
      for (period_name in names(period_mapping)) {
        period_num <- period_mapping[[period_name]]
        period_data <- data[data$period == period_num & !is.na(data$Key1_representative) & data$Key1_representative != "", ]

        if (nrow(period_data) > 0) {
          keywords <- preprocess_korean_tf(period_data$Key1_representative)
          keyword_counts <- table(keywords)

          # Get top 50 keywords for this period (Korean)
          period_keywords_kor <- periods_data_top50_kor[[paste0("p", period_num)]]$keyword
          period_keywords_kor <- period_keywords_kor[!is.na(period_keywords_kor)]

          # Create frequency mapping
          freq_map <- as.numeric(keyword_counts)
          names(freq_map) <- names(keyword_counts)

          keyword_frequencies[[period_name]] <- freq_map
        }
      }

      # Calculate overall frequency (2006-2024)
      all_keywords <- preprocess_korean_tf(data$Key1_representative)
      overall_counts <- table(all_keywords)
      keyword_frequencies[["2006-2024"]] <- as.numeric(overall_counts)
      names(keyword_frequencies[["2006-2024"]]) <- names(overall_counts)

      frequencies_available <- TRUE
      cat("Keyword frequencies calculated successfully.\n")
    } else {
      cat("step 02 data structure not as expected. Frequencies not available.\n")
    }
  }, error = function(e) {
    cat("Error loading step 02 data:", e$message, "\n")
    cat("Frequencies will not be included in Table S3.\n")
  })
} else {
  cat("step 02 RData file not found. Frequencies will not be included in Table S3.\n")
}

# normalize_korean_for_lookup: 파일 상단 정의 사용 (preprocess_korean_tf와 정합)

# Aggregate count from freq_map where names contain pattern (for composite terms)
aggregate_count_by_pattern <- function(freq_map, pattern) {
  if (is.null(freq_map)) return(NA_real_)
  idx <- grepl(pattern, names(freq_map), fixed = TRUE)
  if (!any(idx)) return(NA_real_)
  sum(as.numeric(freq_map[idx]), na.rm = TRUE)
}

# freq_map 이름에 부분문자열이 포함된 항목 합산 (영문 표 번역 라벨 → 한글 토큰 묶음용)
aggregate_count_name_substrings <- function(freq_map, substrings) {
  if (is.null(freq_map)) return(NA_real_)
  nm <- names(freq_map)
  idx <- rep(FALSE, length(nm))
  for (s in substrings) {
    idx <- idx | grepl(s, nm, fixed = TRUE)
  }
  if (!any(idx)) return(NA_real_)
  sum(as.numeric(freq_map[idx]), na.rm = TRUE)
}

# Function to add count to keyword (uses normalized Korean for lookup; fallback by English label)
add_count_to_keyword <- function(keyword_eng, keyword_kor, freq_map) {
  if (is.na(keyword_eng) || keyword_eng == "") {
    return(NA_character_)
  }

  count <- NA_real_
  if (!is.null(freq_map)) {
    if (!is.null(keyword_kor) && !is.na(keyword_kor) && keyword_kor != "") {
      kor_norm <- normalize_korean_for_lookup(keyword_kor)
      if (kor_norm %in% names(freq_map)) {
        count <- as.numeric(freq_map[[kor_norm]])
      }
      if (is.na(count)) {
        idx <- grepl(kor_norm, names(freq_map), fixed = TRUE)
        if (!any(idx)) idx <- grepl(keyword_kor, names(freq_map), fixed = TRUE)
        if (any(idx)) count <- sum(as.numeric(freq_map[idx]), na.rm = TRUE)
      }
    }
    # Fallback: 영문 표기만 있을 때 한글 토큰 부분일치로 합산
    if (is.na(count) || !is.finite(count)) {
      if (keyword_eng %in% c("Treatment", "Cancer Treatment")) {
        count <- aggregate_count_name_substrings(
          freq_map,
          c("암치료", "면역치료", "방사선치료", "항암제", "줄기세포")
        )
      } else if (keyword_eng %in% c("Drug Resistance", "Anticancer Drug Resistance")) {
        count <- aggregate_count_name_substrings(freq_map, c("내성", "저항", "항암제내성"))
      }
    }
  }

  if (!is.na(count) && is.finite(count)) {
    return(paste0(keyword_eng, " (", count, ")"))
  }
  return(keyword_eng)
}

# Helper to safely extract counts from frequency map (used by reorder block and Table S3)
lookup_count_safe <- function(keyword_kor, freq_map) {
  if (is.null(freq_map)) return(NA_real_)
  if (is.na(keyword_kor) || keyword_kor == "") return(NA_real_)
  kor_norm <- normalize_korean_for_lookup(keyword_kor)
  if (kor_norm %in% names(freq_map)) return(as.numeric(freq_map[[kor_norm]]))
  idx <- grepl(kor_norm, names(freq_map), fixed = TRUE)
  if (!any(idx)) idx <- grepl(keyword_kor, names(freq_map), fixed = TRUE)
  if (any(idx)) return(sum(as.numeric(freq_map[idx]), na.rm = TRUE))
  return(NA_real_)
}

# =============================================================================
# Reorder each period by TF (count) so Table S3 rank = actual count rank; Figure 3 uses same order
# =============================================================================
if (frequencies_available) {
  period_names <- c("2006-2010", "2011-2015", "2016-2020", "2021-2024")
  period_slots <- c("p1", "p2", "p3", "p4")

  for (i in seq_along(period_slots)) {
    slot <- period_slots[i]
    period_name <- period_names[i]
    if (is.null(periods_data_top50[[slot]])) next

    eng <- periods_data_top50[[slot]]$keyword
    kor <- periods_data_top50_kor_orig[[slot]]
    n <- length(eng)
    counts <- vapply(seq_len(n), function(j) {
      lookup_count_safe(kor[j], keyword_frequencies[[period_name]])
    }, numeric(1))

    # Sort by count descending (NA last); tie-break by original row order
    ord <- order(-counts, na.last = TRUE, seq_len(n))
    eng_sorted <- eng[ord]
    kor_sorted <- kor[ord]

    periods_data_top50[[slot]] <- data.frame(
      rank = seq_len(n),
      keyword = eng_sorted,
      period = periods_data_top50[[slot]]$period[1],
      stringsAsFactors = FALSE
    )
    periods_data_top50_kor_orig[[slot]] <- kor_sorted
  }

  if (!is.null(periods_data_top50$overall) && !is.null(keyword_frequencies[["2006-2024"]])) {
    eng <- periods_data_top50$overall$keyword
    kor <- periods_data_top50_kor_orig$overall
    n <- length(eng)
    counts <- vapply(seq_len(n), function(j) {
      lookup_count_safe(kor[j], keyword_frequencies[["2006-2024"]])
    }, numeric(1))
    ord <- order(-counts, na.last = TRUE, seq_len(n))
    periods_data_top50$overall <- data.frame(
      rank = seq_len(n),
      keyword = eng[ord],
      period = "2006-2024",
      stringsAsFactors = FALSE
    )
    periods_data_top50_kor_orig$overall <- kor[ord]
  }

  # Rebuild periods_data (top 30) for Figure 3 from reordered top 50
  for (slot in period_slots) {
    if (!is.null(periods_data_top50[[slot]])) {
      top30 <- head(periods_data_top50[[slot]], 30)
      top30$rank <- seq_len(nrow(top30))
      periods_data[[slot]] <- top30
    }
  }
  cat("Reordered Table S3 and Figure 3 data by TF (term frequency) per period.\n")
}

# Create Table S3 only: Rank / 2006-2010 / ... / 2021-2024 / 2006-2024
# Keywords (English) with frequency counts; ranking by Korean-keyword TF (step 02)
tableS3_top50 <- data.frame(
  Rank = 1:50,
  stringsAsFactors = FALSE
)

if (frequencies_available) {
  tableS3_top50[["2006-2010"]] <- mapply(add_count_to_keyword,
                                        periods_data_top50$p1$keyword,
                                        periods_data_top50_kor_orig$p1,
                                        MoreArgs = list(freq_map = keyword_frequencies[["2006-2010"]]))
  tableS3_top50[["2011-2015"]] <- mapply(add_count_to_keyword,
                                        periods_data_top50$p2$keyword,
                                        periods_data_top50_kor_orig$p2,
                                        MoreArgs = list(freq_map = keyword_frequencies[["2011-2015"]]))
  tableS3_top50[["2016-2020"]] <- mapply(add_count_to_keyword,
                                        periods_data_top50$p3$keyword,
                                        periods_data_top50_kor_orig$p3,
                                        MoreArgs = list(freq_map = keyword_frequencies[["2016-2020"]]))
  tableS3_top50[["2021-2024"]] <- mapply(add_count_to_keyword,
                                        periods_data_top50$p4$keyword,
                                        periods_data_top50_kor_orig$p4,
                                        MoreArgs = list(freq_map = keyword_frequencies[["2021-2024"]]))

  if (!is.null(periods_data_top50$overall) && !is.null(periods_data_top50_kor_orig$overall)) {
    tableS3_top50[["2006-2024"]] <- mapply(add_count_to_keyword,
                                          periods_data_top50$overall$keyword,
                                          periods_data_top50_kor_orig$overall,
                                          MoreArgs = list(freq_map = keyword_frequencies[["2006-2024"]]))
    cat("Added 2006-2024 column with counts to Table S3\n")
  } else {
    tableS3_top50[["2006-2024"]] <- NA_character_
  }
  cat("Table S3 created with keyword counts (Korean-keyword TF).\n")
} else {
  # If frequencies not available, keywords only (no counts)
  tableS3_top50[["2006-2010"]] <- periods_data_top50$p1$keyword
  tableS3_top50[["2011-2015"]] <- periods_data_top50$p2$keyword
  tableS3_top50[["2016-2020"]] <- periods_data_top50$p3$keyword
  tableS3_top50[["2021-2024"]] <- periods_data_top50$p4$keyword

  if (!is.null(periods_data_top50$overall)) {
    tableS3_top50[["2006-2024"]] <- periods_data_top50$overall$keyword
    cat("Table S3 created (counts not available).\n")
  } else {
    tableS3_top50[["2006-2024"]] <- NA_character_
  }
}

# Save Table S3 as Excel (키워드·건수; 한글 Key1 기준 TF)
tableS3_xlsx <- file.path(output_dir_tables, paste0("tableS3_epoch_top50_keywords_eng_with_counts_", timestamp, ".xlsx"))
wb_tableS3 <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb_tableS3, "TableS3")
openxlsx::writeData(wb_tableS3, "TableS3", tableS3_top50)
openxlsx::saveWorkbook(wb_tableS3, tableS3_xlsx, overwrite = TRUE)
cat("Table S3 (keywords with counts, Korean-keyword TF) saved to:", tableS3_xlsx, "\n")

# Save final keywords to Excel/CSV (for Figure 3 - top 30)
final_keywords <- dplyr::bind_rows(
  periods_data$p1 %>% dplyr::mutate(period_id = "p1"),
  periods_data$p2 %>% dplyr::mutate(period_id = "p2"),
  periods_data$p3 %>% dplyr::mutate(period_id = "p3"),
  periods_data$p4 %>% dplyr::mutate(period_id = "p4")
) %>%
  dplyr::group_by(period_id) %>%
  dplyr::mutate(rank_final = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::select(period = period, period_id, rank = rank_final, keyword)

# Save final keywords as Excel (for Figure 3 - top 30)
final_xlsx_path <- file.path(output_dir_tables, paste0("ntis5_flow_keywords_final_eng_", timestamp, ".xlsx"))
wb_final <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb_final, "Final_Keywords")
openxlsx::writeData(wb_final, "Final_Keywords", final_keywords)
openxlsx::saveWorkbook(wb_final, final_xlsx_path, overwrite = TRUE)
cat("Saved final keywords (Excel) to:", final_xlsx_path, "\n")

# Build figure
table_flow <- create_table_flow_diagram(periods_data)
labels <- add_epoch_labels(periods_data)
final_plot <- table_flow + labels[[1]] + labels[[2]] + labels[[3]] + labels[[4]] + labels[[5]] + labels[[6]] + labels[[7]] + labels[[8]]

# Save figure (PNG only, minimal margin)
png_filename <- file.path(output_dir_figures, paste0("figure3_epoch_keyword_top30_flow_", timestamp, ".png"))
ggplot2::ggsave(png_filename, plot = final_plot, width = 16, height = 8, dpi = 300, bg = "white")

cat("Figure 3 (시대별 keyword 상위 30 플로우) saved to:", png_filename, "\n")
cat("=== 03_keyword_flow.R COMPLETED ===\n")


