# =============================================================================
# Ministry-level allocation of cancer R&D.
#
# Aggregates cancer-related projects and funding by the ministry recorded as the
# funding source, and expresses each ministry's cancer activity as a share of
# its total R&D portfolio. Funding is reported in billion KRW.
#
# Input :  NTIS_cancer_classification_2006-2024_*.RData
# Output:  table1_inst_pay_cancer_share_*  -> Table 1
# =============================================================================

if (!exists("PROJ_ROOT")) source("config.R")

FINAL_YEAR_RANGE <- c(2021, 2024)

# Publication 시트: 암 프로젝트 수 기준 상위 N개 부처, 나머지 Other 통합
PUBLICATION_TOP_N <- 10L

# Table 1 footnote (edit if needed; years are filled from each sheet’s period)
table1_footnote_eng <- function(yr_min, yr_max) {
  paste0(
    "Note: For each government ministry or department (Inst_pay), percentages were calculated as ",
    "the number of cancer-related projects (or cancer-related funding) divided by that ",
    "department's total number of projects and total R&D funding (billion South Korean won), ",
    "respectively, for ",
    yr_min, "\u2013", yr_max, "."
  )
}

library(dplyr)
library(openxlsx)


KRW_PER_BILLION <- 1e9

output_dir_tables <- OUT_TABLES
if (!dir.exists(output_dir_tables)) dir.create(output_dir_tables, recursive = TRUE)
timestamp <- format(Sys.time(), "%Y%m%d")

cat("=== 06_inst_pay: ministry-level analysis ===\n")

# -----------------------------------------------------------------------------
# 1. Load step 00 classification output (same data as from original xls)
# -----------------------------------------------------------------------------
rdata_files <- list.files(path = output_dir_tables,
                          pattern = "NTIS_cancer_classification_2006-2024_.*\\.RData$",
                          full.names = TRUE)
csv_files   <- list.files(path = output_dir_tables,
                          pattern = "NTIS_cancer_classification_2006-2024_.*\\.csv$",
                          full.names = TRUE)

if (length(rdata_files) > 0) {
  file_info <- file.info(rdata_files)
  latest_file <- rdata_files[order(file_info$mtime, decreasing = TRUE)[1]]
  load(latest_file)
  cat("Loaded RData:", basename(latest_file), "\n")
} else if (length(csv_files) > 0) {
  file_info <- file.info(csv_files)
  latest_file <- csv_files[order(file_info$mtime, decreasing = TRUE)[1]]
  data <- read.csv(latest_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  cat("Loaded CSV:", basename(latest_file), "(UTF-8)\n")
} else {
  stop("No step 00 classification file found (NTIS_cancer_classification_2006-2024_*.RData or *.csv). Run 00_build_classification.R first.")
}

# Ensure required columns
if (!"Inst_pay" %in% colnames(data)) stop("Inst_pay column not found.")
if (!"cancer_related" %in% colnames(data)) stop("cancer_related column not found.")
if (!"Year" %in% colnames(data)) stop("Year column not found.")
if (!"MoneyC" %in% colnames(data)) stop("MoneyC column not found.")

# Coerce types
data$Year <- as.numeric(as.character(data$Year))
data$MoneyC_num <- as.numeric(as.character(data$MoneyC))
data$MoneyC_num[!is.finite(data$MoneyC_num)] <- NA_real_
data$cancer_related <- as.logical(data$cancer_related)
data$cancer_related[is.na(data$cancer_related)] <- FALSE

# 부처명: NA/빈문자 → "미분류"로 통일 (한글 저장 시 인코딩 일관)
data$Inst_pay_chr <- as.character(data$Inst_pay)
data$Inst_pay_chr <- trimws(data$Inst_pay_chr)
data$Inst_pay_chr[is.na(data$Inst_pay_chr) | data$Inst_pay_chr == ""] <- "미분류"

# 2006-2024만 사용
data <- data %>% filter(Year >= 2006, Year <= 2024)
cat("Data rows (2006-2024):", nrow(data), "\n")

# -----------------------------------------------------------------------------
# 2. 시기 정의: FINAL_YEAR_RANGE가 있으면 해당 구간만, 없으면 Total + 4 epochs
# -----------------------------------------------------------------------------
if (!is.null(FINAL_YEAR_RANGE) && length(FINAL_YEAR_RANGE) >= 2) {
  yr_min <- as.integer(FINAL_YEAR_RANGE[1])
  yr_max <- as.integer(FINAL_YEAR_RANGE[2])
  data <- data %>% filter(Year >= yr_min, Year <= yr_max)
  cat("Final table limited to", yr_min, "-", yr_max, ":", nrow(data), "rows\n")
  periods <- setNames(
    list(list(yr_min = yr_min, yr_max = yr_max)),
    paste0("Full_", yr_min, "-", yr_max)
  )
} else {
  periods <- list(
    "Total_2006-2024"   = list(yr_min = 2006, yr_max = 2024),
    "Epoch1_2006-2010"  = list(yr_min = 2006, yr_max = 2010),
    "Epoch2_2011-2015"  = list(yr_min = 2011, yr_max = 2015),
    "Epoch3_2016-2020"  = list(yr_min = 2016, yr_max = 2020),
    "Epoch4_2021-2024"  = list(yr_min = 2021, yr_max = 2024)
  )
}

# -----------------------------------------------------------------------------
# 3. 시기별 Inst_pay 표 (열: 영어, 연구비: billion KRW)
#     저장 직전: funding·pct 열은 소수 첫째 자리 문자열 (예: 1.0, 0.0)
# -----------------------------------------------------------------------------
format_table1_one_decimal_display <- function(df) {
  if (nrow(df) == 0L) return(df)
  fmt <- function(x) {
    x <- as.numeric(x)
    out <- rep(NA_character_, length(x))
    ok <- !is.na(x)
    out[ok] <- sprintf("%.1f", round(x[ok], 1))
    out
  }
  df$total_funding_billion_KRW <- fmt(df$total_funding_billion_KRW)
  df$pct_cancer_projects <- fmt(df$pct_cancer_projects)
  df$cancer_funding_billion_KRW <- fmt(df$cancer_funding_billion_KRW)
  df$pct_cancer_funding <- fmt(df$pct_cancer_funding)
  df
}

empty_table_eng <- function() {
  data.frame(
    Inst_pay = character(),
    n_projects_total = integer(),
    total_funding_billion_KRW = numeric(),
    n_cancer_projects = integer(),
    pct_cancer_projects = numeric(),
    cancer_funding_billion_KRW = numeric(),
    pct_cancer_funding = numeric(),
    stringsAsFactors = FALSE
  )
}

build_inst_pay_table <- function(dat, yr_min, yr_max, krw_per_bn = KRW_PER_BILLION) {
  sub <- dat %>% filter(Year >= yr_min, Year <= yr_max)
  if (nrow(sub) == 0) return(empty_table_eng())

  total_all_n <- nrow(sub)
  total_all_budget <- sum(sub$MoneyC_num, na.rm = TRUE)
  total_cancer_n <- sum(sub$cancer_related, na.rm = TRUE)
  total_cancer_budget <- sum(sub$MoneyC_num[sub$cancer_related], na.rm = TRUE)

  by_inst <- sub %>%
    group_by(Inst_pay_chr) %>%
    summarise(
      n_projects_total = n(),
      total_funding_krw = sum(MoneyC_num, na.rm = TRUE),
      n_cancer_projects = sum(cancer_related, na.rm = TRUE),
      cancer_funding_krw = sum(MoneyC_num[cancer_related], na.rm = TRUE),
      .groups = "drop"
    )

  by_inst$pct_cancer_projects <- ifelse(
    by_inst$n_projects_total > 0,
    round(by_inst$n_cancer_projects / by_inst$n_projects_total * 100, 1),
    NA_real_
  )
  by_inst$pct_cancer_funding <- ifelse(
    by_inst$total_funding_krw > 0,
    round(by_inst$cancer_funding_krw / by_inst$total_funding_krw * 100, 1),
    NA_real_
  )

  by_inst$total_funding_billion_KRW <- round(by_inst$total_funding_krw / krw_per_bn, 1)
  by_inst$cancer_funding_billion_KRW <- round(by_inst$cancer_funding_krw / krw_per_bn, 1)
  by_inst <- by_inst %>%
    transmute(
      Inst_pay = Inst_pay_chr,
      n_projects_total,
      total_funding_billion_KRW,
      n_cancer_projects,
      pct_cancer_projects,
      cancer_funding_billion_KRW,
      pct_cancer_funding
    ) %>%
    arrange(desc(n_cancer_projects))

  row_total <- data.frame(
    Inst_pay = "National total (reference)",
    n_projects_total = total_all_n,
    total_funding_billion_KRW = round(total_all_budget / krw_per_bn, 1),
    n_cancer_projects = total_cancer_n,
    pct_cancer_projects = if (total_all_n > 0) round(total_cancer_n / total_all_n * 100, 1) else NA_real_,
    cancer_funding_billion_KRW = round(total_cancer_budget / krw_per_bn, 1),
    pct_cancer_funding = if (total_all_budget > 0) round(total_cancer_budget / total_all_budget * 100, 1) else NA_real_,
    stringsAsFactors = FALSE
  )

  rbind(row_total, by_inst)
}

# Nation row + 부처별 상위 top_n, 나머지 Other 1행 (암 프로젝트 수 정렬 유지 가정)
collapse_inst_pay_publication <- function(tbl_full, top_n = PUBLICATION_TOP_N) {
  if (nrow(tbl_full) < 1L) return(tbl_full)
  national <- tbl_full[1L, , drop = FALSE]
  inst <- if (nrow(tbl_full) >= 2L) tbl_full[-1L, , drop = FALSE] else data.frame()
  if (nrow(inst) == 0L) return(national)

  n_take <- min(as.integer(top_n), nrow(inst))
  top <- inst[seq_len(n_take), , drop = FALSE]

  if (nrow(inst) <= n_take) {
    return(rbind(national, top))
  }

  rest <- inst[-seq_len(n_take), , drop = FALSE]
  other_n <- sum(rest$n_projects_total)
  other_cancer_n <- sum(rest$n_cancer_projects)
  other_tot_bn <- sum(rest$total_funding_billion_KRW)
  other_can_bn <- sum(rest$cancer_funding_billion_KRW)

  other_row <- data.frame(
    Inst_pay = "Other",
    n_projects_total = other_n,
    total_funding_billion_KRW = round(other_tot_bn, 1),
    n_cancer_projects = other_cancer_n,
    pct_cancer_projects = if (other_n > 0) round(other_cancer_n / other_n * 100, 1) else NA_real_,
    cancer_funding_billion_KRW = round(other_can_bn, 1),
    pct_cancer_funding = if (other_tot_bn > 0) round(other_can_bn / other_tot_bn * 100, 1) else NA_real_,
    stringsAsFactors = FALSE
  )

  rbind(national, top, other_row)
}

tables_list <- list()
tables_pub_list <- list()
for (period_name in names(periods)) {
  p <- periods[[period_name]]
  tbl <- build_inst_pay_table(data, p$yr_min, p$yr_max)
  tables_list[[period_name]] <- tbl
  tables_pub_list[[period_name]] <- collapse_inst_pay_publication(tbl, PUBLICATION_TOP_N)
  cat("Built:", period_name, "- full rows:", nrow(tbl), ", publication rows:", nrow(tables_pub_list[[period_name]]), "\n")
}

# -----------------------------------------------------------------------------
# 4. Excel 저장 (UTF-8)
# -----------------------------------------------------------------------------
col_widths_eng <- c(32, 18, 22, 18, 20, 24, 20)

out_xlsx <- file.path(output_dir_tables, paste0("table1_inst_pay_cancer_share_", timestamp, ".xlsx"))
wb <- openxlsx::createWorkbook()

single_period <- length(periods) == 1L
for (period_name in names(periods)) {
  p <- periods[[period_name]]
  foot <- table1_footnote_eng(p$yr_min, p$yr_max)

  sheet_full <- period_name
  if (nchar(sheet_full) > 31) sheet_full <- substr(period_name, 1, 31)
  openxlsx::addWorksheet(wb, sheet_full)
  tbl_full_disp <- format_table1_one_decimal_display(tables_list[[period_name]])
  openxlsx::writeData(wb, sheet_full, tbl_full_disp)
  openxlsx::setColWidths(wb, sheet_full, cols = 1:7, widths = col_widths_eng)
  openxlsx::writeData(wb, sheet_full, foot, startRow = nrow(tbl_full_disp) + 2L, colNames = FALSE)

  sheet_pub <- if (single_period) "Publication" else {
    sp <- paste0("Pub_", sheet_full)
    if (nchar(sp) > 31) substr(sp, 1, 31) else sp
  }
  openxlsx::addWorksheet(wb, sheet_pub)
  tbl_pub_disp <- format_table1_one_decimal_display(tables_pub_list[[period_name]])
  openxlsx::writeData(wb, sheet_pub, tbl_pub_disp)
  openxlsx::setColWidths(wb, sheet_pub, cols = 1:7, widths = col_widths_eng)
  openxlsx::writeData(wb, sheet_pub, foot, startRow = nrow(tbl_pub_disp) + 2L, colNames = FALSE)
}

openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)
cat("\nSaved (Table 1 – Inst_pay):", out_xlsx, "\n")
cat(
  "Workbook: Full table + Publication (National + top ",
  PUBLICATION_TOP_N,
  " Inst_pay by cancer projects + Other).\n",
  sep = ""
)
cat("=== 06_inst_pay COMPLETED ===\n")
