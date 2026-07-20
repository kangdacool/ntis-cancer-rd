# =============================================================================
# Research activity against national incidence and mortality.
#
# Joins the site-specific annual research series to published national cancer
# incidence and mortality statistics, and fits piecewise regressions to locate
# breakpoints in each series.
#
# Input :  ntis6_cancer_types_yearly_summary_*, ntis2_yearly_total_stats_*,
#          data/epi/24cancer_1999_2022_incidence_260109.xlsx,
#          data/epi/7_cancer_1999_2024_death_260109.xlsx
# Output:  figure4_epidemiology_research_trends_*,
#          ntis7_cancer_research_vs_epi_piecewise_summary_*  -> Figure 4
# =============================================================================

if (!exists("PROJ_ROOT")) source("config.R")

required_packages <- c("data.table", "dplyr", "tidyr", "ggplot2", "readxl", "openxlsx", "patchwork", "cowplot")
invisible(lapply(required_packages, library, character.only = TRUE))

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(openxlsx)
library(patchwork)
library(cowplot)
library(grid)
library(gridExtra)


output_dir_tables <- OUT_TABLES
output_dir_figures <- OUT_FIGURES
if (!dir.exists(output_dir_tables)) dir.create(output_dir_tables, recursive = TRUE)
if (!dir.exists(output_dir_figures)) dir.create(output_dir_figures, recursive = TRUE)

timestamp <- format(Sys.time(), "%Y%m%d")

cat("=== 05_compare_epi.R - Research vs Death/Incidence Comparison ===\n")

# =============================================================================
# 1. Load step 04 yearly statistics (count & cost)
# =============================================================================

cancer_type_names <- c("갑상선암", "대장암", "폐암", "유방암", "위암",
                       "전립선암", "간암", "췌장암", "신장암",
                       "자궁경부암", "자궁내막암", "난소암")

# Helper: get latest file by pattern
get_latest_file <- function(pattern, dir) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NULL)
  info <- file.info(files)
  files[order(info$mtime, decreasing = TRUE)][1]
}

# Count-based summary
ntis6_count_file <- get_latest_file("ntis6_cancer_types_yearly_summary_count_.*\\.csv$", output_dir_tables)
if (is.null(ntis6_count_file)) {
  stop("step 04 yearly summary (count) file not found in output/tables")
}
cat("Loading step 04 count summary:", basename(ntis6_count_file), "\n")
ntis6_count <- fread(ntis6_count_file)

# Cost-based summary (total cost, not *_cost_avg)
ntis6_cost_file <- get_latest_file("ntis6_cancer_types_yearly_summary_cost_[0-9]{8}\\.csv$", output_dir_tables)
if (is.null(ntis6_cost_file)) {
  cat("Warning: step 04 yearly summary (cost) file not found. Budget correlations will be limited.\n")
  ntis6_cost <- NULL
} else {
  cat("Loading step 04 cost summary:", basename(ntis6_cost_file), "\n")
  ntis6_cost <- fread(ntis6_cost_file)
  # Validate expected column; if not present, skip cost-based analysis
  if (!"total_cancer_cost" %in% colnames(ntis6_cost)) {
    cat("Warning: total_cancer_cost column not found in cost summary. Skipping budget-based correlations.\n")
    ntis6_cost <- NULL
  }
}

# Load yearly total statistics from step 01 (for overall project share calculation)
yearly_stats_available <- FALSE
yearly_total_stats <- NULL
yearly_stats_rdata_files <- list.files(path = output_dir_tables, pattern = "ntis2_yearly_total_stats_.*\\.RData$", full.names = TRUE)
if (length(yearly_stats_rdata_files) > 0) {
  file_info <- file.info(yearly_stats_rdata_files)
  yearly_stats_rdata_files <- yearly_stats_rdata_files[order(file_info$mtime, decreasing = TRUE)]
  stats_file <- yearly_stats_rdata_files[1]
  cat("Loading yearly total statistics from step 01:", basename(stats_file), "\n")
  load(stats_file)
  yearly_stats_available <- TRUE
  cat("Yearly total statistics loaded - will use for overall project share calculation\n")
} else {
  cat("Warning: Yearly total statistics not found - overall project share will be calculated from cancer projects only\n")
}

# Long format: project count & share -----------------------------------------

# Count
ntis6_count_long_counts <- ntis6_count %>%
  select(Year, total_cancer, all_of(cancer_type_names)) %>%
  pivot_longer(cols = all_of(cancer_type_names),
               names_to = "cancer_kor",
               values_to = "projects_count")

# Share 1: percentage of cancer projects (암 연구 내부 비율)
share_cols <- paste0(cancer_type_names, "_pct")
ntis6_count_long_share_cancer <- ntis6_count %>%
  select(Year, all_of(share_cols)) %>%
  pivot_longer(cols = all_of(share_cols),
               names_to = "cancer_kor_share",
               values_to = "projects_share_cancer") %>%
  mutate(cancer_kor = sub("_pct$", "", cancer_kor_share), .keep = "unused")

# Share 2: percentage of total projects (전체 국가 연구에서의 비율)
# Calculate: each cancer type / total_projects * 100
if (yearly_stats_available && !is.null(yearly_total_stats)) {
  ntis6_count_long_share_total <- ntis6_count_long_counts %>%
    left_join(
      yearly_total_stats %>%
        dplyr::select(Year, total_projects) %>%
        rename(year = Year),
      by = c("Year" = "year")
    ) %>%
    mutate(
      projects_share_total = ifelse(
        !is.na(total_projects) & total_projects > 0,
        (projects_count / total_projects) * 100,
        NA_real_
      )
    ) %>%
    dplyr::select(Year, cancer_kor, projects_share_total)
} else {
  ntis6_count_long_share_total <- ntis6_count_long_counts %>%
    mutate(projects_share_total = NA_real_) %>%
    dplyr::select(Year, cancer_kor, projects_share_total)
}

ntis6_count_long <- ntis6_count_long_counts %>%
  left_join(ntis6_count_long_share_cancer, by = c("Year", "cancer_kor")) %>%
  left_join(ntis6_count_long_share_total, by = c("Year", "cancer_kor")) %>%
  # For backward compatibility, keep projects_share as projects_share_cancer
  mutate(projects_share = projects_share_cancer)

# Cost (budget) ---------------------------------------------------------------
if (!is.null(ntis6_cost)) {
  ntis6_cost_long_cost <- ntis6_cost %>%
    select(Year, total_cancer_cost, all_of(cancer_type_names)) %>%
    pivot_longer(cols = all_of(cancer_type_names),
                 names_to = "cancer_kor",
                 values_to = "budget_total")
  share_cols_cost <- paste0(cancer_type_names, "_pct")
  ntis6_cost_long_share <- ntis6_cost %>%
    select(Year, all_of(share_cols_cost)) %>%
    pivot_longer(cols = all_of(share_cols_cost),
                 names_to = "cancer_kor_share",
                 values_to = "budget_share") %>%
    mutate(cancer_kor = sub("_pct$", "", cancer_kor_share), .keep = "unused")
  ntis6_cost_long <- ntis6_cost_long_cost %>%
    left_join(ntis6_cost_long_share, by = c("Year", "cancer_kor"))
} else {
  ntis6_cost_long <- NULL
}

# Merge count & cost into one table ------------------------------------------

ntis6_long <- ntis6_count_long
if (!is.null(ntis6_cost_long)) {
  ntis6_long <- ntis6_long %>%
    left_join(ntis6_cost_long, by = c("Year", "cancer_kor"))
}

cat("step 04 long table:", nrow(ntis6_long), "rows\n")

# =============================================================================
# 2. Load death & incidence statistics (wide -> long)
# =============================================================================

# Mapping from English labels in Excel to Korean cancer names used in step 04
# Note: Excel files use "ovary cancer" (not "ovarian cancer") and "renal cancer" (not "kidney cancer")
cancer_map <- c(
  "gastric cancer"      = "위암",
  "colorectal cancer"   = "대장암",
  "lung cancer"         = "폐암",
  "breast cancer"       = "유방암",
  "prostate cancer"     = "전립선암",
  "liver cancer"        = "간암",
  "pancreatic cancer"   = "췌장암",
  "kidney cancer"       = "신장암",
  "renal cancer"        = "신장암",  # Excel uses "renal cancer"
  "thyroid cancer"      = "갑상선암",
  "cervical cancer"     = "자궁경부암",
  "endometrial cancer"  = "자궁내막암",
  "ovarian cancer"      = "난소암",
  "ovary cancer"        = "난소암"   # Excel uses "ovary cancer"
)

# Death -----------------------------------------------------------------------

death_file <- file.path(EPI_DIR, "7_cancer_1999_2024_death_260109.xlsx")
cat("Loading death statistics from:", death_file, "\n")

sheets_death <- excel_sheets(death_file)
death_raw <- read_excel(death_file, sheet = sheets_death[1])
colnames(death_raw)[1] <- "cancer_eng"

# Filter to mapped cancers only
death_filtered <- death_raw %>%
  filter(cancer_eng %in% names(cancer_map)) %>%
  mutate(cancer_kor = cancer_map[cancer_eng])

# Wide -> long: death count & rate
# Only use columns whose names match death/deathrate + year (e.g., death2006, deathrate2006)
death_long <- death_filtered %>%
  pivot_longer(cols = matches("^(deathrate|death)[0-9]{4}$"),
               names_to = c("metric_type", "year"),
               names_pattern = "(deathrate|death)([0-9]{4})",
               values_to = "value") %>%
  mutate(
    year = as.integer(year),
    # Convert value to numeric (handle comma-separated numbers and all types of spaces)
    # Remove commas, all whitespace characters (including non-breaking spaces \u00A0), and convert
    value = as.numeric(gsub("[,\\s\u00A0\u2000-\u200B\u202F\u205F\u3000]", "", as.character(value)))
  ) %>%
  pivot_wider(id_cols = c(year, cancer_kor),
              names_from = metric_type,
              values_from = value)

colnames(death_long) <- sub("^deathrate$", "death_rate", colnames(death_long))

# Ensure death and death_rate are numeric (in case any remain as character)
# Handle all types of spaces including non-breaking spaces
if ("death" %in% colnames(death_long)) {
  death_long$death <- suppressWarnings(as.numeric(gsub("[,\\s\u00A0\u2000-\u200B\u202F\u205F\u3000]", "", as.character(death_long$death))))
}
if ("death_rate" %in% colnames(death_long)) {
  death_long$death_rate <- suppressWarnings(as.numeric(gsub("[,\\s\u00A0\u2000-\u200B\u202F\u205F\u3000]", "", as.character(death_long$death_rate))))
}

# Incidence -------------------------------------------------------------------

incidence_file <- file.path(EPI_DIR, "24cancer_1999_2022_incidence_260109.xlsx")
cat("Loading incidence statistics from:", incidence_file, "\n")

sheets_inc <- excel_sheets(incidence_file)
inc_raw <- read_excel(incidence_file, sheet = sheets_inc[1])
colnames(inc_raw)[1] <- "cancer_eng"

inc_filtered <- inc_raw %>%
  filter(cancer_eng %in% names(cancer_map)) %>%
  mutate(cancer_kor = cancer_map[cancer_eng])

# Wide -> long: incidence count & rate
# Only use columns whose names match inci/incirate + year (e.g., inci2006, incirate2006)
inc_long <- inc_filtered %>%
  pivot_longer(cols = matches("^(incirate|inci)[0-9]{4}$"),
               names_to = c("metric_type", "year"),
               names_pattern = "(incirate|inci)([0-9]{4})",
               values_to = "value") %>%
  mutate(
    year = as.integer(year),
    # Convert value to numeric (handle comma-separated numbers and all types of spaces)
    # Remove commas, all whitespace characters (including non-breaking spaces \u00A0), and convert
    value = as.numeric(gsub("[,\\s\u00A0\u2000-\u200B\u202F\u205F\u3000]", "", as.character(value)))
  ) %>%
  pivot_wider(id_cols = c(year, cancer_kor),
              names_from = metric_type,
              values_from = value)

colnames(inc_long) <- sub("^incirate$", "incidence_rate", colnames(inc_long))
colnames(inc_long) <- sub("^inci$", "incidence_count", colnames(inc_long))

# Ensure incidence_count and incidence_rate are numeric (in case any remain as character)
# Handle all types of spaces including non-breaking spaces
if ("incidence_count" %in% colnames(inc_long)) {
  inc_long$incidence_count <- suppressWarnings(as.numeric(gsub("[,\\s\u00A0\u2000-\u200B\u202F\u205F\u3000]", "", as.character(inc_long$incidence_count))))
}
if ("incidence_rate" %in% colnames(inc_long)) {
  inc_long$incidence_rate <- suppressWarnings(as.numeric(gsub("[,\\s\u00A0\u2000-\u200B\u202F\u205F\u3000]", "", as.character(inc_long$incidence_rate))))
}

# =============================================================================
# 3. Join NTIS research stats with death/incidence
# =============================================================================

# Restrict to common year range, e.g., 2006–2022
min_year <- max(min(ntis6_long$Year, na.rm = TRUE), 2006)
max_year <- min(max(ntis6_long$Year, na.rm = TRUE), 2022)

analysis_data <- ntis6_long %>%
  rename(year = Year) %>%
  filter(year >= min_year & year <= max_year)

# Join with death and incidence
analysis_data <- analysis_data %>%
  left_join(death_long,  by = c("year", "cancer_kor")) %>%
  left_join(inc_long,    by = c("year", "cancer_kor"))

# Ensure key epidemiologic fields are numeric (guard against mixed types from Excel)
analysis_data <- analysis_data %>%
  mutate(
    death          = suppressWarnings(as.numeric(death)),
    death_rate     = suppressWarnings(as.numeric(death_rate)),
    incidence_count = suppressWarnings(as.numeric(incidence_count)),
    incidence_rate  = suppressWarnings(as.numeric(incidence_rate))
  )

cat("Joined analysis data:", nrow(analysis_data), "rows\n")

# =============================================================================
# 5. Piecewise regression based on epidemiology (death_rate vs year)
#    - Find breakpoint in death_rate over time for each cancer
#    - Summarise research/epi before vs after breakpoint
# =============================================================================

cat("\n=== PIECEWISE ANALYSIS (DEATH RATE ~ YEAR) ===\n")

if (!require(segmented, quietly = TRUE)) {
  cat("Package 'segmented' not installed. Installing now...\n")
  install.packages("segmented", dependencies = TRUE)
  library(segmented)
}

piecewise_results <- data.frame(
  cancer_kor            = character(),
  break_year_death      = integer(),
  n_before              = integer(),
  n_after               = integer(),
  projects_share_before = numeric(),
  projects_share_after  = numeric(),
  death_rate_before     = numeric(),
  death_rate_after      = numeric(),
  incidence_rate_before = numeric(),
  incidence_rate_after  = numeric(),
  budget_share_before   = numeric(),
  budget_share_after    = numeric(),
  stringsAsFactors      = FALSE
)

for (cancer in cancer_type_names) {
  df_c <- analysis_data %>%
    filter(cancer_kor == cancer) %>%
    arrange(year)

  # Need enough non-NA death_rate values
  df_c_dr <- df_c %>% filter(!is.na(death_rate))
  if (nrow(df_c_dr) < 6) {
    cat("  Skipping", cancer, "- not enough data for piecewise death_rate\n")
    next
  }

  # Fit basic linear model
  m0 <- tryCatch(lm(death_rate ~ year, data = df_c_dr),
                 error = function(e) NULL)
  if (is.null(m0)) {
    cat("  Skipping", cancer, "- lm failed\n")
    next
  }

  # Fit segmented (one breakpoint)
  m_seg <- tryCatch(
    segmented::segmented(m0, seg.Z = ~ year, npsi = 1),
    error = function(e) NULL
  )
  if (is.null(m_seg)) {
    cat("  Skipping", cancer, "- segmented failed\n")
    next
  }

  psi <- tryCatch(m_seg$psi, error = function(e) NULL)
  if (is.null(psi) || !"year" %in% rownames(psi)) {
    cat("  Skipping", cancer, "- breakpoint not found\n")
    next
  }

  break_year <- round(psi["year", "Est."])
  cat("  Cancer:", cancer, "- estimated death_rate breakpoint at year", break_year, "\n")

  # Summarise before / after breakpoint using full analysis_data (including research metrics)
  df_before <- df_c %>% filter(year <= break_year)
  df_after  <- df_c %>% filter(year > break_year)

  if (nrow(df_before) < 2 || nrow(df_after) < 2) {
    cat("    Not enough data on one side of breakpoint; skipping summary\n")
    next
  }

  mean_projects_share_before <- mean(df_before$projects_share, na.rm = TRUE)
  mean_projects_share_after  <- mean(df_after$projects_share, na.rm = TRUE)

  mean_death_rate_before <- mean(df_before$death_rate, na.rm = TRUE)
  mean_death_rate_after  <- mean(df_after$death_rate, na.rm = TRUE)

  mean_incidence_rate_before <- if ("incidence_rate" %in% colnames(df_c)) {
    mean(df_before$incidence_rate, na.rm = TRUE)
  } else {
    NA_real_
  }
  mean_incidence_rate_after <- if ("incidence_rate" %in% colnames(df_c)) {
    mean(df_after$incidence_rate, na.rm = TRUE)
  } else {
    NA_real_
  }

  mean_budget_share_before <- if ("budget_share" %in% colnames(df_c)) {
    mean(df_before$budget_share, na.rm = TRUE)
  } else {
    NA_real_
  }
  mean_budget_share_after <- if ("budget_share" %in% colnames(df_c)) {
    mean(df_after$budget_share, na.rm = TRUE)
  } else {
    NA_real_
  }

  piecewise_results <- rbind(
    piecewise_results,
    data.frame(
      cancer_kor            = cancer,
      break_year_death      = break_year,
      n_before              = nrow(df_before),
      n_after               = nrow(df_after),
      projects_share_before = mean_projects_share_before,
      projects_share_after  = mean_projects_share_after,
      death_rate_before     = mean_death_rate_before,
      death_rate_after      = mean_death_rate_after,
      incidence_rate_before = mean_incidence_rate_before,
      incidence_rate_after  = mean_incidence_rate_after,
      budget_share_before   = mean_budget_share_before,
      budget_share_after    = mean_budget_share_after,
      stringsAsFactors      = FALSE
    )
  )
}

if (nrow(piecewise_results) > 0) {
  pw_file <- file.path(output_dir_tables, paste0("ntis7_cancer_research_vs_epi_piecewise_summary_", timestamp, ".xlsx"))
  wb_pw <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb_pw, "piecewise_summary")
  openxlsx::writeData(wb_pw, "piecewise_summary", piecewise_results)
  openxlsx::saveWorkbook(wb_pw, pw_file, overwrite = TRUE)
  cat("Piecewise (death_rate-based) summary saved to:", pw_file, "\n")
} else {
  cat("No valid piecewise results were generated.\n")
}


# =============================================================================
# 6. Trend plots: Epidemiology vs Research Activity
#    - Overall (all cancers together)
#    - Each cancer separately
# =============================================================================

cat("\n=== TREND PLOTS: EPIDEMIOLOGY vs RESEARCH ===\n")

# Helper: create safe ID for filenames based on English cancer label
get_cancer_id <- function(cancer_kor_name) {
  eng_names <- names(cancer_map)
  kor_names <- as.character(cancer_map)
  idx <- which(kor_names == cancer_kor_name)
  if (length(idx) == 0) {
    # fallback: simple index if mapping not found
    return(paste0("cancer_", match(cancer_kor_name, cancer_type_names)))
  }
  id_raw <- eng_names[idx[1]]
  id_safe <- gsub(" ", "_", id_raw)
  id_safe
}

# 6-1. Overall epidemiology trends (death_rate & incidence_rate) ---------------

overall_epi_long <- analysis_data %>%
  dplyr::select(year, cancer_kor, death_rate, incidence_rate) %>%
  pivot_longer(cols = c(death_rate, incidence_rate),
               names_to = "metric",
               values_to = "value") %>%
  filter(!is.na(value))

if (nrow(overall_epi_long) > 0) {
  p_overall_epi <- ggplot(overall_epi_long,
                          aes(x = year, y = value,
                              color = cancer_kor,
                              linetype = metric)) +
    geom_line(linewidth = 1) +
    labs(
      title = "전체 암종의 사망률 및 발생률 추세 (연도별)",
      x = "연도",
      y = "율 (100,000명 당 또는 표준화율)",
      color = "암종",
      linetype = "지표"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"
    )

  fig_overall_epi <- file.path(
    output_dir_figures,
    paste0("ntis7_overall_death_incidence_trends_", timestamp, ".png")
  )
  ggsave(fig_overall_epi, p_overall_epi, width = 9, height = 6, dpi = 300)
  cat("Saved overall epidemiology trends figure to:", fig_overall_epi, "\n")
} else {
  cat("No data available for overall epidemiology trends plot.\n")
}

# 6-2. Overall research activity trends (projects_count & projects_share) ------

overall_research_long <- analysis_data %>%
  dplyr::select(year, cancer_kor, projects_count, projects_share) %>%
  pivot_longer(cols = c(projects_count, projects_share),
               names_to = "metric",
               values_to = "value") %>%
  filter(!is.na(value))

if (nrow(overall_research_long) > 0) {
  p_overall_research <- ggplot(overall_research_long,
                               aes(x = year, y = value,
                                   color = cancer_kor,
                                   linetype = metric)) +
    geom_line(linewidth = 1) +
    labs(
      title = "전체 암종 연구 프로젝트 개수 및 비율 추세 (연도별)",
      x = "연도",
      y = "값 (개수 또는 %)",
      color = "암종",
      linetype = "지표"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"
    )

  fig_overall_research <- file.path(
    output_dir_figures,
    paste0("ntis7_overall_projects_trends_", timestamp, ".png")
  )
  ggsave(fig_overall_research, p_overall_research, width = 9, height = 6, dpi = 300)
  cat("Saved overall research activity trends figure to:", fig_overall_research, "\n")
} else {
  cat("No data available for overall research activity trends plot.\n")
}

# 6-3. Overall (all cancers combined) 2-panel overlap -------------------------

# Aggregate data for all cancers combined
# For research: use total_cancer from ntis6_count for count
# For research share: use total_projects from yearly_total_stats (step 01) for overall project share
# For epidemiology: sum all cancer types' rates

overall_2panel_data <- analysis_data %>%
  group_by(year) %>%
  summarise(
    # Research: total projects count (sum of all cancer types)
    projects_count_total = sum(projects_count, na.rm = TRUE),
    # Epidemiology: sum of all cancer types' rates
    death_rate_total = sum(death_rate, na.rm = TRUE),
    incidence_rate_total = sum(incidence_rate, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(year)

# Get total_cancer from ntis6_count if available (for projects_count_total)
if (!is.null(ntis6_count) && "total_cancer" %in% colnames(ntis6_count)) {
  overall_2panel_data <- overall_2panel_data %>%
    left_join(
      ntis6_count %>%
        dplyr::select(Year, total_cancer) %>%
        rename(year = Year),
      by = "year"
    ) %>%
    mutate(
      # Use total_cancer if available, otherwise use sum
      projects_count_total = ifelse(!is.na(total_cancer), total_cancer, projects_count_total)
    ) %>%
    dplyr::select(-total_cancer)
}

# Calculate projects_share_total: total_cancer_projects / total_projects * 100
# Use yearly_total_stats from step 01 if available
if (yearly_stats_available && !is.null(yearly_total_stats)) {
  overall_2panel_data <- overall_2panel_data %>%
    left_join(
      yearly_total_stats %>%
        dplyr::select(Year, total_projects, total_cancer_projects) %>%
        rename(year = Year),
      by = "year"
    ) %>%
    mutate(
      # Calculate share: total_cancer_projects / total_projects * 100
      projects_share_total = ifelse(
        !is.na(total_projects) & total_projects > 0,
        (total_cancer_projects / total_projects) * 100,
        NA_real_
      )
    ) %>%
    dplyr::select(-total_projects, -total_cancer_projects)
} else {
  # If yearly_total_stats not available, cannot calculate proper share
  # Set to NA to indicate missing data
  overall_2panel_data$projects_share_total <- NA_real_
  cat("Warning: Cannot calculate overall project share - yearly_total_stats not available\n")
}

# Create 2-panel overlap plot for overall (all cancers)
if (nrow(overall_2panel_data) > 0) {
  df_overall <- overall_2panel_data

  # (a) Epidemiology: incidence_rate & death_rate overlap
  has_inc_overall <- any(!is.na(df_overall$incidence_rate_total))
  has_death_overall <- any(!is.na(df_overall$death_rate_total))

  if (!has_inc_overall && !has_death_overall) {
    p_overall_epi <- ggplot() + theme_void() +
      labs(title = "전체 암종 - 발생률/사망률 데이터 없음")
  } else {
    max_inc_overall <- if (has_inc_overall) max(df_overall$incidence_rate_total, na.rm = TRUE) else NA_real_
    max_death_overall <- if (has_death_overall) max(df_overall$death_rate_total, na.rm = TRUE) else NA_real_

    if (is.na(max_inc_overall) || max_inc_overall == 0 || is.na(max_death_overall) || max_death_overall == 0) {
      scale_epi_overall <- 1
    } else {
      scale_epi_overall <- max_inc_overall / max_death_overall
    }

    df_epi_overall <- df_overall
    if (has_death_overall) {
      df_epi_overall$death_rate_scaled <- df_epi_overall$death_rate_total * scale_epi_overall
    } else {
      df_epi_overall$death_rate_scaled <- NA_real_
    }

    p_overall_epi <- ggplot(df_epi_overall, aes(x = year)) +
      { if (has_inc_overall) geom_line(aes(y = incidence_rate_total, color = "incidence_rate"),
                                       linewidth = 1) else NULL } +
      { if (has_death_overall) geom_line(aes(y = death_rate_scaled, color = "death_rate"),
                                        linewidth = 1, linetype = "dashed") else NULL } +
      scale_y_continuous(
        name = "발생률 (incidence rate)",
        sec.axis = sec_axis(~ . / scale_epi_overall,
                            name = "사망률 (mortality rate)")
      ) +
      scale_color_manual(
        values = c(
          "incidence_rate" = "#1b9e77",
          "death_rate" = "#d95f02"
        ),
        labels = c(
          "incidence_rate" = "발생률 (incidence rate)",
          "death_rate" = "사망률 (mortality rate)"
        ),
        name = "지표"
      ) +
      labs(
        title = "전체 암종 - 발생률 및 사망률 (이중 y축, overlap)",
        x = "연도"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
  }

  # (b) Research: projects_count & projects_share overlap
  has_proj_n_overall <- any(!is.na(df_overall$projects_count_total))
  has_proj_share_overall <- any(!is.na(df_overall$projects_share_total))

  if (!has_proj_n_overall && !has_proj_share_overall) {
    p_overall_research <- ggplot() + theme_void() +
      labs(title = "전체 암종 - 연구 프로젝트 데이터 없음")
  } else {
    max_n_overall <- if (has_proj_n_overall) max(df_overall$projects_count_total, na.rm = TRUE) else NA_real_
    max_share_overall <- if (has_proj_share_overall) max(df_overall$projects_share_total, na.rm = TRUE) else NA_real_

    if (is.na(max_n_overall) || max_n_overall == 0 || is.na(max_share_overall) || max_share_overall == 0) {
      scale_factor_overall <- 1
    } else {
      scale_factor_overall <- max_n_overall / max_share_overall
    }

    df_plot_overall <- df_overall
    if (has_proj_share_overall) {
      df_plot_overall$projects_share_scaled <- df_plot_overall$projects_share_total * scale_factor_overall
    } else {
      df_plot_overall$projects_share_scaled <- NA_real_
    }

    p_overall_research <- ggplot(df_plot_overall, aes(x = year)) +
      { if (has_proj_n_overall) geom_line(aes(y = projects_count_total, color = "projects_count"),
                                          linewidth = 1) else NULL } +
      { if (has_proj_share_overall) geom_line(aes(y = projects_share_scaled, color = "projects_share"),
                                             linewidth = 1, linetype = "dashed") else NULL } +
      scale_y_continuous(
        name = "연구 프로젝트 개수 (total projects)",
        sec.axis = sec_axis(~ . / scale_factor_overall,
                            name = "연구 프로젝트 비율 (project share, %)")
      ) +
      scale_color_manual(
        values = c(
          "projects_count" = "#7570b3",
          "projects_share" = "#e7298a"
        ),
        labels = c(
          "projects_count" = "프로젝트 개수 (total projects)",
          "projects_share" = "프로젝트 비율 (project share, %)"
        ),
        name = "연구 지표"
      ) +
      labs(
        title = "전체 암종 - 연구 프로젝트 개수 및 비율 (이중 y축, overlap)",
        x = "연도"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
  }

  # (c) Combine two panels
  combined_overall_2panel <- p_overall_epi / p_overall_research +
    patchwork::plot_annotation(
      title = "전체 암종 - 역학 및 연구 프로젝트 트렌드 (overlap)"
    ) &
    theme(
      plot.title = element_text(hjust = 0.5)
    )

  fig_overall_2panel <- file.path(
    output_dir_figures,
    paste0("ntis7_all_cancers_2panel_overlap_epi_research_", timestamp, ".png")
  )
  ggsave(fig_overall_2panel, combined_overall_2panel, width = 10, height = 8, dpi = 300)
  cat("Saved overall (all cancers) 2-panel overlapped epi/research trends to:", fig_overall_2panel, "\n")
} else {
  cat("No data available for overall (all cancers) 2-panel overlap plot.\n")
}

# 6-3.5. Overall (all cancers) unified trends plot -------------------------
# 역학 정보: 점선 (발생률, 사망률)
# 연구 정보: 실선 (프로젝트 비율, 예산 비율)
# 모든 지표의 scale을 맞춰서 트렌드 비교 가능하도록

if (nrow(overall_2panel_data) > 0) {
  df_overall <- overall_2panel_data

  # Check available data
  has_inc_overall <- any(!is.na(df_overall$incidence_rate_total))
  has_death_overall <- any(!is.na(df_overall$death_rate_total))
  has_proj_share_overall <- any(!is.na(df_overall$projects_share_total))

  # For budget_share, we need to calculate from overall_analysis_data
  has_budget_share_overall <- FALSE
  if (!is.null(ntis6_cost) && "total_cancer_cost" %in% colnames(ntis6_cost) &&
      yearly_stats_available && !is.null(yearly_total_stats)) {
    # Calculate overall budget share: total_cancer_cost / total_money * 100
    # Use total_money from step 01 (it's called total_money, not total_budget)
    if ("total_money" %in% colnames(yearly_total_stats)) {
      budget_data <- ntis6_cost %>%
        dplyr::select(Year, total_cancer_cost) %>%
        left_join(
          yearly_total_stats %>%
            dplyr::select(Year, total_money) %>%
            rename(year = Year),
          by = c("Year" = "year")
        ) %>%
        mutate(
          budget_share_total = ifelse(
            !is.na(total_money) & total_money > 0,
            (total_cancer_cost / total_money) * 100,
            NA_real_
          )
        ) %>%
        dplyr::select(Year, budget_share_total) %>%
        rename(year = Year)

      df_overall <- df_overall %>%
        left_join(budget_data, by = "year")
      has_budget_share_overall <- any(!is.na(df_overall$budget_share_total))
    } else if ("total_budget" %in% colnames(yearly_total_stats)) {
      # Fallback to total_budget if it exists
      budget_data <- ntis6_cost %>%
        dplyr::select(Year, total_cancer_cost) %>%
        left_join(
          yearly_total_stats %>%
            dplyr::select(Year, total_budget) %>%
            rename(year = Year),
          by = c("Year" = "year")
        ) %>%
        mutate(
          budget_share_total = ifelse(
            !is.na(total_budget) & total_budget > 0,
            (total_cancer_cost / total_budget) * 100,
            NA_real_
          )
        ) %>%
        dplyr::select(Year, budget_share_total) %>%
        rename(year = Year)

      df_overall <- df_overall %>%
        left_join(budget_data, by = "year")
      has_budget_share_overall <- any(!is.na(df_overall$budget_share_total))
    }
  }

  if (!has_inc_overall && !has_death_overall && !has_proj_share_overall && !has_budget_share_overall) {
    cat("No data available for overall (all cancers) unified trends plot.\n")
  } else {
    # Calculate max values for scaling
    max_inc <- if (has_inc_overall) max(df_overall$incidence_rate_total, na.rm = TRUE) else NA_real_
    max_death <- if (has_death_overall) max(df_overall$death_rate_total, na.rm = TRUE) else NA_real_
    max_proj_share <- if (has_proj_share_overall) max(df_overall$projects_share_total, na.rm = TRUE) else NA_real_
    max_budget_share <- if (has_budget_share_overall) max(df_overall$budget_share_total, na.rm = TRUE) else NA_real_

    # Find the maximum among all available metrics for scaling
    max_values <- c(max_inc, max_death, max_proj_share, max_budget_share)
    max_values <- max_values[!is.na(max_values)]
    if (length(max_values) == 0) {
      cat("No valid data for overall (all cancers) unified trends plot.\n")
    } else {
      global_max <- max(max_values, na.rm = TRUE)

      # Calculate scaling factors for each metric
      scale_inc <- if (has_inc_overall && max_inc > 0) global_max / max_inc else 1
      scale_death <- if (has_death_overall && max_death > 0) global_max / max_death else 1
      scale_proj_share <- if (has_proj_share_overall && max_proj_share > 0) global_max / max_proj_share else 1
      scale_budget_share <- if (has_budget_share_overall && max_budget_share > 0) global_max / max_budget_share else 1

      # Prepare data with scaled values
      df_plot_overall <- df_overall %>%
        mutate(
          incidence_rate_scaled = if (has_inc_overall) incidence_rate_total * scale_inc else NA_real_,
          death_rate_scaled = if (has_death_overall) death_rate_total * scale_death else NA_real_,
          projects_share_scaled = if (has_proj_share_overall) projects_share_total * scale_proj_share else NA_real_,
          budget_share_scaled = if (has_budget_share_overall) budget_share_total * scale_budget_share else NA_real_
        )

      # Create unified plot with scaled values for trend comparison
      # Show actual y-axis values for each metric using annotations
      # Since ggplot2 supports only 2 axes, we'll use:
      # - Left y-axis: Incidence rate (actual values)
      # - Right y-axis: Death rate (actual values) - secondary axis
      # - Project share and Funding share: Show actual value ranges in annotations

      # Calculate scale factor for death rate secondary axis
      if (has_inc_overall && has_death_overall && max_inc > 0 && max_death > 0) {
        death_scale_factor <- max_inc / max_death
      } else {
        death_scale_factor <- 1
      }

      # Prepare data with scaled values for unified comparison
      df_plot_overall <- df_plot_overall %>%
        mutate(
          # Scale death rate to match incidence rate scale for left y-axis
          death_rate_scaled_for_axis = if (has_death_overall) {
            death_rate_total * death_scale_factor
          } else {
            NA_real_
          }
        )

      # Create unified plot with scaled values and actual y-axes
      p_overall_unified <- ggplot(df_plot_overall, aes(x = year)) +
        # All metrics plotted with scaled values for trend comparison
        { if (has_inc_overall) geom_line(aes(y = incidence_rate_scaled, color = "incidence_rate"),
                                         linewidth = 1, linetype = "dashed") else NULL } +
        { if (has_death_overall) geom_line(aes(y = death_rate_scaled, color = "death_rate"),
                                           linewidth = 1, linetype = "dashed") else NULL } +
        { if (has_proj_share_overall) geom_line(aes(y = projects_share_scaled, color = "projects_share"),
                                                linewidth = 1, linetype = "solid") else NULL } +
        { if (has_budget_share_overall) geom_line(aes(y = budget_share_scaled, color = "budget_share"),
                                                   linewidth = 1, linetype = "solid") else NULL } +
        scale_x_continuous(
          name = "연도",
          breaks = seq(2006, 2022, by = 2),
          limits = c(2006, 2022)
        ) +
        # Primary y-axis: Show actual incidence rate values
        scale_y_continuous(
          name = if (has_inc_overall) "Incidence rate (per 100,000)" else "Scaled Values",
          limits = c(0, global_max * 1.1),
          # Transform scaled values back to actual incidence rate values
          labels = if (has_inc_overall && scale_inc > 0) {
            function(x) round(x / scale_inc, 1)
          } else {
            waiver()
          },
          # Secondary y-axis: Show actual death rate values
          sec.axis = sec_axis(
            ~ if (has_death_overall && scale_death > 0) . / scale_death else .,
            name = if (has_death_overall) "Mortality rate (per 100,000)" else NULL,
            labels = if (has_death_overall && scale_death > 0) {
              function(x) round(x, 1)
            } else {
              waiver()
            }
          )
        ) +
        # Add annotations showing actual value ranges for project share and funding share
        # Funding share on top, Project share below
        { if (has_budget_share_overall) {
          annotate("text",
                   x = Inf, y = Inf,
                   label = paste0("Funding share (%): ",
                                 round(min(df_overall$budget_share_total, na.rm = TRUE), 2),
                                 " - ", round(max(df_overall$budget_share_total, na.rm = TRUE), 2)),
                   hjust = 1.05, vjust = 1.2,
                   size = 3.5, color = "#e7298a", fontface = "bold")
        } else NULL } +
        { if (has_proj_share_overall) {
          annotate("text",
                   x = Inf, y = Inf,
                   label = paste0("Project share (%): ",
                                 round(min(df_overall$projects_share_total, na.rm = TRUE), 2),
                                 " - ", round(max(df_overall$projects_share_total, na.rm = TRUE), 2)),
                   hjust = 1.05, vjust = if (has_budget_share_overall) 2.8 else 1.2,
                   size = 3.5, color = "#7570b3", fontface = "bold")
        } else NULL } +
        scale_color_manual(
          values = c(
            "incidence_rate" = "#1b9e77",
            "death_rate" = "#d95f02",
            "projects_share" = "#7570b3",
            "budget_share" = "#e7298a"
          ),
          labels = c(
            "incidence_rate" = "Incidence rate",
            "death_rate" = "Mortality rate",
            "projects_share" = "Project share (%)",
            "budget_share" = "Funding share (%)"
          ),
          name = "Legend"
        ) +
        scale_linetype_manual(
          values = c(
            "dashed" = "dashed",
            "solid" = "solid"
          ),
          guide = "none"
        ) +
        labs(
          title = "전체 암종 - 역학 및 연구 트렌드 비교",
          y = "Scaled Values (for trend comparison)"
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = c(0.98, 0.02),
          legend.justification = c(1, 0),
          legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
          axis.title.y.right = element_text(color = "#d95f02"),
          axis.text.y.right = element_text(color = "#d95f02"),
          axis.ticks.y.right = element_line(color = "#d95f02")
        )

      # Save plot
      fig_overall_unified <- file.path(
        output_dir_figures,
        paste0("ntis7_all_cancers_unified_trends_", timestamp, ".png")
      )
      ggsave(fig_overall_unified, p_overall_unified, width = 8, height = 6, dpi = 300)
      cat("Saved overall (all cancers) unified trends plot to:", fig_overall_unified, "\n")

      # Create unified_4y plot with 4 separate y-axes showing actual values
      plots_list <- list()

      # 1. Incidence rate
      if (has_inc_overall) {
        p_inc <- ggplot(df_overall, aes(x = year, y = incidence_rate_total)) +
          geom_line(linewidth = 1, color = "#1b9e77", linetype = "dashed") +
          scale_x_continuous(
            name = "연도",
            breaks = seq(2006, 2022, by = 2),
            limits = c(2006, 2022)
          ) +
          labs(
            title = "Incidence rate",
            y = "Incidence rate (per 100,000)"
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
          )
        plots_list[["incidence"]] <- p_inc
      }

      # 2. Death rate
      if (has_death_overall) {
        p_death <- ggplot(df_overall, aes(x = year, y = death_rate_total)) +
          geom_line(linewidth = 1, color = "#d95f02", linetype = "dashed") +
          scale_x_continuous(
            name = "연도",
            breaks = seq(2006, 2022, by = 2),
            limits = c(2006, 2022)
          ) +
          labs(
            title = "Mortality rate",
            y = "Mortality rate (per 100,000)"
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
          )
        plots_list[["death"]] <- p_death
      }

      # 3. Project share
      if (has_proj_share_overall) {
        p_proj <- ggplot(df_overall, aes(x = year, y = projects_share_total)) +
          geom_line(linewidth = 1, color = "#7570b3", linetype = "solid") +
          scale_x_continuous(
            name = "연도",
            breaks = seq(2006, 2022, by = 2),
            limits = c(2006, 2022)
          ) +
          labs(
            title = "Project share",
            y = "Project share (%)"
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
          )
        plots_list[["projects"]] <- p_proj
      }

      # 4. Funding share
      if (has_budget_share_overall) {
        p_budget <- ggplot(df_overall, aes(x = year, y = budget_share_total)) +
          geom_line(linewidth = 1, color = "#e7298a", linetype = "solid") +
          scale_x_continuous(
            name = "연도",
            breaks = seq(2006, 2022, by = 2),
            limits = c(2006, 2022)
          ) +
          labs(
            title = "Funding share",
            x = "연도",
            y = "Funding share (%)"
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
          )
        plots_list[["budget"]] <- p_budget
      }

      # Combine all plots
      if (length(plots_list) > 0) {
        p_overall_unified_4y <- patchwork::wrap_plots(plots_list, ncol = 1) +
          patchwork::plot_annotation(
            title = "전체 암종 - 역학 및 연구 트렌드 비교 (4개 y축)",
            theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
          )

        fig_overall_unified_4y <- file.path(
          output_dir_figures,
          paste0("ntis7_all_cancers_unified_4y_trends_", timestamp, ".png")
        )
        ggsave(fig_overall_unified_4y, p_overall_unified_4y, width = 8, height = 10, dpi = 300)
        cat("Saved overall (all cancers) unified_4y trends plot to:", fig_overall_unified_4y, "\n")
      }
    }
  }
} else {
  cat("No data available for overall (all cancers) unified trends plot.\n")
}

# 6-4. Per-cancer epidemiology & research trends (single unified plot) ------------
# 역학 정보: 점선 (발생률, 사망률)
# 연구 정보: 실선 (프로젝트 개수 비율, 프로젝트 돈 비율)
# 모든 지표의 scale을 맞춰서 트렌드 비교 가능하도록

for (cancer in cancer_type_names) {
  df_c <- analysis_data %>%
    dplyr::filter(cancer_kor == cancer) %>%
    dplyr::arrange(year)

  if (nrow(df_c) == 0) {
    cat("Skipping trend plots for", cancer, "- no data.\n")
    next
  }

  cancer_id <- get_cancer_id(cancer)

  # Check available data
  has_inc <- any(!is.na(df_c$incidence_rate))
  has_death <- any(!is.na(df_c$death_rate))
  has_proj_share <- any(!is.na(df_c$projects_share_cancer))
  has_budget_share <- any(!is.na(df_c$budget_share))

  if (!has_inc && !has_death && !has_proj_share && !has_budget_share) {
    cat("Skipping trend plots for", cancer, "- no data available.\n")
    next
  }

  # Calculate max values for scaling
  max_inc <- if (has_inc) max(df_c$incidence_rate, na.rm = TRUE) else NA_real_
  max_death <- if (has_death) max(df_c$death_rate, na.rm = TRUE) else NA_real_
  max_proj_share <- if (has_proj_share) max(df_c$projects_share_cancer, na.rm = TRUE) else NA_real_
  max_budget_share <- if (has_budget_share) max(df_c$budget_share, na.rm = TRUE) else NA_real_

  # Find the maximum among all available metrics for scaling
  max_values <- c(max_inc, max_death, max_proj_share, max_budget_share)
  max_values <- max_values[!is.na(max_values)]
  if (length(max_values) == 0) {
    cat("Skipping trend plots for", cancer, "- no valid data.\n")
    next
  }
  global_max <- max(max_values, na.rm = TRUE)

  # Calculate scaling factors for each metric
  scale_inc <- if (has_inc && max_inc > 0) global_max / max_inc else 1
  scale_death <- if (has_death && max_death > 0) global_max / max_death else 1
  scale_proj_share <- if (has_proj_share && max_proj_share > 0) global_max / max_proj_share else 1
  scale_budget_share <- if (has_budget_share && max_budget_share > 0) global_max / max_budget_share else 1

  # Prepare data with scaled values
  df_plot <- df_c %>%
    mutate(
      incidence_rate_scaled = if (has_inc) incidence_rate * scale_inc else NA_real_,
      death_rate_scaled = if (has_death) death_rate * scale_death else NA_real_,
      projects_share_scaled = if (has_proj_share) projects_share_cancer * scale_proj_share else NA_real_,
      budget_share_scaled = if (has_budget_share) budget_share * scale_budget_share else NA_real_
    )

  # Create unified plot with scaled values for trend comparison
  # Show actual y-axis values for each metric
  # - Left y-axis: Incidence rate (actual values)
  # - Right y-axis: Death rate (actual values) - secondary axis
  # - Project share and Funding share: Show actual value ranges in annotations

  # Create unified plot with scaled values and actual y-axes
  p_unified <- ggplot(df_plot, aes(x = year)) +
    # All metrics plotted with scaled values for trend comparison
    { if (has_inc) geom_line(aes(y = incidence_rate_scaled, color = "incidence_rate"),
                           linewidth = 1, linetype = "dashed") else NULL } +
    { if (has_death) geom_line(aes(y = death_rate_scaled, color = "death_rate"),
                               linewidth = 1, linetype = "dashed") else NULL } +
    { if (has_proj_share) geom_line(aes(y = projects_share_scaled, color = "projects_share"),
                                    linewidth = 1, linetype = "solid") else NULL } +
    { if (has_budget_share) geom_line(aes(y = budget_share_scaled, color = "budget_share"),
                                       linewidth = 1, linetype = "solid") else NULL } +
    scale_x_continuous(
      name = "연도",
      breaks = seq(2006, 2022, by = 2),
      limits = c(2006, 2022)
    ) +
    # Primary y-axis: Show actual incidence rate values
    scale_y_continuous(
      name = if (has_inc) "Incidence rate (per 100,000)" else "Scaled Values",
      limits = c(0, global_max * 1.1),
      # Transform scaled values back to actual incidence rate values
      labels = if (has_inc && scale_inc > 0) {
        function(x) round(x / scale_inc, 1)
      } else {
        waiver()
      },
      # Secondary y-axis: Show actual death rate values
      sec.axis = sec_axis(
        ~ if (has_death && scale_death > 0) . / scale_death else .,
        name = if (has_death) "Mortality rate (per 100,000)" else NULL,
        labels = if (has_death && scale_death > 0) {
          function(x) round(x, 1)
        } else {
          waiver()
        }
      )
    ) +
    # Add annotations showing actual value ranges for project share and funding share
    # Funding share on top, Project share below
    { if (has_budget_share) {
      annotate("text",
               x = Inf, y = Inf,
               label = paste0("Funding share (%): ",
                             round(min(df_c$budget_share, na.rm = TRUE), 2),
                             " - ", round(max(df_c$budget_share, na.rm = TRUE), 2)),
               hjust = 1.05, vjust = 1.2,
               size = 3.5, color = "#e7298a", fontface = "bold")
    } else NULL } +
    { if (has_proj_share) {
      annotate("text",
               x = Inf, y = Inf,
               label = paste0("Project share (%): ",
                             round(min(df_c$projects_share_cancer, na.rm = TRUE), 2),
                             " - ", round(max(df_c$projects_share_cancer, na.rm = TRUE), 2)),
               hjust = 1.05, vjust = if (has_budget_share) 2.8 else 1.2,
               size = 3.5, color = "#7570b3", fontface = "bold")
    } else NULL } +
    scale_color_manual(
      values = c(
        "incidence_rate" = "#1b9e77",
        "death_rate" = "#d95f02",
        "projects_share" = "#7570b3",
        "budget_share" = "#e7298a"
      ),
      labels = c(
        "incidence_rate" = "Incidence rate",
        "death_rate" = "Mortality rate",
        "projects_share" = "Project share (%)",
        "budget_share" = "Funding share (%)"
      ),
      name = "Metrics"
    ) +
    scale_linetype_manual(
      values = c(
        "dashed" = "dashed",
        "solid" = "solid"
      ),
      guide = "none"
    ) +
    labs(
      title = paste0(cancer, " - 역학 및 연구 트렌드 비교"),
      y = "Scaled Values (for trend comparison)"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = c(0.98, 0.02),
      legend.justification = c(1, 0),
      legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
      axis.title.y.right = element_text(color = "#d95f02"),
      axis.text.y.right = element_text(color = "#d95f02"),
      axis.ticks.y.right = element_line(color = "#d95f02")
    )

  # Save plot
  fig_unified <- file.path(
    output_dir_figures,
    paste0("ntis7_", cancer_id, "_unified_trends_", timestamp, ".png")
  )
  ggsave(fig_unified, p_unified, width = 8, height = 6, dpi = 300)
  cat("  Saved unified trends plot for", cancer, "to:", fig_unified, "\n")

  # Create unified_4y plot with 4 separate y-axes showing actual values
  plots_list <- list()

  # 1. Incidence rate
  if (has_inc) {
    p_inc <- ggplot(df_c, aes(x = year, y = incidence_rate)) +
      geom_line(linewidth = 1, color = "#1b9e77", linetype = "dashed") +
      scale_x_continuous(
        name = "연도",
        breaks = seq(2006, 2022, by = 2),
        limits = c(2006, 2022)
      ) +
      labs(
        title = "Incidence rate",
        y = "Incidence rate (per 100,000)"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
    plots_list[["incidence"]] <- p_inc
  }

  # 2. Death rate
  if (has_death) {
    p_death <- ggplot(df_c, aes(x = year, y = death_rate)) +
      geom_line(linewidth = 1, color = "#d95f02", linetype = "dashed") +
      scale_x_continuous(
        name = "연도",
        breaks = seq(2006, 2022, by = 2),
        limits = c(2006, 2022)
      ) +
      labs(
        title = "Mortality rate",
        y = "Mortality rate (per 100,000)"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
    plots_list[["death"]] <- p_death
  }

  # 3. Project share
  if (has_proj_share) {
    p_proj <- ggplot(df_c, aes(x = year, y = projects_share_cancer)) +
      geom_line(linewidth = 1, color = "#7570b3", linetype = "solid") +
      scale_x_continuous(
        name = "연도",
        breaks = seq(2006, 2022, by = 2),
        limits = c(2006, 2022)
      ) +
      labs(
        title = "Project share",
        y = "Project share (%)"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
    plots_list[["projects"]] <- p_proj
  }

  # 4. Funding share
  if (has_budget_share) {
    p_budget <- ggplot(df_c, aes(x = year, y = budget_share)) +
      geom_line(linewidth = 1, color = "#e7298a", linetype = "solid") +
      scale_x_continuous(
        name = "연도",
        breaks = seq(2006, 2022, by = 2),
        limits = c(2006, 2022)
      ) +
      labs(
        title = "Funding share",
        x = "연도",
        y = "Funding share (%)"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
      )
    plots_list[["budget"]] <- p_budget
  }

  # Combine all plots
  if (length(plots_list) > 0) {
    p_unified_4y <- patchwork::wrap_plots(plots_list, ncol = 1) +
      patchwork::plot_annotation(
        title = paste0(cancer, " - 역학 및 연구 트렌드 비교 (4개 y축)"),
        theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
      )

    fig_unified_4y <- file.path(
      output_dir_figures,
      paste0("ntis7_", cancer_id, "_unified_4y_trends_", timestamp, ".png")
    )
    ggsave(fig_unified_4y, p_unified_4y, width = 8, height = 10, dpi = 300)
    cat("  Saved unified_4y trends plot for", cancer, "to:", fig_unified_4y, "\n")
  }
}

# =============================================================================
# 7. Create 4x2 panel plot for Q1 journal publication (research metrics only; no incidence/mortality)
#    (A) All cancers (B) Breast (C) Colon (D) Lung (E) Liver (F) Stomach (G) Pancreatic (H) Prostate
#    x-axis: 2006–2024 (Figure 4 only)
# =============================================================================

cat("\n=== CREATING 4x2 PANEL PLOT (FIGURE 4 – RESEARCH TRENDS ONLY) ===\n")

# Helper function to create unified plot without title (research metrics only; no incidence/mortality)
# panel_label: e.g. "B · Breast cancer" (shown in corner)
create_unified_plot_no_title <- function(df_plot_data, cancer_name, panel_label, is_overall = FALSE) {
  if (is_overall) {
    has_proj_share <- any(!is.na(df_plot_data$projects_share_total))
    has_budget_share <- any(!is.na(df_plot_data$budget_share_total))

    max_proj_share <- if (has_proj_share) max(df_plot_data$projects_share_total, na.rm = TRUE) else NA_real_
    max_budget_share <- if (has_budget_share) max(df_plot_data$budget_share_total, na.rm = TRUE) else NA_real_

    max_values <- c(max_proj_share, max_budget_share)
    max_values <- max_values[!is.na(max_values)]
    if (length(max_values) == 0) return(NULL)
    global_max <- max(max_values, na.rm = TRUE)

    scale_proj_share <- if (has_proj_share && max_proj_share > 0) global_max / max_proj_share else 1
    scale_budget_share <- if (has_budget_share && max_budget_share > 0) global_max / max_budget_share else 1

    df_plot <- df_plot_data %>%
      mutate(
        projects_share_scaled = if (has_proj_share) projects_share_total * scale_proj_share else NA_real_,
        budget_share_scaled = if (has_budget_share) budget_share_total * scale_budget_share else NA_real_
      )
  } else {
    has_proj_share <- any(!is.na(df_plot_data$projects_share_cancer))
    has_budget_share <- any(!is.na(df_plot_data$budget_share))

    max_proj_share <- if (has_proj_share) max(df_plot_data$projects_share_cancer, na.rm = TRUE) else NA_real_
    max_budget_share <- if (has_budget_share) max(df_plot_data$budget_share, na.rm = TRUE) else NA_real_

    max_values <- c(max_proj_share, max_budget_share)
    max_values <- max_values[!is.na(max_values)]
    if (length(max_values) == 0) return(NULL)
    global_max <- max(max_values, na.rm = TRUE)

    scale_proj_share <- if (has_proj_share && max_proj_share > 0) global_max / max_proj_share else 1
    scale_budget_share <- if (has_budget_share && max_budget_share > 0) global_max / max_budget_share else 1

    df_plot <- df_plot_data %>%
      mutate(
        projects_share_scaled = if (has_proj_share) projects_share_cancer * scale_proj_share else NA_real_,
        budget_share_scaled = if (has_budget_share) budget_share * scale_budget_share else NA_real_
      )
  }

  p <- ggplot(df_plot, aes(x = year)) +
    { if (has_proj_share) geom_line(aes(y = projects_share_scaled, color = "projects_share"),
                                    linewidth = 0.8, linetype = "solid") else NULL } +
    { if (has_budget_share) geom_line(aes(y = budget_share_scaled, color = "budget_share"),
                                       linewidth = 0.8, linetype = "solid") else NULL } +
    scale_x_continuous(
      name = "Year",
      breaks = seq(2006, 2024, by = 2),
      limits = c(2006, 2024)
    ) +
    scale_y_continuous(
      name = "Share (%)",
      limits = c(0, global_max * 1.1),
      labels = waiver()
    ) +
    { if (has_budget_share) {
      annotate("text",
               x = Inf, y = Inf,
               label = paste0("Funding: ",
                             round(if (is_overall) min(df_plot_data$budget_share_total, na.rm = TRUE)
                                   else min(df_plot_data$budget_share, na.rm = TRUE), 2),
                             "-", round(if (is_overall) max(df_plot_data$budget_share_total, na.rm = TRUE)
                                        else max(df_plot_data$budget_share, na.rm = TRUE), 2), "%"),
               hjust = 1.05, vjust = 1.2,
               size = 2.5, color = "#e7298a", fontface = "bold")
    } else NULL } +
    { if (has_proj_share) {
      annotate("text",
               x = Inf, y = Inf,
               label = paste0("Project: ",
                             round(if (is_overall) min(df_plot_data$projects_share_total, na.rm = TRUE)
                                   else min(df_plot_data$projects_share_cancer, na.rm = TRUE), 2),
                             "-", round(if (is_overall) max(df_plot_data$projects_share_total, na.rm = TRUE)
                                        else max(df_plot_data$projects_share_cancer, na.rm = TRUE), 2), "%"),
               hjust = 1.05, vjust = if (has_budget_share) 2.5 else 1.2,
               size = 2.5, color = "#7570b3", fontface = "bold")
    } else NULL } +
    scale_color_manual(
      values = c(
        "projects_share" = "#7570b3",
        "budget_share" = "#e7298a"
      ),
      labels = c(
        "projects_share" = "Project share (%)",
        "budget_share" = "Funding share (%)"
      ),
      name = "Metrics"
    ) +
    labs(x = "Year", y = "") +
    annotate("text", x = -Inf, y = Inf,
             label = panel_label,
             hjust = -0.1, vjust = 1.5,
             size = 5, fontface = "bold") +
    theme_bw() +
    theme(
      plot.title = element_blank(),
      legend.position = "none",
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text = element_text(size = 8),
      plot.margin = margin(5, 5, 5, 5)
    )

  return(p)
}

# Create plots for each cancer type
panel_plots <- list()

# (A) All cancer
if (exists("overall_2panel_data") && nrow(overall_2panel_data) > 0) {
  # Check if budget_share_total exists, if not calculate it
  if (!"budget_share_total" %in% colnames(overall_2panel_data)) {
    if (!is.null(ntis6_cost) && "total_cancer_cost" %in% colnames(ntis6_cost) &&
        yearly_stats_available && !is.null(yearly_total_stats)) {
      if ("total_money" %in% colnames(yearly_total_stats)) {
        budget_data <- ntis6_cost %>%
          dplyr::select(Year, total_cancer_cost) %>%
          left_join(
            yearly_total_stats %>%
              dplyr::select(Year, total_money) %>%
              rename(year = Year),
            by = c("Year" = "year")
          ) %>%
          mutate(
            budget_share_total = ifelse(
              !is.na(total_money) & total_money > 0,
              (total_cancer_cost / total_money) * 100,
              NA_real_
            )
          ) %>%
          dplyr::select(Year, budget_share_total) %>%
          rename(year = Year)

        overall_2panel_data <- overall_2panel_data %>%
          left_join(budget_data, by = "year")
      }
    }
  }
  panel_plots[["A"]] <- create_unified_plot_no_title(overall_2panel_data, "All cancers", "A · All cancers", is_overall = TRUE)
}

# (B) Breast cancer (유방암)
df_breast <- analysis_data %>% dplyr::filter(cancer_kor == "유방암") %>% dplyr::arrange(year)
if (nrow(df_breast) > 0) {
  panel_plots[["B"]] <- create_unified_plot_no_title(df_breast, "Breast cancer", "B · Breast cancer", is_overall = FALSE)
}

# (C) Colon cancer (대장암)
df_colorectal <- analysis_data %>% dplyr::filter(cancer_kor == "대장암") %>% dplyr::arrange(year)
if (nrow(df_colorectal) > 0) {
  panel_plots[["C"]] <- create_unified_plot_no_title(df_colorectal, "Colorectal cancer", "C · Colorectal cancer", is_overall = FALSE)
}

# (D) Lung cancer (폐암)
df_lung <- analysis_data %>% dplyr::filter(cancer_kor == "폐암") %>% dplyr::arrange(year)
if (nrow(df_lung) > 0) {
  panel_plots[["D"]] <- create_unified_plot_no_title(df_lung, "Lung cancer", "D · Lung cancer", is_overall = FALSE)
}

# (E) Liver cancer (간암)
df_liver <- analysis_data %>% dplyr::filter(cancer_kor == "간암") %>% dplyr::arrange(year)
if (nrow(df_liver) > 0) {
  panel_plots[["E"]] <- create_unified_plot_no_title(df_liver, "Liver cancer", "E · Liver cancer", is_overall = FALSE)
}

# (F) Stomach cancer (위암)
df_gastric <- analysis_data %>% dplyr::filter(cancer_kor == "위암") %>% dplyr::arrange(year)
if (nrow(df_gastric) > 0) {
  panel_plots[["F"]] <- create_unified_plot_no_title(df_gastric, "Gastric cancer", "F · Gastric cancer", is_overall = FALSE)
}

# (G) Pancreatic cancer (췌장암)
df_pancreatic <- analysis_data %>% dplyr::filter(cancer_kor == "췌장암") %>% dplyr::arrange(year)
if (nrow(df_pancreatic) > 0) {
  panel_plots[["G"]] <- create_unified_plot_no_title(df_pancreatic, "Pancreatic cancer", "G · Pancreatic cancer", is_overall = FALSE)
}

# (H) Prostate cancer (전립선암)
df_prostate <- analysis_data %>% dplyr::filter(cancer_kor == "전립선암") %>% dplyr::arrange(year)
if (nrow(df_prostate) > 0) {
  panel_plots[["H"]] <- create_unified_plot_no_title(df_prostate, "Prostate cancer", "H · Prostate cancer", is_overall = FALSE)
}

# Create a shared legend and combine plots (research metrics only; no incidence/mortality)
if (length(panel_plots) > 0) {
  dummy_legend_data <- data.frame(
    x = rep(1:2, 2),
    y = rep(1:2, 2),
    metric = rep(c("projects_share", "budget_share"), each = 2)
  )
  dummy_legend_data$metric <- factor(dummy_legend_data$metric,
                                      levels = c("projects_share", "budget_share"))

  legend_only_plot <- ggplot(dummy_legend_data,
                              aes(x = x, y = y, color = metric, group = metric)) +
    geom_line(linewidth = 1) +
    scale_color_manual(
      values = c(
        "projects_share" = "#7570b3",
        "budget_share" = "#e7298a"
      ),
      labels = c(
        "projects_share" = "Project share (%)",
        "budget_share" = "Funding share (%)"
      ),
      breaks = c("projects_share", "budget_share"),
      name = "Metrics"
    ) +
    scale_x_continuous(limits = c(0, 0), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 0), expand = c(0, 0)) +
    coord_cartesian(xlim = c(0, 0), ylim = c(0, 0), clip = "on") +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.justification = "center",
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12, face = "bold"),
      plot.margin = margin(0, 0, 0, 0),
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(0.5, "cm"),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank()
    ) +
    guides(color = guide_legend(
      nrow = 1, byrow = TRUE,
      override.aes = list(linewidth = 1.2, linetype = "solid"),
      order = 1
    ))

  # Combine plots in 4x2 layout: A, B / C, D / E, F / G, H
  plot_list <- list(
    panel_plots[["A"]], panel_plots[["B"]],
    panel_plots[["C"]], panel_plots[["D"]],
    panel_plots[["E"]], panel_plots[["F"]],
    panel_plots[["G"]], panel_plots[["H"]]
  )

  # Remove NULL plots
  plot_list <- plot_list[!sapply(plot_list, is.null)]

  if (length(plot_list) == 8) {
    # Create 4x2 panel with plots
    panel_plots_combined <- (plot_list[[1]] | plot_list[[2]]) /
      (plot_list[[3]] | plot_list[[4]]) /
      (plot_list[[5]] | plot_list[[6]]) /
      (plot_list[[7]] | plot_list[[8]])

    # Combine using patchwork - the legend plot will show only the legend
    panel_combined <- panel_plots_combined /
      legend_only_plot +
      plot_layout(heights = c(1, 1, 1, 1, 0.08))

    # Save with two resolutions: 600 DPI and 1000 DPI
    fig_panel_600 <- file.path(
      output_dir_figures,
      paste0("figure4_epidemiology_research_trends_4x2_panel_600dpi_", timestamp, ".png")
    )
    fig_panel_1000 <- file.path(
      output_dir_figures,
      paste0("figure4_epidemiology_research_trends_4x2_panel_1000dpi_", timestamp, ".png")
    )
    ggsave(fig_panel_600, panel_combined, width = 14, height = 16, dpi = 600)
    ggsave(fig_panel_1000, panel_combined, width = 14, height = 16, dpi = 1000)
    cat("Figure 4 (4x2 panel of research trends only; incidence/mortality removed) saved to:", fig_panel_600, "\n")
    cat("Figure 4 (4x2 panel of research trends only; incidence/mortality removed) saved to:", fig_panel_1000, "\n")
  } else {
    cat("Warning: Not all 8 plots were created. Only", length(plot_list), "plots available.\n")
  }
} else {
  cat("No plots available for panel creation.\n")
}

# =============================================================================
# CREATE TABLE 4: EPIDEMIOLOGY AND RESEARCH PROPORTION TRENDS (YEARLY FORMAT)
# =============================================================================

cat("\n=== CREATING TABLE 4: EPIDEMIOLOGY AND RESEARCH PROPORTION TRENDS ===\n")

# Helper function to get value or NA
get_value <- function(df, col_name) {
  if (col_name %in% colnames(df)) {
    val <- df[[col_name]]
    ifelse(is.na(val), NA_real_, val)
  } else {
    NA_real_
  }
}

# (Table 4 and Table 5 not used in paper - omitted.)

cat("=== 05_compare_epi.R COMPLETED ===\n")
