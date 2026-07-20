# =============================================================================
# Representative text selection and the research-stage portfolio.
#
# For each project-period combination, selects a representative title and
# keyword string from the constituent project-years, then summarises the
# research-stage composition of cancer-related projects over time.
#
# Input :  ntis2_period_reshape_cancer_*, ntis2_all_summary_tables_*
# Output:  ntis3_analysis_with_representative_texts_*  -> Figure 2 and Table S2
# =============================================================================

if (!exists("PROJ_ROOT")) source("config.R")

required_packages <- c("readxl", "dplyr", "stringr", "writexl", "data.table", "ggplot2", "gridExtra", "openxlsx")
invisible(lapply(required_packages, library, character.only = TRUE))


# =============================================================================
# READ step 01 DATA
# =============================================================================

cat("=== 02_analysis STARTING ===\n")

# Find the most recent step 01 period-based reshaped file (prefer RData, fallback to CSV)
output_dir_tables <- OUT_TABLES
ntis2_rdata_files <- list.files(path = output_dir_tables, pattern = "ntis2_period_reshape_cancer_.*\\.RData$", full.names = TRUE)
ntis2_csv_files <- list.files(path = output_dir_tables, pattern = "ntis2_period_reshape_cancer_.*\\.csv$", full.names = TRUE)

# Prefer RData, fallback to CSV
if (length(ntis2_rdata_files) > 0) {
  file_info <- file.info(ntis2_rdata_files)
  ntis2_rdata_files <- ntis2_rdata_files[order(file_info$mtime, decreasing = TRUE)]
  input_file <- ntis2_rdata_files[1]
  cat("Loading step 01 RData:", basename(input_file), "\n")
  load(input_file)
  # dt_final is the variable name in RData
  if (!exists("dt_final")) {
    stop("Variable 'dt_final' not found in RData file. Please check step 01 output.")
  }
  data <- dt_final
  cat("Data loaded from RData:", nrow(data), "rows x", ncol(data), "columns\n")
} else if (length(ntis2_csv_files) > 0) {
  file_info <- file.info(ntis2_csv_files)
  ntis2_csv_files <- ntis2_csv_files[order(file_info$mtime, decreasing = TRUE)]
  input_file <- ntis2_csv_files[1]
  cat("Loading step 01 CSV:", basename(input_file), " (RData not found, using CSV)\n")
  data <- fread(input_file)
  cat("Data loaded from CSV:", nrow(data), "rows x", ncol(data), "columns\n")
} else {
  stop("No step 01 period-based reshaped files found in output/tables! Please run 01_filter.R first.")
}

# =============================================================================
# CREATE REPRESENTATIVE TEXT COLUMNS
# =============================================================================

cat("\n=== CREATING REPRESENTATIVE TEXT COLUMNS ===\n")

create_representative_text <- function(data, text_type) {
  cat("Creating representative", text_type, "columns...\n")

  # Get all year-tagged columns for this text type
  text_cols <- grep(paste0("^", text_type, "_\\d{4}$"), colnames(data), value = TRUE)

  if (length(text_cols) == 0) {
    cat("  No", text_type, "columns found\n")
    return(data)
  }

  # Create representative column
  data[[paste0(text_type, "_representative")]] <- NA_character_

  for (i in 1:nrow(data)) {
    # Get all non-NA values for this row
    values <- data[i, ..text_cols]
    values <- unlist(values)
    values <- values[!is.na(values) & values != ""]

    if (length(values) > 0) {
      # IMPROVED SELECTION: Most frequent value with tie-breaking
      value_counts <- table(values)
      max_count <- max(value_counts)
      most_frequent_values <- names(value_counts)[value_counts == max_count]

      if (length(most_frequent_values) == 1) {
        # Single most frequent value - use it
        data[i, paste0(text_type, "_representative") := most_frequent_values[1]]
      } else {
        # TIE: Multiple values with same frequency
        # Break tie by choosing the longest text (most descriptive)
        longest_length <- max(nchar(most_frequent_values))
        longest_values <- most_frequent_values[nchar(most_frequent_values) == longest_length]

        if (length(longest_values) == 1) {
          # Single longest value - use it
          data[i, paste0(text_type, "_representative") := longest_values[1]]
        } else {
          # STILL TIE: Multiple values with same frequency and length
          # Final fallback: use the first occurrence (original behavior)
          data[i, paste0(text_type, "_representative") := longest_values[1]]
        }
      }
    }
  }

  cat("  Representative", text_type, "column created (using most frequent with tie-breaking)\n")
  return(data)
}

# Create representative columns for all text types
data <- create_representative_text(data, "Text1")
data <- create_representative_text(data, "Text2")
data <- create_representative_text(data, "Key1")
data <- create_representative_text(data, "Key2")

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

cat("\n=== SUMMARY STATISTICS ===\n")

# Basic counts
total_rows <- nrow(data)
cat("Total period-wise projects:", total_rows, "\n")

# Period distribution
period_summary <- data %>%
  group_by(period) %>%
  summarise(
    total = n(),
    .groups = 'drop'
  )

cat("\nPeriod Distribution:\n")
for (i in 1:nrow(period_summary)) {
  row <- period_summary[i, ]
  cat(sprintf("Period %d: %d projects\n", row$period, row$total))
}

# Representative text coverage
text_coverage <- data.frame(
  Column = c("Text1_representative", "Text2_representative", "Key1_representative", "Key2_representative"),
  Non_NA_Count = c(
    sum(!is.na(data$Text1_representative)),
    sum(!is.na(data$Text2_representative)),
    sum(!is.na(data$Key1_representative)),
    sum(!is.na(data$Key2_representative))
  )
)

text_coverage$Coverage_Percent <- round(text_coverage$Non_NA_Count / total_rows * 100, 1)

cat("\nRepresentative Text Coverage:\n")
for (i in 1:nrow(text_coverage)) {
  row <- text_coverage[i, ]
  cat(sprintf("%s: %d rows (%.1f%%)\n", row$Column, row$Non_NA_Count, row$Coverage_Percent))
}

# =============================================================================
# DETAILED DIAGNOSTIC ANALYSIS
# =============================================================================

cat("\n=== DETAILED DIAGNOSTIC ANALYSIS ===\n")

# Function to analyze empty representative columns
analyze_empty_representatives <- function(data, text_type) {
  cat("\n--- Analysis for", text_type, "---\n")

  # Get year-tagged columns for this text type
  text_cols <- grep(paste0("^", text_type, "_\\d{4}$"), colnames(data), value = TRUE)
  rep_col <- paste0(text_type, "_representative")

  # Find rows with empty representative columns
  empty_rows <- data[is.na(data[[rep_col]]), ]
  total_empty <- nrow(empty_rows)

  if (total_empty == 0) {
    cat("  No empty representative columns found\n")
    return()
  }

  cat("  Empty representative columns:", total_empty, "rows\n")

  # Analyze patterns in empty rows
  empty_patterns <- data.frame(
    Pattern = character(),
    Count = numeric(),
    Percentage = numeric(),
    stringsAsFactors = FALSE
  )

  # Pattern 1: All NA values
  all_na_count <- sum(apply(empty_rows[, ..text_cols], 1, function(x) all(is.na(x))))
  if (all_na_count > 0) {
    empty_patterns <- rbind(empty_patterns, data.frame(
      Pattern = "All year columns are NA",
      Count = all_na_count,
      Percentage = round(all_na_count / total_empty * 100, 1),
      stringsAsFactors = FALSE
    ))
  }

  # Pattern 2: All empty strings
  all_empty_count <- sum(apply(empty_rows[, ..text_cols], 1, function(x) {
    !all(is.na(x)) && all(x[!is.na(x)] == "")
  }))
  if (all_empty_count > 0) {
    empty_patterns <- rbind(empty_patterns, data.frame(
      Pattern = "All non-NA values are empty strings",
      Count = all_empty_count,
      Percentage = round(all_empty_count / total_empty * 100, 1),
      stringsAsFactors = FALSE
    ))
  }

  # Pattern 3: Mixed NA and empty strings
  mixed_count <- total_empty - all_na_count - all_empty_count
  if (mixed_count > 0) {
    empty_patterns <- rbind(empty_patterns, data.frame(
      Pattern = "Mixed NA and empty strings",
      Count = mixed_count,
      Percentage = round(mixed_count / total_empty * 100, 1),
      stringsAsFactors = FALSE
    ))
  }

  # Display patterns
  cat("  Empty patterns:\n")
  for (i in 1:nrow(empty_patterns)) {
    row <- empty_patterns[i, ]
    cat(sprintf("    %s: %d rows (%.1f%%)\n", row$Pattern, row$Count, row$Percentage))
  }

  # Period analysis for empty rows
  if (total_empty > 0) {
    cat("  Empty rows by period:\n")
    period_empty <- empty_rows %>%
      group_by(period) %>%
      summarise(
        empty_count = n(),
        .groups = 'drop'
      )

    for (i in 1:nrow(period_empty)) {
      row <- period_empty[i, ]
      period_total <- sum(data$period == row$period)
      cat(sprintf("    Period %d: %d/%d empty (%.1f%%)\n",
                  row$period, row$empty_count, period_total,
                  row$empty_count/period_total*100))
    }
  }
}

# Analyze each text type
analyze_empty_representatives(data, "Text1")
analyze_empty_representatives(data, "Text2")
analyze_empty_representatives(data, "Key1")
analyze_empty_representatives(data, "Key2")

# Summary of data availability by period
cat("\n--- Data Availability by Period ---\n")
for (period in sort(unique(data$period))) {
  period_data <- data[data$period == period, ]
  cat(sprintf("\nPeriod %d (%d total projects):\n", period, nrow(period_data)))

  for (text_type in c("Text1", "Text2", "Key1", "Key2")) {
    text_cols <- grep(paste0("^", text_type, "_\\d{4}$"), colnames(period_data), value = TRUE)
    rep_col <- paste0(text_type, "_representative")

    if (length(text_cols) > 0) {
      # Count non-NA values in year columns
      year_data_count <- sum(!is.na(period_data[, ..text_cols]))
      year_total_cells <- nrow(period_data) * length(text_cols)
      year_coverage <- year_data_count / year_total_cells * 100

      # Count representative values
      rep_count <- sum(!is.na(period_data[[rep_col]]))
      rep_coverage <- rep_count / nrow(period_data) * 100

      cat(sprintf("  %s: %d/%d year cells (%.1f%%) → %d/%d representative (%.1f%%)\n",
                  text_type, year_data_count, year_total_cells, year_coverage,
                  rep_count, nrow(period_data), rep_coverage))
    }
  }
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("\n=== SAVING RESULTS ===\n")

# Create output directory
output_dir_tables <- OUT_TABLES
if (!dir.exists(output_dir_tables)) dir.create(output_dir_tables, recursive = TRUE)

# Save the enhanced data
timestamp <- format(Sys.time(), "%Y%m%d")
output_file <- file.path(output_dir_tables, paste0("ntis3_analysis_with_representative_texts_", timestamp, ".csv"))
fwrite(data, output_file)
cat("Enhanced data saved to:", output_file, "\n")

# Also save as RData for faster loading
rdata_file <- file.path(output_dir_tables, paste0("ntis3_analysis_with_representative_texts_", timestamp, ".RData"))
save(data, file = rdata_file)
cat("Enhanced data also saved to:", rdata_file, " (RData format for faster loading)\n")

# Create summary report
summary_report <- data.frame(
  Metric = c("Total Period-wise Projects", "Text1 Coverage", "Text2 Coverage", "Key1 Coverage", "Key2 Coverage"),
  Value = c(total_rows,
            paste0(text_coverage$Non_NA_Count[1], " (", text_coverage$Coverage_Percent[1], "%)"),
            paste0(text_coverage$Non_NA_Count[2], " (", text_coverage$Coverage_Percent[2], "%)"),
            paste0(text_coverage$Non_NA_Count[3], " (", text_coverage$Coverage_Percent[3], "%)"),
            paste0(text_coverage$Non_NA_Count[4], " (", text_coverage$Coverage_Percent[4], "%)"))
)

summary_file <- file.path(output_dir_tables, paste0("ntis3_summary_report_", timestamp, ".csv"))
write.csv(summary_report, summary_file, row.names = FALSE)
cat("Summary report saved to:", summary_file, "\n")

# =============================================================================
# HELPER: Insert publication-style section rows (Overall, Epoch, Annually) with empty data columns
# Input df: 24 rows = 1 overall (2006-2024), 4 epochs, 19 years. First column = Time.
# Output: 27 rows = Overall (header), 2006-2024, Epoch (header), 4 epochs, Annually (header), 19 years.
# =============================================================================
insert_section_headers <- function(df) {
  nms <- names(df)
  other_nms <- nms[nms != "Time"]
  blank_row <- function(time_label) {
    r <- as.data.frame(matrix(NA, nrow = 1, ncol = length(nms)))
    names(r) <- nms
    r$Time <- time_label
    for (col in other_nms) r[[col]] <- NA_real_
    r
  }
  rbind(
    blank_row("Overall"),
    df[1L, , drop = FALSE],
    blank_row("Epoch"),
    df[2:5, , drop = FALSE],
    blank_row("Annually"),
    df[6:24, , drop = FALSE]
  )
}

# =============================================================================
# CREATE TABLE S1: BUDGET AND PROJECT COUNT SUMMARY
# =============================================================================

cat("\n=== CREATING TABLE S1: BUDGET AND PROJECT COUNT SUMMARY ===\n")

output_dir_figures <- OUT_FIGURES
if (!dir.exists(output_dir_figures)) dir.create(output_dir_figures, recursive = TRUE)

# Load latest yearly_total_stats from step 01
yearly_stats_files <- list.files(
  path = output_dir_tables,
  pattern = "ntis2_yearly_total_stats_.*\\.RData$",
  full.names = TRUE
)

if (length(yearly_stats_files) == 0) {
  cat("Yearly total stats from step 01 not found – please run 01_filter.R first.\n")
} else {
  file_info <- file.info(yearly_stats_files)
  latest_stats_file <- yearly_stats_files[order(file_info$mtime, decreasing = TRUE)][1]
  cat("Loading yearly statistics from:", basename(latest_stats_file), "\n")
  load(latest_stats_file)  # loads yearly_total_stats

  if (!exists("yearly_total_stats")) {
    cat("Object 'yearly_total_stats' not found in RData. Table S1 and Figure 1 not created.\n")
  } else {
    # Prepare data for Table S1
    ys <- yearly_total_stats
    ys$Year <- as.numeric(ys$Year)
    ys <- ys[!is.na(ys$Year) & ys$Year >= 2006 & ys$Year <= 2024, ]

    # Calculate proportions
    ys$budget_proportion <- ifelse(
      ys$total_money > 0,
      round(ys$total_cancer_money / ys$total_money * 100, 2),
      NA_real_
    )
    ys$project_proportion <- ifelse(
      ys$total_projects > 0,
      round(ys$total_cancer_projects / ys$total_projects * 100, 2),
      NA_real_
    )

    # Create Table S1 with three sections: Overall, Epoch, Year
    # Columns ordered as: Project count, Project %, then Funding (Billion KRW), Funding %
    table1_rows <- data.frame(
      Time = character(),
      Project_count = numeric(),
      Project_proportion = numeric(),
      Funding_billion_KRW = numeric(),
      Funding_proportion = numeric(),
      stringsAsFactors = FALSE
    )

    # 1) Overall (2006-2024)
    overall_budget <- sum(ys$total_cancer_money, na.rm = TRUE)
    overall_total_budget <- sum(ys$total_money, na.rm = TRUE)
    overall_budget_prop <- ifelse(overall_total_budget > 0,
                                  round(overall_budget / overall_total_budget * 100, 2),
                                  NA_real_)
    overall_projects <- sum(ys$total_cancer_projects, na.rm = TRUE)
    overall_total_projects <- sum(ys$total_projects, na.rm = TRUE)
    overall_project_prop <- ifelse(overall_total_projects > 0,
                                  round(overall_projects / overall_total_projects * 100, 2),
                                  NA_real_)

    table1_rows <- rbind(
      table1_rows,
      data.frame(
        Time = "2006-2024",
        Project_count = overall_projects,
        Project_proportion = overall_project_prop,
        Funding_billion_KRW = round(overall_budget / 1e9, 2),
        Funding_proportion = overall_budget_prop,
        stringsAsFactors = FALSE
      )
    )

    # 2) Epochs (2006-2010, 2011-2015, 2016-2020, 2021-2024)
    epochs <- list(
      c(2006, 2010),
      c(2011, 2015),
      c(2016, 2020),
      c(2021, 2024)
    )

    for (epoch in epochs) {
      epoch_data <- ys[ys$Year >= epoch[1] & ys$Year <= epoch[2], ]
      epoch_budget <- sum(epoch_data$total_cancer_money, na.rm = TRUE)
      epoch_total_budget <- sum(epoch_data$total_money, na.rm = TRUE)
      epoch_budget_prop <- ifelse(epoch_total_budget > 0,
                                  round(epoch_budget / epoch_total_budget * 100, 2),
                                  NA_real_)
      epoch_projects <- sum(epoch_data$total_cancer_projects, na.rm = TRUE)
      epoch_total_projects <- sum(epoch_data$total_projects, na.rm = TRUE)
      epoch_project_prop <- ifelse(epoch_total_projects > 0,
                                   round(epoch_projects / epoch_total_projects * 100, 2),
                                   NA_real_)

      table1_rows <- rbind(
        table1_rows,
        data.frame(
          Time = paste0(epoch[1], "-", epoch[2]),
          Project_count = epoch_projects,
          Project_proportion = epoch_project_prop,
          Funding_billion_KRW = round(epoch_budget / 1e9, 2),
          Funding_proportion = epoch_budget_prop,
          stringsAsFactors = FALSE
        )
      )
    }

    # 3) Yearly (2006-2024)
    for (year in 2006:2024) {
      year_data <- ys[ys$Year == year, ]
      if (nrow(year_data) > 0) {
        year_budget <- year_data$total_cancer_money[1]
        year_total_budget <- year_data$total_money[1]
        year_budget_prop <- year_data$budget_proportion[1]
        year_projects <- year_data$total_cancer_projects[1]
        year_total_projects <- year_data$total_projects[1]
        year_project_prop <- year_data$project_proportion[1]

        table1_rows <- rbind(
          table1_rows,
          data.frame(
            Time = as.character(year),
            Project_count = year_projects,
            Project_proportion = year_project_prop,
            Funding_billion_KRW = round(year_budget / 1e9, 2),
            Funding_proportion = year_budget_prop,
            stringsAsFactors = FALSE
          )
        )
      }
    }

    # Insert section header rows (Overall, Epoch, Annually) for publication format
    table1_rows <- insert_section_headers(table1_rows)
    # Round all numeric columns to 1 decimal place for display
    for (col in c("Project_count", "Project_proportion", "Funding_billion_KRW", "Funding_proportion")) {
      table1_rows[[col]] <- round(table1_rows[[col]], 1)
    }

    # Save Table S1 as Excel
    table1_xlsx <- file.path(output_dir_tables, paste0("tableS1_temporal_trends_cancer_research_", timestamp, ".xlsx"))
    wb_table1 <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb_table1, "TableS1")
    openxlsx::writeData(wb_table1, "TableS1", table1_rows)
    openxlsx::saveWorkbook(wb_table1, table1_xlsx, overwrite = TRUE)
    cat("Table S1 (Temporal trends in cancer-related research, 2006-2024) saved to:", table1_xlsx, "\n")

    # =============================================================================
    # CREATE FIGURE 1: TEMPORAL TRENDS IN CANCER-RELATED RESEARCH (2006-2024)
    # =============================================================================

    cat("\n=== CREATING FIGURE 1: TEMPORAL TRENDS IN CANCER-RELATED RESEARCH (2006-2024) ===\n")

    ys <- yearly_total_stats
    # Ensure proper types
    ys$Year <- as.numeric(ys$Year)

    # Keep valid years 2006–2024
    ys <- ys[!is.na(ys$Year) & ys$Year >= 2006 & ys$Year <= 2024, ]

    # -------------------------------------------------------------------------
    # Panel A: Cancer-related project count & proportion
    # -------------------------------------------------------------------------
    ys$proj_prop <- ifelse(
      ys$total_projects > 0,
      ys$total_cancer_projects / ys$total_projects * 100,
      NA_real_
    )

    # Scaling for dual y-axis
    max_proj <- max(ys$total_cancer_projects, na.rm = TRUE)
    max_proj_prop <- max(ys$proj_prop, na.rm = TRUE)
    proj_scale_factor <- ifelse(is.finite(max_proj_prop) && max_proj_prop > 0,
                                max_proj / max_proj_prop, 1)

    panelA <- ggplot(ys, aes(x = Year)) +
      # Cancer-related project count (left y-axis)
      geom_line(aes(y = total_cancer_projects, color = "Count"), linewidth = 1.1) +
      geom_point(aes(y = total_cancer_projects, color = "Count"), size = 2.5) +
      # Cancer-related project proportion (right y-axis; scaled)
      geom_line(aes(y = proj_prop * proj_scale_factor, color = "Proportion"),
                linewidth = 1.1, linetype = "longdash") +
      geom_point(aes(y = proj_prop * proj_scale_factor, color = "Proportion"),
                 size = 2.5, shape = 17) +
      geom_text(
        aes(y = proj_prop * proj_scale_factor,
            label = ifelse(!is.na(proj_prop), sprintf("%.1f%%", proj_prop), ""),
            color = "Proportion"),
        vjust = -0.8, size = 3
      ) +
      scale_x_continuous(breaks = seq(2006, 2024, 2), minor_breaks = 2006:2024) +
      scale_y_continuous(
        name = "Cancer-Related Project Count",
        limits = c(0, NA),
        sec.axis = sec_axis(~ . / proj_scale_factor,
                            name = "Cancer-Related Proportion (%)",
                            labels = function(x) sprintf("%.1f", x))
      ) +
      scale_color_manual(
        name = NULL,
        values = c("Count" = "#2484B8", "Proportion" = "#C03B8E"),
        labels = c("Cancer-Related Project Count", "Cancer-Related Proportion")
      ) +
      theme_minimal() +
      theme(
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
        legend.direction = "horizontal",
        legend.box = "horizontal",
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 11, face = "bold", color = "#2484B8"),
        axis.title.y.right = element_text(size = 11, face = "bold", color = "#C03B8E"),
        axis.text = element_text(size = 9),
        plot.title = element_blank(),
        plot.margin = margin(10, 20, 10, 10),
        plot.tag.position = c(0.08, 0.98),
        plot.tag = element_text(size = 12, face = "bold")
      ) +
      labs(title = NULL, tag = "(A)")

    # -------------------------------------------------------------------------
    # Panel B: Cancer-related funding (Billion KRW) & proportion
    # -------------------------------------------------------------------------
    if ("total_money" %in% colnames(ys) && "total_cancer_money" %in% colnames(ys)) {
      # Funding in Billion KRW to match Table 1 units
      ys$fund_billion <- ys$total_cancer_money / 1e9
      ys$fund_prop <- ifelse(
        ys$total_money > 0,
        ys$total_cancer_money / ys$total_money * 100,
        NA_real_
      )

      max_fund <- max(ys$fund_billion, na.rm = TRUE)
      max_fund_prop <- max(ys$fund_prop, na.rm = TRUE)
      fund_scale_factor <- ifelse(is.finite(max_fund_prop) && max_fund_prop > 0,
                                  max_fund / max_fund_prop, 1)

      panelB <- ggplot(ys, aes(x = Year)) +
        geom_line(aes(y = fund_billion, color = "Funding"), linewidth = 1.1) +
        geom_point(aes(y = fund_billion, color = "Funding"), size = 2.5) +
        geom_line(aes(y = fund_prop * fund_scale_factor, color = "Proportion"),
                  linewidth = 1.1, linetype = "longdash") +
        geom_point(aes(y = fund_prop * fund_scale_factor, color = "Proportion"),
                   size = 2.5, shape = 17) +
        geom_text(
          aes(y = fund_prop * fund_scale_factor,
              label = ifelse(!is.na(fund_prop), sprintf("%.1f%%", fund_prop), ""),
              color = "Proportion"),
          vjust = -0.8, size = 3
        ) +
        scale_x_continuous(breaks = seq(2006, 2024, 2), minor_breaks = 2006:2024) +
        scale_y_continuous(
          name = "Cancer-Related Funding (Billion KRW)",
          limits = c(0, NA),
          sec.axis = sec_axis(~ . / fund_scale_factor,
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
          legend.position = c(1, 0),
          legend.justification = c(1, 0),
          legend.direction = "horizontal",
          legend.box = "horizontal",
          axis.title.x = element_text(size = 11, face = "bold"),
          axis.title.y = element_text(size = 11, face = "bold", color = "#2484B8"),
          axis.title.y.right = element_text(size = 11, face = "bold", color = "#C03B8E"),
          axis.text = element_text(size = 9),
          plot.title = element_blank(),
          plot.margin = margin(10, 20, 10, 10),
          plot.tag.position = c(0.08, 0.98),
          plot.tag = element_text(size = 12, face = "bold")
        ) +
        labs(title = NULL, tag = "(B)")

      # Combine panels A and B into a single Figure 1
      fig1_file <- file.path(output_dir_figures,
                             paste0("figure1_temporal_trends_cancer_research_", timestamp, ".png"))
      png(fig1_file, width = 9, height = 12, units = "in", res = 300)
      gridExtra::grid.arrange(panelA, panelB, ncol = 1)
      dev.off()
      cat("Figure 1 (panels A&B) saved to:", fig1_file, "\n")
    } else {
      cat("total_money / total_cancer_money not available in yearly_total_stats – Panel B not created.\n")
    }
  }
}

# =============================================================================
# CREATE TABLE 2: RESEARCH-STAGE PORTFOLIO SUMMARY
# =============================================================================

cat("\n=== CREATING TABLE S2: RESEARCH-STAGE PORTFOLIO SUMMARY ===\n")

# Load latest step 01 summary tables (contains Table5/6 for R&D stages)
summary_files <- list.files(
  path = output_dir_tables,
  pattern = "ntis2_all_summary_tables_.*\\.xlsx$",
  full.names = TRUE
)

if (length(summary_files) == 0) {
  cat("step 01 summary tables not found – please run 01_filter.R first.\n")
} else {
  sf_info <- file.info(summary_files)
  latest_summary <- summary_files[order(sf_info$mtime, decreasing = TRUE)][1]
  cat("Loading step 01 summary tables from:", basename(latest_summary), "\n")

  # Project count table (Table 6)
  table6_projects <- tryCatch(
    readxl::read_excel(latest_summary, sheet = "Table6_Projects_RnD"),
    error = function(e) NULL
  )

  # Funding table (Table 5)
  table5_money <- tryCatch(
    readxl::read_excel(latest_summary, sheet = "Table5_Money_RnD"),
    error = function(e) NULL
  )

  if (is.null(table6_projects) || is.null(table5_money)) {
    cat("Failed to read Table5/6 from step 01 summary – Table S2 and Figure 2 not created.\n")
  } else {
    # Mapping R&D stage labels
    rd_labels_map <- c(
      "기초연구" = "Basic Research",
      "응용연구" = "Applied Research",
      "개발연구" = "Development Research",
      "기타"     = "Others"
    )

    # Prepare data
    table5_money$Year <- as.numeric(table5_money$Year)
    table5_money <- table5_money[!is.na(table5_money$Year) & table5_money$Year >= 2006 & table5_money$Year <= 2024, ]

    table6_projects$Year <- as.numeric(table6_projects$Year)
    table6_projects <- table6_projects[!is.na(table6_projects$Year) & table6_projects$Year >= 2006 & table6_projects$Year <= 2024, ]

    # Map Korean column names to English
    col_mapping <- c(
      "기초연구" = "Basic_research",
      "응용연구" = "Applied_research",
      "개발연구" = "Development_research",
      "기타" = "Others"
    )

    # Function to create proportion table from data
    create_proportion_table <- function(data_table, data_type) {
      table_rows <- data.frame(
        Time = character(),
        Basic_research = numeric(),
        Applied_research = numeric(),
        Development_research = numeric(),
        Others = numeric(),
        stringsAsFactors = FALSE
      )

      # Helper function to get value or 0
      get_value <- function(df, col_name) {
        if (col_name %in% colnames(df)) {
          val <- df[[col_name]]
          ifelse(is.na(val), 0, val)
        } else {
          0
        }
      }

      # Helper function to calculate proportions
      calculate_proportions <- function(basic, applied, development, others) {
        total <- basic + applied + development + others
        if (total > 0) {
          list(
            basic_prop = round(basic / total * 100, 1),
            applied_prop = round(applied / total * 100, 1),
            development_prop = round(development / total * 100, 1),
            others_prop = round(others / total * 100, 1)
          )
        } else {
          list(
            basic_prop = 0,
            applied_prop = 0,
            development_prop = 0,
            others_prop = 0
          )
        }
      }

      # 1) Overall (2006-2024)
      overall_basic <- sum(get_value(data_table, "기초연구"), na.rm = TRUE)
      overall_applied <- sum(get_value(data_table, "응용연구"), na.rm = TRUE)
      overall_development <- sum(get_value(data_table, "개발연구"), na.rm = TRUE)
      overall_others <- sum(get_value(data_table, "기타"), na.rm = TRUE)

      overall_props <- calculate_proportions(overall_basic, overall_applied, overall_development, overall_others)

      table_rows <- rbind(table_rows, data.frame(
        Time = "2006-2024",
        Basic_research = overall_props$basic_prop,
        Applied_research = overall_props$applied_prop,
        Development_research = overall_props$development_prop,
        Others = overall_props$others_prop,
        stringsAsFactors = FALSE
      ))

      # 2) Epochs (2006-2010, 2011-2015, 2016-2020, 2021-2024)
      epochs <- list(
        c(2006, 2010),
        c(2011, 2015),
        c(2016, 2020),
        c(2021, 2024)
      )

      for (epoch in epochs) {
        epoch_data <- data_table[data_table$Year >= epoch[1] & data_table$Year <= epoch[2], ]
        epoch_basic <- sum(get_value(epoch_data, "기초연구"), na.rm = TRUE)
        epoch_applied <- sum(get_value(epoch_data, "응용연구"), na.rm = TRUE)
        epoch_development <- sum(get_value(epoch_data, "개발연구"), na.rm = TRUE)
        epoch_others <- sum(get_value(epoch_data, "기타"), na.rm = TRUE)

        epoch_props <- calculate_proportions(epoch_basic, epoch_applied, epoch_development, epoch_others)

        table_rows <- rbind(table_rows, data.frame(
          Time = paste0(epoch[1], "-", epoch[2]),
          Basic_research = epoch_props$basic_prop,
          Applied_research = epoch_props$applied_prop,
          Development_research = epoch_props$development_prop,
          Others = epoch_props$others_prop,
          stringsAsFactors = FALSE
        ))
      }

      # 3) Yearly (2006-2024)
      for (year in 2006:2024) {
        year_data <- data_table[data_table$Year == year, ]
        if (nrow(year_data) > 0) {
          year_basic <- get_value(year_data, "기초연구")[1]
          year_applied <- get_value(year_data, "응용연구")[1]
          year_development <- get_value(year_data, "개발연구")[1]
          year_others <- get_value(year_data, "기타")[1]

          year_props <- calculate_proportions(year_basic, year_applied, year_development, year_others)

          table_rows <- rbind(table_rows, data.frame(
            Time = as.character(year),
            Basic_research = year_props$basic_prop,
            Applied_research = year_props$applied_prop,
            Development_research = year_props$development_prop,
            Others = year_props$others_prop,
            stringsAsFactors = FALSE
          ))
        }
      }

      return(table_rows)
    }

    # Function to create absolute table (count or budget in billion KRW) by Basic/Applied/Development/Others, same structure as 2a
    create_absolute_table <- function(data_table, type = "count") {
      get_value <- function(df, col_name) {
        if (col_name %in% colnames(df)) {
          val <- df[[col_name]]
          val <- as.numeric(as.character(val))
          ifelse(is.na(val), 0, val)
        } else {
          0
        }
      }
      table_rows <- data.frame(
        Time = character(),
        Basic_research = numeric(),
        Applied_research = numeric(),
        Development_research = numeric(),
        Others = numeric(),
        stringsAsFactors = FALSE
      )
      # 1) Overall (2006-2024)
      overall_basic   <- sum(get_value(data_table, "기초연구"), na.rm = TRUE)
      overall_applied <- sum(get_value(data_table, "응용연구"), na.rm = TRUE)
      overall_dev     <- sum(get_value(data_table, "개발연구"), na.rm = TRUE)
      overall_others  <- sum(get_value(data_table, "기타"), na.rm = TRUE)
      if (type == "budget") {
        overall_basic   <- round(overall_basic / 1e9, 2)
        overall_applied <- round(overall_applied / 1e9, 2)
        overall_dev     <- round(overall_dev / 1e9, 2)
        overall_others  <- round(overall_others / 1e9, 2)
      }
      table_rows <- rbind(table_rows, data.frame(
        Time = "2006-2024",
        Basic_research = overall_basic,
        Applied_research = overall_applied,
        Development_research = overall_dev,
        Others = overall_others,
        stringsAsFactors = FALSE
      ))
      # 2) Epochs
      epochs <- list(c(2006, 2010), c(2011, 2015), c(2016, 2020), c(2021, 2024))
      for (epoch in epochs) {
        epoch_data <- data_table[data_table$Year >= epoch[1] & data_table$Year <= epoch[2], ]
        b   <- sum(get_value(epoch_data, "기초연구"), na.rm = TRUE)
        a   <- sum(get_value(epoch_data, "응용연구"), na.rm = TRUE)
        d   <- sum(get_value(epoch_data, "개발연구"), na.rm = TRUE)
        o   <- sum(get_value(epoch_data, "기타"), na.rm = TRUE)
        if (type == "budget") { b <- round(b / 1e9, 2); a <- round(a / 1e9, 2); d <- round(d / 1e9, 2); o <- round(o / 1e9, 2) }
        table_rows <- rbind(table_rows, data.frame(
          Time = paste0(epoch[1], "-", epoch[2]),
          Basic_research = b, Applied_research = a, Development_research = d, Others = o,
          stringsAsFactors = FALSE
        ))
      }
      # 3) Yearly
      for (year in 2006:2024) {
        year_data <- data_table[data_table$Year == year, ]
        if (nrow(year_data) > 0) {
          b <- get_value(year_data, "기초연구")[1]
          a <- get_value(year_data, "응용연구")[1]
          d <- get_value(year_data, "개발연구")[1]
          o <- get_value(year_data, "기타")[1]
          if (type == "budget") { b <- round(b / 1e9, 2); a <- round(a / 1e9, 2); d <- round(d / 1e9, 2); o <- round(o / 1e9, 2) }
          table_rows <- rbind(table_rows, data.frame(
            Time = as.character(year),
            Basic_research = b, Applied_research = a, Development_research = d, Others = o,
            stringsAsFactors = FALSE
          ))
        }
      }
      return(table_rows)
    }

    # Create Table S2a: proportion tables; S2b: publication format (count + funding per stage)
    table2_projects_prop <- create_proportion_table(table6_projects, "projects")
    table2_budget_prop <- create_proportion_table(table5_money, "budget")
    table2b_count  <- create_absolute_table(table6_projects, "count")
    table2b_budget <- create_absolute_table(table5_money, "budget")
    table2b_pub <- data.frame(
      Time = table2b_count$Time,
      Basic_research_count    = as.integer(table2b_count$Basic_research),
      Basic_research_funding = table2b_budget$Basic_research,
      Applied_research_count    = as.integer(table2b_count$Applied_research),
      Applied_research_funding = table2b_budget$Applied_research,
      Development_research_count    = as.integer(table2b_count$Development_research),
      Development_research_funding = table2b_budget$Development_research,
      Others_count   = as.integer(table2b_count$Others),
      Others_funding = table2b_budget$Others,
      stringsAsFactors = FALSE
    )
    # Insert section header rows (Overall, Epoch, Annually) for publication format
    table2_projects_prop <- insert_section_headers(table2_projects_prop)
    table2_budget_prop   <- insert_section_headers(table2_budget_prop)
    table2b_pub          <- insert_section_headers(table2b_pub)
    # Round all numeric columns to 1 decimal place for display
    for (col in c("Basic_research", "Applied_research", "Development_research", "Others")) {
      table2_projects_prop[[col]] <- round(table2_projects_prop[[col]], 1)
      table2_budget_prop[[col]]   <- round(table2_budget_prop[[col]], 1)
    }
    for (col in c("Basic_research_count", "Basic_research_funding", "Applied_research_count", "Applied_research_funding",
                  "Development_research_count", "Development_research_funding", "Others_count", "Others_funding")) {
      table2b_pub[[col]] <- round(table2b_pub[[col]], 1)
    }
    cat("Table S2b: Publication format (count + funding per stage) –", nrow(table2b_pub), "rows.\n")

    # Save Table S2 as Excel: TableS2a (two sheets) + TableS2b (one sheet, publication format)
    table2_xlsx <- file.path(output_dir_tables, paste0("tableS2_research_stage_portfolio_", timestamp, ".xlsx"))
    wb_table2 <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb_table2, "TableS2a_Project_Prop")
    openxlsx::addWorksheet(wb_table2, "TableS2a_Funding_Prop")
    openxlsx::writeData(wb_table2, "TableS2a_Project_Prop", table2_projects_prop)
    openxlsx::writeData(wb_table2, "TableS2a_Funding_Prop", table2_budget_prop)
    openxlsx::addWorksheet(wb_table2, "TableS2b_Publication")
    openxlsx::writeData(wb_table2, "TableS2b_Publication", table2b_pub)
    openxlsx::saveWorkbook(wb_table2, table2_xlsx, overwrite = TRUE)
    cat("Table S2 (TableS2a + TableS2b publication) saved to:", table2_xlsx, "\n")

    # =============================================================================
    # CREATE FIGURE 2: RESEARCH-STAGE PORTFOLIO OF KOREA'S CANCER R&D (2006-2024)
    # =============================================================================

    cat("\n=== CREATING FIGURE 2: RESEARCH-STAGE PORTFOLIO OF KOREA'S CANCER R&D (2006-2024) ===\n")

    # Mapping R&D stage labels
    rd_labels_map <- c(
      "기초연구" = "Basic Research",
      "응용연구" = "Applied Research",
      "개발연구" = "Development Research",
      "기타"     = "Others"
    )

    # Overlay data: absolute total count & cancer funding (scaled 0-100 for trend only, no legend)
    overlay_stats <- NULL
    if (exists("yearly_total_stats") &&
        all(c("Year", "total_projects", "total_cancer_money") %in% colnames(yearly_total_stats))) {
      overlay_stats <- as.data.frame(yearly_total_stats)
    } else if (length(yearly_stats_files) > 0) {
      load(yearly_stats_files[order(file.info(yearly_stats_files)$mtime, decreasing = TRUE)][1])
      if (exists("yearly_total_stats") &&
          all(c("Year", "total_projects", "total_cancer_money") %in% colnames(yearly_total_stats))) {
        overlay_stats <- as.data.frame(yearly_total_stats)
      }
    }
    if (!is.null(overlay_stats)) {
      overlay_stats$Year <- as.numeric(as.character(overlay_stats$Year))
      overlay_stats <- overlay_stats[!is.na(overlay_stats$Year) & overlay_stats$Year >= 2006 & overlay_stats$Year <= 2024, ]
    }

    # --------------------------
    # Figure 2: 2x2 layout
    # (A) Absolute project count per research stage
    # (B) Portfolio of project count (% by year)
    # (C) Absolute funding per research stage
    # (D) Portfolio of funding (% by year)
    # Colors unchanged; stack order reversed (position_stack(reverse = TRUE))
    # --------------------------
    rd_types <- c("기초연구", "응용연구", "개발연구", "기타")
    # Use the same blue/purple palette as Figure 1, extended to 4 colors
    fill_colors <- c(
      "Basic Research" = "#2484B8",   # same blue as Figure 1 (Count)
      "Applied Research" = "#FFB84D", # yellow/orange reference
      "Development Research" = "#C03B8E", # same purple as Figure 1 (Proportion)
      "Others" = "#B87A4A"           # brown tuned to match the set
    )
    rd_levels <- c("Basic Research", "Applied Research", "Development Research", "Others")

    # Helper to extract legend grob from a ggplot
    g_legend <- function(a.gplot) {
      gt <- ggplotGrob(a.gplot)
      gtable::gtable_filter(gt, "guide-box")
    }

    for (rt in rd_types) {
      if (!rt %in% names(table6_projects)) table6_projects[[rt]] <- 0
      else table6_projects[[rt]] <- as.numeric(as.character(table6_projects[[rt]]))
      table6_projects[[rt]][is.na(table6_projects[[rt]])] <- 0
    }
    table6_projects <- table6_projects %>%
      tidyr::complete(Year = 2006:2024, fill = as.list(stats::setNames(rep(0, length(rd_types)), rd_types)))
    proj_long <- table6_projects %>%
      dplyr::rename(Year = Year) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(rd_types),
        names_to = "RD_Type",
        values_to = "Count"
      ) %>%
      dplyr::mutate(
        Count = dplyr::if_else(is.na(Count), 0, as.numeric(Count))
      ) %>%
      dplyr::group_by(Year) %>%
      dplyr::mutate(
        Year_Total = sum(Count, na.rm = TRUE),
        Prop = dplyr::if_else(Year_Total > 0, Count / Year_Total * 100, 0)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        RD_Type_Eng = ifelse(RD_Type %in% names(rd_labels_map),
                             rd_labels_map[RD_Type],
                             as.character(RD_Type)),
        RD_Type_Eng = factor(RD_Type_Eng, levels = rd_levels)
      )

    for (rt in rd_types) {
      if (!rt %in% names(table5_money)) table5_money[[rt]] <- 0
      else table5_money[[rt]] <- as.numeric(as.character(table5_money[[rt]]))
      table5_money[[rt]][is.na(table5_money[[rt]])] <- 0
    }
    table5_money <- table5_money %>%
      tidyr::complete(Year = 2006:2024, fill = as.list(stats::setNames(rep(0, length(rd_types)), rd_types)))
    fund_long <- table5_money %>%
      dplyr::rename(Year = Year) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(rd_types),
        names_to = "RD_Type",
        values_to = "Money"
      ) %>%
      dplyr::mutate(
        Money = dplyr::if_else(is.na(Money), 0, as.numeric(Money))
      ) %>%
      dplyr::group_by(Year) %>%
      dplyr::mutate(
        Year_Total = sum(Money, na.rm = TRUE),
        Prop = dplyr::if_else(Year_Total > 0, Money / Year_Total * 100, 0)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        RD_Type_Eng = ifelse(RD_Type %in% names(rd_labels_map),
                             rd_labels_map[RD_Type],
                             as.character(RD_Type)),
        RD_Type_Eng = factor(RD_Type_Eng, levels = rd_levels),
        Money_billion = Money / 1e9
      )

    theme_fig2 <- list(
      theme_minimal(),
      theme(
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 8),
        plot.title = element_blank(),
        panel.grid.minor = element_blank(),
        plot.tag.position = c(0.08, 0.90),
        plot.tag = element_text(size = 12, face = "bold")
      )
    )

    # (A) Absolute project count per research stage
    panel2A <- ggplot(proj_long, aes(x = Year, y = Count, fill = RD_Type_Eng)) +
      geom_col(width = 0.8, position = position_stack(reverse = TRUE), color = NA) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_continuous(breaks = seq(2006, 2024, 2)) +
      scale_fill_manual(values = fill_colors) +
      theme_fig2 +
      labs(title = NULL, y = "Project Count", tag = "(A)")

    # (B) Portfolio of project count (% by year)
    panel2B <- ggplot(proj_long, aes(x = Year, y = Prop, fill = RD_Type_Eng)) +
      geom_col(width = 0.8, position = position_stack(reverse = TRUE), color = NA) +
      geom_text(
        aes(label = ifelse(
          !is.na(Prop) & (Prop >= 5 | (RD_Type_Eng == "Others" & Prop > 0)),
          sprintf("%.0f", round(Prop)),
          ""
        )),
        position = position_stack(vjust = 0.5, reverse = TRUE),
        size = 2.5,
        color = "white",
        fontface = "bold"
      ) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0), limits = c(0, 100)) +
      scale_x_continuous(breaks = seq(2006, 2024, 2)) +
      scale_fill_manual(values = fill_colors) +
      theme_fig2 +
      labs(title = NULL, y = "Project Count (%)", tag = "(B)")

    # (C) Absolute funding per research stage (Billion KRW)
    panel2C <- ggplot(fund_long, aes(x = Year, y = Money_billion, fill = RD_Type_Eng)) +
      geom_col(width = 0.8, position = position_stack(reverse = TRUE), color = NA) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_continuous(breaks = seq(2006, 2024, 2)) +
      scale_fill_manual(values = fill_colors) +
      theme_fig2 +
      theme(axis.title.x = element_text(size = 10, face = "bold")) +
      labs(title = NULL, x = "Year", y = "Funding (Billion KRW)", tag = "(C)")

    # (D) Portfolio of funding (% by year) — legend extracted separately
    panel2D_core <- ggplot(fund_long, aes(x = Year, y = Prop, fill = RD_Type_Eng)) +
      geom_col(width = 0.8, position = position_stack(reverse = TRUE), color = NA) +
      geom_text(
        aes(label = ifelse(
          !is.na(Prop) & (Prop >= 5 | (RD_Type_Eng == "Others" & Prop > 0)),
          sprintf("%.0f", round(Prop)),
          ""
        )),
        position = position_stack(vjust = 0.5, reverse = TRUE),
        size = 2.5,
        color = "white",
        fontface = "bold"
      ) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0), limits = c(0, 100)) +
      scale_x_continuous(breaks = seq(2006, 2024, 2)) +
      scale_fill_manual(name = NULL, values = fill_colors)

    # Panel (D) without legend (for 2x2 grid)
    panel2D <- panel2D_core +
      theme_fig2 +
      theme(
        axis.title.x = element_text(size = 10, face = "bold"),
        legend.position = "none"
      ) +
      labs(title = NULL, x = "Year", y = "Funding (%)", tag = "(D)")

    # Legend extracted from a stripped-down plot
    legend_plot <- panel2D_core +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.justification = "center",
        legend.text = element_text(size = 9),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
    legend_grob <- g_legend(legend_plot)

    # Figure 2: 2x2, y-axis 0–102% for (B)/(D) so small top segments (e.g. Others) are visible
    panel2B_102 <- panel2B + scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0), limits = c(0, 102))
    panel2D_102 <- panel2D + scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0), limits = c(0, 102))
    grid_2x2 <- gridExtra::arrangeGrob(panel2A, panel2C, panel2B_102, panel2D_102, ncol = 2)
    fig2_file <- file.path(output_dir_figures,
                            paste0("figure2_research_stage_portfolio_", timestamp, ".png"))
    png(fig2_file, width = 10, height = 10, units = "in", res = 300)
    gridExtra::grid.arrange(grid_2x2, legend_grob, ncol = 1, heights = c(12, 0.8))
    dev.off()
    cat("Figure 2 (2x2, y 0–102%) saved to:", fig2_file, "\n")
  }
}

cat("\n=== 02_analysis COMPLETED ===\n")
