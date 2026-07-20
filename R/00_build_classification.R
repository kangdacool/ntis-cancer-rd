# =============================================================================
# Cancer classification of the NTIS project census.
#
# Applies the bilingual regular-expression lexicon to four text fields of every
# project-year record (Korean and English titles, Korean and English keywords)
# and counts how many of the four match. Records are de-duplicated by
# (project number, year). Because the OpenAPI issues a year-specific identifier
# rather than a stable project key, a project's records are linked across years
# by title, which is what lets the downstream project-level rule compute a
# maximum and mean matched-field count over all years a project was active.
#
# Input :  NTIS OpenAPI census CSV  (CENSUS_CSV; see tools/ntis_api_census.py)
# Output:  NTIS_cancer_classification_2006-2024_*.RData / .csv
# =============================================================================

if (!exists("PROJ_ROOT")) source("config.R")

suppressMessages(library(data.table))

# ---- cancer lexicon (lexicon/cancer_lexicon.csv) ----
source(file.path(LEXICON_DIR, "cancer_lexicon.R"))
# One alternation over all patterns; a field counts as a hit if any pattern matches.
BIG <- paste(get_cancer_keywords(), collapse = "|")
hit_field <- function(x) { x[is.na(x) | nchar(x) < 3] <- ""; as.integer(grepl(BIG, x, useBytes = TRUE)) }

# ---- load API census, map to pipeline columns, dedup by project-year ----
CENSUS <- CENSUS_CSV
OUT    <- OUT_TABLES
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

d <- fread(CENSUS, encoding = "UTF-8", colClasses = "character")
d[, Year := as.numeric(Year)]
d <- d[!is.na(Year) & Year >= 2006 & Year <= 2024 & !is.na(ProjectNumber) & ProjectNumber != ""]
n0 <- nrow(d)
d <- d[!duplicated(d[, .(ProjectNumber, Year)])]
cat(sprintf("API census rows %d -> unique project-years %d\n", n0, nrow(d)))

data <- data.table(
  Num1 = d$ProjectNumber, Year = d$Year,
  Inst_pay = d$Ministry, Inst_manage = d$ManageAgency,
  Text1 = d$Title_KR, Text2 = d$Title_EN, Key1 = d$Keyword_KR, Key2 = d$Keyword_EN,
  Class_I = d$DevStage, MoneyC = as.numeric(d$GovFunds)
)
data[, filenum := 1L]
data[, NO := as.character(.I)]
# The OpenAPI issues a year-specific identifier, so a project's records carry no shared key across
# years. Title is the available cross-year linker: grouping by it lets step 01 compute the maximum and
# mean matched-field count "across all years a project was active", as the Methods specify, rather
# than per individual record.
data[, .pk := ifelse(is.na(Text1) | Text1 == "", paste0("__u", .I), Text1)]
data[, NO_base := as.character(.GRP), by = .pk]
data[, .pk := NULL]
data[, Class_D := NA_character_]  # 6T technology classification: not exposed by the API and not used in the paper

for (col in c("Text1","Text2","Key1","Key2"))
  data[[paste0(col, "_cancer_hit")]] <- hit_field(data[[col]])
hit_cols <- paste0(c("Text1","Text2","Key1","Key2"), "_cancer_hit")
data[, cancer_total_hits := rowSums(as.matrix(data[, ..hit_cols]))]
data[, cancer_related := cancer_total_hits > 0]

cat(sprintf("unique project-years=%d  cancer_related=%d\n", nrow(data), sum(data$cancer_related)))
cat("--- cancer_related by year (sanity vs python: 2019~3935, 2023~3756, 2024~3389) ---\n")
print(data[cancer_related == TRUE, .N, by = Year][order(Year)])

save(data, file = file.path(OUT, "NTIS_cancer_classification_2006-2024_api260717.RData"))
fwrite(data, file.path(OUT, "NTIS_cancer_classification_2006-2024_api260717.csv"))
cat("saved API classification to", OUT, "\n")
