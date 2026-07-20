##################################################################
#####  cancer_lexicon.R  --  read the cancer lexicon         #####
##################################################################
# Reads cancer_lexicon.csv, which the Python census tool reads too.
#
#   source(file.path(LEXICON_DIR, "cancer_lexicon.R"))
#   kw <- get_cancer_keywords()

get_cancer_keywords <- function(path = NULL) {
  if (is.null(path)) {
    path <- if (exists("LEXICON_DIR")) {
      file.path(LEXICON_DIR, "cancer_lexicon.csv")
    } else {
      "cancer_lexicon.csv"
    }
  }
  if (!file.exists(path)) stop("cancer lexicon not found: ", path)
  lx <- utils::read.csv(path, encoding = "UTF-8", colClasses = "character",
                        stringsAsFactors = FALSE)
  if (!"pattern" %in% names(lx)) stop("cancer lexicon is missing a 'pattern' column: ", path)
  lx$pattern
}

# Single alternation regex used to test a text field. Matching is
# Single alternation over all patterns, applied with useBytes = TRUE.
cancer_regex <- function(...) paste(get_cancer_keywords(...), collapse = "|")
