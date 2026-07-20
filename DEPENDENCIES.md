# Dependencies

## R

Developed and run on **R 4.4.1**. Any R ≥ 4.2 should work.

```r
install.packages(c(
  "data.table", "dplyr", "tidyr", "stringr",       # data manipulation
  "readxl", "openxlsx", "writexl",                 # Excel I/O
  "ggplot2", "patchwork", "cowplot", "gridExtra",  # figures
  "viridis", "segmented"                           # colour scales; breakpoint regression
))
```

`grid` and `utils` ship with R. Run the line above once before `R/run_all.R`;
the scripts do not install packages themselves.

`segmented` is used only by `R/05_compare_epi.R`, for the piecewise-regression
breakpoint summary accompanying Figure 4.

## Python

Standard library only, so no `pip install` is required. Developed on Python 3.11;
any Python 3.9 or later should work.

`tools/ntis_api_census.py` and `tools/ntis_api_pull.py` use `csv`, `re`, `os`,
`sys`, `time`, `argparse`, `urllib`, and `xml.etree.ElementTree`.

## Resources

| | |
|---|---|
| Census download | several hours, ~550 MB |
| `R/run_all.R` | roughly 40–60 minutes |
| Peak memory | ~4 GB (step `01_filter.R`) |
| Disk | ~2 GB for intermediates in `output/tables/` |
