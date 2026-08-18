# ntis-cancer-rd

Analysis code for:

> Seo K, Lee S-Y, Yu S, Kim T, Han K-T, Choi KS, Shin A.
> **Cancer Research and Development in South Korea, 2006–2024: A Text-Mining Analysis of
> National Science & Technology Information Service (NTIS) Records.**
> *Cancer Research and Treatment* (under revision).

The study identifies government-funded cancer R&D projects in the Korean national
research registry (NTIS) using a bilingual regular-expression lexicon, and describes
how their number, funding, research stage, keyword composition, and cancer-site
distribution changed between 2006 and 2024.

Every table and figure in the paper is produced from one input, the NTIS OpenAPI
project census, by the scripts in `R/`.

## Quick start

Install the R packages listed in [DEPENDENCIES.md](DEPENDENCIES.md). The Python
tools require only the standard library.

Register at [ntis.go.kr](https://www.ntis.go.kr) for a 통합인증키 and export it.
Keys are restricted to the IP range of the registering institution.

```bash
export NTIS_API_KEY=...            # bash
$env:NTIS_API_KEY = '...'          # PowerShell
```

Build the census, then run the analysis:

```bash
python tools/ntis_api_census.py    # several hours, ~550 MB -> data/raw/
Rscript R/run_all.R                # 40-60 minutes -> output/
```

To use a census stored elsewhere, set `NTIS_CENSUS_CSV` and skip the first command.

## Display items

| Manuscript item | Script | Output |
|---|---|---|
| Classification C0 / C1 / C2 | `R/00_build_classification.R` | `NTIS_cancer_classification_*.RData` |
| Figure 1, Table S1 (temporal trends) | `R/01_filter.R` | `figure1_*.png`, `tableS1_*.xlsx` |
| Figure 2, Table S2 (research stage) | `R/01_filter.R`, `R/02_analysis.R` | `figure2_*.png`, `tableS2_*.xlsx` |
| Figure 3, Table S3 (keyword composition) | `R/03_keyword_flow.R` | `figure3_*.png`, `tableS3_*.xlsx` |
| Site-specific series (feeds Figure 4) | `R/04_cancer_types.R` | `ntis6_cancer_types_*.csv` |
| Figure 4 (R&D vs incidence and mortality) | `R/05_compare_epi.R` | `figure4_*.png` |
| Table 1 (ministry allocation) | `R/06_inst_pay.R` | `table1_inst_pay_*.xlsx` |
| Table S4 (C2-only sensitivity) | `R/07_sensitivity_C2only.R` | `tableS4_*.xlsx` |
| Supplementary Figure S1 | `R/08_figS1.R` | `figureS1_*.png` |
| Cancer lexicon (Supplementary Materials) | — | `lexicon/cancer_lexicon.csv` |

## The cancer lexicon

`lexicon/cancer_lexicon.csv` holds 204 regular-expression patterns (79 English,
125 Korean), each with its language, category, and, where the design is not
self-evident, a note recording what it excludes.

Several Korean patterns use character-class exclusions because the standalone
token 암 (cancer) is a frequent substring of unrelated words: 암반 (bedrock),
암호 (password), 암시 (hint), 암모니아 (ammonia). For example:

- `,암,` is comma-anchored, matching only a whole entry in the comma-delimited
  keyword fields rather than any occurrence of the character.
- `[^픽] 암,` excludes a preceding 픽, so tokens such as 픽업 do not match.
- `종양[^괴]` matches 종양 (tumour) but not compounds beginning 종양괴.

Project-level classification uses two statistics computed across all years a
project was active: C0 is no match in any year, C2 is a maximum matched-field
count above zero with a mean of at least 1, and C1 is a maximum above zero with
a mean below 1. Cancer-related projects are C1 and C2 combined.

## Layout

```
R/            analysis pipeline, run in numeric order by run_all.R
lexicon/      cancer lexicon and Korean-English keyword translations
tools/        NTIS OpenAPI clients
data/epi/     national cancer incidence and mortality statistics, for Figure 4
data/raw/     destination for the census
output/       tables and figures
```

## Limits on reproduction

A census pulled today will not be identical to ours. NTIS is a live registry and
adds records retroactively for years already past, so a fresh pull will contain
more 2023 and 2024 projects than the 2026-07-13 pull analysed here; counts for
2006 to 2022 should agree closely. The lag is large in the most recent years: in
our own check, a snapshot taken six months earlier held 23.2% fewer 2023 projects
and 40.9% fewer 2024 projects than the census used here. That comparison is a note
on data currency for anyone reproducing this work, not a result reported in the
paper, which treats 2024 as provisional.

Our API key is bound to the registering institution's IP range and cannot be
shared, so you will need your own.

The physician validation labels are not included. The stratified samples the two
physicians reviewed contain project-level records that cannot be redistributed;
the sampling code is here, the adjudicated labels are not. Manuscript assembly
also lives elsewhere.

## Data sources

NTIS (National Science & Technology Information Service), Republic of Korea,
retrieved through the public OpenAPI.

`data/epi/` holds national cancer incidence (1999–2022) and mortality
(1999–2024) statistics published by the Korea Central Cancer Registry and
Statistics Korea.

## License and citation

Released under the [MIT License](LICENSE). Archived at Zenodo:
[10.5281/zenodo.21451721](https://doi.org/10.5281/zenodo.21451721). If you use
this code, please cite the article; see [CITATION.cff](CITATION.cff).
