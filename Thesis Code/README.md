# Replication Package

## Child Marriage, Legal Regimes, and Life Outcomes

**Pedro Silva - Dhruv Yadav**

---

## Paper

This package contains the analysis code for the working paper *Child Marriage,
Legal Regimes, and Life Outcomes*. The paper estimates the causal effect of
child marriage on women's education, fertility, and labor market outcomes,
using variation in minimum legal marriage ages across international borders in
Sub-Saharan Africa.

---

## Empirical Strategy

The identification strategy is a **fuzzy border regression discontinuity (RD)
design**. Women living near the same international border are compared across
11 border pairs where the minimum legal marriage age differs between countries.
Child marriage is instrumented by residing on the strict-law side of the border.

Controls included in all specifications:

- Border-pair fixed effects with pair-specific RD slopes and treated-side kinks
- Ethnicity fixed effects (Murdock 1959 map)
- Survey-year fixed effects
- Pixel-level geographic controls: nighttime lights (VIIRS), population density
  (PRIO-GRID), and agricultural suitability

**11 border pairs:** AGO-NAM, BEN-BFA, BEN-NER, BFA-CIV, BFA-GHA, BDI-TZA,
NAM-ZAF, NAM-ZMB, NAM-ZWE, RWA-TZA, TZA-UGA

**Core specification (Stata):**

```stata
global RDBP "i.bp c.rd#i.bp c.rd#i.bp#i.treated"
global FE   "i.survey_year nightlights_mean population_density_mean agri_suitability_mean i.eth_id"
global X    "c.v012 i.v025"

ivregress 2sls Y (child_marriage = treated) $RDBP $X $FE, cluster(v021)
```

Standard errors are clustered by DHS primary sampling unit (`v021`). Table 11
also reports Conley (1999) spatial HAC SEs at 50, 100, and 200 km cutoffs and
a Lehner data-driven spatial cutoff.

---

## Requirements

**Stata** version 15 or later:

- `ivreg2` is installed automatically from SSC on the first run of
  `05_weak_iv_diagnostics.do`

**R** version 4.0 or later:

```r
install.packages(c("haven", "dplyr", "fixest", "readr", "stringr",
                   "purrr", "tibble"))
```

`fixest` version 0.10.0 or later is required for formula-based Conley spatial
standard errors.

---

## Data

The analysis uses DHS (Demographic and Health Surveys) microdata with
GPS-located clusters, merged with pixel-level geographic variables from VIIRS,
PRIO-GRID, and FAO datasets. The restricted DHS-derived analysis dataset is not
distributed with this package.

To run the scripts, place the licensed dataset in `data/` with this exact
filename:

```text
The_11_countries_union_migration_merged.dta
```

---

## Quickstart

Open Stata or R, set the working directory to the `replication_clean/` folder,
and run the relevant master script.

**Stata** produces all main and robustness tables: Tables 5-9, 13-14, and 18.

```stata
cd "path/to/replication_clean"
do master.do
```

**R** produces the Conley SE tables.

```r
setwd("path/to/replication_clean")
source("master.R")
```

All outputs are written to `results/`.

---

## Folder Structure

```text
replication_clean/
|-- master.do
|-- master.R
|-- code/
|   |-- 00_setup.do
|   |-- 01_main_iv_reducedform.do
|   |-- 02_first_stage.do
|   |-- 03_heterogeneity_urban_rural.do
|   |-- 04_robustness_migration.do
|   |-- 05_weak_iv_diagnostics.do
|   |-- 06_robustness_no_namibia.do
|   |-- 07_robustness_no_niger.do
|   |-- 08_robustness_no_pixel_controls.do
|   |-- 09_conley_se_robustness.R
|   |-- 10_balance_test.do
|   |-- 11_lehner_conley.R
|   `-- 12_graphs.do
|-- data/
|   |-- README.md
|   `-- .gitignore
`-- results/
    `-- generated tables and CSV files
```

`00_setup.do` handles data loading, sample restrictions, variable cleaning, and
outcome construction. Every Stata script begins with `do "code/00_setup.do"`
and then applies its own filters or global overrides before running regressions.

---

## Script-to-Table Map

| Script | Table | Description |
|--------|-------|-------------|
| `01_main_iv_reducedform.do` | 5, 7 | Main IV-2SLS and reduced-form estimates; 15 outcomes x 3 bandwidths |
| `02_first_stage.do` | 6 | First-stage coefficients and F-statistics, matched to each outcome sample |
| `03_heterogeneity_urban_rural.do` | 8 | Heterogeneity by urban vs. rural residence |
| `04_robustness_migration.do` | 9 | Exclude women who migrated in the same period as their marriage |
| `05_weak_iv_diagnostics.do` | 18 | Kleibergen-Paap F-statistics and Anderson-Rubin p-values |
| `06_robustness_no_namibia.do` | 13 | Exclude the 4 Namibia border pairs |
| `07_robustness_no_niger.do` | — | Exclude the Benin-Niger (BEN-NER) border pair |
| `08_robustness_no_pixel_controls.do` | 14 | Drop pixel-level geographic controls and ethnicity fixed effects |
| `09_conley_se_robustness.R` | 11 | Conley spatial HAC SEs at 50, 100, and 200 km cutoffs |
| `10_balance_test.do` | — | RD covariate balance test |
| `11_lehner_conley.R` | — | Conley 50 km table and Lehner data-driven spatial cutoff |
| `12_graphs.do` | — | RD plots for all 15 outcomes |

---

## Outputs

All files are written to `results/`.

| Output file | Description |
|-------------|-------------|
| `results_withedumargins_iv.tex` | Table 5 — IV-2SLS main results |
| `results_withedumargins_reducedform.tex` | Table 7 — Reduced-form main results |
| `results_firststage_iv.tex` | Table 6 — First-stage results |
| `results_urban_iv.tex`, `results_urban_reducedform.tex` | Table 8 — Urban heterogeneity |
| `results_rural_iv.tex`, `results_rural_reducedform.tex` | Table 8 — Rural heterogeneity |
| `results_migration_iv.tex`, `results_migration_reducedform.tex` | Table 9 — Migration robustness |
| `conley_reduced_form_results.tex` | Table 11 — Conley SE robustness (50/100/200 km) |
| `results_nonamibia_iv.tex`, `results_nonamibia_reducedform.tex` | Table 13 — No Namibia |
| `results_noniger_iv.tex`, `results_noniger_reducedform.tex` | No Niger robustness |
| `results_nopixel_iv.tex`, `results_nopixel_reducedform.tex` | Table 14 — No pixel controls |
| `results_weakiv_all_outcomes.tex` | Table 18 — Weak-IV diagnostics |
| `balance_table.tex` | RD covariate balance |
| `conley_50_table.tex`, `conley_lehner_table.tex` | Conley 50 km and Lehner cutoff tables |
| `*.png` (16 files) | RD plots for child marriage and all 15 outcomes |

---
