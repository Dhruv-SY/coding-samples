*==============================================================*
* MASTER DO-FILE
* Child Marriage, Legal Regimes, and Life Outcomes
* Dhruv Yadav and Pedro Silva
*
* This file runs all Stata scripts in sequence.
* Set the working directory to the root of this replication
* package (the folder containing master.do) before running.
*==============================================================*

*--------------------------------------------------------------*
* SET WORKING DIRECTORY
* Edit this line to point to your local copy of replication_clean/
*--------------------------------------------------------------*
* cd "C:\path\to\replication_clean"

*--------------------------------------------------------------*
* CONFIRM DATA EXISTS
*--------------------------------------------------------------*
capture confirm file "data/The_11_countries_union_migration_merged.dta"
if _rc {
    di as error "Dataset not found."
    di as error "Place The_11_countries_union_migration_merged.dta in the data/ subfolder."
    exit 1
}

*--------------------------------------------------------------*
* RUN SCRIPTS
* Each script loads data fresh, constructs outcomes, runs
* regressions, and writes LaTeX tables to results/.
*--------------------------------------------------------------*

* --- MAIN RESULTS ---
* Tables 5 & 7: IV-2SLS and reduced-form (15 outcomes, 3 bandwidths)
do "code/01_main_iv_reducedform.do"

* Table 6: First-stage coefficients and F-statistics
do "code/02_first_stage.do"

* Table 8: Heterogeneity by urban/rural residence
do "code/03_heterogeneity_urban_rural.do"

* Table 9: Exclude migrants who moved in same period as marriage
do "code/04_robustness_migration.do"

* Table 18: Kleibergen-Paap F-stats and Anderson-Rubin p-values
* (requires ivreg2 from SSC; installed automatically on first run)
do "code/05_weak_iv_diagnostics.do"

* --- ROBUSTNESS ---
* Table 13: Exclude 4 Namibia border pairs
do "code/06_robustness_no_namibia.do"

* Robustness: Exclude Niger border pair (BEN-NER)
do "code/07_robustness_no_niger.do"

* Table 14: Drop pixel-level geographic controls and ethnicity FE
do "code/08_robustness_no_pixel_controls.do"

* --- SUPPLEMENTARY ---
* RD covariate balance table
do "code/10_balance_test.do"

* RD figures
do "code/12_graphs.do"

di ""
di "============================================================"
di " All Stata scripts completed. LaTeX tables in results/"
di "============================================================"
