****************************************************
* 10_balance_test.do
* Balance table for RD covariates
* Population density shown as people per square km
****************************************************

clear all
set more off

capture mkdir "results"

use "data/The_11_countries_union_migration_merged.dta", clear

*---------------------------------------------------*
* 1) BASIC SAMPLE RESTRICTIONS + IDS
*---------------------------------------------------*

* Match main analysis sample restrictions
drop if v012 < 18 & v511 == .

capture drop bp eth_id
encode country_pair, gen(bp)
encode eth_fe, gen(eth_id)

drop if missing(child_marriage, treated, rd, bp, survey_year, v021)

*---------------------------------------------------*
* 2) RD SPECIFICATION
*---------------------------------------------------*

global RDBP "i.bp c.rd#i.bp c.rd#i.bp#i.treated"

* Balance sample: 100 km bandwidth
capture drop balance_sample
gen byte balance_sample = abs(rd) <= 100

*---------------------------------------------------*
* 3) CLEAN / CONSTRUCT BALANCE VARIABLES
*---------------------------------------------------*

capture drop urban pop_density_km2

* Urban/rural
gen byte urban = .
replace urban = 1 if v025 == 1
replace urban = 0 if v025 == 2
label var urban "Urban"

* Convert 12.5 km x 12.5 km pixel population to people per km^2
* Pixel area = 12.5 * 12.5 = 156.25 square km
gen pop_density_km2 = population_density_mean / 156.25
label var pop_density_km2 "Population density (people/km²)"

* Labels
label var v012 "Age"
label var nightlights_mean "Pixel-level nightlights"
label var agri_suitability_mean "Pixel-level agricultural suitability"

*---------------------------------------------------*
* 4) DIAGNOSTIC CHECK
*---------------------------------------------------*

di "=================================================="
di "Population density diagnostics"
di "=================================================="

sum population_density_mean pop_density_km2, detail

tabstat population_density_mean pop_density_km2, by(treated) ///
    stat(n mean p50 sd p90 p95 p99 max)

*---------------------------------------------------*
* 5) VARIABLES TO TEST
*---------------------------------------------------*

local balvars ///
    v012 ///
    urban ///
    nightlights_mean ///
    pop_density_km2 ///
    agri_suitability_mean

*---------------------------------------------------*
* 6) STORE BALANCE RESULTS: PANEL A
*---------------------------------------------------*

tempfile balance_results
tempname balhold

postfile `balhold' str60 varname ///
    double mean_c mean_t b se p N ///
    using `balance_results', replace

foreach y of local balvars {

    preserve

        keep if balance_sample == 1
        keep if !missing(`y', treated, rd, bp, v021)

        quietly summarize `y' if treated == 0
        local mean_c = r(mean)

        quietly summarize `y' if treated == 1
        local mean_t = r(mean)

        capture quietly reg `y' treated $RDBP, vce(cluster v021)

        if _rc == 0 {
            local b  = _b[treated]
            local se = _se[treated]
            local t  = `b' / `se'
            local p  = 2 * ttail(e(df_r), abs(`t'))
            local N  = e(N)

            post `balhold' ///
                ("`y'") ///
                (`mean_c') ///
                (`mean_t') ///
                (`b') ///
                (`se') ///
                (`p') ///
                (`N')
        }
        else {
            di as error "Balance regression failed for `y', rc=" _rc
        }

    restore
}

postclose `balhold'

*---------------------------------------------------*
* 7) STORE JOINT TESTS: PANEL B
*---------------------------------------------------*

tempfile joint_results
tempname jhold

postfile `jhold' str60 testname double p N using `joint_results', replace

preserve

    keep if balance_sample == 1

    capture quietly reg treated i.survey_year $RDBP, vce(cluster v021)
    if _rc == 0 {
        capture quietly testparm i.survey_year
        if _rc == 0 {
            local p_sy = r(p)
            local N_sy = e(N)
            post `jhold' ("Survey-year fixed effects") (`p_sy') (`N_sy')
        }
    }

    capture quietly reg treated i.eth_id $RDBP, vce(cluster v021)
    if _rc == 0 {
        capture quietly testparm i.eth_id
        if _rc == 0 {
            local p_eth = r(p)
            local N_eth = e(N)
            post `jhold' ("Ethnicity fixed effects") (`p_eth') (`N_eth')
        }
    }

restore

postclose `jhold'

*---------------------------------------------------*
* 8) FORMAT PANEL A
*---------------------------------------------------*

use `balance_results', clear

replace varname = "Age"                                      if varname == "v012"
replace varname = "Urban"                                    if varname == "urban"
replace varname = "Pixel-level nightlights"                  if varname == "nightlights_mean"
replace varname = "Population density (people/km$^2$)"       if varname == "pop_density_km2"
replace varname = "Pixel-level agricultural suitability"     if varname == "agri_suitability_mean"

gen star = cond(p < .01, "***", cond(p < .05, "**", cond(p < .1, "*", "")))

gen mean_c_s = string(mean_c, "%9.3f")
gen mean_t_s = string(mean_t, "%9.3f")
gen coef_s   = string(b, "%9.3f") + star
gen se_s     = "(" + string(se, "%9.3f") + ")"
gen N_s      = string(N, "%9.0f")

tempfile panelA
save `panelA', replace

*---------------------------------------------------*
* 9) FORMAT PANEL B
*---------------------------------------------------*

use `joint_results', clear

gen p_s = string(p, "%9.3f")
gen N_s = string(N, "%9.0f")

tempfile panelB
save `panelB', replace

*---------------------------------------------------*
* 10) EXPORT LATEX BALANCE TABLE
*---------------------------------------------------*

use `panelA', clear

file open tex using "results/balance_table.tex", write replace

file write tex "\begin{table}[htbp]" _n
file write tex "\centering" _n
file write tex "\caption{Balance of Covariates at the RD Cutoff}" _n
file write tex "\label{tab:balance}" _n
file write tex "\small" _n
file write tex "\begin{tabular}{lccccc}" _n
file write tex "\hline\hline" _n
file write tex "Variable & Control mean & Treated mean & RD difference & S.E. & Obs. \\" _n
file write tex "\hline" _n
file write tex "\multicolumn{6}{l}{\textit{Panel A: Covariates}} \\" _n

count
local N = r(N)

forvalues i = 1/`N' {
    local v   = varname[`i']
    local mc  = mean_c_s[`i']
    local mt  = mean_t_s[`i']
    local cf  = coef_s[`i']
    local se  = se_s[`i']
    local nn  = N_s[`i']

    file write tex "`v'" " & " "`mc'" " & " "`mt'" " & " "`cf'" " & " "`se'" " & " "`nn'" " \\" _n
}

file write tex "\hline" _n
file write tex "\multicolumn{6}{l}{\textit{Panel B: Joint tests for fixed effects}} \\" _n

use `panelB', clear
count
local N2 = r(N)

forvalues i = 1/`N2' {
    local v  = testname[`i']
    local pp = p_s[`i']
    local nn = N_s[`i']

    file write tex "`v'" " & \multicolumn{2}{c}{} & \multicolumn{2}{c}{p-value: `pp'} & `nn' \\" _n
}

file write tex "\hline" _n
file write tex "\multicolumn{6}{p{0.92\linewidth}}{\footnotesize Notes: Panel A reports control-group means, treated-group means, and the RD difference from a regression of each covariate on treatment, including border-pair fixed effects, border-pair-specific slopes, and treated-side kinks in distance to the border. Standard errors are clustered at the DHS cluster level. Population density is measured using 12.5 km $\times$ 12.5 km pixels and converted to people per km$^2$ by dividing the pixel population by 156.25. Panel B reports joint tests for survey-year and ethnicity fixed effects. Stars indicate significance: * \$p<0.10\$, ** \$p<0.05\$, *** \$p<0.01\$. All estimates use observations within 100 km of the cutoff.}" _n
file write tex "\end{tabular}" _n
file write tex "\end{table}" _n

file close tex

