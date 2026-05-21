****************************************************
* 05_weak_iv_diagnostics.do
* Weak-IV diagnostics: Kleibergen-Paap F-stat and Anderson-Rubin p-value
****************************************************

do "code/00_setup.do"

*---------------------------------------------------*
* 5) OUTCOMES + BANDWIDTHS
*---------------------------------------------------*
local bandwidths "50 100 200"
local outcomes  "v133 v106 v212 v201 v218 v525 v822 multiple_unions not_working domestic_work paid_cash white_collar white_collar_cond"

*---------------------------------------------------*
* 6) INSTALL ivreg2 IF NEEDED
*---------------------------------------------------*
capture which ivreg2
if _rc {
    capture ssc install ivreg2, replace
    if _rc {
        di as error "ivreg2 not found and could not be installed from SSC."
        di as error "Install manually: ssc install ivreg2"
        di as error "If running on an offline system, transfer ivreg2.pkg manually."
        exit 1
    }
}

*---------------------------------------------------*
* 7) STORE WEAK-IV DIAGNOSTICS
*---------------------------------------------------*
tempname weakhold
tempfile weakiv_all_outcomes
postfile `weakhold' str40 outcome ///
    double kp_f50 ar_p50 N50 ///
    double kp_f100 ar_p100 N100 ///
    double kp_f200 ar_p200 N200 ///
    using `weakiv_all_outcomes', replace

foreach y of local outcomes {

    di "========================================"
    di "Weak-IV diagnostics | Outcome: `y'"
    di "========================================"

    local kp_f50   .
    local ar_p50   .
    local N50      .

    local kp_f100  .
    local ar_p100  .
    local N100     .

    local kp_f200  .
    local ar_p200  .
    local N200     .

    foreach h of local bandwidths {

        preserve
            keep if abs(rd) <= `h'

            if "`y'" == "white_collar_cond" {
                keep if v714 == 1
                keep if !missing(white_collar_cond)
            }
            else {
                keep if !missing(`y')
            }

            count
            if r(N) == 0 {
                restore
                continue
            }

            local N = r(N)

            quietly ivreg2 `y' $RDBP $X $FE ///
                (child_marriage = treated), ///
                cluster(v021) robust first

            local kpf = .
            local arp = .

            capture local kpf = e(rkf)
            capture local arp = e(arfp)

            if `h' == 50 {
                local kp_f50 = `kpf'
                local ar_p50 = `arp'
                local N50    = `N'
            }
            else if `h' == 100 {
                local kp_f100 = `kpf'
                local ar_p100 = `arp'
                local N100    = `N'
            }
            else if `h' == 200 {
                local kp_f200 = `kpf'
                local ar_p200 = `arp'
                local N200    = `N'
            }

        restore
    }

    post `weakhold' ("`y'") ///
        (`kp_f50') (`ar_p50') (`N50') ///
        (`kp_f100') (`ar_p100') (`N100') ///
        (`kp_f200') (`ar_p200') (`N200')
}

postclose `weakhold'

*---------------------------------------------------*
* 8) OPEN RESULTS
*---------------------------------------------------*
use `weakiv_all_outcomes', clear

*---------------------------------------------------*
* 9) BETTER OUTCOME LABELS
*---------------------------------------------------*
replace outcome = "Education (years)"          if outcome == "v133"
replace outcome = "Education level"            if outcome == "v106"
replace outcome = "Age at first birth"         if outcome == "v212"
replace outcome = "Children ever born"         if outcome == "v201"
replace outcome = "Number of living children"  if outcome == "v218"
replace outcome = "Age at first sex"           if outcome == "v525"
replace outcome = "Condom use"                 if outcome == "v822"
replace outcome = "Multiple unions"            if outcome == "multiple_unions"
replace outcome = "Not working"                if outcome == "not_working"
replace outcome = "Domestic work"              if outcome == "domestic_work"
replace outcome = "Paid in cash"               if outcome == "paid_cash"
replace outcome = "White-collar employment"    if outcome == "white_collar"
replace outcome = "White-collar among workers" if outcome == "white_collar_cond"

*---------------------------------------------------*
* 10) FORMAT STRINGS
*---------------------------------------------------*
gen kp_f50_s   = string(kp_f50,"%9.2f")
gen kp_f100_s  = string(kp_f100,"%9.2f")
gen kp_f200_s  = string(kp_f200,"%9.2f")

gen ar_p50_s   = string(ar_p50,"%9.4f")
gen ar_p100_s  = string(ar_p100,"%9.4f")
gen ar_p200_s  = string(ar_p200,"%9.4f")

gen N50_s      = string(N50,"%9.0f")
gen N100_s     = string(N100,"%9.0f")
gen N200_s     = string(N200,"%9.0f")

*---------------------------------------------------*
* 11) EXPORT LATEX TABLE
*---------------------------------------------------*
file open texweak using "results/results_weakiv_all_outcomes.tex", write replace

file write texweak "\begin{table}[htbp]" _n
file write texweak "\centering" _n
file write texweak "\caption{Weak-IV Diagnostics Across Outcomes}" _n
file write texweak "\label{tab:weakiv_all}" _n
file write texweak "\begin{tabular}{lccc}" _n
file write texweak "\hline\hline" _n
file write texweak " & \multicolumn{3}{c}{Bandwidth (km)} \\" _n
file write texweak "\cline{2-4}" _n
file write texweak "Outcome & 50 & 100 & 200 \\" _n
file write texweak "\hline" _n

quietly count
local Nrows = r(N)

forvalues i = 1/`Nrows' {
    local out  = outcome[`i']

    local k50  = kp_f50_s[`i']
    local k100 = kp_f100_s[`i']
    local k200 = kp_f200_s[`i']

    local p50  = ar_p50_s[`i']
    local p100 = ar_p100_s[`i']
    local p200 = ar_p200_s[`i']

    local n50  = N50_s[`i']
    local n100 = N100_s[`i']
    local n200 = N200_s[`i']

    file write texweak "`out'" " & " "`k50'" " & " "`k100'" " & " "`k200'" " \\" _n
    file write texweak "AR p-value" " & " "`p50'" " & " "`p100'" " & " "`p200'" " \\" _n
    file write texweak "Obs." " & " "`n50'" " & " "`n100'" " & " "`n200'" " \\" _n
    file write texweak "\addlinespace" _n
}

file write texweak "\hline" _n
file write texweak "\multicolumn{4}{p{0.9\linewidth}}{\footnotesize Notes: Each row reports weak-IV diagnostics for the corresponding IV specification. The first line gives the Kleibergen--Paap rk Wald F statistic. The second line gives the Anderson--Rubin weak-instrument-robust p-value for the null hypothesis that child marriage has no causal effect on the outcome. All specifications use the same bandwidth-specific samples, controls, and fixed effects as the main IV regressions.}" _n
file write texweak "\end{tabular}" _n
file write texweak "\end{table}" _n

file close texweak
