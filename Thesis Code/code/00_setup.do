*==============================================================*
* 00_setup.do
* Shared preamble for all Stata replication scripts.
* Do NOT run this file directly — it is called by each script.
*==============================================================*

*---------------------------------------------------*
* 0) LOAD DATA
*---------------------------------------------------*
use "data/The_11_countries_union_migration_merged.dta", clear

*---------------------------------------------------*
* 1) SAMPLE RESTRICTIONS + IDS
*---------------------------------------------------*
drop if v012 < 18 & v511 == .

capture drop bp eth_id multiple_unions not_working domestic_work paid_cash ///
    white_collar white_collar_cond educ_any educ_years_pos 

encode country_pair, gen(bp)
encode eth_fe, gen(eth_id)

drop if missing(child_marriage, treated, rd, bp, survey_year, v021)

*---------------------------------------------------*
* 2) RD SHAPE + FE + CONTROLS
*---------------------------------------------------*
global RDBP "i.bp c.rd#i.bp c.rd#i.bp#i.treated"
global FE   "i.survey_year nightlights_mean population_density_mean agri_suitability_mean i.eth_id"
global X    "c.v012 i.v025"

*---------------------------------------------------*
* 3) CLEAN KEY VARIABLES
*---------------------------------------------------*
replace v133 = . if inlist(v133, 97, 98, 99)
replace v106 = . if inlist(v106, 9)
replace v525 = . if inlist(v525, 0, 96, 97, 98, 99)
replace v741 = . if v741 == 9
replace v717 = . if inlist(v717, 10, 96, 98, 99)
replace v822 = . if inlist(v822, 8, 9)

*---------------------------------------------------*
* 4) CONSTRUCT OUTCOMES
*---------------------------------------------------*

* Extensive margin: any education
gen educ_any = .
replace educ_any = 1 if v133 > 0 & !missing(v133)
replace educ_any = 0 if v133 == 0
label var educ_any "Any education"

* Intensive margin: years conditional on having any schooling
gen educ_years_pos = .
replace educ_years_pos = v133 if v133 > 0 & !missing(v133)
label var educ_years_pos "Education years | schooling > 0"

* multiple_unions: ever been in more than one union
* v503 = 1 (one union only)   -> 0
* v503 = 2+ (more than one)   -> 1
* v503 = 9 (don't know)       -> .
* v503 = . (never in union)   -> 0
gen multiple_unions = 0
replace multiple_unions = 1 if v503 >= 2 & !missing(v503)
replace multiple_unions = . if v503 == 9
label var multiple_unions "More than one union"

gen not_working = .
replace not_working = 1 if v714 == 0
replace not_working = 0 if v714 == 1
label var not_working "Not working"

gen domestic_work = .
replace domestic_work = 1 if v717 == 6
replace domestic_work = 0 if v714 == 1 & v717 != 6
* NOTE: Working women with missing v717 satisfy v717 != 6 (Stata sysmiss != 6),
* so they are coded 0. This treats unknown-occupation workers as non-domestic.
label var domestic_work "Works in domestic jobs"

gen paid_cash = .
replace paid_cash = 1 if v714 == 1 & inlist(v741,1,2)
replace paid_cash = 0 if v714 == 1 & inlist(v741,0,3)
label var paid_cash "Currently working and paid in cash (cash only or cash and in-kind)"

* white_collar extensive margin:
* Order matters: step 4 (v714==0 -> 0) overrides step 3 (missing v717 -> .)
* Net result: working women with missing v717 remain missing;
* non-working women are 0 regardless of v717.
gen white_collar = .
replace white_collar = 1 if inlist(v717,1,2)
replace white_collar = 0 if v714 == 1 & !missing(v717) & !inlist(v717,1,2)
replace white_collar = . if missing(v717)
replace white_collar = 0 if v714 == 0
label var white_collar "White-collar employment"

gen white_collar_cond = .
replace white_collar_cond = 1 if v714 == 1 & inlist(v717,1,2)
replace white_collar_cond = 0 if v714 == 1 & !missing(v717) & !inlist(v717,1,2)
label var white_collar_cond "White-collar among workers"

