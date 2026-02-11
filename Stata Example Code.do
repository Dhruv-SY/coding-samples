* Clear workspace and load data

clear all
set more off
use "Up_in_smoke_households.dta", clear

*Answer 2

* List all variables starting with v_mo_control and build a macro. This is the village x month × year fixed effects

ds v_mo_control*, has(type numeric)
local fe_vars `r(varlist)'

* Here we run the four regressions, with fixed effects and clustering the errors of households

regress stovebuilt treat `fe_vars', cluster(hhid_M)
regress anystove treat `fe_vars', cluster(hhid_M)
regress goodcond treat `fe_vars', cluster(hhid_M)
regress mealslowpol_good treat `fe_vars', cluster(hhid_M)

*Tabulating all the regressions results into one table

eststo clear

eststo model1: regress stovebuilt treat `fe_vars', cluster(hhid_M)
eststo model2: regress anystove treat `fe_vars', cluster(hhid_M)
eststo model3: regress goodcond treat `fe_vars', cluster(hhid_M)
eststo model4: regress mealslowpol_good treat `fe_vars', cluster(hhid_M)

esttab model1 model2 model3 model4 using results.tex, ///
    se b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) ///
    keep(treat) ///
    label ///
    title("Effect of Treatment on Stove Adoption") ///
    replace


*Answer 3

*1

*Loading the data

clear all
set more off
use "Up_in_smoke_adults.dta", clear

* List all variables starting with v_mo_control and build a macro

ds v_mo_control*, has(type numeric)
local fe_vars `r(varlist)'

* Run the OLS regression of CO_adult on treat including the fixed effects

regress CO_adult treat `fe_vars', cluster(hhid_M)

*Here, we tabulate the results that we have obtained. We use latex for this.

eststo clear

eststo primary: regress CO_adult treat `fe_vars', cluster(hhid_M)

esttab primary using results1.tex, ///
    se b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) ///
    keep(treat) label ///
    title("Effect of Treatment on CO Adults") ///
    replace
	

*2

* List all variables starting with v_mo_control and build a macro

ds v_mo_control*, has(type numeric)
local fe_vars `r(varlist)'

* Run the IV regression:

* Here we instrument "anystove" with "treat" while controlling for the fixed effects.

ivregress 2sls CO_adult (anystove = treat) `fe_vars', cluster(hhid_M)

*Here, we tabulate the results that we have obtained. We use latex for this.

eststo Anystove: ivregress 2sls CO_adult (anystove = treat) `fe_vars', cluster(hhid_M)

esttab Anystove using results2.tex, ///
    se b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) ///
    keep(anystove) label ///
    title("Effect of Treatment on CO Adults") ///
    replace
	
	
*3

clear all
set more off
use "Up_in_smoke_children.dta", clear

* Inspect the data structure and key variables

describe

* List all variables starting with v_mo_control and build a macro

ds v_mo_control*, has(type numeric)
local fe_vars `r(varlist)'	
	
eststo primary1: regress CO_children treat `fe_vars', cluster(hhid_M)
eststo Anystove1: ivregress 2sls CO_children (anystove = treat) `fe_vars', cluster(hhid_M)

*Tabulating our results into latex

esttab primary1 Anystove1 using results3.tex, ///
    se b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) ///
    keep(anystove) label ///
    title("Effect of Treatment on CO children") ///
    replace
	

*4

*Here we run the regressions with anystove and treat to see the correlation between the two variables and see if it satisfies the relevance condition

*Load the data set

clear all
set more off
use "Up_in_smoke_adults.dta", clear

* List all variables starting with v_mo_control and build a macro

ds v_mo_control*, has(type numeric)
local fe_vars `r(varlist)'


*Tabulating our results into latex

eststo result1: regress anystove treat `fe_vars', cluster(hhid_M)

esttab result1 using result1.tex, ///
    se b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) ///
    keep(treat) label ///
    title("Effect of Treatment on anystove") ///
    replace
	
*Load the data set

clear all
set more off
use "Up_in_smoke_children.dta", clear

* List all variables starting with v_mo_control and build a macro

ds v_mo_control*, has(type numeric)
local fe_vars `r(varlist)'


*Tabulating our results into latex

eststo result2: regress anystove treat `fe_vars', cluster(hhid_M)

esttab result2 using result2.tex, ///
    se b(3) se(3) star(* 0.1 ** 0.05 *** 0.01) ///
    keep(treat) label ///
    title("Effect of Treatment on anystove") ///
    replace


