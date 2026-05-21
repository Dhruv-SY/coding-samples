****************************************************
* 12_graphs.do
* RD graphs using cleaned outcomes from 00_setup.do
* Raw binned means + unadjusted linear fits
****************************************************

clear all
set more off

*---------------------------------------------------*
* 0) CALL SHARED SETUP
*---------------------------------------------------*
do "code/00_setup.do"

*---------------------------------------------------*
* 1) ADD / CONFIRM CONDOM ATTITUDE OUTCOME
*    v822 = wife justified asking husband to use condom if he has STI
*---------------------------------------------------*

capture confirm variable condom_ask_justified
if _rc {
    gen condom_ask_justified = .
    replace condom_ask_justified = 1 if v822 == 1
    replace condom_ask_justified = 0 if v822 == 0
    label var condom_ask_justified "Wife justified asking husband to use condom if he has STI"
}

*---------------------------------------------------*
* 2) GRAPH SETUP
*    Equal-observation bins separately by side
*    20 bins per side
*---------------------------------------------------*

capture drop sample40 group gleft gright rd_mean
gen sample40 = rd > -100 & rd < 100
gen group = .

xtile gleft  = rd if rd > -100 & rd < 0,  n(20)
xtile gright = rd if rd >= 0 & rd < 100, n(20)

replace group = gleft if rd > -100 & rd < 0
replace group = 20 + gright if rd >= 0 & rd < 100

egen rd_mean = mean(rd) if sample40, by(group)

*---------------------------------------------------*
* 3) REUSABLE GRAPH PROGRAM
*---------------------------------------------------*

capture program drop rd_graph
program define rd_graph
    syntax varname(numeric), YTITLE(string) FILE(string)

    capture drop ymean_graph tag_graph yhat_left yhat_right

    egen ymean_graph = mean(`varlist') if sample40 & !missing(`varlist'), by(group)
    egen tag_graph   = tag(group)     if sample40 & !missing(`varlist')

    quietly reg `varlist' rd if rd < 0 & rd > -100 & !missing(`varlist'), vce(robust)
    predict yhat_left if e(sample)

    quietly reg `varlist' rd if rd >= 0 & rd < 100 & !missing(`varlist'), vce(robust)
    predict yhat_right if e(sample)

    twoway ///
        (scatter ymean_graph rd_mean if tag_graph == 1 & !missing(ymean_graph), ///
            mcolor(black) msize(small)) ///
        (line yhat_left rd if rd < 0 & rd > -100, ///
            sort lwidth(medthick)) ///
        (line yhat_right rd if rd >= 0 & rd < 100, ///
            sort lwidth(medthick)) ///
        , ///
        xline(0) ///
        xtitle("Distance to Border (km)") ///
        ytitle("`ytitle'") ///
        legend(off) ///
        graphregion(color(white)) ///
        plotregion(color(white))

    graph export "results/`file'", replace width(2000)

    capture drop ymean_graph tag_graph yhat_left yhat_right
end

****************************************************
* 4) RD GRAPHS
****************************************************

rd_graph child_marriage, ///
    ytitle("Child Marriage") ///
    file("child_marriage_rd_plot.png")

rd_graph multiple_unions, ///
    ytitle("Multiple Unions") ///
    file("multiple_unions_rd_plot.png")

rd_graph v133, ///
    ytitle("Education Years") ///
    file("education_years_rd_plot.png")

rd_graph educ_any, ///
    ytitle("Any Education") ///
    file("any_education_rd_plot.png")

rd_graph educ_years_pos, ///
    ytitle("Education Years Among Schooled Women") ///
    file("education_years_positive_rd_plot.png")

rd_graph v106, ///
    ytitle("Education Level") ///
    file("education_level_rd_plot.png")

rd_graph v212, ///
    ytitle("Age at First Birth") ///
    file("age_first_birth_rd_plot.png")

rd_graph v201, ///
    ytitle("Children Ever Born") ///
    file("children_ever_born_rd_plot.png")

rd_graph v218, ///
    ytitle("Living Children") ///
    file("living_children_rd_plot.png")

rd_graph v525, ///
    ytitle("Age at First Sex") ///
    file("age_first_sex_rd_plot.png")

rd_graph condom_ask_justified, ///
    ytitle("Wife Justified Asking Husband to Use Condom if He Has STI") ///
    file("condom_ask_justified_rd_plot.png")

rd_graph not_working, ///
    ytitle("Not Working") ///
    file("not_working_rd_plot.png")

rd_graph domestic_work, ///
    ytitle("Domestic Work Among Workers") ///
    file("domestic_work_rd_plot.png")

rd_graph paid_cash, ///
    ytitle("Paid in Cash Among Workers") ///
    file("paid_cash_rd_plot.png")

rd_graph white_collar, ///
    ytitle("White-Collar Employment") ///
    file("white_collar_rd_plot.png")

rd_graph white_collar_cond, ///
    ytitle("White-Collar Employment Among Workers") ///
    file("white_collar_conditional_rd_plot.png")
