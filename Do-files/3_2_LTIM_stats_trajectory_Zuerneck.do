*===============================================================================
*===============================================================================

*LTIM Summary Statistics and Trajectory Analysis
*Simon Zuerneck 
*Stata Version: 18.0
********************************************************************************

clear all
set more off 
capture log close 
cd $master_thesis
log using Log\3_2_LTIM_stats_trajectory_Zuerneck.log, text replace 
********************************************************************************

*===============================================================================
*Table 1: LTIM -- Summary Statistics
*===============================================================================

estimates clear 
drop _all

*Locals
local outcomes "inflow_2 inflow_3 inflow_4"
local group "group_iasio"
local dataset "LTIM"
local start_date 2012q4

cd "$master_thesis/Data/`dataset'"
*Create a tempfile with the statistics of interest
tempname sumtable
postfile `sumtable' str40(group) str20(variable) mean_total sd_total ///
	mean_pre sd_pre mean_post sd_post t_stat p_val str15(diff_stars) ///
	using summarytable.dta, replace

use `dataset'_panel.dta, clear
********************************************************************************

****************
**Computations**
****************

*Collapse for all outcomes by group
collapse (sum) `outcomes', by(`group' date_stata)

*Generate a post-treatment indicator
gen post = (date_stata >= tq(2016q3))

keep if date_stata >= tq(`start_date')
	
*Loop through treated and control group
foreach treated in 1 0 {
	
    if `treated'==1 local gname "EU"
    if `treated'==0 local gname "Non-EU"

	*Loop over outcomes 
    foreach v of local outcomes {
		
		*Summary statistics
        sum `v' if `group' == `treated'
        local mean_total = r(mean)
        local sd_total   = r(sd)
        sum `v' if `group' == `treated' & post == 0
        local mean_pre = r(mean)
        local sd_pre   = r(sd)
        sum `v' if `group' == `treated' & post == 1
        local mean_post = r(mean)
        local sd_post   = r(sd)
        ttest `v' if `group' == `treated', by(post)
        local tdiff = r(t)
		
		*T-test results
		ttest `v' if `group' == `treated', by(post)
		local t_stat = r(t)
		local p_val = r(p)
		return list

		*Create a difference with stars to indicate significance
		local diff = `mean_post' - `mean_pre'
		local stars = ""
		if `p_val' < 0.01 {
			local stars = "***"
		}
		else if `p_val' < 0.05 {
			local stars = "**"
		}
		else if `p_val' < 0.1 {
			local stars = "*"
		}
		local diff =  `mean_post' - `mean_pre'
		local diff_fmt = trim("`: display %9.2f `diff''")
		local diff_stars = "`diff_fmt'`stars'"

		*Put result in the tempfile table 
        post `sumtable' ("`gname'") ("`v'") (`mean_total') (`sd_total') ///
            (`mean_pre') (`sd_pre') (`mean_post') (`sd_post') ///
            (`t_stat') (`p_val') ("`diff_stars'")		
    }
}
postclose `sumtable'
********************************************************************************

****************************
**Latex Table Construction**
****************************

use summarytable.dta, clear
br

*Do some renaming 
replace variable = "Work Related" if variable == "inflow_2"
replace variable = "Definite Job" if variable == "inflow_3"
replace variable = "Looking for Work" if variable == "inflow_4"

*Open table
cd "$master_thesis/Tables"
capture file close latexout
file open latexout using "`dataset'_summary_panels.tex", write replace

*Write the header 
file write latexout "\begin{tabular}{llccccccc}" _n
file write latexout "\hline\midrule" _n
file write latexout " & & \multicolumn{2}{c}{(1)} & \multicolumn{2}{c}{(2)} & \multicolumn{2}{c}{(3)} & (4) \\" _n
file write latexout " & & \multicolumn{2}{c}{Total} & \multicolumn{2}{c}{Before} & \multicolumn{2}{c}{After} & \\" _n
file write latexout "Group & Outcome & Mean & SD & Mean & SD & Mean & SD & Diff. \\" _n
file write latexout "\midrule" _n

*Put the statistics in the table
local lastgroup = ""

forvalues i=1/`=_N' {

	local group = group[`i']
    local var = variable[`i']
    local mtotal = string(mean_total[`i'], "%9.2f")
    local sdtotal = string(sd_total[`i'], "%9.2f")
    local mpre = string(mean_pre[`i'], "%9.2f")
    local sdpre = string(sd_pre[`i'], "%9.2f")
    local mpost = string(mean_post[`i'], "%9.2f")
    local sdpost = string(sd_post[`i'], "%9.2f")
	local diff = diff_stars[`i']
    *local ttest = string(tdiff[`i'], "%9.2f")

    *Detect panel changes and format the panels 
    if "`group'" != "`lastgroup'" {
        if `i' > 1 file write latexout "\midrule" _n
        file write latexout "`group'" "\\" _n	
		file write latexout " & `var' & `mtotal' & `sdtotal' & `mpre' & `sdpre' & `mpost' & `sdpost' & `diff' \\" _n		
    }
    else {
        file write latexout " & `var' & `mtotal' & `sdtotal' & `mpre' & `sdpre' & `mpost' & `sdpost' & `diff' \\" _n
    }

    local lastgroup = "`group'"
}

*Write the closer 
file write latexout "\hline\hline" _n
file write latexout "\end{tabular}" _n
file close latexout
********************************************************************************

*===============================================================================
*Figure 1: LTIM - Pre- and Post-Reform Trajectories
*===============================================================================

****************************************
**Computations and Figure Construction**
****************************************

*Locals
local dataset "LTIM"
local group "group_iasio"
local unit "subgroup"
local outcome inflow_2 inflow_3 inflow_4
cd "$master_thesis/Data/`dataset'"
use `dataset'_panel.dta, clear

*Local to store the graphs 
local `dataset'_trends ""

*Loop for the migrational reason of interest 
foreach v of local outcome {
	
	*Local for the title label 
	local title_label : variable label `v'
	di "`title_label'"
	
	replace `v' = 0 if missing(`v')

	preserve 
	
	*Compute the mean inflow per unit per group per time
	collapse (mean) `v', by(date_stata `group')
	
	*Format the date for the x-axis labels 
	format date_stata %tq
	sort date_stata

	local xlabels ""
	forvalues q = `=tq(2012q4)'/`=tq(2019q4)' {
		if mod(`q', 4) == 0 | mod(`q', 4) == 2 { 
			local xlabels "`xlabels' `q'"
		}
	}

	*Create a trend plot
	capture graph drop `dataset'_trends_`v'
	twoway ///	
		(connected `v' date_stata if date_stata >= tq(2012q4) & ///
		date_stata <= tq(2019q4) & `group' == 1, sort lwidth(medium) ///
		lcolor("26 133 255") mcolor("26 133 255") msymbol(circle)) ///	
		(connected `v' date_stata if date_stata >= tq(2012q4) & ///
		date_stata <= tq(2019q4) & `group' == 0, sort lwidth(medium) ///
		lcolor("212 17 89") mcolor("212 17 89") msymbol(circle) ///
		lpattern(dash)), ///
		legend(order(1 "EU Inflows" 2 "Non-EU Inflows") ///
			position(6) cols(2) region(style(none) lstyle(solid) lcolor(black) ///
			lwidth(medium))) ///
		legend(size(small) colgap(vsmall) keygap(small)) ///
		xline(`=tq(2016,2) + (23/91)', lcolor(black) lpattern(dash)) ///
		xlabel(`xlabels', format(%tq) angle(0) labsize(vsmall)) ///
		xlabel(, grid) ///
		xtitle("") ytitle("Mean Inflows per Unit (Subregion)") ///
		ylabel(, angle(0)) ///
		title("Migration Reason: `title_label'", ///
			tstyle(heading) color(black) position(12) ring(100)) ///
		graphregion(color(white)) ///
		bgcolor(white) ///
		plotregion(color(white)) ///
		name(`dataset'_trends_`v')
					
	*Store the graph 
	local `dataset'_trends "``dataset'_trends' `dataset'_trends_`v'"
		
	restore
}
di "Stored graphs: ``dataset'_trends'"

*Combine graphs
grc1leg2 ``dataset'_trends', leg(LTIM_trends_inflow_2) iscale(*.7) ///
graphregion(color(white)) graphregion(margin(zero)) 

*Save the graph 
cd "$master_thesis/Graphs"
graph export "`dataset'_trends_combined.pdf", replace as (pdf)
********************************************************************************

*===============================================================================
*Figure 15: LTIM - Pre- and Post-Reform Trajectories of Summed Inflows 
*(With confidence intervals from the Dataset)
*===============================================================================

****************************************
**Computations and Figure Construction**
****************************************

*Locals
local dataset "LTIM"
local group "group_iasio"
local unit "subgroup"
local outcome inflow_2 inflow_3 inflow_4
cd "$master_thesis/Data/`dataset'"
use `dataset'_panel.dta, clear
local `dataset'_sum_trends_CI ""


*Local to store the graphs 
local `dataset'_sum_trends_CI ""
 
*Loop for the migrational reason of interest 
foreach v of local outcome {
	
	*Local for the title label 
	local title_label : variable label `v'
	di "`title_label'"
	
	replace `v' = 0 if `v' == .

	*Local for the confidence interview
	local ci = "ci_`v'"
	di "ci_inflow_`v'"

	preserve 
	
	*Sum of inflows 
	bysort date_stata `group' (`unit'): ///
		egen sum_`v' = total(`v') if !missing(`unit')
	
	*Confidence intervals 
	*(confidence intervalls are +/- CI)
	capture gen `ci'_low = `v' - `ci'
	capture gen `ci'_high = `v' + `ci'
	
	bysort date_stata group_iasio (`unit'): ///
		egen sum_`ci'_low = total(`ci'_low) if ///
		!missing(group_iasio)
	
	bysort date_stata `group' (`unit'): ///
		egen sum_`ci'_high = total(`ci'_high) if ///
		!missing(`group')
	
	*Format the date for the x-axis labels 
	format date_stata %tq
	sort date_stata

	local xlabels ""
	forvalues q = `=tq(2012q4)'/`=tq(2019q4)' {
		if mod(`q', 4) == 0 | mod(`q', 4) == 2 { 
			local xlabels "`xlabels' `q'"
		}
	}

	*Create a trend plot
	capture graph drop `dataset'_sum_trends_`v'_CI
	twoway ///	
		(rarea sum_`ci'_low sum_`ci'_high date_stata if ///
		date_stata >= tq(2012q4) & date_stata <= tq(2019q4) & ///
		`group' == 1, sort color("26 133 255%90") fcolor("26 133 255%9") ///
		lwidth(none)) ///	
		(rarea sum_`ci'_low sum_`ci'_high date_stata if /// 
		date_stata >= tq(2012q4) & date_stata <= tq(2019q4) & ///
		`group' == 0, sort color("212 17 89%90") fcolor("212 17 89%9") ///
		lwidth(none)) ///	
		(connected sum_`v' date_stata if date_stata >= tq(2012q4) & ///
		date_stata <= tq(2019q4) & `group' == 1, sort lwidth(medium) ///
		lcolor("26 133 255") mcolor("26 133 255") msymbol(circle)) ///	
		(connected sum_`v' date_stata if date_stata >= tq(2012q4) & ///
		date_stata <= tq(2019q4) & `group' == 0, sort lwidth(medium) ///
		lcolor("212 17 89") mcolor("212 17 89") msymbol(circle) ///
		lpattern(dash)), ///
		legend(order(3 "EU Inflows" 4 "Non-EU Inflows") ///
			position(6) cols(2) region(style(none) lstyle(solid) lcolor(black) ///
			lwidth(medium))) ///
		legend(size(small) colgap(vsmall) keygap(small)) ///
		xline(`=tq(2016,2) + (23/91)', lcolor(black) lpattern(dash)) ///
		xlabel(`xlabels', format(%tq) angle(0) labsize(vsmall)) ///
		xlabel(, grid) ///
		xtitle("") ytitle("Summed Inflows in Thousands") ///
		ylabel(, angle(0)) ///
		title("Migration Reason: `title_label'", ///
			tstyle(heading) color(black) position(12) ring(100)) ///
		graphregion(color(white)) ///
		bgcolor(white) ///
		plotregion(color(white)) ///
		name(`dataset'_sum_trends_`v'_CI)
					
	*Store the graph 
	local `dataset'_sum_trends_CI "``dataset'_sum_trends_CI' `dataset'_sum_trends_`v'_CI"
		
	restore
}
di "Stored graphs: ``dataset'_sum_trends_CI'"

*Combine graphs and save them as PDF
cd "$master_thesis/Graphs"
grc1leg2 ``dataset'_sum_trends_CI', leg(LTIM_sum_trends_inflow_2_CI) iscale(*.7) ///
graphregion(color(white)) graphregion(margin(zero)) 

*Save the graph 
cd "$master_thesis/Graphs"
graph export "`dataset'_sum_trends_CI_combined.pdf", replace as (pdf)
********************************************************************************

log close 
cd $my_path_do
*===============================================================================
*===============================================================================