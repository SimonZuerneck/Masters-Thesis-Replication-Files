*===============================================================================
*===============================================================================

*UKLFS Summary Statistics and Trajectory Analysis  
*Simon Zuerneck 
*Stata Version: 18.0
********************************************************************************

clear all
set more off 
capture log close 
cd $master_thesis
log using Log\5_1_UKLFS_stats_trajectory_Zuerneck.log, text replace 
********************************************************************************

*===============================================================================
*Table 10: UKLFS –- Summary Statistics
*===============================================================================
cd "$master_thesis/Data/UKLFS"
use UKLFS_base_panel_subgroups.dta, clear


estimates clear 
drop _all

*Locals
local outcome "lfs_total"
local group group_iasio
local dataset "UKLFS"
local start_date 2012q4

*Create a tempfile with the statistics of interest
cd "$master_thesis/Data/`dataset'"
capture erase "summarytable.dta"
tempname sumtable
postfile `sumtable'  str40(panel) str20(variable) mean_total ///
	sd_total mean_pre sd_pre mean_post sd_post t_stat p_val ///
	str15(diff_stars) using summarytable.dta, replace
********************************************************************************

****************
**Computations**
****************

use `dataset'_panel.dta, clear

*Collapse sum of outcomes by all groups
collapse (sum) `outcome', by(`group' date_stata)

keep if date_stata >= tq(`start_date')

*Generate a post-treatment indicator
gen post = (date_stata >= tq(2016q3))

*Loop through treated and control within each group
foreach treated in 1 0  {	
		
	*Label for panel title 
	if `treated'==1 local panel "EU"
	if `treated'==0 local panel "Non-EU"
				
		*Summary statistics
		sum `outcome' if `group' == `treated'
		local mean_total = r(mean)
		local sd_total   = r(sd)
		sum `outcome' if `group' == `treated' & post == 0
		local mean_pre = r(mean)
		local sd_pre   = r(sd)			
		sum `outcome' if `group' == `treated' & post == 1
		local mean_post = r(mean)
		local sd_post   = r(sd)
				
		*T-test results
		ttest `outcome' if `group' == `treated', by(post)
		local t_stat = r(t)
		local p_val = r(p)
		return list

		*Create a difference with stars
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
			
		*Put results in the tempfile table 
		post `sumtable' ("`panel'") ("`outcome'") (`mean_total') ///
			(`sd_total') (`mean_pre') (`sd_pre') (`mean_post') ///
			(`sd_post') (`t_stat') (`p_val') ("`diff_stars'")
	
}
postclose `sumtable'
********************************************************************************

****************************
**Latex Table Construction**
****************************

use summarytable.dta, clear

*Open table
cd "$master_thesis/Tables"
capture file close latexout
file open latexout using "`dataset'_summary_panels.tex", write replace

*Write the header 
file write latexout "\begin{tabular}{lccccccc}" _n
file write latexout "\hline\midrule" _n
file write latexout "& \multicolumn{2}{c}{(1)} & \multicolumn{2}{c}{(2)} & \multicolumn{2}{c}{(3)} & (4) \\" _n
file write latexout "& \multicolumn{2}{c}{Total} & \multicolumn{2}{c}{Before} & \multicolumn{2}{c}{After} & \\" _n
file write latexout "Group & Mean & SD & Mean & SD & Mean & SD & Diff. \\" _n
file write latexout "\midrule" _n

*Put the statistics in the table
forvalues i=1/`=_N' {

	*Bring everything in the correct format
	local panel = panel[`i']
	local var = variable[`i']
    local mtotal = string(mean_total[`i'], "%9.2f")
    local sdtotal = string(sd_total[`i'], "%9.2f")
    local mpre = string(mean_pre[`i'], "%9.2f")
    local sdpre = string(sd_pre[`i'], "%9.2f")
    local mpost = string(mean_post[`i'], "%9.2f")
    local sdpost = string(sd_post[`i'], "%9.2f")
	local diff = diff_stars[`i']
	
	*Write it into Latex
    file write latexout "`panel' & `mtotal' & `sdtotal' & `mpre' & `sdpre' & `mpost' & `sdpost' & `diff' \\" _n

}

*Write the closer
file write latexout "\hline\hline" _n
file write latexout "\end{tabular}" _n
file close latexout	
********************************************************************************

*===============================================================================
*Figure 13: UKLFS — Pre- and Post-Reform Trajectories of Non-Log-Transformed 
*Inflows

*Figure 14: UKLFS — Pre- and Post-Reform Trajectories of Log-Transformed 
*Inflows
*===============================================================================

****************************************
**Computations and Figure Construction**
****************************************

*Locals
local dataset "UKLFS"
local group "group_iasio"
local unit "subgroup"
local outcome lfs_total ln_lfs_total
cd "$master_thesis/Data/`dataset'"
use `dataset'_panel.dta, clear
 
*Loop through log-transformed and not log-transformed outcomes
foreach v of local outcome {

	preserve 
	replace `v' = 0 if `v' == .

	*Compute the mean per unit per group per time
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

	*Title labels 
	if "`v'" == "lfs_total" {
		local title_label "Non-Log-Transformed Inflows"
	}
	else if "`v'" == "ln_lfs_total" {
		local title_label "Log-Transformed Inflows"	
	}
	
	*Normal connected graph
	capture graph drop `dataset'_trends_`v'
	twoway ///	
		(connected `v' date_stata if date_stata >= tq(2012q4) & ///
		date_stata <= tq(2019q4) & `group' == 1, sort lwidth(medium) ///
		lcolor("26 133 255") mcolor("26 133 255") msymbol(circle)) ///	
		(connected `v' date_stata if date_stata >= tq(2012q4) & ///
		date_stata <= tq(2019q4) & `group' == 0, sort lwidth(medium) ///
		lcolor("212 17 89") mcolor("212 17 89") msymbol(circle) ///
		lpattern(dash)), ///
		legend(order(1 "EU Respondents" 2 "Non-EU Respondents") ///
			position(6) cols(2) region(style(none) lstyle(solid) lcolor(black) ///
			lwidth(medium))) ///
		legend(size(small) colgap(vsmall) keygap(small)) ///
		xline(`=tq(2016,2) + (23/91)', lcolor(black) lpattern(dash)) ///
		xlabel(`xlabels', format(%tq) angle(0) labsize(vsmall)) ///
		xlabel(, grid) ///
		xtitle("") ytitle("Mean Inflows per Unit (Subregion)") ///
		ylabel(, angle(0)) ///
		title("`title_label': Non-Smoothed Trends", ///
			tstyle(heading) color(black) position(12) ring(100)) ///
		graphregion(color(white)) ///
		bgcolor(white) ///
		plotregion(color(white)) ///
		name(`dataset'_trends_`v')
		
	*Smoothed lwess graph 
	capture graph drop `dataset'_smooth_trends_`v'
	twoway ///	
		(lowess `v' date_stata if date_stata >= tq(2012q4) & ///
		date_stata <= tq(2019q4) & `group' == 1, sort lwidth(medium) ///
		lcolor("26 133 255") mcolor("26 133 255") msymbol(circle)) ///	
		(lowess `v' date_stata if date_stata >= tq(2012q4) & ///
		date_stata <= tq(2019q4) & `group' == 0, sort lwidth(medium) ///
		lcolor("212 17 89") mcolor("212 17 89") msymbol(circle) ///
		lpattern(dash)), ///
		legend(order(1 "EU Respondents" 2 "Non-EU Respondents") ///
			position(6) cols(2) region(style(none) lstyle(solid) lcolor(black) ///
			lwidth(medium))) ///
		legend(size(small) colgap(vsmall) keygap(small)) ///
		xline(`=tq(2016,2) + (23/91)', lcolor(black) lpattern(dash)) ///
		xlabel(`xlabels', format(%tq) angle(0) labsize(vsmall)) ///
		xlabel(, grid) ///
		xtitle("") ytitle("Mean Inflows per Unit (Subregion)") ///
		ylabel(, angle(0)) ///
		title("`title_label': Smoothed Trends", ///
			tstyle(heading) color(black) position(12) ring(100)) ///
		graphregion(color(white)) ///
		bgcolor(white) ///
		plotregion(color(white)) ///
		name(`dataset'_smooth_trends_`v')		
		
	restore
}
********************************************************************************

*Combine graphs and save them as PDF
cd "$master_thesis/Graphs"
grc1leg2 `dataset'_trends_lfs_total ///
	`dataset'_smooth_trends_lfs_total `dataset'_trends_ln_lfs_total ///
	`dataset'_smooth_trends_ln_lfs_total, ///
	leg(`dataset'_smooth_trends_ln_lfs_total) iscale(*.7) ///
	graphregion(color(white)) graphregion(margin(zero)) 

graph export "`dataset'_trends_combined.pdf", replace as (pdf)
********************************************************************************

log close 
cd $my_path_do
*===============================================================================
*===============================================================================