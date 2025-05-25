*===============================================================================
*===============================================================================


*NINo Summary Statistics and Trajectory Analysis
*Simon Zuerneck 
*Stata Version: 18.0
********************************************************************************

clear all
set more off 
capture log close 
cd $master_thesis
log using Log\4_2_NINo_stats_trajectory_Zuerneck.log, text replace 
********************************************************************************

*===============================================================================
*Table 2: NINo -- Summary Statistics (overall)
*===============================================================================

estimates clear 
drop _all

*Locals
local outcome nino_total
local group group_simon group_clifton
local dataset "NINo"
local start_date 2014q4

cd "$master_thesis/Data/`dataset'"
*Create a tempfile with the statistics of interest
tempname sumtable
postfile `sumtable'  str40(panel) str40(group) str20(variable)	mean_total ///
	sd_total mean_pre sd_pre mean_post sd_post t_stat p_val ///
	str15(diff_stars) using summarytable.dta, replace
********************************************************************************

****************
**Computations**
****************

foreach g of local group {

    preserve 
		
	use `dataset'_panel.dta, clear

	*Collapse sum of outcomes by all groups
	collapse (sum) `outcome', by(`g' date_stata)

	keep if date_stata >= tq(`start_date')

	*Generate a post-treatment indicator
	gen post = (date_stata >= tq(2016q3))

	*Loop through treated and control within each group
	foreach treated in 1 0  {	
		
		*Label for panel title 
		if `treated'==1 local panel "EU"
		if `treated'==0 local panel "Non-EU"
				
			*Summary statistics
			sum `outcome' if `g' == `treated'
			local mean_total = r(mean)
			local sd_total   = r(sd)
			sum `outcome' if `g' == `treated' & post == 0
			local mean_pre = r(mean)
			local sd_pre   = r(sd)			
			sum `outcome' if `g' == `treated' & post == 1
			local mean_post = r(mean)
			local sd_post   = r(sd)
				
			*T-test results
			ttest `outcome' if `g' == `treated', by(post)
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
				
			*Put result in the tempfile table 
			post `sumtable' ("`panel'") ("`g'") ("`outcome'") (`mean_total') ///
				(`sd_total') (`mean_pre') (`sd_pre') (`mean_post') ///
				(`sd_post') (`t_stat') (`p_val') ("`diff_stars'")
		
	}
	
	restore
}
postclose `sumtable'
********************************************************************************

****************************
**Latex Table Construction**
****************************

use summarytable.dta, clear

*Do some renaming and sorting
replace variable = "NINo Registrations" if variable == "nino_total"
replace group = "Composition a" if group == "group_simon"
replace group = "Composition b" if group == "group_clifton"
gsort panel group


*Open table
cd "$master_thesis/Tables"
capture file close latexout
file open latexout using "`dataset'_summary_panels.tex", write replace

*Write the header 
file write latexout "\begin{tabular}{llccccccc}" _n
file write latexout "\hline\midrule" _n
file write latexout "& & \multicolumn{2}{c}{(1)} & \multicolumn{2}{c}{(2)} & \multicolumn{2}{c}{(3)} & (4) \\" _n
file write latexout "& & \multicolumn{2}{c}{Total} & \multicolumn{2}{c}{Before} & \multicolumn{2}{c}{After} & \\" _n
file write latexout "Group & Composition & Mean & SD & Mean & SD & Mean & SD & Diff. \\" _n
file write latexout "\midrule" _n

*Generate a panel local
local last_panel = ""

*Put the statistics in the table
forvalues i=1/`=_N' {

    local panel = panel[`i']
    local gname = group[`i']
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
	if "`panel'" != "`last_panel'" {
        if `i' > 1 file write latexout "\midrule" _n
        file write latexout "`panel' \\" _n
    }
    file write latexout " \hspace{0.5cm} & `gname' & `mtotal' & `sdtotal' & `mpre' & `sdpre' & `mpost' & `sdpost' & `diff' \\" _n
	local last_panel = "`panel'"
	
}

*Write the closer
file write latexout "\hline\hline" _n
file write latexout "\end{tabular}" _n
file close latexout	
********************************************************************************

*===============================================================================
*Figure 3: NINo -- Pre- and Post-Reform Trajectories of Total Registrations for 
*Different Group Compositions
*===============================================================================

*Locals
local dataset "NIno"
local group group_clifton group_simon
local unit "c_subgroup"
local outcome nino_total
local start_date 2014q4

cd "$master_thesis/Data/`dataset'"
use `dataset'_panel.dta, clear
estimates clear 

foreach g of local group {
	
	preserve 
	
	keep if date_stata >= tq(`start_date')
	
	collapse (mean) `outcome' , by(date_stata `g')
	drop if `g' == .
	
	local title_label "NINo Registrations"
	di "`title_label'"
	
	*format the date and sort the data accordingly
	format date_stata %tq
	sort date_stata
	local xlabels ""
	forvalues q = `=tq(`start_date')'/`=tq(2019q4)' {
		if mod(`q', 4) == 1 | mod(`q', 4) == 3 { 
			local xlabels "`xlabels' `q'"
		}
	}
	
	*Create a trend plot
	capture graph drop `dataset'_trends_`g'
	twoway ///
	(connected `outcome' date_stata if date_stata >= tq(`start_date') & date_stata ///
	<= tq(2019q4) & `g' == 1, sort lwidth(medium) ///
		lcolor("26 133 255") mcolor("26 133 255") msymbol(circle)) ///
	(connected `outcome' date_stata if date_stata >= tq(`start_date') & ///
	date_stata <= tq(2019q4) & `g' == 0, sort lwidth(medium) ///
		lcolor("212 17 89") mcolor("212 17 89") msymbol(circle) ///
		lpattern(dash)), ///
	legend(order(1 "EU Average Registrations" 2 "Non-EU Average Registrations" ) ///
		position(6) cols(2) region(style(none) lstyle(solid) lcolor(black) ///
		lwidth(medium))) ///
	legend(size(small) colgap(vsmall) keygap(small)) ///
    xline(`=tq(2016,2) + (23/91)', lcolor(black) lpattern(dash)) ///
    xlabel(`xlabels', format(%tq) angle(0) labsize(vsmall)) ///
    xlabel(, grid) ///
    xtitle("") ytitle("Mean NINo Registrations per Unit (Country)") ///
    ylabel(, angle(0)) ///
	title("`title_label'", ///
		tstyle(heading) color(black) position(12) ring(100)) ///
    graphregion(color(white)) ///
	bgcolor(white) ///
    plotregion(color(white)) ///
	name(`dataset'_trends_`g')	
	
	*Save the graphs
	cd "$master_thesis/Graphs"
	graph export "`dataset'_trends_`g'.pdf", replace as (pdf)	
	
	restore
}
********************************************************************************

*===============================================================================
*Figure 16: NINo -- Pre- and Post-Reform Trajectories of Total Registrations for 
*Different Group Compositions
*===============================================================================

****************************************
**Computations and Figure Construction**
****************************************

*Locals
local dataset "NIno"
local group group_clifton group_simon
local unit "c_subgroup"
local outcome nino_total
local start_date 2014q4

cd "$master_thesis/Data/`dataset'"
use `dataset'_panel.dta, clear
estimates clear 

foreach g of local group {
	
	preserve 
	
	keep if date_stata >= tq(`start_date')
	
	collapse (sum) `outcome' , by(date_stata `g')
	drop if `g' == .
	
	local title_label "NINo Registrations"
	di "`title_label'"
	
	*Format the date and sort the data accordingly
	format date_stata %tq
	sort date_stata
	local xlabels ""
	forvalues q = `=tq(`start_date')'/`=tq(2019q4)' {
		if mod(`q', 4) == 1 | mod(`q', 4) == 3 { 
			local xlabels "`xlabels' `q'"
		}
	}
	
	*Create a trend plot
	capture graph drop `dataset'_sum_trends_`g'
	twoway ///
	(connected `outcome' date_stata if date_stata >= tq(`start_date') & date_stata ///
	<= tq(2019q4) & `g' == 1, sort lwidth(medium) ///
		lcolor("26 133 255") mcolor("26 133 255") msymbol(circle)) ///
	(connected `outcome' date_stata if date_stata >= tq(`start_date') & ///
	date_stata <= tq(2019q4) & `g' == 0, sort lwidth(medium) ///
		lcolor("212 17 89") mcolor("212 17 89") msymbol(circle) ///
		lpattern(dash)), ///
	legend(order(1 "EU Registrations" 2 "Non-EU Registrations" ) ///
		position(6) cols(2) region(style(none) lstyle(solid) lcolor(black) ///
		lwidth(medium))) ///
	legend(size(small) colgap(vsmall) keygap(small)) ///
    xline(`=tq(2016,2) + (23/91)', lcolor(black) lpattern(dash)) ///
    xlabel(`xlabels', format(%tq) angle(0) labsize(vsmall)) ///
    xlabel(, grid) ///
    xtitle("") ytitle("NINo Registrations in Thousands") ///
    ylabel(, angle(0)) ///
	title("`title_label'", ///
		tstyle(heading) color(black) position(12) ring(100)) ///
    graphregion(color(white)) ///
	bgcolor(white) ///
    plotregion(color(white)) ///
	name(`dataset'_sum_trends_`g')	
	
	*Save the graphs
	cd "$master_thesis/Graphs"
	graph export "`dataset'_sum_trends_`g'.pdf", replace as (pdf)	
	
	restore
}
********************************************************************************

log close
cd $my_path_do 
*===============================================================================
*===============================================================================
