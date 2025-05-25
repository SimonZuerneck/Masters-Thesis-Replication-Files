*===============================================================================
*===============================================================================

*NINo Robustness Checks
*Simon Zuerneck 
*Stata Version: 18.0
********************************************************************************

clear all
set more off 
capture log close 
cd $master_thesis
log using Log\4_4_NINo_Robustness_Checks_Zuerneck.log, text replace 
********************************************************************************

*===============================================================================
*Robustness Check: Increasing the Number of Pre-Treatment Periods for NINo
*(Unit of analysis: country)
*(Estimates based on a balanced panel)
*(No controls, not log-transformed)
*(Inference: cluster (DiD) block bootstrap (SDiD))
*===============================================================================

*===============================================================================
*Figure 11: NINo Extended -- Pre- and Post-Reform Trajectories
*===============================================================================

****************************************
**Computations and Figure Construction**
****************************************

*Locals
local dataset "NIno"
local group group_clifton group_simon
local unit "c_subgroup"
local outcome nino_total
local start_date 2012q4

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
	legend(order(1 "EU Registrations" 2 "Non-EU Registrations" ) ///
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
	graph export "`dataset'_extended_trends_`g'.pdf", replace as (pdf)	
	
	restore
}
********************************************************************************

*===============================================================================
*Table 9: NINo Extended -- Pooled DiD and SDiD Specification Estimates
*===============================================================================	

estimates clear 
drop _all

*Locals
local dataset "NINo"
local group group_simon group_clifton 
local unit "country"
local outcome nino_total
local start_date 2012q4

*Temporary dataset to store results
cd "$master_thesis/Data/`dataset'"
use `dataset'_panel.dta, clear
capture erase "resultsfile.dta"
tempname results
postfile `results' str6(model) str15(group) str15(period) ///
    str15(coef_stars) float(r2 n pval tstat) str25(ci) using resultsfile.dta, replace
********************************************************************************

****************
**Computations**
****************

*DiD Approach 
foreach t in 2018q1 2019q4 {

    foreach g of local group {
		
        preserve 
		
		*Local for the Model
		local model ""
		local model "DiD"
		
		use `dataset'_panel.dta, clear
	
		collapse (sum) `outcome' (first) `g' c_`unit'  , by(date_stata `unit')
		drop if `g' == .
		
        keep if date_stata >= tq(`start_date') & date_stata <= tq(`t')
		
		*Gen treated post indicator 
		gen treated_post = (`g' == 1 & date_stata >= tq(2016q3))
		
		*Missing values do not work for sdid (replace them with 0)
		replace `outcome' = 0 if `outcome' == .
		
		*DiD Regression - clustering at the unit level
		di "DiD Regression - clustering at the unit level"
		reg `outcome' treated_post i.c_`unit' i.date_stata, ///
			vce(cluster c_`unit') 
	
		*Store results
		matrix results = r(table)
		local r2 = e(r2)
		local n = e(N)
		local coef = results[1,1]
		local pval = results[4,1]
		local tstat = results[3.,1]
		local lb_cluster = results[5,1]   
		local ub_cluster = results[6,1]   
		
		*Format the CIs
		local lb_fmt = trim("`: display %9.2f `lb_cluster''")
		local ub_fmt = trim("`: display %9.2f `ub_cluster''")
		local ci = "[`lb_fmt', `ub_fmt']"
		
		*Generate significance stars 
		local stars = ""
        if `pval' < 0.01 {
            local stars = "***"
        }
        else if `pval' < 0.05 {
            local stars = "**"
        }
        else if `pval' < 0.1 {
            local stars = "*"
        }
		local coef_fmt = trim("`: display %9.3f `coef''")
		local coef_stars = "`coef_fmt'`stars'"
		
		
		post `results' ("`model'") ("`g'") ("`t'") ("`coef_stars'") (`r2') (`n') ///
			(`pval') (`tstat') ("`ci'")
		
		restore 
	}	
}

*SDiD approach
foreach t in 2018q1 2019q4 {
	
	foreach g of local group {
		
        preserve 
		
		*Local for the Model
		local model ""
		local model "SDiD"
		
		use `dataset'_panel.dta, clear
	
		collapse (sum) `outcome' (first) `g', by(date_stata `unit')
		drop if `g' == .
		
		*start_date
		if "`g'" == "group_simon" {
			local start_date 2014q1 // 10 pre-treatment periods considered
		}
		else if "`g'" == "group_clifton" {
			local start_date 2014q1 // 10 pre-treatment periods considered
		}
		
        keep if date_stata >= tq(`start_date') & date_stata <= tq(`t')
		
		*Gen treated post indicator 
		gen treated_post = (`g' == 1 & date_stata >= tq(2016q3))
		
		*Missing values do not work for sdid (replace them with 0)
		replace `outcome' = 0 if `outcome' == .
		
		*SDiD Regressions
        sdid `outcome' `unit' date_stata treated_post, method(sdid) ///
		vce(bootstrap) seed(1213) reps(1000)
			
		*Grap the results
		local att = e(ATT)
		local se = e(se)
		local n = e(N)
		local df = `n' - 1 // calculate degrees of freedom 

		*Compute t-stat and p-value (as they are not erturned)
		local tstat = `att'/`se'
		local pval = 2 * ttail(`df', abs(`tstat'))

		*Compute 95% CI manually and format it 
		local lb = `att' - invttail(`df', 0.025) * `se'
		local ub = `att' + invttail(`df', 0.025) * `se'
		local lb_fmt = trim("`: display %9.2f `lb''")
		local ub_fmt = trim("`: display %9.2f `ub''")
		local ci = "[`lb_fmt', `ub_fmt']"

		*Generate significance stars 
		local stars = ""
		if `pval' < 0.01 {
			local stars = "***"
		}
		else if `pval' < 0.05 {
			local stars = "**"
		}
		else if `pval' < 0.1 {
			local stars = "*"
		}
		local coef_fmt = trim("`: display %9.3f `att''")
		local coef_stars = "`coef_fmt'`stars'"
				
		*Store results 	
		post `results' ("`model'") ("`g'") ("`t'") ("`coef_stars'") (.) (`n') ///
			(`pval') (`tstat') ("`ci'")
		
       restore
    }
		
}
postclose `results'
********************************************************************************	
	

****************************
**Latex Table Construction**
****************************

use resultsfile.dta, clear

*Locals 
local dataset "NINo"
local group group_simon group_clifton

cd "$master_thesis/Data/`dataset'"
use resultsfile.dta, clear


*Open table
cd "$master_thesis/Tables"
capture file close table
file open table using "`dataset'_extended_DiD_SDiD_pooled.tex", write replace

*Write the header 
file write table "\begin{tabular}{llcc}" _n
file write table "\hline\midrule" _n
file write table "& & Composition A & Composition B\\" _n
file write table "& & (1) & (1) \\" _n
file write table "Period & & NINo Registrations & NINo Registrations\\" _n
file write table "\cmidrule(l){1-3} \cmidrule(l){4-4}" _n

*Loop through the obtained results
foreach t in 2018q1 2019q4 {
	
	
	foreach s in DiD SDiD {
	
		*Period
		file write table "2016q3--`t'" 
		file write table " \\[0.3em]" _n
	
		*Specification
		if "`s'" == "DiD"  {
			file write table "\multirow{5}{*}{\hspace{1em}\textit{DiD}}"
		}
		else if "`s'" == "SDiD" {
			file write table "\multirow{4}{*}{\hspace{1em}\textit{SDiD}}"
		}
		
		*Coefficient with stars
		file write table " & ATT" 
		foreach g of local group  {
			levelsof coef_stars if model == "`s'" & group == "`g'" ///
			& period == "`t'", local(bvals)
			local b : word 1 of `bvals'
			file write table " & `b'"
		}
		file write table " \\" _n

		*P-values
		file write table " & p-value"
		foreach g of local group {
			qui su pval if model == "`s'" & group == "`g'" ///
			& period == "`t'", meanonly
			local p = string(r(mean), "%9.3f")
			file write table " & (`p')"
		}
		file write table " \\" _n

		*Confidence intervals
		file write table " & 95\% CI"
		foreach g of local group {
			qui levelsof ci if model == "`s'" & group == "`g'" ///
			& period == "`t'", local(civals)
			local c : word 1 of `civals'
			file write table " & `c'"
		}
		file write table " \\" _n

		*Number of observations
		file write table " & N"
		foreach g of local group {
			su n if model=="`s'" & group =="`g'" & period=="`t'", meanonly
			local n = string(r(mean),"%9.0f")
			file write table " & `n'"
		}
		file write table " \\" _n
		
		*R^2 only for DiD
		if "`s'" == "DiD" {
			file write table "& R\textsuperscript{2}"
			foreach g of local group {
				su r2 if model=="`s'" & group =="`g'" & period=="`t'", meanonly
				local r2 = string(r(mean),"%9.3f")
				file write table " & `r2'"			
			}
			file write table " \\" _n
			file write table "\cmidrule(l){2-3} \cmidrule(l){4-4}" _n
		}

	}
	
	*Mid-formating 
	if "`t'" == "2018q1" file write table "\cmidrule(l){1-3} \cmidrule(l){4-4}" _n
}

*Write the closer 
file write table "\hline\hline" _n
file write table "\end{tabular}" _n
file close table
********************************************************************************

*===============================================================================
*Figure 12: NINo Extended -- Unit-Weighting for Different Group Compositions
*===============================================================================

estimates clear 
drop _all

*Locals
local dataset "NINo"
local group group_simon group_clifton
local outcome nino_total
local model "SDiD"
local unit "country"

cd "$master_thesis/Data/`dataset'"
use `dataset'_panel.dta, clear
********************************************************************************

****************
**Computations**
****************
 
foreach g of local group {
	
	preserve 
	
	collapse (sum) `outcome' (first) `g', by(date_stata `unit')
	drop if `g' == .
	
		*start_date
		if "`g'" == "group_simon" {
			local start_date 2014q1 // 10 pre-treatment periods considered
		}
		else if "`g'" == "group_clifton" {
			local start_date 2014q1 // 10 pre-treatment periods considered
		}

	*Gen treated post indicator 
	gen treated_post = (`g' == 1 & date_stata >= tq(2016q3))

	keep if date_stata >= tq(`start_date')
	
	local title_label "NINo Registrations"

	*Missing values do not work for sdid (replace them with 0)
	replace `outcome' = 0 if `outcome' == .
	
	*Generate the x-axis labels for the time_w plot
	local xlabels ""
	forvalues q = `=tq(`start_date')'/`=tq(2019q4)' {
		if mod(`q', 4) == 0 | mod(`q', 4) == 2 { 
			local xlabels "`xlabels' `q'"
		}
	}
	
	*SDiD Regressions	
	capture graph drop `dataset'_`model'_time_w_`g'
	capture graph drop `dataset'_`model'_unit_w_`g'
	sdid `outcome' `unit' date_stata treated_post, method(sdid) ///
	vce(noinference) graph g1on ///
	g1_opt( ///
		msize(large) ///
		xlabel(, noticks nolabels) ///
		xtitle("") ///
	)
	
	*Rename and save the graphs
	cd "$master_thesis/Graphs"
	graph display g1_226
	graph rename g1_226 `dataset'_`model'_unit_w_`g'
	graph export "`dataset'_extended_`model'_unit_w_`g'.pdf", replace as (pdf)
	
	
	restore 
}
********************************************************************************

log close
cd $my_path_do
*===============================================================================
*===============================================================================