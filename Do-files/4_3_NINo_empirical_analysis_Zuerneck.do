*===============================================================================
*===============================================================================

*NINo Empirical Analysis
*Simon Zuerneck 
*Stata Version: 18.0
********************************************************************************

clear all
set more off 
capture log close 
cd $master_thesis
log using Log\4_3_NINo_empirical_analysis_Zuerneck.log, text replace 
********************************************************************************

*===============================================================================
*Some testing 
*===============================================================================

*Locals
local dataset "NINo"
local group group_simon group_clifton
local unit "country"
local outcome nino_total

foreach g of local group {
	
	preserve 
	
	cd "$master_thesis/Data/`dataset'"
	use `dataset'_panel.dta, clear

	collapse (sum) `outcome' (first) `g' c_`unit'  , by(date_stata `unit')
	drop if `g' == .
	
	xtset c_country date_stata

	gen treated_post = (`g' == 1 & date_stata >= tq(2016q3))

	*Testing for first-order serial correlation (auto correlation) in the error
	*term 
	xtserial `outcome' `g' treated_post date_stata	
	di "Null hypothesis of no first-order serial correlation rejected at conventional significance levels"
	*Serial Correlation very, very (!) likely
	
	di "Regression with fixed effects included manually"
	reg `outcome' treated_post i.c_`unit' i.date_stata
	*estat hettest 
	
	*Testing for a violation of the homoscedasticity assumption
	di ""
	di ""
	di "Regression with unit-fixed effects included automatically"
	xtreg `outcome' treated_post i.date_stata, fe
	xttest3
	di "Null hypothesis of groupwise homoscedasticity rejected at conventional significance levels"
	
	restore 
}
********************************************************************************

*===============================================================================
*Canonical Difference-in-Differences
*(Unit of analysis: country)
*(Estimates based on a balanced panel)
*(No controls, not log-transformed)
*(Inference: cluster robust)
*===============================================================================

*===============================================================================
*Table 4: NINo -- Pooled DiD Specification Estimates
*===============================================================================	

estimates clear 
drop _all

*Locals
local dataset "NINo"
local group group_simon group_clifton
local unit "country"
local outcome nino_total
local model "DiD"
local start_date 2014q4
local inference "cluster"

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
		local t_stat = results[3.,1]
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
			(`pval') (`t_stat') ("`ci'")
		
		restore 
	}
}
postclose `results'
********************************************************************************

****************************
**Latex Table Construction**
****************************

use resultsfile.dta, clear

*Open table
cd "$master_thesis/Tables"
capture file close table
file open table using "`dataset'_`model'_pooled_`inference'.tex", write replace

*Write the header 
file write table "\begin{tabular}{llcc}" _n
file write table "\hline\midrule" _n
file write table "& & Composition A & Composition B\\" _n
file write table "& & (1) & (1) \\" _n
file write table "Period & & NINo Registrations & NINo Registrations\\" _n
file write table "\cmidrule(l){1-3} \cmidrule(l){4-4}" _n

*Loop through the obtained results
foreach t in 2018q1 2019q4 {
	
	*Period
	file write table "2016q3--`t'" 
	file write table " \\[0.3em]" _n
	
	*Coefficient with stars
	file write table " & ATT" 
	foreach g of local group  {
		qui levelsof coef_stars if model == "`model'" & group == "`g'" ///
		& period == "`t'", local(bvals)
		local b : word 1 of `bvals'
		file write table " & `b'"
	}
	file write table " \\" _n

	*P-values
	file write table " & p-value"
	foreach g of local group {
		qui su pval if model == "`model'" & group == "`g'" ///
		& period == "`t'", meanonly
		local p = string(r(mean), "%9.3f")
		file write table " & (`p')"
	}
	file write table " \\" _n

	*Confidence intervals
	file write table " & 95\% CI"
	foreach g of local group {
		qui levelsof ci if model == "`model'" & group == "`g'" ///
		& period == "`t'", local(civals)
		local c : word 1 of `civals'
		file write table " & `c'"
	}
	file write table " \\" _n

	*Number of observations
    file write table " & N"
    foreach g of local group {
        su n if model=="`model'" & group =="`g'" & period=="`t'", meanonly
        local n = string(r(mean),"%9.0f")
        file write table " & `n'"
    }
    file write table " \\" _n
	
	*R^2 
    file write table "& R\textsuperscript{2}"
    foreach g of local group {
        su r2 if model=="`model'" & group =="`g'" & period=="`t'", meanonly
        local r2 = string(r(mean),"%9.3f")
        file write table " & `r2'"
    }
    file write table "\\" _n

	*Mid-formating 
    if "`t'" == "2018q1" file write table "\cmidrule(l){1-3} \cmidrule(l){4-4}" _n
}

*Write the closer 
file write table "\hline\hline" _n
file write table "\end{tabular}" _n
file close table
********************************************************************************

*===============================================================================
*Figure 4: NINo -- Estimated Event Study Coefficients Using DiD
*==============================================================================3

estimates clear 
drop _all

*Locals
local dataset "NINo"
local group group_simon group_clifton 
local unit "country"
local outcome nino_total
local model "DiD"
local start_date 2014q4
local inference "cluster"

*Temporary dataset to store results
cd "$master_thesis/Data/`dataset'"
capture erase "resultsfile.dta"
tempname results
postfile `results' str6(model) str15(group) str15(coef_stars) ///
	float(period r2 n pval tstat) str25(ci) using resultsfile.dta, replace
********************************************************************************

****************
**Computations**
****************

foreach g of local group {
		
	preserve 
	
	cd "$master_thesis/Data/`dataset'"
	use `dataset'_panel.dta, clear
	
	collapse (sum) `outcome' (first) `g' c_`unit'  , by(date_stata `unit')
	drop if `g' == .
	
	keep if date_stata >= tq(`start_date') 
	tab date_stata
		
	*Gen treated post indicator 
	gen treated_post = (`g' == 1 & date_stata >= tq(2016q3))
		
	*Missing values do not work for sdid (replace them with 0)
	replace `outcome' = 0 if `outcome' == .

	*Generate a simpler date indicator
	egen date_ind = group(date_stata) 
	
	*Reference period (omitted due to multicollinearity) 
	*Reference period is the period right before the referendum 
	* --> 2016q2 <=> 7
	local reference 7
	
	*Event study DiD regression
	reg `outcome' i.`g'##ib(7).date_ind i.c_`unit' ///
	i.date_ind, vce(cluster c_`unit') 
	****************************************************************************

	***********************
	**Figure Construction**
	***********************
	
	*Get R^2 and N 
	local r2 = e(r2)
	local n = e(N)
	
	*ereturn list
	*return list
	*matrix list r(table)	
	matrix R = r(table)

	*Loop over interaction terms (skip period 15 = ref)
	local lower = `reference' - 1
	local upper = `reference' + 1
	foreach k of numlist 1/`lower' `upper'/21 {
		
		display "Interaction term processed for date_ind: `i'"

		local term = "1.`g'#`k'.date_ind"
		
		*column index of the interaction term
		local col = colnumb(R, "`term'")
		di "This is the column: `col'"
		
		*Get results
		local coef   = R[1, `col']
		di "Event study coefficient for `term': `coef'"
		local t_stat = R[3, `col']
		local p_val  = R[4, `col']
		local lb    = R[5, `col']
		local ub    = R[6, `col']
		di "Lower boud CI for `term': `lb'"
		
		*Format the CIs 
		local lb_fmt = trim("`: display %9.2f `lb''")
		local ub_fmt = trim("`: display %9.2f `ub''")
		local ci = "[`lb_fmt', `ub_fmt']"
		
		*Generate significance stars 
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
		local coef_fmt = trim("`: display %9.3f `coef''")
		local coef_stars = "`coef_fmt'`stars'"
		
		post `results' ("`model'") ("`g'") ("`coef_stars'") (`k') ///
			(`r2') (`n') (`p_val') (`t_stat') ("`ci'")
		
	}
	
	*Store estimation results that should not be displayed in a local 
	levelsof date_ind, local(levels)
		local timefixed ""
		foreach v of local levels {
		local timefixed "`timefixed' `v'.date_ind"	
	}
	
	display "`timefixed'"
	
	levelsof c_`unit', local(levels)
	local groupfixed ""
		foreach v of local levels {
		local groupfixed "`groupfixed' `v'.c_`unit' "	
	}

	display "`groupfixed'"

	*Local for the title label 
	local title_label "NINo Registrations"
	di "`title_label'"

	*Local for the coefficent labels
	local coef_labels ""

	local first = tq(2014q4) 

	*Loop through all period indices 
	forvalues i = 1/21 {
		local this_q = `first' + (`i' - 1) 
		local qlabel : display %tq `this_q'
    
		*Append coefficient label mapping
		local coef_labels "`coef_labels' `i' = "EU x `qlabel'""
		
		*Separate coefficients before and after the policy change 
        if `i' <= 7 {
            local pre_coef "`pre_coef' `i'"
        }
        else {
            local post_coef "`post_coef' `i'"
        }
	
	}

	*Generate the Graph
	capture graph drop `dataset'_`g'_ev_`model'
	coefplot, ///
		color("0 191 127") ciopt(color("0 191 127" "0 191 127") ///
		lwidth(thin thin) recast(. rcap)) ///
		vertical drop(_cons 1.`g' `groupfixed' `timefixed') levels(95 90) /// 
		rename(^1.`g'#([0-9]+).date_ind$ = \1, regex) ///
		coeflabels(`coef_labels', format(%tq) angle(90) labsize(vsmall)) ///
		yline(0, lcolor(red) lpattern(dash)) ///
		legend(order(3 "Point Estimate" 2 "90% CI" 1 "95% CI")  ///
			position(6) cols(3) region(style(none) lstyle(solid) lcolor(black) ///
			lwidth(medium))) ///
		xline(6.5, lcolor(black) lpattern(dash)) ///
		xtitle("") ///
		ytitle("Estimated DiD Coefficients") ///
		xlabel(, grid) ///
		ylabel(, angle(0)) ///
		title("`title_label'", ///
		tstyle(heading) color(black) position(12) ring(100)) ///
		graphregion(color(white)) ///
		bgcolor(white) ///
		plotregion(color(white)) ///
		name(`dataset'_`g'_ev_`model')
	
	*Save the graph
	cd "$master_thesis/Graphs"
	graph export "`dataset'_`model'_event_study_`g'.pdf", replace as (pdf)

	
	restore 
}	
cd "$master_thesis/Data/`dataset'"
postclose `results'
********************************************************************************
	
****************************
**Latex Table Construction**  (supplementory)
****************************

*Locals
local dataset "NINo"
local group group_simon group_clifton 
local unit "country"
local outcome nino_total
local model "DiD"
local start_date 2014q4
local inference "cluster"


*Table for the post-reform periods: 
cd "$master_thesis/Data/`dataset'"
use resultsfile.dta, clear
keep if period > 6
gen date_stata = tq(2016q3) + period - 8
format date_stata %tq
gen quarter = string(date_stata, "%tq")

*Open table
cd "$master_thesis/Tables"
capture file close table
file open table using "`dataset'_`model'_event_post_`inference'.tex", write replace

*Write the header 
file write table "\begin{tabular}{l@{\hskip 22pt}ccc}" _n
file write table "\hline\midrule" _n
file write table "& Composition A & Composition B \\" _n
file write table "& (1) & (1) \\" _n
file write table "& NINo Registrations & NINo Registrations \\" _n
file write table "\cmidrule(l){1-2} \cmidrule(l){3-3}" _n

*Loop through the obtained results (14 post-reform quarters )
levelsof quarter, local(quarters)
foreach t of local quarters {
    
    *Interaction term coefficient with stars
    file write table "EU x `t'"
    foreach g of local group {
        capture levelsof coef_stars if model == "`model'" & group == "`g'" ///
		& quarter == "`t'", local(bvals)
        if _rc == 0 {
            local b : word 1 of `bvals'
            file write table "& `b'"
        }
        else file write table "& ."
    }
    file write table " \\" _n
    
    *P-values
    file write table "p-value"
    foreach g of local group {
        qui su pval if model == "`model'" & group == "`g'" &  ///
		quarter == "`t'", meanonly
        local p = string(r(mean), "%9.3f")
        file write table " & (`p')"
    }
    file write table " \\" _n

    *Confidence Intervals
    file write table "95\% CI"
    foreach g of local group {
        capture levelsof ci if model == "`model'" & group == "`g'" & ///
		quarter == "`t'", local(civals)
        local c : word 1 of `civals'
        file write table " & `c'"
    }
    file write table " \\[0.5em]" _n

    *N only at last quarter
    if "`t'" == "2019q4" {
        file write table "\cmidrule(l){1-2} \cmidrule(l){3-3}" _n
        file write table "N"
        foreach g of local group {
            su n if model=="`model'" & group=="`g'" & quarter=="`t'", meanonly
            local n = string(r(mean),"%9.0f")
            file write table " & `n'"
        }
        file write table " \\" _n
    }
	
    *R2  only at last quarter
    if "`t'" == "2019q4" {
        file write table "\cmidrule(l){1-2} \cmidrule(l){3-3}" _n
        file write table "R\textsuperscript{2}"
        foreach g of local group {
            su r2 if model=="`model'" & group=="`g'" & quarter=="`t'", meanonly
            local r2 = string(r(mean),"%9.3f")
            file write table " & `r2'"
        }
        file write table " \\" _n
    }
	
	
	
}

*Write the closer 
file write table "\hline\hline" _n
file write table "\end{tabular}" _n
file close table	
********************************************************************************

*Table for pre-reform quarters 
cd "$master_thesis/Data/`dataset'"
use resultsfile.dta, clear
keep if period <= 7
gen date_stata = tq(2016q3) + period - 8
format date_stata %tq
gen quarter = string(date_stata, "%tq")

*Open table
cd "$master_thesis/Tables"
capture file close table
file open table using "`dataset'_`model'_event_pre_`inference'.tex", write replace

*Write the header 
file write table "\begin{tabular}{l@{\hskip 22pt}ccc}" _n
file write table "\hline\midrule" _n
file write table "& Composition A & Composition B \\" _n
file write table "& (1) & (1) \\" _n
file write table "& NINo Registrations & NINo Registrations \\" _n
file write table "\cmidrule(l){1-2} \cmidrule(l){3-3}" _n

*Loop through the obtained results (14 post-reform quarters )
levelsof quarter, local(quarters)
foreach t of local quarters {
    
    *Interaction term coefficient with stars
    file write table "EU x `t'"
    foreach g of local group {
        capture levelsof coef_stars if model == "`model'" & group == "`g'" ///
		& quarter == "`t'", local(bvals)
        if _rc == 0 {
            local b : word 1 of `bvals'
            file write table "& `b'"
        }
        else file write table "& ."
    }
    file write table " \\" _n
    
    *P-values
    file write table "p-value"
    foreach g of local group {
        qui su pval if model == "`model'" & group == "`g'" &  ///
		quarter == "`t'", meanonly
        local p = string(r(mean), "%9.3f")
        file write table " & (`p')"
    }
    file write table " \\" _n

    *Confidence Intervals
    file write table "95\% CI"
    foreach g of local group {
        capture levelsof ci if model == "`model'" & group == "`g'" & ///
		quarter == "`t'", local(civals)
        local c : word 1 of `civals'
        file write table " & `c'"
    }
    file write table " \\[0.5em]" _n

    *N only at last quarter
    if "`t'" == "2016q1" {
        file write table "\cmidrule(l){1-2} \cmidrule(l){3-3}" _n
        file write table "N"
        foreach g of local group {
            su n if model=="`model'" & group=="`g'" & quarter=="`t'", meanonly
            local n = string(r(mean),"%9.0f")
            file write table " & `n'"
        }
        file write table " \\" _n
    }
	
    *R2 only at last quarter
    if "`t'" == "2016q1" {
        file write table "\cmidrule(l){1-2} \cmidrule(l){3-3}" _n
        file write table "R\textsuperscript{2}"
        foreach g of local group {
            su r2 if model=="`model'" & group=="`g'" & quarter=="`t'", meanonly
            local r2 = string(r(mean),"%9.3f")
            file write table " & `r2'"
        }
        file write table " \\" _n
    }
	
	
	
}

*Write the closer 
file write table "\hline\hline" _n
file write table "\end{tabular}" _n
file close table	
********************************************************************************

*===============================================================================
*Synthetic Difference-in-Differences 
*(Unit of analysis: country)
*(Estimates based on a balanced panel)
*(No controls, not log-transformed)
*(Inference: block bootstrap)
*===============================================================================

*===============================================================================
*Table 6: NINo -- Pooled SDiD Specification Estimates
*===============================================================================

estimates clear 
drop _all

*Locals
local dataset "NINo"
local group group_simon group_clifton
local unit "country"
local outcome nino_total
local model "SDiD"
local inference "bootstrap"

*Temporary dataset to store results
cd "$master_thesis/Data/`dataset'"
capture erase "resultsfile.dta"
tempname results
postfile `results' str6(model) str15(group) str15(period) ///
    str15(coef_stars) float(n pval) str25(ci) using resultsfile.dta, replace
********************************************************************************

****************
**Computations**
****************

*SDiD Approach 
foreach t in 2018q1 2019q4 {
	
    foreach g of local group {
		
        preserve 
		
		use `dataset'_panel.dta, clear
	
		collapse (sum) `outcome' (first) `g', by(date_stata `unit')
		drop if `g' == .
		
		*Start date
		if "`g'" == "group_simon" {
			local start_date 2015q1 // 6 pre-treatment periods considered
		}
		else if "`g'" == "group_clifton" {
			local start_date 2015q1 // 6 pre-treatment periods considered
		}
		
        keep if date_stata >= tq(`start_date') & date_stata <= tq(`t')
		
		*Gen treated post indicator 
		gen treated_post = (`g' == 1 & date_stata >= tq(2016q3))
		
		*Missing values do not work for sdid (replace them with 0)
		replace `outcome' = 0 if `outcome' == .
		
		*SDiD Regressions
        sdid `outcome' `unit' date_stata treated_post, method(sdid) ///
		vce(`inference') seed(1213) reps(1000)
			
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
		post `results' ("`model'") ("`g'") ("`t'") ("`coef_stars'") (`n') ///
			(`pval') ("`ci'")
		
       restore
    }
}
postclose `results'
********************************************************************************
	
****************************
**Latex Table Construction** 
****************************

use resultsfile.dta, clear

*Open table
cd "$master_thesis/Tables"
capture file close table
file open table using "`dataset'_`model'_pooled_`inference'.tex", write replace

*Write the header 
file write table "\begin{tabular}{llcc}" _n
file write table "\hline\midrule" _n
file write table "& & Composition A & Composition B \\" _n
file write table "& & (1) & (2) \\" _n
file write table "Period & & NINo Registrations & NINo Registrations\\" _n
file write table "\cmidrule(l){1-3} \cmidrule(l){4-4}" _n

*Loop through the obtained results
foreach t in 2018q1 2019q4 {
	
	*Period
	file write table "2016q3--`t'" 
	file write table " \\[0.3em]" _n
	
	*Coefficient with stars
	file write table " & ATT" 
	foreach g of local group  {
		qui levelsof coef_stars if model == "`model'" & group == "`g'" ///
		& period == "`t'", local(bvals)
		local b : word 1 of `bvals'
		file write table " & `b'"
	}
	file write table " \\" _n

	*P-values
	file write table " & p-value"
	foreach g of local group {
		qui su pval if model == "`model'" & group == "`g'" ///
		& period == "`t'", meanonly
		local p = string(r(mean), "%9.3f")
		file write table " & (`p')"
	}
	file write table " \\" _n

	*Confidence intervals
	file write table " & 95\% CI"
	foreach g of local group {
		qui levelsof ci if model == "`model'" & group == "`g'" ///
		& period == "`t'", local(civals)
		local c : word 1 of `civals'
		file write table " & `c'"
	}
	file write table " \\" _n

	*Number of observations
    file write table " & N"
    foreach g of local group {
        su n if model=="`model'" & group =="`g'" & period=="`t'", meanonly
        local n = string(r(mean),"%9.0f")
        file write table " & `n'"
    }
    file write table " \\" _n

	*Mid-formating 
    if "`t'" == "2018q1" file write table "\cmidrule(l){1-3} \cmidrule(l){4-4}" _n
}

*Write the closer 
file write table "\hline\hline" _n
file write table "\end{tabular}" _n
file close table
********************************************************************************

*===============================================================================
*Figure 5: NINo -- Period- and Unit-Weighting for Different Group Compositions
*===============================================================================

estimates clear 
drop _all

*Locals
local dataset "NINo"
local group group_simon group_clifton
local outcome nino_total
local model "SDiD"
local unit "country"

*Temporary dataset to store results for the top 10 unit weighting table 
cd "$master_thesis/Data/`dataset'"
capture erase "resultsfile.dta"
tempname results
postfile `results' str50(country_simon) float(u_w_simon) ///
	str50(country_clifton) float(u_w_clifton) using resultsfile.dta, replace

use `dataset'_panel.dta, clear
********************************************************************************

****************
**Computations**
****************
 
foreach g of local group {
	
	preserve 
	
	collapse (sum) `outcome' (first) `g', by(date_stata `unit')
	drop if `g' == .
	
	*Start date
	if "`g'" == "group_simon" {
        local start_date 2015q1 // 6 pre-treatment periods considered
    }
    else if "`g'" == "group_clifton" {
        local start_date 2015q1 // 6 pre-treatment periods considered
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
	vce(noinference) returnweights graph g1on ///
	g2_opt( ///
		legend(order(2 "EU Registrations" 1 "Non-EU Registrations" ) ///
			position(6) cols(2) region(style(none) lstyle(solid) ///
			lcolor(black) lwidth(medium))) ///
		legend(size(small) colgap(vsmall) keygap(small)) ///
		xlabel(`xlabels', format(%tq) angle(0) grid labsize(vsmall)) ///
		xline(`=tq(2016,2) + (23/91)', lcolor(black) lpattern(dash)) ///
		xtitle("") ytitle("Mean NINo Registrations per Unit (Country)") ///
		ylabel(, angle(0)) ///
		title(" ", size(medsmall)) ///
		graphregion(color(white)) ///
		bgcolor(white) ///
		plotregion(color(white)) ///
	) ///
	g1_opt( ///
		msize(large) ///
		xlabel(, noticks nolabels) ///
		xtitle("") ///
	)
	
	*Rename and save the graphs
	cd "$master_thesis/Graphs"
	graph display g2_226
	graph rename g2_226 `dataset'_`model'_time_w_`g'
	graph export "`dataset'_`model'_time_w_`g'.pdf", replace as (pdf)
	graph display g1_226
	graph rename g1_226 `dataset'_`model'_unit_w_`g'
	graph export "`dataset'_`model'_unit_w_`g'.pdf", replace as (pdf)
	
	*Store countries with their unit weights for each composition in the postfile:
	keep if `g' == 0 
	collapse (first) omega226, by(country) 
	cd "$master_thesis/Data/`dataset'"
	quietly {
		forvalues i = 1/`=_N' {
			if "`g'" == "group_simon"  {
				post `results' (country[`i']) (omega226[`i']) ("" ) (.)
			}
			else if "`g'" == "group_clifton" {
				post `results' ("" ) (.) (country[`i']) (omega226[`i'])
			}
		}
	}
	
	restore 
}
postclose `results'
********************************************************************************

*===============================================================================
*Table 13: NINo -- Top 20 Donor Countries in the SDiD Synthetic Control Groups
*===============================================================================

*Code for Figure 5 must run before this table can be generated 
local dataset "NINo"
cd "$master_thesis/Data/`dataset'"
use resultsfile.dta, clear

*Keep only top-ten countries by weight 
foreach v in "simon" "clifton" {
	gen keep_`v' = 0
	gsort -u_w_`v'
	quietly replace keep_`v' = 1 in 1/20
}
keep if keep_simon == 1 | keep_clifton == 1
drop keep_simon keep_clifton
********************************************************************************

****************************
**Latex Table Construction**
****************************

*Open table
cd "$master_thesis/Tables"
capture file close table
file open table using "`dataset'_top_20_donor.tex", write replace

*Write the header 
file write table "\begin{tabular}{llll}" _n
file write table "\hline\midrule" _n
file write table "\multicolumn{2}{c}{\textbf{Composition A}} & \multicolumn{2}{c}{\textbf{Composition B}} \\" _n
file write table "Country & Weight & Country & Weight \\" _n
file write table "\cmidrule(lr){1-2} \cmidrule(lr){3-4}" _n

*Fill in the top 20 countries for each composition
forvalues i = 1/20 {

    *Sort by highest weight 
    gsort -u_w_simon
    local cs = country_simon[`i']
    local ws : display %6.3f u_w_simon[`i']

    *Sort by highest weight 
    gsort -u_w_clifton
    local cc = country_clifton[`i']
    local wc : display %6.3f u_w_clifton[`i']

    * -- Write one row to the table
    file write table "`cs' & `ws' & `cc' & `wc' \\" _n
}
		
*Write the closer 
file write table "\hline\hline" _n
file write table "\end{tabular}" _n
file close table
********************************************************************************		
*===============================================================================
*Figure 8: NINo -- Estimated Event Study Coefficients Using SDiD
*===============================================================================

estimates clear 

*Locals
local dataset "NINo"
local group group_simon group_clifton
local unit "country"
local outcome nino_total
local model "SDiD"
local se_sdid "bootstrap"

*Temporary dataset to store results
cd "$master_thesis/Data/`dataset'"
capture erase "resultsfile.dta"
tempname results
postfile `results' str6(model) str15(outcome) str15(period) double(coef) ///
    double(se) double(lb) double(ub) str25(ci) double(tstat) double(pval) ///
	double(n) str15(coef_stars) using resultsfile.dta, replace
********************************************************************************

****************************************
**Computations and Figure Construction**
****************************************

cd "$master_thesis/Data/`dataset'"
use `dataset'_panel.dta, clear

foreach g of local group {
	
	preserve
	
	collapse (sum) `outcome' (first) `g' c_`unit'  , by(date_stata `unit')
	drop if `g' == .

	*start_date
	if "`g'" == "group_simon" {
        local start_date 2015q1 // 6 pre-treatment periods considered
    }
    else if "`g'" == "group_clifton" {
        local start_date 2015q1 // 6 pre-treatment periods considered
    }

	keep if date_stata >= tq(`start_date')
	
	*Generate a simpler date indicator
	egen date_ind = group(date_stata) 

	*Gen treated post indicator 
	gen treated_post = (`g' == 1 & date_stata >= tq(2016q3))
	
	*Missing values do not work for sdid (replace them with 0)
	replace `outcome' = 0 if `outcome' == .
	
	*SDiD event does not return number of observations --> obtain it manually 
	*from the final sample to compute p-values
	count
	local n = r(N)		
	display "This is n: `n'"

	*Local for the title label 
	local title_label "NINo Registrations"
	
	*SDiD Regressions
	sdid_event `outcome' `unit' date_ind treated_post, ///
		method(sdid) vce(`se_sdid') brep(1000) placebo(all)
	
	*Grap the results for the tables
	local df = `n' - 1 // degrees of freedom 
	ereturn list
	mat list e(H)
	mat M = e(H)
	local nrows = rowsof(M)
	local rownames : rownames M
	forvalues i = 1/`nrows' {
		local rn : word `i' of `rownames'
		local coef = M[`i',1]
		local se   = M[`i',2]
		local lb   = M[`i',3]
		local ub   = M[`i',4]
	
	*Compute 95% CI manually and format it 
	local lb_fmt = trim("`: display %9.2f `lb''")
	local ub_fmt = trim("`: display %9.2f `ub''")
	local ci = "[`lb_fmt', `ub_fmt']"
		
	*Compute t-stat and p-value
    local tstat = `coef'/`se'
	local pval = 2 * ttail(`df', abs(`tstat'))
	
    *Add significance stars
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

    *Post to dataset
    post `results' ("`model'") ("`outcome'") ("`rn'") (`coef') (`se') (`lb') ///
		(`ub') ("`ci'") (`tstat')  (`pval') (`n') ("`coef_stars'")
    }

	*Store the estimates for the graph
	mat res = e(H)[2..21,1..5]
	svmat res
	gen time = _n - 1 if !missing(res1)
    replace time = 14 - _n if _n > 14 & !missing(res1)
	sort time
	
	*Generate the label mapping
	local first = tq(`start_date')
	local last  = tq(2019q4)

	local xlabels ""
	local time = -6

	forvalues q = `first'/`last' {
		local qnum = mod(`q', 4) + 1 // because tq() returns a counter not quarter
		if inlist(`qnum', 1, 3) {
			local label : display %tq `q'
			local xlabels `xlabels' `time' "`label'"
		}
		local ++time
	}
		
	*Put a vertical line at the time when the treatment occurs 
	local pre_quarter = tq(2016q2)
	local precise = `pre_quarter' + 23/91
	local referendum = `precise' - `first' - 6

	*Generate plot without titles 
	capture graph drop `dataset'_`g'_ev_SDiD
	twoway ///
		(rarea res3 res4 time, color("0 191 127%25") fcolor("0 191 127%25") ///
		lwidth(none)) ///
		(scatter res1 time, color("0 191 127") msymbol(circle)), ///
		xtitle("") ytitle("Estimated SDiD Coefficients") ///
		legend(order(2 "Point Estimate" 1 "95% CI")  ///
			position(6) cols(2) region(style(none) lstyle(solid) lcolor(black) ///
			lwidth(medium))) ///
		xlabel(`xlabels', angle(0) labsize(vsmall)) ///
		xline(`referendum', lcolor(black) lpattern(dash)) ///
		yline(0, lc(red) lp(shortdash)) ///+
		title("`title_label'", ///
			tstyle(heading) color(black) position(12) ring(100)) ///
		scheme(stgcolor) /// 
		graphregion(color(white)) ///
		bgcolor(white) ///
		plotregion(color(white)) ///
		name(`dataset'_`g'_ev_SDiD) 
		
	*Save the graph
	cd "$master_thesis/Graphs"
	graph export "`dataset'_`model'_event_study_`g'.pdf", replace as (pdf)	
	
	restore
}
cd "$master_thesis/Data/`dataset'"
postclose `results'
********************************************************************************

*===============================================================================
*Extra: Generate the tables for the SDiD event study 
*===============================================================================

*Locals
local dataset "NINo"
local group group_simon group_clifton 
local unit "country"
local outcome nino_total
local model "SDiD"
local start_date 2015q1
local inference "bootstrap"
********************************************************************************

*Table for the post-reform periods: 
cd "$master_thesis/Data/`dataset'"
use resultsfile.dta, clear
keep if period > 6
gen date_stata = tq(2016q3) + period - 8
format date_stata %tq
gen quarter = string(date_stata, "%tq")

*Open table
cd "$master_thesis/Tables"
capture file close table
file open table using "`dataset'_`model'_event_post_`inference'.tex", write replace

*Write the header 
file write table "\begin{tabular}{l@{\hskip 22pt}ccc}" _n
file write table "\hline\midrule" _n
file write table "& Composition A & Composition B \\" _n
file write table "& (1) & (1) \\" _n
file write table "& NINo Registrations & NINo Registrations \\" _n
file write table "\cmidrule(l){1-2} \cmidrule(l){3-3}" _n

*Loop through the obtained results (14 post-reform quarters )
levelsof quarter, local(quarters)
foreach t of local quarters {
    
    *Interaction term coefficient with stars
    file write table "EU x `t'"
    foreach g of local group {
        capture levelsof coef_stars if model == "`model'" & group == "`g'" ///
		& quarter == "`t'", local(bvals)
        if _rc == 0 {
            local b : word 1 of `bvals'
            file write table "& `b'"
        }
        else file write table "& ."
    }
    file write table " \\" _n
    
    *P-values
    file write table "p-value"
    foreach g of local group {
        qui su pval if model == "`model'" & group == "`g'" &  ///
		quarter == "`t'", meanonly
        local p = string(r(mean), "%9.3f")
        file write table " & (`p')"
    }
    file write table " \\" _n

    *Confidence Intervals
    file write table "95\% CI"
    foreach g of local group {
        capture levelsof ci if model == "`model'" & group == "`g'" & ///
		quarter == "`t'", local(civals)
        local c : word 1 of `civals'
        file write table " & `c'"
    }
    file write table " \\[0.5em]" _n

    *N only at last quarter
    if "`t'" == "2019q4" {
        file write table "\cmidrule(l){1-2} \cmidrule(l){3-3}" _n
        file write table "N"
        foreach g of local group {
            su n if model=="`model'" & group=="`g'" & quarter=="`t'", meanonly
            local n = string(r(mean),"%9.0f")
            file write table " & `n'"
        }
        file write table " \\" _n
    }
	
}

*Write the closer 
file write table "\hline\hline" _n
file write table "\end{tabular}" _n
file close table	
********************************************************************************

*Table for pre-reform quarters 
cd "$master_thesis/Data/`dataset'"
use resultsfile.dta, clear
keep if period <= 7
gen date_stata = tq(2016q3) + period - 8
format date_stata %tq
gen quarter = string(date_stata, "%tq")

*Open table
cd "$master_thesis/Tables"
capture file close table
file open table using "`dataset'_`model'_event_pre_`inference'.tex", write replace

*Write the header 
file write table "\begin{tabular}{l@{\hskip 22pt}ccc}" _n
file write table "\hline\midrule" _n
file write table "& Composition A & Composition B \\" _n
file write table "& (1) & (1) \\" _n
file write table "& NINo Registrations & NINo Registrations \\" _n
file write table "\cmidrule(l){1-2} \cmidrule(l){3-3}" _n

*Loop through the obtained results (14 post-reform quarters )
levelsof quarter, local(quarters)
foreach t of local quarters {
    
    *Interaction term coefficient with stars
    file write table "EU x `t'"
    foreach g of local group {
        capture levelsof coef_stars if model == "`model'" & group == "`g'" ///
		& quarter == "`t'", local(bvals)
        if _rc == 0 {
            local b : word 1 of `bvals'
            file write table "& `b'"
        }
        else file write table "& ."
    }
    file write table " \\" _n
    
    *P-values
    file write table "p-value"
    foreach g of local group {
        qui su pval if model == "`model'" & group == "`g'" &  ///
		quarter == "`t'", meanonly
        local p = string(r(mean), "%9.3f")
        file write table " & (`p')"
    }
    file write table " \\" _n

    *Confidence Intervals
    file write table "95\% CI"
    foreach g of local group {
        capture levelsof ci if model == "`model'" & group == "`g'" & ///
		quarter == "`t'", local(civals)
        local c : word 1 of `civals'
        file write table " & `c'"
    }
    file write table " \\[0.5em]" _n

    *N only at last quarter
    if "`t'" == "2016q1" {
        file write table "\cmidrule(l){1-2} \cmidrule(l){3-3}" _n
        file write table "N"
        foreach g of local group {
            su n if model=="`model'" & group=="`g'" & quarter=="`t'", meanonly
            local n = string(r(mean),"%9.0f")
            file write table " & `n'"
        }
        file write table " \\" _n
    }

}

*Write the closer 
file write table "\hline\hline" _n
file write table "\end{tabular}" _n
file close table	
********************************************************************************

log close 
cd $my_path_do
*===============================================================================
*===============================================================================
