*===============================================================================
*===============================================================================

*LTIM Robustness Checks
*Simon Zuerneck 
*Stata Version: 18.0
********************************************************************************

clear all
set more off 
capture log close 
cd $master_thesis
log using Log\3_4_LTIM_Robustness_Checks_Zuerneck.log, text replace 
********************************************************************************

*===============================================================================
*Robustness Check 1: DiD Treatment Effects for an Unbalanced Panel
*(Unit of analysis: subregion)
*(Estimates based on a UNBALANCED panel)
*(No controls, not log-transformed)
*(Inference: wild cluster bootstrap)
*===============================================================================

*===============================================================================
*Table 7: LTIM Unbalanced Panel -- Pooled DiD Specification Estimates
*===============================================================================

estimates clear 

*Locals
local dataset "LTIM"
local group "group_iasio"
local unit "subgroup"
local outcome inflow_2 inflow_3 inflow_4
local model "DiD"
local inference "wild"

*Temporary dataset to store results
cd "$master_thesis/Data/`dataset'"
capture erase "resultsfile.dta"
tempname results
postfile `results' str6(model) str15(outcome) str15(period) ///
    str15(coef_stars) float(r2 n pval) str25(ci) using resultsfile.dta, replace
	
use `dataset'_panel.dta, clear
********************************************************************************

****************
**Computations**
****************
	
*Gen treated post indicator 
gen treated_post = (`group' == 1 & date_stata >= tq(2016q3))

*DiD Approach 
foreach t in 2018q1 2019q4 {
	
    foreach v of local outcome {
		
        preserve
		
        keep if date_stata >= tq(2012q4) & date_stata <= tq(`t')
		
		*DiD Regressions 		
        reg `v' treated_post i.c_`unit' i.date_stata, cluster(c_`unit')
		
		*Run wild cluster bootstrap
		boottest treated_post, cluster(c_`unit') reps(9999) seed(12345) ///
			weighttype(rademacher) boottype(wild) 
			
		*Grap the results
	    local pval = r(p)
        local rawci = r(CIstr)
		
		*Format the confidence intervals 
		local rawci = subinstr("`rawci'", "[", "", .)
		local rawci = subinstr("`rawci'", "]", "", .)
		local rawci = ustrregexra("`rawci'", "\u2212", "-")
		local comma_pos = strpos("`rawci'", ",")
		local lb_raw = substr("`rawci'", 1, `comma_pos' - 1)
		local ub_raw = substr("`rawci'", `comma_pos' + 1, .)
		local lb_fmt = trim("`: display %9.2f `lb_raw''")
		local ub_fmt = trim("`: display %9.2f `ub_raw''")
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
		local coef = _b[treated_post]
		local coef_fmt = trim("`: display %9.3f `coef''")
		local coef_stars = "`coef_fmt'`stars'"
				
		*Store results of interest in the temporary dataset so that they exist 
		*outside the loop		
		post `results' ("`model'") ("`v'") ("`t'") ("`coef_stars'") (e(r2)) ///
			(e(N)) (`pval') ("`ci'")
		
        restore
    }
}
postclose `results'
********************************************************************************

****************************
**Latex Table Construction**
****************************

*Locals
local dataset "LTIM"
local group "group_iasio"
local unit "subgroup"
local outcome inflow_2 inflow_3 inflow_4
local model "DiD"
local inference "wild"
cd "$master_thesis/Data/`dataset'"
use resultsfile.dta, clear

*Open table
cd "$master_thesis/Tables"
capture file close table
file open table using "`dataset'_unbalanced_DiD_pooled_`inference'.tex", write replace

*Write the header 
file write table "\begin{tabular}{l@{\hskip 15pt}l@{\hskip 15pt}ccc}" _n
file write table "\hline\midrule" _n
file write table "& & (1) & (2) & (3) \\" _n
file write table "Period & & Work Related & Definite Job & Looking for Work \\" _n
file write table "\midrule" _n

*Loop through the obtained results
foreach t in 2018q1 2019q4 {
	
	*Period
	file write table "2016q3--`t'" 
	file write table " \\[0.3em]" _n
	
	*Coefficient with stars	
	file write table "& ATT" 
	foreach v in inflow_2 inflow_3 inflow_4 {
		qui levelsof coef_stars if model == "`model'" & outcome == "`v'" ///
		& period == "`t'", local(bvals)
		local b : word 1 of `bvals'
		file write table " & `b'"
	}
	file write table " \\" _n

	*P-values
	file write table "& p-value"
	foreach v in inflow_2 inflow_3 inflow_4 {
		qui su pval if model == "`model'" & outcome == "`v'" ///
		& period == "`t'", meanonly
		local p = string(r(mean), "%9.3f")
		file write table " & (`p')"
	}
	file write table " \\" _n

	*Confidence intervals
	file write table "& 95\% CI"
	foreach v in inflow_2 inflow_3 inflow_4 {
		qui levelsof ci if model == "`model'" & outcome == "`v'" ///
		& period == "`t'", local(civals)
		local c : word 1 of `civals'
		file write table " & `c'"
	}
	file write table " \\" _n

	*Number of observations
    file write table "& N"
    foreach v in inflow_2 inflow_3 inflow_4 {
        su n if model=="`model'" & outcome=="`v'" & period=="`t'", meanonly
        local n = string(r(mean),"%9.0f")
        file write table " & `n'"
    }
    file write table " \\" _n

	*R^2 
    file write table "& R\textsuperscript{2}"
    foreach v in inflow_2 inflow_3 inflow_4 {
        su r2 if model=="`model'" & outcome=="`v'" & period=="`t'", meanonly
        local r2 = string(r(mean),"%9.3f")
        file write table " & `r2'"
    }
    file write table "\\" _n

	*Mid-formating 
    if "`t'" == "2018q1" file write table "\cmidrule(lr){1-5} " _n
}

*Write the closer 
file write table "\hline\hline" _n
file write table "\end{tabular}" _n
file close table
********************************************************************************

*===============================================================================
*Figure 9: LTIM Unbalanced Panel -- Estimated Event-Study Coefficients Using DiD
*===============================================================================

estimates clear 

*Locals
local dataset "LTIM"
local group "group_iasio"
local unit "subgroup"
local outcome inflow_2 inflow_3 inflow_4
local model "DiD"
local inference "wild"
cd "$master_thesis/Data/`dataset'"
capture erase "resultsfile.dta"

*Temporary dataset to store results
cd "$master_thesis/Data/`dataset'"
tempname results
postfile `results' str15(outcome) int(period) double(coef) str15(coef_stars) ///
	double(lb95) double(ub95) double(lb90) double(ub90) double(pvalue) ///
	str25(ci) using resultsfile.dta, replace
********************************************************************************

****************
**Computations**
****************

foreach v of local outcome {
	
	cd "$master_thesis/Data/`dataset'"
	use `dataset'_panel.dta, clear
	
	keep if date_stata >= tq(2012q4)
	
	*Reference period (omitted due to multicollinearity) 
	*Reference period is the period right before the referendum 
	* --> 2016q2 <=> 15
	
	*Generate a simpler date indicator
	egen date_ind = group(date_stata) 
	
	reg `v' i.`group'##ib(15).date_ind i.c_`unit' i.date_ind, ///
		vce(robust)
	
	reg `v' i.`group'##ib(15).date_ind i.c_`unit' i.date_ind, ///
		cluster(c_`unit')
	
	*DiD Event Study Regression 
	reg `v' i.`group'##ib(15).date_ind i.c_`unit' i.date_ind, ///
		cluster(c_`unit') coeflegend
	estimates store did_event

	*Loop over interaction terms (skip period 15 = ref)
	foreach i of numlist 1/14 16/29 {
	
		local term = "1.`group'#`i'.date_ind"
		
		*Get coefficient
		local coef = _b[`"`term'"']
		
		di "this is the event: `term'"

		*Run boottest (95% CI)
		boottest `term', cluster(c_`unit') reps(9999) level(95) ///
			weight(rademacher) boottype(wild) seed(12345)
			
		*Construct CIs
		local pval = r(p)		
		local rawci95 = r(CIstr)
		local rawci95 = subinstr("`rawci95'", "[", "", .)
		local rawci95 = subinstr("`rawci95'", "]", "", .)
		local rawci95 = ustrregexra("`rawci95'", "\u2212", "-")
		local cpos = strpos("`rawci95'", ",")
		local lb95 = real(substr("`rawci95'", 1, `cpos' - 1))
		local ub95 = real(substr("`rawci95'", `cpos' + 1, .))
		
		*Format the CIs for the regression tables
		local lb_fmt = trim("`: display %9.2f `lb95''")
		local ub_fmt = trim("`: display %9.2f `ub95''")
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
		
		*Run boottest 90% CI
		boottest `term', cluster(c_`unit') reps(9999) level(90) ///
			weight(rademacher) boottype(wild) seed(12345)
		
		*Construct CIs
		local rawci90 = r(CIstr)
		local rawci90 = subinstr("`rawci90'", "[", "", .)
		local rawci90 = subinstr("`rawci90'", "]", "", .)
		local rawci90 = ustrregexra("`rawci90'", "\u2212", "-")
		local cpos = strpos("`rawci90'", ",")
		local lb90 = real(substr("`rawci90'", 1, `cpos' - 1))
		local ub90 = real(substr("`rawci90'", `cpos' + 1, .))
		
		post `results' ("`v'") (`i') (`coef') ("`coef_stars'") (`lb95') ///
		(`ub95') (`lb90') (`ub90') (`pval') ("`ci'")

	}
	
	
}
cd "$master_thesis/Data/`dataset'"
postclose `results'	
********************************************************************************

***********************
**Figure Construction**
***********************
local dataset "LTIM"
local group "group_iasio"
local unit "subgroup"
local outcome inflow_2 inflow_3 inflow_4
local model "DiD"
local inference "wild"
local `dataset'_`model'_event_study ""

cd "$master_thesis/Data/`dataset'"
use resultsfile.dta, clear

*Regenerate the date variable 
gen date_stata = tq(2016q3) + period - 16
format date_stata %tq
gen quarter = string(date_stata, "%tq")

foreach v of local outcome {

	preserve 
	
	keep if outcome == "`v'"
	
	*Title labels 
	if "`v'" == "inflow_2" {
		local title_label "Work Related"
	}
	else if "`v'" == "inflow_3" {
		local title_label "Definite Job"	
	}
	else if "`v'" == "inflow_4" {
		local title_label "Looking for Work"	
	}
	

	*Generate x-axis positions (since 2016q2 is omitted)
	gen x = _n
	
	*Local for the coefficient labels
	local coef_labels ""
	
	local first = tq(2012q4)
	forvalues i = 1/14 {
		local this_q = `first' + `i' - 1
		local qlabel : display %tq `this_q'
		if "`qlabel'" != "2016q2" {
			local coef_labels `"`coef_labels' `i' `"EUx`qlabel'"'"'
		}
	}
	local first = tq(2012q4)
	forvalues i = 15/28 {
		local this_q = `first' + `i'
		local qlabel : display %tq `this_q'
		if "`qlabel'" != "2016q2" {
			local coef_labels `"`coef_labels' `i' `"EUx`qlabel'"'"'
		}
	}
		
	*Create the plot
	capture graph drop `dataset'_`v'_ev_`model'
	twoway ///
		(rspike ub95 lb95 x, lcolor("0 191 127" ) lwidth(thin)) ///
		(rcap ub90 lb90 x, lcolor("0 191 127" ) lwidth(thin)) ///
		(scatter coef x, mcolor("0 191 127") msymbol(circle)), ///
		legend(order(3 "Point Estimate" 2 "90% CI" 1 "95% CI")  ///
			position(6) cols(3) region(style(none) lstyle(solid) lcolor(black) ///
			lwidth(medium))) ///
		ytitle("Estimated ATT") ///
		yline(0, lcolor(red) lpattern(dash)) ///
		xline(14.5, lcolor(black) lpattern(dash)) ///
		xlabel(`coef_labels', format(%tq) angle(90) labsize(vsmall)) ///
		xtitle("") ///
		ytitle("Estimated DiD Coefficients") ///
		title("Migration Reason: `title_label'") ///
		graphregion(color(white)) bgcolor(white) plotregion(color(white)) ///
		name(`dataset'_`v'_ev_`model')
		
	*Store the graphs
	local `dataset'_`model'_event_study "``dataset'_`model'_event_study' `dataset'_`v'_ev_`model'"
 	
	restore
	
}
di "Stored graphs : ``dataset'_`model'_event_study'"

*Combine graphs and save them as PDF
cd "$master_thesis/Graphs"
grc1leg2 ``dataset'_`model'_event_study', leg(`dataset'_inflow_4_ev_DiD) ///
	iscale(*.7) graphregion(color(white)) graphregion(margin(zero)) 

graph export "`dataset'_unbalanced_`model'_event_study_combined.pdf", replace as (pdf)
********************************************************************************

*===============================================================================
*Robustness Check 2: Comparison of Inference Methods With Few Clusters
*(Unit of analysis: subregion)
*(Estimates based on a balanced panel)
*(No controls, not log-transformed)
*(Inference: wild cluster bootstrap)
*===============================================================================

*===============================================================================
*Table 8: LTIM Inference Comparision of Pooled DiD Estimates
*===============================================================================

estimates clear 

*Locals
local dataset "LTIM"
local group "group_iasio"
local unit "subgroup"
local outcome inflow_2 inflow_3 inflow_4
local model "DiD"
local inference "comparison"
cd "$master_thesis/Data/`dataset'"
capture erase "resultsfile.dta"

*Temporary dataset to store results
cd "$master_thesis/Data/`dataset'"
tempname results
postfile `results' str12(outcome) str15(period) str15(model) str15(coef) ///
	str25(ci) float(pval r2 n) using resultsfile.dta, replace
	
use `dataset'_panel.dta, clear
********************************************************************************

****************
**Computations**
****************
	
*Gen treated post indicator 
gen treated_post = (`group' == 1 & date_stata >= tq(2016q3))

foreach t in 2018q1 2019q4 {
	
    foreach v of local outcome {
		
        preserve
		
        keep if date_stata >= tq(2012q4) & date_stata <= tq(`t')
		
		*Missing values do not work for sdid (replace them with 0)
		replace `v' = 0 if `v' == .

		*DiD Regression - robust option
		di "DiD Regression - robust option"
		reg `v' treated_post i.c_`unit' i.date_stata, vce(robust)
		
		*store results
		matrix results = r(table)
		local coef = results[1,1]
		local r2 = e(r2)
		local n = e(N)
		
		local pval_robust = results[4,1]
		local lb_robust = results[5,1]   
		local ub_robust = results[6,1]   
		
		*Format confidence intervalls 
		local lb_fmt = trim("`: display %9.2f `lb_robust''")
		local ub_fmt = trim("`: display %9.2f `ub_robust''")
		local ci_robust = "[`lb_fmt', `ub_fmt']"
		
		post `results' ("`v'") ("`t'") ("Robust") ("`coef'") ("`ci_robust'") ///
		(`pval_robust') (`r2') (`n')
		
		
		*DiD Regression - clustering at the unit level
		di "DiD Regression - clustering at the unit level"
		reg `v' treated_post i.c_`unit' i.date_stata, cluster(c_`unit')	
		
		*Store results
		matrix results = r(table)
		local coef_cluster = results[1,1]
		local pval_cluster = results[4,1]
		local lb_cluster = results[5,1]   
		local ub_cluster = results[6,1]   
		
		*Format confidence intervalls 
		local lb_fmt = trim("`: display %9.2f `lb_cluster''")
		local ub_fmt = trim("`: display %9.2f `ub_cluster''")
		local ci_cluster = "[`lb_fmt', `ub_fmt']"
		
		post `results' ("`v'") ("`t'") ("Clustered") ("`coef'") ("`ci_cluster'") ///
		(`pval_cluster') (`r2') (`n')
		
		*DiD Regression - wild cluster bootstrap
		di "DiD Regression - wild cluster bootstrap"
        reg `v' treated_post i.c_`unit' i.date_stata, cluster(c_`unit')
		boottest treated_post, cluster(c_`unit') reps(9999) seed(12345) ///
			weighttype(rademacher) boottype(wild) 
		
		*Store results
	    local pval_wild = r(p)
        local rawci = r(CIstr)
		
		*Format the confidence intervalls 
		local rawci = subinstr("`rawci'", "[", "", .)
		local rawci = subinstr("`rawci'", "]", "", .)
		local rawci = ustrregexra("`rawci'", "\u2212", "-")
		local comma_pos = strpos("`rawci'", ",")
		local lb_raw = substr("`rawci'", 1, `comma_pos' - 1)
		local ub_raw = substr("`rawci'", `comma_pos' + 1, .)
		local lb_fmt = trim("`: display %9.2f `lb_raw''")
		local ub_fmt = trim("`: display %9.2f `ub_raw''")
		local ci_wild = "[`lb_fmt', `ub_fmt']"
		
		post `results' ("`v'") ("`t'") ("WCB") ("`coef'") ("`ci_wild'") ///
		(`pval_wild') (`r2') (`n')
		
		
		*DiD Regression - block bootstrap
		di "DiD Regression - block bootstrap"	
		sdid `v' `unit' date_stata treated_post, method(did) ///
		vce(bootstrap) seed(1213) reps(1000)
		
		*Store results
		local att = e(ATT)
		local se = e(se)
		local df = `n' - 1

		*Compute t-stat and p-value (as they are not e returned)
		local tstat = `att'/`se'
		local pval_block = 2 * ttail(`df', abs(`tstat'))

		*Compute 95% CI manually
		local lb = `att' - invttail(`df', 0.025) * `se'
		local ub = `att' + invttail(`df', 0.025) * `se'
		local lb_fmt = trim("`: display %9.2f `lb''")
		local ub_fmt = trim("`: display %9.2f `ub''")
		local ci_block = "[`lb_fmt', `ub_fmt']"
		
		post `results' ("`v'") ("`t'") ("Block Boot.") ("`coef'") ("`ci_block'") ///
		(`pval_block') (`r2') (`n')
		
        restore
    }
}
postclose `results'
********************************************************************************

****************************
**Latex Table Construction**
****************************

local dataset "LTIM"
cd "$master_thesis/Data/`dataset'"

*Format the dataset
use resultsfile.dta, clear
destring coef, replace

local models `" "WCB" "Robust" "Clustered" "Block Boot." "'

*Open table
local dataset "LTIM"
cd "$master_thesis/Tables"
capture file close table
file open table using "`dataset'_DiD_inference_comparison.tex", write replace

*Write the header 
file write table "\begin{tabular}{l@{\hskip 15pt}l@{\hskip 15pt}ccc}" _n
file write table "\hline\midrule" _n
file write table "& & (1) & (2) & (3) \\" _n
file write table "& & Work Related & Definite Job & Looking for Work \\" _n
file write table "\midrule" _n

*Loop through the obtained results
foreach t in 2018q1 2019q4 {
	
	*Period
	file write table "2016q3--`t'" 
	file write table "\\[0.3em]" _n
	
	*Coefficient	
	file write table "& ATT" 
	foreach v in inflow_2 inflow_3 inflow_4 {
		sum coef if model == "Robust" & outcome == "`v'" ///
		& period == "`t'", meanonly
		local b = string(r(mean), "%9.3f")
		file write table " & `b'"
	}
	file write table " \\[0.4em]" _n
	
	foreach m of local models {
	
		*Inference Model
		file write table "\multirow{2}{*}{\hspace{1em}\textit{`m'}}"
		
		*P-values	
		file write table "& p-value"
		foreach v in inflow_2 inflow_3 inflow_4 {
			qui sum pval if model == "`m'" & outcome == "`v'" ///
			& period == "`t'", meanonly
			local p = string(r(mean), "%9.3f")
			file write table " & (`p')"	
		}
		file write table " \\" _n
	
		*Confidence intervals
		file write table "& 95\% CI"
		foreach v in inflow_2 inflow_3 inflow_4 {
			qui levelsof ci if model == "`m'" & outcome == "`v'" ///
			& period == "`t'", local(civals)
			local c : word 1 of `civals'
			file write table " & `c'"
		}
		file write table " \\[0.4em]" _n
	}

	*Number of Observations
	file write table "& N" 
	foreach v in inflow_2 inflow_3 inflow_4 {
		qui sum n if model == "Robust" & outcome == "`v'" ///
		& period == "`t'", meanonly
		local n = string(r(mean), "%9.0f")
		file write table " & `n'"
	}
	file write table " \\" _n

	*R^2
	file write table "& R\textsuperscript{2}" 
	foreach v in inflow_2 inflow_3 inflow_4 {
		qui sum r2 if model == "Robust" & outcome == "`v'" ///
		& period == "`t'", meanonly
		local r2 = string(r(mean), "%9.3f")
		file write table " & `r2'"
	}
	file write table " \\" _n

	*Mid-formating 
    if "`t'" == "2018q1" file write table "\cmidrule(lr){1-5} " _n
}


*Write the closer 
file write table "\hline\hline" _n
file write table "\end{tabular}" _n
file close table
********************************************************************************

*===============================================================================
*Robustness Check 3: Estimates for Log-Transformed Work-Related Migration 
*Inflows
*(Unit of analysis: subregion)
*(Estimates based on a balanced panel)
*(No controls, not log-transformed)
*(Inference: wild cluster bootstrap AND robust)
*===============================================================================

*===============================================================================
*Figure 10: LTIM -- Inference Comparison after Log Transformation 
*===============================================================================

estimates clear 

***********************************
**Applying wild cluster bootstrap** 
***********************************

*Locals
local dataset "LTIM"
local group "group_iasio"
local unit "subgroup"
local outcome inflow_2
local model "DiD"
local inference "wild"
cd "$master_thesis/Data/`dataset'"
capture erase "resultsfile.dta"

*Temporary dataset to store results
cd "$master_thesis/Data/`dataset'"
tempname results
postfile `results' str15(outcome) int(period) double(coef) str15(coef_stars) ///
	double(lb95) double(ub95) double(lb90) double(ub90) double(pvalue) ///
	str25(ci) using resultsfile.dta, replace
********************************************************************************

****************
**Computations**
****************

foreach v of local outcome {
	
	cd "$master_thesis/Data/`dataset'"
	use `dataset'_panel.dta, clear
	
	keep if date_stata >= tq(2012q4) 
	
	*Missing values do not work for sdid (replace them with 0)
	replace ln_`v' = 0 if ln_`v' == .	
	
	*Reference period (omitted due to multicollinearity) 
	*Reference period is the period right before the referendum 
	* --> 2016q2 <=> 15
	
	*Generate a simpler date indicator
	egen date_ind = group(date_stata) 
	
	*DiD Event Study Regression with WCB
	reg ln_`v' i.`group'##ib(15).date_ind i.c_`unit' i.date_ind, ///
		cluster(c_`unit') coeflegend
	estimates store did_event

	*Loop over interaction terms (skip period 14 = ref)
	foreach i of numlist 1/14 16/29 {
	
		local term = "1.`group'#`i'.date_ind"
		
		*Get coefficient
		local coef = _b[`"`term'"']
		
		di "this is the event: `term'"

		*Run boottest (95% CI)
		boottest `term', cluster(c_`unit') reps(9999) level(95) ///
			weight(rademacher) boottype(wild) seed(12345)
			
		*Construct CIs
		local pval = r(p)		
		local rawci95 = r(CIstr)
		local rawci95 = subinstr("`rawci95'", "[", "", .)
		local rawci95 = subinstr("`rawci95'", "]", "", .)
		local rawci95 = ustrregexra("`rawci95'", "\u2212", "-")
		local cpos = strpos("`rawci95'", ",")
		local lb95 = real(substr("`rawci95'", 1, `cpos' - 1))
		local ub95 = real(substr("`rawci95'", `cpos' + 1, .))
		
		*Format the CIs for the regression tables
		local lb_fmt = trim("`: display %9.2f `lb95''")
		local ub_fmt = trim("`: display %9.2f `ub95''")
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
		
		*Run boottest 90% CI
		boottest `term', cluster(c_`unit') reps(9999) level(90) ///
			weight(rademacher) boottype(wild) seed(12345)
		
		*Construct CIs
		local rawci90 = r(CIstr)
		local rawci90 = subinstr("`rawci90'", "[", "", .)
		local rawci90 = subinstr("`rawci90'", "]", "", .)
		local rawci90 = ustrregexra("`rawci90'", "\u2212", "-")
		local cpos = strpos("`rawci90'", ",")
		local lb90 = real(substr("`rawci90'", 1, `cpos' - 1))
		local ub90 = real(substr("`rawci90'", `cpos' + 1, .))
		
		post `results' ("ln_`v'") (`i') (`coef') ("`coef_stars'") (`lb95') ///
		(`ub95') (`lb90') (`ub90') (`pval') ("`ci'")

	}
	
	
}
cd "$master_thesis/Data/`dataset'"
postclose `results'	
********************************************************************************

***********************
**Figure Construction**
***********************

local dataset "LTIM"
local group "group_iasio"
local unit "subgroup"
local outcome inflow_2
local model "DiD"
local inference "wild"
cd "$master_thesis/Data/`dataset'"
use resultsfile.dta, clear

*Regenerate the date variable 
gen date_stata = tq(2016q3) + period - 16
format date_stata %tq
gen quarter = string(date_stata, "%tq")

foreach v of local outcome {

	preserve 
	
	*Title labels 
	if "ln_`v'" == "ln_inflow_2" {
		local title_label "Work Related"
	}

	*Generate x-axis positions (since 2016q2 is omitted)
	gen x = _n
	
	*Local for the coefficient labels
	local coef_labels ""
	
	local first = tq(2012q4)
	forvalues i = 1/14 {
		local this_q = `first' + `i' - 1
		local qlabel : display %tq `this_q'
		if "`qlabel'" != "2016q2" {
			local coef_labels `"`coef_labels' `i' `"EUx`qlabel'"'"'
		}
	}
	local first = tq(2012q4)
	forvalues i = 15/28 {
		local this_q = `first' + `i'
		local qlabel : display %tq `this_q'
		if "`qlabel'" != "2016q2" {
			local coef_labels `"`coef_labels' `i' `"EUx`qlabel'"'"'
		}
	}
		
	*Create the plot
	capture graph drop `dataset'_ln_`v'_ev_`inference'
	twoway ///
		(rspike ub95 lb95 x, lcolor("0 191 127" ) lwidth(thin)) ///
		(rcap ub90 lb90 x, lcolor("0 191 127" ) lwidth(thin)) ///
		(scatter coef x, mcolor("0 191 127") msymbol(circle)), ///
		legend(order(3 "Point Estimate" 2 "90% CI" 1 "95% CI")  ///
			position(6) cols(3) region(style(none) lstyle(solid) lcolor(black) ///
			lwidth(medium))) ///
		ytitle("Estimated ATT") ///
		yline(0, lcolor(red) lpattern(dash)) ///
		xline(14.5, lcolor(black) lpattern(dash)) ///
		xlabel(`coef_labels', format(%tq) angle(90) labsize(vsmall)) ///
		ylabel(-2(0.5)0.5, angle(0)) ///
		xtitle("") ///
		ytitle("Estimated DiD Coefficients") ///
		title("Migration Reason: `title_label'") ///
		graphregion(color(white)) bgcolor(white) plotregion(color(white)) ///
		name(`dataset'_ln_`v'_ev_`inference')
 	
	*Save
	cd "$master_thesis/Graphs"
	graph export "`dataset'_ln_`v'_ev_`inference'.pdf", replace as (pdf)
	
	restore
	
}
********************************************************************************

***********************************
**Applying robust standard errors**
***********************************

*Locals 
local inference "robust"
local dataset "LTIM"
local group "group_iasio"
local unit "subgroup"
local outcome inflow_2 
local model "DiD"

cd "$master_thesis/Data/`dataset'"
capture erase "resultsfile.dta"
********************************************************************************

****************************************
**Computations and Figure Construction**
****************************************

foreach v of local outcome {
	
	cd "$master_thesis/Data/`dataset'"
	use `dataset'_panel.dta, clear
	
	keep if date_stata >= tq(2012q4) 
	
	*Missing values do not work for sdid (replace them with 0)
	replace ln_`v' = 0 if ln_`v' == .	
	
	*Reference period (omitted due to multicollinearity) 
	*Reference period is the period right before the referendum 
	* --> 2016q2 <=> 14
	
	*Generate a simpler date indicator
	egen date_ind = group(date_stata) 
	
	*DiD Event Study Regression with robust standard errors 
	reg ln_`v' i.`group'##ib(15).date_ind i.c_`unit' ///
	i.date_ind, vce(robust) coeflegend
	
	*Store estimation results that should not be displayed in a local 
	levelsof date_ind, local(levels)
		local timefixed ""
		foreach k of local levels {
		local timefixed "`timefixed' `k'.date_ind"	
	}
	
	display "`timefixed'"
	
	levelsof c_subgroup, local(levels)
	local groupfixed ""
		foreach k of local levels {
		local groupfixed "`groupfixed' `k'.c_subgroup"	
	}
	
	*Local for the title label 
	local title_label : variable label `v'
	di "`title_label'"
	
	*Local for the coefficient labels
	local coef_labels ""

	local first = tq(2012q4) 

	*Loop through all period indices 
	forvalues i = 1/29 {
		local this_q = `first' + (`i' - 1) 
		local qlabel : display %tq `this_q'
    
		*Append coefficient label mapping
		local coef_labels "`coef_labels' `i' = "EU x `qlabel'""
		
		*Separate coefficients before and after the policy change 
        if `i' <= 15 {
            local pre_coef "`pre_coef' `i'"
        }
        else {
            local post_coef "`post_coef' `i'"
        }
	
	}	
	display "`groupfixed'"

	*Generate the Graph
	capture graph drop `dataset'_ln_`v'_ev_`inference'
	coefplot, ///
		color("0 191 127") ciopt(color("0 191 127" "0 191 127") ///
		lwidth(thin thin ) recast(. rcap)) ///
		vertical drop(_cons 1.`group' `groupfixed' `timefixed') levels(95 90) /// 
		rename(^1.`group'#([0-9]+).date_ind$ = \1, regex) ///
		coeflabels(`coef_labels', format(%tq) angle(90) labsize(vsmall)) ///
		legend(order(3 "Point Estimate" 2 "90% CI" 1 "95% CI")  ///
			position(6) cols(3) region(style(none) lstyle(solid) lcolor(black) ///
			lwidth(medium))) ///
		yline(0, lcolor(red) lpattern(dash)) ///
		xline(14.5, lcolor(black) lpattern(dash)) ///
		ytitle("Estimated DiD Coefficients") ///
		title("Migration Reason: `title_label'") ///
		xlabel(, grid) ///
		ylabel(-2(0.5)0.5, angle(0)) ///
		bgcolor(white) ///
		plotregion(color(white)) ///
		name(`dataset'_ln_`v'_ev_`inference')	
	
	*Save
	cd "$master_thesis/Graphs"
	graph export "`dataset'_ln_`v'_ev_`inference'.pdf", replace as (pdf)
	
}
********************************************************************************

log close
cd $my_path_do
*===============================================================================
*===============================================================================