*===============================================================================
*===============================================================================

*UKLFS Thesis  
*Simon Zuerneck 
*Stata Version: 18.0
********************************************************************************

clear all
set more off 
capture log close 
cd $master_thesis
log using Log\5_3_UKLFS_emprical_analysis_Zuerneck.log, text replace 
********************************************************************************

*===============================================================================
*Canonical Difference-in-Differences
*(Unit of analysis: subregion)
*(Estimates based on a balanced panel)
*(No controls, not log-transformed)
*(Inference: wild cluster bootstrap)
*===============================================================================

*===============================================================================
*Table 11: UKLFS –- Pooled DiD Specification Estimates
*===============================================================================	

estimates clear 

*Locals
local dataset "UKLFS"
local group "group_iasio"
local unit "subgroup"

*Temporary dataset to store results
cd "$master_thesis/Data/`dataset'"
capture erase "resultsfile.dta"
tempname results
postfile `results' str15(inference) str15(outcome) str15(period) ///
	float(coef r2 n pval) str25(ci) using resultsfile.dta, replace
	
use `dataset'_panel.dta, clear
********************************************************************************

****************
**Computations**
****************	
	
*Gen treated post indicator 
gen treated_post = (`group' == 1 & date_stata >= tq(2016q3))

*Log-transformed outcome -- robust standard errors 
local outcome ln_lfs_total
local inference "Robust"

foreach t in 2018q1 2019q4 {
	
    foreach v of local outcome {
		
        preserve
		
        keep if date_stata >= tq(2012q4) & date_stata <= tq(`t')
		
		*Missing values do not work for sdid (replace them with 0)
		replace `outcome' = 0 if `outcome' == .
		
		*DiD Regressions 		
        reg `outcome' treated_post i.c_`unit' i.date_stata, vce(robust)
		
		*Store results
		matrix results = r(table)
		local r2 = e(r2)
		local n = e(N)
		local coef = results[1,1]
		local pval = results[4,1]
		local tstat = results[3.,1]
		local lb_robust = results[5,1]   
		local ub_robust = results[6,1]   
		
		*Format the CIs
		local lb_fmt = trim("`: display %9.2f `lb_robust''")
		local ub_fmt = trim("`: display %9.2f `ub_robust''")
		local ci = "[`lb_fmt', `ub_fmt']"
				
		*Store results of interest in the temporary dataset so that they exist 
		*outside the loop		
		post `results' ("`inference'") ("`outcome'") ("`t'") (`coef') ///
			(`r2') (`n') (`pval') ("`ci'")
		
        restore
    }
}

*Log-transformed outcome -- WCB inference 
local outcome ln_lfs_total
local inference "WCB"

foreach t in 2018q1 2019q4 {
	
    foreach v of local outcome {
		
        preserve
		
        keep if date_stata >= tq(2012q4) & date_stata <= tq(`t')
		
		*Missing values do not work for sdid (replace them with 0)
		replace `outcome' = 0 if `outcome' == .
		
		*DiD Regressions 		
        reg `outcome' treated_post i.c_`unit' i.date_stata, cluster(c_`unit')
		
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
				
		*Store results of interest in the temporary dataset so that they exist 
		*outside the loop		
		post `results' ("`inference'") ("`outcome'") ("`t'") (`coef') ///
			(`r2') (`n') (`pval') ("`ci'")
		
        restore
    }
}

*Not log-transformed robust standard errors
local outcome lfs_total
local inference "Robust"

foreach t in 2018q1 2019q4 {
	
    foreach v of local outcome {
		
        preserve
		
        keep if date_stata >= tq(2012q4) & date_stata <= tq(`t')
		
		*Missing values do not work for sdid (replace them with 0)
		replace `outcome' = 0 if `outcome' == .
		
		*DiD Regressions 		
        reg `outcome' treated_post i.c_`unit' i.date_stata, vce(robust)
		
		*Store results
		matrix results = r(table)
		local r2 = e(r2)
		local n = e(N)
		local coef = results[1,1]
		local pval = results[4,1]
		local tstat = results[3.,1]
		local lb_robust = results[5,1]   
		local ub_robust = results[6,1]   
		
		*Format the CIs
		local lb_fmt = trim("`: display %9.2f `lb_robust''")
		local ub_fmt = trim("`: display %9.2f `ub_robust''")
		local ci = "[`lb_fmt', `ub_fmt']"
				
		*Store results of interest in the temporary dataset so that they exist 
		*outside the loop		
		post `results' ("`inference'") ("`outcome'") ("`t'") (`coef') ///
			(`r2') (`n') (`pval') ("`ci'")
		
        restore
    }
}

*Not Log-transformed outcome -- WBC inference 
local outcome lfs_total
local inference "WCB"

foreach t in 2018q1 2019q4 {
	
    foreach v of local outcome {
		
        preserve
		
        keep if date_stata >= tq(2012q4) & date_stata <= tq(`t')
		
		*Missing values do not work for sdid (replace them with 0)
		replace `outcome' = 0 if `outcome' == .
		
		*DiD Regressions 		
        reg `outcome' treated_post i.c_`unit' i.date_stata, cluster(c_`unit')
		
		*Run wild cluster bootstrap
		boottest treated_post, cluster(c_`unit') reps(999999) seed(12345) ///
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
				
		*Store results of interest in the temporary dataset so that they exist 
		*outside the loop		
		post `results' ("`inference'") ("`outcome'") ("`t'") (`coef') ///
			(`r2') (`n') (`pval') ("`ci'")
		
        restore
    }
}
postclose `results'
********************************************************************************

****************************
**Latex Table Construction**
****************************

*Locals
local dataset "UKLFS"
local group "group_iasio"
local unit "subgroup"
cd "$master_thesis/Data/`dataset'"
use resultsfile.dta, clear

*Loop through the different specification 
*(non-log-transformed outcome, log-transformed outcome)
levelsof outcome, local(outcomes)

*(WCB inference, robust-inference)
levelsof inference, local(models)

*Open table
cd "$master_thesis/Tables"
capture file close table
file open table using "`dataset'_DiD_pooled.tex", write replace

*Write the header 
file write table "\begin{tabular}{llcccc}" _n
file write table "\hline\midrule" _n
file write table "& & (1) & (2) \\" _n
file write table "& & (Non-Log-Transformed) & (Log-Transformed) \\" _n
file write table "\cmidrule(l){1-3} \cmidrule(l){4-4}"  _n

*Loop through the obtained results
foreach t in 2018q1 2019q4 {
	
	*Period
	file write table "2016q3--`t'" 
	file write table " \\[0.3em]" _n
	
	*Coefficient	
	file write table "& ATT" 
	foreach v of local outcomes {
		sum coef if inference == "Robust" & outcome == "`v'" ///
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
		foreach v of local outcomes {
			qui sum pval if inference == "`m'" & outcome == "`v'" ///
			& period == "`t'", meanonly
			local p = string(r(mean), "%9.3f")
			file write table " & (`p')"	
		}
		file write table " \\" _n
	
		*Confidence intervals
		file write table "& 95\% CI"
		foreach v of local outcomes {
			qui levelsof ci if inference == "`m'" & outcome == "`v'" ///
			& period == "`t'", local(civals)
			local c : word 1 of `civals'
			file write table " & `c'"
		}
		file write table " \\[0.4em]" _n
	}
	
	*Number of Observations
	file write table "& N" 
	foreach v of local outcomes {
		qui sum n if inference == "Robust" & outcome == "`v'" ///
		& period == "`t'", meanonly
		local n = string(r(mean), "%9.0f")
		file write table " & `n'"
	}
	file write table " \\" _n

	*R^2
	file write table "& R\textsuperscript{2}" 
	foreach v of local outcomes {
		qui sum r2 if inference == "Robust" & outcome == "`v'" ///
		& period == "`t'", meanonly
		local r2 = string(r(mean), "%9.3f")
		file write table " & `r2'"
	}
	file write table " \\" _n

	*Mid-formating 
    if "`t'" == "2018q1" file write table "\cmidrule(l){1-3} \cmidrule(l){4-4}"  _n
}	


*Write the closer 
file write table "\hline\hline" _n
file write table "\end{tabular}" _n
file close table
********************************************************************************

*===============================================================================
*Figure 15: UKLFS -- Estimated Event Study Coefficients Using DiD
*==============================================================================3

estimates clear 
drop _all

***********************************
**Applying wild cluster bootstrap** 
***********************************

*Locals
local dataset "UKLFS"
local group "group_iasio"
local unit "subgroup"
local outcome lfs_total ln_lfs_total
local model "DiD"
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
	replace `v' = 0 if `v' == .	
	
	*Reference period (omitted due to multicollinearity) 
	*Reference period is the period right before the referendum 
	* --> 2016q2 <=> 15
	
	*Generate a simpler date indicator
	egen date_ind = group(date_stata) 
	
	*DiD Event Study Regression with WCB
	reg `v' i.`group'##ib(15).date_ind i.c_`unit' i.date_ind, ///
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

local inference "wild"
local dataset "UKLFS"
local group "group_iasio"
local unit "subgroup"
local outcome lfs_total ln_lfs_total
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
	if "`v'" == "lfs_total" {
		local title_label "Non-Log-Transformed Inflows"
	}
	else if "`v'" == "ln_lfs_total" {
		local title_label "Log-Transformed Inflows"	
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
	
	*Local for y-axis labels 
	if "`v'" == "lfs_total" {
		local yaxis -40(20)20
	}
	else if "`v'" == "ln_lfs_total" {
		local yaxis -3(1)1
	}	
		
	*Create the plot
	capture graph drop `dataset'_`v'_ev_`inference'
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
		ylabel(`yaxis', angle(0)) ///
		xtitle("") ///
		ytitle("Estimated DiD Coefficients") ///
		title("`title_label' (WCB-Based CIs)") ///
		graphregion(color(white)) bgcolor(white) plotregion(color(white)) ///
		name(`dataset'_`v'_ev_`inference')
	
	restore
	
}
********************************************************************************

***********************************
**Applying robust standard errors**
***********************************

*Locals 
local inference "robust"
local dataset "UKLFS"
local group "group_iasio"
local unit "subgroup"
local outcome lfs_total ln_lfs_total 
local model "DiD"

cd "$master_thesis/Data/`dataset'"
********************************************************************************

****************************************
**Computations and Figure Construction**
****************************************

foreach v of local outcome {
	
	cd "$master_thesis/Data/`dataset'"
	use `dataset'_panel.dta, clear
	
	keep if date_stata >= tq(2012q4) 
	
	*Missing values do not work for sdid (replace them with 0)
	replace `v' = 0 if `v' == .	
	
	*Reference period (omitted due to multicollinearity) 
	*Reference period is the period right before the referendum 
	* --> 2016q2 <=> 14
	
	*Generate a simpler date indicator
	egen date_ind = group(date_stata) 
	
	*DiD Event Study Regression with robust standard errors 
	reg `v' i.`group'##ib(15).date_ind i.c_`unit' ///
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
	
	*Title labels 
	if "`v'" == "lfs_total" {
		local title_label "Non-Log-Transformed Inflows"
	}
	else if "`v'" == "ln_lfs_total" {
		local title_label "Log-Transformed Inflows"	
	}
	
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

	*Local for y-axis labels 
	if "`v'" == "lfs_total" {
		local yaxis -40(20)20
	}
	else if "`v'" == "ln_lfs_total" {
		local yaxis -3(1)1
	}	

	*Generate the Graph
	capture graph drop `dataset'_`v'_ev_`inference'
	coefplot, ///
		color("0 191 127") ciopt(color("0 191 127" "0 191 127") ///
		lwidth(thin thin ) recast( rcap)) ///
		vertical drop(_cons 1.`group' `groupfixed' `timefixed') levels(95 90) /// 
		rename(^1.`group'#([0-9]+).date_ind$ = \1, regex) ///
		coeflabels(`coef_labels', format(%tq) angle(90) labsize(vsmall)) ///
		legend(order(3 "Point Estimate" 2 "90% CI" 1 "95% CI")  ///
			position(6) cols(3) region(style(none) lstyle(solid) lcolor(black) ///
			lwidth(medium))) ///
		yline(0, lcolor(red) lpattern(dash)) ///
		xline(14.5, lcolor(black) lpattern(dash)) ///
		ytitle("Estimated DiD Coefficients") ///
		title("`title_label' (Robust-SE-Based CIs)") ///
		xlabel(, grid) ///
		ylabel(`yaxis' , angle(0)) ///
		bgcolor(white) ///
		plotregion(color(white)) ///
		name(`dataset'_`v'_ev_`inference')	
	
}
********************************************************************************

*Combine graphs and save them as PDF
cd "$master_thesis/Graphs"
grc1leg2 UKLFS_lfs_total_ev_wild UKLFS_lfs_total_ev_robust ///
	UKLFS_ln_lfs_total_ev_wild UKLFS_ln_lfs_total_ev_robust, ///
	leg(UKLFS_lfs_total_ev_wild) iscale(*.7) graphregion(color(white)) ///
	graphregion(margin(zero)) 

graph export "`dataset'_`model'_event_study_combined.pdf", replace as (pdf)
********************************************************************************
	
log close 
cd $my_path_do
*===============================================================================
*===============================================================================
