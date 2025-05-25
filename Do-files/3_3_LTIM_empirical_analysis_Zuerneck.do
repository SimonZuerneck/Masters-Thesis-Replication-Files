*===============================================================================
*===============================================================================

*LTIM Empirical Analysis
*Simon Zuerneck 
*Stata Version: 18.0
********************************************************************************

clear all
set more off 
capture log close 
cd $master_thesis
log using Log\3_3_LTIM_empirical_analysis_Zuerneck.log, text replace 
********************************************************************************

*===============================================================================
*Some testing 
*===============================================================================

*Locals
local dataset "LTIM"
local group "group_iasio"
local unit "subgroup"
local outcome inflow_2 inflow_3 inflow_4

cd "$master_thesis/Data/`dataset'"
use `dataset'_panel.dta, clear

xtset c_subgroup date_stata
gen treated_post = (`group' == 1 & date_stata >= tq(2016q3))

foreach v of local outcome {
	
	*Testing for first-order serial correlation (autocorrelation) in the error
	*term 
	xtserial `v' `group' treated_post date_stata
	di "Null hypothesis of no first-order serial correlation rejected at conventional significance levels"
	*Serial Correlation very, very (!) likely

	
	di "Regression with fixed effects included manually"
	reg `v' treated_post i.c_`unit' i.date_stata
	*estat hettest 
	
	*Testing for a violation of the homoscedasticity assumption
	di ""
	di ""
	di "Regression with unit-fixed effects included automatically"
	xtreg `v' treated_post i.date_stata, fe
	xttest3
	di "Null hypothesis of groupwise homoscedasticity rejected at conventional significance levels"
	
}
********************************************************************************

*===============================================================================
*Canonical Difference-in-Differences
*(Unit of analysis: subregion)
*(Estimates based on a balanced panel)
*(No controls, not log-transformed)
*(Inference: wild cluster bootstrap)
*===============================================================================

*===============================================================================
*Table 3: LTIM -- Pooled DiD Specification Estimates
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
		
		*Missing values do not work for sdid (replace them with 0)
		replace `v' = 0 if `v' == .
		
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
file open table using "`dataset'_DiD_pooled_`inference'.tex", write replace

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
*Figure 2: LTIM -- Estimated Event-Study Coefficients Using DiD
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
	
	*Missing values do not work for sdid (replace them with 0)
	replace `v' = 0 if `v' == .	
	
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
		
		*Run boottest (90% CI)
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

*Locals
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

graph export "`dataset'_`model'_event_study_combined.pdf", replace as (pdf)
********************************************************************************

*===============================================================================
*Synthetic Difference-in-Differences 
*(Unit of analysis: subregion)
*(Estimates based on a balanced panel)
*(No controls, not log-transformed)
*(Inference: block bootstrap)
*===============================================================================

*===============================================================================
*Table 5: LTIM -- Pooled SDiD Specification Estimates
*===============================================================================

estimates clear 
drop _all

*Locals
local dataset "LTIM"
local group "group_iasio"
local unit "subgroup"
local outcome inflow_2 inflow_3 inflow_4
local model "SDiD"
local start_date 2014q1 
local inference "bootstrap"

*Temporary dataset to store results
cd "$master_thesis/Data/`dataset'"
capture erase "resultsfile.dta"
tempname results
postfile `results' str6(model) str15(outcome) str15(period) ///
    str15(coef_stars) float(n pval) str25(ci) using resultsfile.dta, replace
	
use `dataset'_panel.dta, clear
********************************************************************************

****************
**Computations**
****************
	
*Gen treated post indicator 
gen treated_post = (`group' == 1 & date_stata >= tq(2016q3))

*SDiD Approach 
foreach t in 2018q1 2019q4 {
	
    foreach v of local outcome {
		
        preserve
		
        keep if date_stata >= tq(`start_date') & date_stata <= tq(`t')
		
		*Missing values do not work for sdid (replace them with 0)
		replace `v' = 0 if `v' == .
		
		*SDiD Regressions
        sdid `v' `unit' date_stata treated_post, method(sdid) ///
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
		post `results' ("`model'") ("`v'") ("`t'") ("`coef_stars'") (`n') ///
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
	file write table " & ATT" 
	foreach v in inflow_2 inflow_3 inflow_4 {
		qui levelsof coef_stars if model == "SDiD" & outcome == "`v'" ///
		& period == "`t'", local(bvals)
		local b : word 1 of `bvals'
		file write table " & `b'"
	}
	file write table " \\" _n

	*P-values
	file write table " & p-value"
	foreach v in inflow_2 inflow_3 inflow_4 {
		qui su pval if model == "`model'" & outcome == "`v'" ///
		& period == "`t'", meanonly
		local p = string(r(mean), "%9.3f")
		file write table " & (`p')"
	}
	file write table " \\" _n

	*Confidence intervals
	file write table " & 95\% CI"
	foreach v in inflow_2 inflow_3 inflow_4 {
		qui levelsof ci if model == "`model'" & outcome == "`v'" ///
		& period == "`t'", local(civals)
		local c : word 1 of `civals'
		file write table " & `c'"
	}
	file write table " \\" _n

	*Number of observations
    file write table " & N"
    foreach v in inflow_2 inflow_3 inflow_4 {
        su n if model=="`model'" & outcome=="`v'" & period=="`t'", meanonly
        local n = string(r(mean),"%9.0f")
        file write table " & `n'"
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
*Figure 5: LTIM -- Period- and Unit-Weighting for SDiD Estimates by Outcome
*===============================================================================

estimates clear 
drop _all

*Locals
local dataset "LTIM"
local group "group_iasio"
local unit "subgroup"
local outcome inflow_2 inflow_3 inflow_4
local model "SDiD"
local start_date 2014q1 //10 pre-treatment periods considered

cd "$master_thesis/Data/`dataset'"
use `dataset'_panel.dta, clear
********************************************************************************

****************
**Computations**
****************

*Gen treated post indicator 
gen treated_post = (`group' == 1 & date_stata >= tq(2016q3))
 
foreach v of local outcome {
	
	preserve 
	
	keep if date_stata >= tq(`start_date')
	
	*Missing values do not work for sdid (replace them with 0)
	replace `v' = 0 if `v' == .
	
	*Local for the title label 
	local title_label : variable label `v'
	di "`title_label'"

	*Generate the x-axis labels for the time_w plot
	local xlabels ""
	forvalues q = `=tq(`start_date')'/`=tq(2019q4)' {
		if mod(`q', 4) == 0 | mod(`q', 4) == 2 { 
			local xlabels "`xlabels' `q'"
		}
	}
	
	*Rename subregions for proper labeling of the unit unit_w graph
	replace subgroup = "EU 14" if subgroup == "European Union EU15"
	*As UK is excluded 
	replace subgroup = "EU8" if subgroup == "European Union EU18"
	replace subgroup = "EU2" if subgroup == "European Union EU2"
	replace subgroup = "EU Other" if subgroup == "European Union Other"
	replace subgroup = "LAC" if subgroup == "Central and South America"
	replace subgroup = "NAM" if subgroup == "North America"
	replace subgroup = "EA" if subgroup == "East Asia"
	replace subgroup = "ME&CA" if subgroup == "Middle East and Central Asia"
	replace subgroup = "SA" if subgroup == "South Asia"
	replace subgroup = "SEA" if subgroup == "South East Asia"
	replace subgroup = "NA" if subgroup == "North Africa"
	replace subgroup = "SSA" if subgroup == "Sub-Saharan Africa"
	replace subgroup = "OC" if subgroup == "Oceania"
	replace subgroup = "Europe Other" if subgroup == "Other Europe"
	tab subgroup
		 
	*SDiD Regressions	
	capture graph drop `dataset'_`model'_time_w_`v'
	capture graph drop `dataset'_`model'_unit_w_`v'
	sdid `v' `unit' date_stata treated_post, method(sdid) ///
	vce(noinference) graph g1on ///
	g2_opt( ///
		legend(order(2 "EU Inflows " 1 "Non-EU Inflows" ) ///
			position(6) cols(2) region(style(none) lstyle(solid) ///
			lcolor(black) lwidth(medium))) ///
		legend(size(small) colgap(vsmall) keygap(small)) ///
		xlabel(`xlabels', format(%tq) angle(0) grid labsize(vsmall)) ///
		xline(`=tq(2016,2) + (23/91)', lcolor(black) lpattern(dash)) ///
		xtitle("") ytitle("Mean Inflows per Unit (Subregion)") ///
		ylabel(, angle(0)) ///
		title(" ", size(medsmall)) ///
		graphregion(color(white)) ///
		bgcolor(white) ///
		plotregion(color(white)) ///
	) ///
	g1_opt( ///
		msize(large) ///
		xlabel(, angle(0) grid labsize(small)) ///
		xtitle("") ///
	)
	
	*Rename and save the graphs
	cd "$master_thesis/Graphs"
	graph display g2_226
	graph rename g2_226 `dataset'_`model'_time_w_`v'
	graph export "`dataset'_`model'_time_w_`v'.pdf", replace as (pdf)
	graph display g1_226
	graph rename g1_226 `dataset'_`model'_unit_w_`v'
	graph export "`dataset'_`model'_unit_w_`v'.pdf", replace as (pdf)
	
	restore 
}	
********************************************************************************

*===============================================================================
*Figure 6: LTIM -- Estimated Event-Study Coefficients Using SDiD
*===============================================================================

estimates clear 

*Locals
local dataset "LTIM"
local group "group_iasio"
local unit "subgroup"
local outcome inflow_2 inflow_3 inflow_4
local model "SDiD"
local start_date 2014q1 //10 pre-treatment periods considered
local se_sdid "bootstrap"
local `dataset'_`model'_event_study ""

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

foreach v of local outcome {
	
	preserve

	keep if date_stata >= tq(`start_date')
	
	*Generate a simpler date indicator
	egen date_ind = group(date_stata) 

	*Gen treated post indicator 
	gen treated_post = (`group' == 1 & date_stata >= tq(2016q3))
	
	*Missing values do not work for sdid (replace them with 0)
	replace `v' = 0 if `v' == .
	
	*SDiD event does not return number of observations --> obtain it manually 
	*from the final sample to compute p-values
	count
	local n = r(N)		
	display "This is n: `n'"

	*Local for the title label 
	local title_label : variable label `v'
	
	*SDiD Regressions
	sdid_event `v' `unit' date_ind treated_post, ///
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
    post `results' ("`model'") ("`v'") ("`rn'") (`coef') (`se') (`lb') ///
		(`ub') ("`ci'") (`tstat')  (`pval') (`n') ("`coef_stars'")
    }

	*Store the estimates for the graph
	mat res = e(H)[2..25,1..5]
	svmat res
	gen time = _n - 1 if !missing(res1)
    replace time = 14 - _n if _n > 14 & !missing(res1)
	sort time
	
	*Generate the label mapping
	local first = tq(`start_date')
	local last  = tq(2019q4)

	local xlabels ""
	local time = -10

	forvalues q = `first'/`last' {
		local qnum = mod(`q', 4) + 1  // because tq() returns a counter not quarter
		if inlist(`qnum', 1, 3) {
			local label : display %tq `q'
			local xlabels `xlabels' `time' "`label'"
		}
		local ++time
	}
		
	*Put a vertical line at the time when the treatment occurs 
	local pre_quarter = tq(2016q2)
	local precise = `pre_quarter' + 23/91
	local referendum = `precise' - `first' - 10

	*Generate plot without titles 
	capture graph drop `dataset'_`v'_ev_SDiD
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
		title("Migration Reason: `title_label'", ///
			tstyle(heading) color(black) position(12) ring(100)) ///
		scheme(stgcolor) /// 
		graphregion(color(white)) ///
		bgcolor(white) ///
		plotregion(color(white)) ///
		name(`dataset'_`v'_ev_SDiD) 
		
	*Store the graphs
	local `dataset'_`model'_event_study "``dataset'_`model'_event_study' `dataset'_`v'_ev_SDiD"	
	
	restore
}
postclose `results'
di "Stored graphs : ``dataset'_`model'_event_study'"

*Combine graphs and safe them as PDF
cd "$master_thesis/Graphs"
grc1leg2 ``dataset'_`model'_event_study', ///
leg(`dataset'_inflow_4_ev_SDiD) iscale(*.7) graphregion(color(white)) ///
graphregion(margin(zero)) 


graph export "`dataset'_`model'_event_study_combined.pdf", replace as (pdf)
********************************************************************************

*===============================================================================
*Extra: generate the tables for the SDiD event-study 
*===============================================================================

*Locals
local dataset "LTIM"
local group "group_iasio"
local unit "subgroup"
local outcome inflow_2 inflow_3 inflow_4
local model "SDiD"
local start_date 2014q1 //10 pre-treatment periods considered
local inference "bootstrap"
********************************************************************************

*Table for the post-reform periods: 
cd "$master_thesis/Data/`dataset'"
use resultsfile.dta, clear
drop if period == "ATT"
drop if strpos(period, "Placebo")
gen effect_num = real(regexs(1)) if regexm(period, "Effect_([0-9]+)")
gen date_stata = tq(2016q3) + effect_num - 1
format date_stata %tq
gen quarter = string(date_stata, "%tq")

*Open table
cd "$master_thesis/Tables"
capture file close table
file open table using "`dataset'_`model'_event_post_`inference'.tex", write replace

*Write the header 
file write table "\begin{tabular}{l@{\hskip 22pt}ccc}" _n
file write table "\hline\midrule" _n
file write table "& (1) & (2) & (3) \\" _n
file write table "& Work Related & Definite Job & Looking for Work \\" _n
file write table "\midrule" _n

*Loop through the obtained results (14 post-reform quarters )
levelsof quarter, local(quarters)
foreach t of local quarters {
    
    *Interaction term coefficient with stars
    file write table "EU x `t'"
    foreach v of local outcome {
        capture levelsof coef_stars if model == "`model'" & outcome == "`v'" ///
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
    foreach v of local outcome {
        qui su pval if model == "`model'" & outcome == "`v'" &  ///
		quarter == "`t'", meanonly
        local p = string(r(mean), "%9.3f")
        file write table " & (`p')"
    }
    file write table " \\" _n

    *Confidence Intervals
    file write table "95\% CI"
    foreach v of local outcome {
        capture levelsof ci if model == "`model'" & outcome == "`v'" & ///
		quarter == "`t'", local(civals)
        local c : word 1 of `civals'
        file write table " & `c'"
    }
    file write table " \\[0.5em]" _n

    *Observations only at last quarter
    if "`t'" == "2019q4" {
        file write table "\cmidrule(lr){1-4} " _n
        file write table "N"
        foreach v of local outcome {
            su n if model=="`model'" & outcome=="`v'" & quarter=="`t'", meanonly
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
keep if strpos(period, "Placebo")
gen effect_num = real(regexs(1)) if regexm(period, "Placebo_([0-9]+)")
gen date_stata = tq(2014q1) + effect_num - 1
format date_stata %tq
gen quarter = string(date_stata, "%tq")

*Open table
cd "$master_thesis/Tables"
capture file close table
file open table using "`dataset'_`model'_event_pre_`inference'.tex", write replace

*Write the header 
file write table "\begin{tabular}{l@{\hskip 22pt}ccc}" _n
file write table "\hline\midrule" _n
file write table "& (1) & (2) & (3) \\" _n
file write table "& Work Related & Definite Job & Looking for Work \\" _n
file write table "\midrule" _n

*Loop through the obtained results (14 post-reform quarters )
levelsof quarter, local(quarters)
foreach t of local quarters {
    
    *Interaction term coefficient with stars
    file write table "EU x `t'"
    foreach v of local outcome {
        capture levelsof coef_stars if model == "`model'" & outcome == "`v'" ///
		& quarter == "`t'", local(bvals)
        if _rc == 0 {
            local b : word 1 of `bvals'
            file write table "& `b'"
        }
        else file write table "& ."
    }
    file write table " \\" _n
    
    *p-values
    file write table "p-value"
    foreach v of local outcome {
        qui su pval if model == "`model'" & outcome == "`v'" &  ///
		quarter == "`t'", meanonly
        local p = string(r(mean), "%9.3f")
        file write table " & (`p')"
    }
    file write table " \\" _n

    *Confidence Intervals
    file write table "95\% CI"
    foreach v of local outcome {
        capture levelsof ci if model == "`model'" & outcome == "`v'" & ///
		quarter == "`t'", local(civals)
        local c : word 1 of `civals'
        file write table " & `c'"
    }
    file write table " \\[0.5em]" _n

    *Observations only at last quarter
    if "`t'" == "2016q2" {
        file write table "\cmidrule(lr){1-4} " _n
        file write table "N"
        foreach v of local outcome {
            su n if model=="`model'" & outcome=="`v'" & quarter=="`t'", meanonly
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
