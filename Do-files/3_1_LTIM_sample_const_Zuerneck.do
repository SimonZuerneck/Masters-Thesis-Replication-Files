*===============================================================================
*===============================================================================

*LTIM Sample Construction
*Simon Zuerneck 
*Stata Version: 18.0
********************************************************************************

clear all
set more off 
capture log close 
cd $master_thesis
log using Log\3.1_LTIM_sample_const_Zuerneck.log, text replace 
********************************************************************************

/*Good to know: 

*LTIM (Long Term International Migration)	

EU15: Austria, Belgium, Denmark, Finland, France, Germany, Greece, Italy, 
Luxembourg, Netherlands, Portugal, Republic of Ireland, Spain, Sweden, 
the United Kingdom**
EU8: Czech Republic, Estonia, Hungary, Latvia, Lithuania, Poland, Slovakia,
Slovenia
EU2: Bulgaria and Romania
EU other: Malta, Cyprus and Croatia

**British citizens are excluded from all citizenship groupings and are shown separately. 

Brexit referendum took place on June 23, 2016 --> second quarter 2016q2
*/
********************************************************************************

*===============================================================================
*Dataset construction 
*===============================================================================

*Load Excel data 
cd "$my_path_in/LTIM"
import excel "LTIM_quarterly_reason_citizenship_flows.xlsx", sheet("Sheet1") ///
firstrow clear
*Dataset needs some restructuring 

*Rename variables
rename Mainreasonformigration date 
rename Allcitizenships est_all_citizenships 
rename C ci_all_citizenships 
rename BritishIncludingOverseasTerr est_brit_incl_oversea_terr
rename F ci_brit_incl_oversea_terr

rename NonBritish est_non_british
rename H ci_non_british

rename EuropeanUnion2 est_eu
rename J ci_eu
rename K est_eu_15
rename L ci_eu_15
rename M est_eu_8 
rename N ci_eu_8 
rename O est_eu_2
rename P ci_eu_2
rename Q est_eu_other 
rename R ci_eu_other 

rename NonEuropeanUnion3 est_non_eu
rename U ci_non_eu
rename V est_other_europe 
rename W ci_other_europe 
rename X est_asia
rename Y ci_asia 
rename Z est_middle_est_central_asia
rename AA ci_middle_est_central_asia
rename AB est_east_asia
rename AC ci_east_asia 
rename AD est_south_asia 
rename AE ci_south_asia
rename AF est_sea 
rename AG ci_sea
rename AI est_rest_world
rename AJ ci_rest_world
rename AK est_ssa
rename AL ci_ssa
rename AM est_north_africa
rename AN ci_north_africa 
rename AO est_north_america 
rename AP ci_north_america 
rename AQ est_c_s_america 
rename AR ci_c_s_america 
rename AS est_oceania
rename AT ci_oceania 
rename AU est_stateless 
rename AV ci_stateless

*Get rid of all (meaningless) labels 
des 
foreach var of varlist _all {
    label variable `var' "" 
    cap label values `var' "" 
}

*Drop empty columns only containing "." 
des
foreach var of varlist _all {
    capture confirm numeric variable `var'
    if _rc == 0 { 
        quietly summarize `var'
        if r(max) == . & r(min) == . { 
            drop `var'
        }
    }	
}
des

*Bring variables in the correct format 
*The numbers behind the names are not a mistake but part of the name in the 
*original dataset 
gen mig_reason = 0 
replace mig_reason = 1 if date == "All reasons"
replace mig_reason = 2 if date == "Work related4"
replace mig_reason = 3 if date == "Definite job"
replace mig_reason = 4 if date == "Looking for work"
replace mig_reason = 5 if date == "Accompany/Join"
replace mig_reason = 6 if date == "Formal study"
replace mig_reason = 7 if date == "Going home to live5"
replace mig_reason = 8 if date == "Other"
replace mig_reason = 9 if date == "No reason stated6"

lab define mig_lbl 1 "All Reasons" 2 "Work Related" 3 "Definite Job" 4 ///
"Looking for Work" 5 "Accompany/Join" 6 "Formal Study" 7 ///
"Going Home to Live" 8 "Other" 9 "No Reason Stated"
lab values mig_reason mig_lbl 

*Fill out the migration dummy throughout the survey dates
gen temp_mig_reason = mig_reason
replace temp_mig_reason = temp_mig_reason[_n-1] if temp_mig_reason == 0 

*Verify that it worked and restore labels 
drop mig_reason
rename temp_mig_reason mig_reason
lab values mig_reason mig_lbl 
tab mig_reason

*Generate a flow dummy
gen flow_dummy = 0
replace flow_dummy = 1 if date == "Outflow"
gen cum_flow_dummy = sum(flow_dummy)
replace flow_dummy = cum_flow_dummy
drop cum_flow_dummy
lab define flow_lbl 0 "Inflow" 1 "Outflow"
lab values flow_dummy flow_lbl

*Generate a numeric survey date variable (format MMYY)
tab date //lots of junk in here 
gen temp_month = substr(date, 4, 3)
gen temp_year = substr(date, 8, 2)

*Generate a numeric quarter variable
gen quarter = . //remember that data is quarterly surveyed 
local months = "Mar Jun Sep Dec"
forval q = 1/4 {
	local abbrev : word `q' of `months'
	replace quarter = `q' if temp_month == "`abbrev'"
}
tab quarter, missing //worked
*The missing values flag values in the original date variable that are not 
*actual dates (and thus not in the specific date format "YE MMM YY)

*Generate a numeric year variable (the data is from 2010 to 2020)
cap gen year = .  
forvalues y = 10/20 {
	replace year = 2000 + `y' if temp_year == "`y'"
}
tab year, missing //worked 
tab quarter, missing 
*The amount of missing year values matches missing quarter values --> perfect!

*Combine year and quarter in one stata-format data variables 
gen date_stata = quarterly(string(year) + "q" + string(quarter), "YQ")
format date_stata %tq

*Clean up  
drop temp*
drop if quarter == . 
tab date 

*Create a flag for the weird 2020 date 
gen date_flag = 0 
replace date_flag = 1 if date == "YE Mar 20p"
label var date_flag "Weird 2020 Data"
rename date date_string 

*Destring numeric data
destring, replace
des // check for which it did not work 
ds, has(type string)
local stringvars `r(varlist)'
display "`stringvars'"
*date ci_all_citizenships ci_non_british ci_non_eu
*Ok problematic some variables are string even though they are supposed to be 
*numeric

*Lets take a look at what the string variables contain
foreach v of local stringvars {
	tab `v' //Lets change ":" to missing:
	replace `v' = "." if `v' == ":"
	tab `v', m 
	destring, replace 
}
des

*Change all variable names to lowercase (is it from wide to long)
rename _all, lower

*Reshape the dataset from long wide 
reshape long est_ ci_, i(date_stata mig_reason flow_dummy) ///
j(category) string


*Drop unnecessary variables and do some renaming 
drop date_string
rename est_ estimation 
rename ci_ ci_interval

*Create a nationality variable 
gen nationality = ""

*Treated Group (4 different units of observation)
replace nationality = "European Union EU15" if category == "eu_15"
replace nationality = "European Union EU8" if category == "eu_8"
replace nationality = "European Union EU2" if category == "eu_2"
replace nationality = "European Union Other" if category == "eu_other"

*Control Group (10 different units of observation)
replace nationality = "Central and South America" if category == "c_s_america"
replace nationality = "North America" if category == "north_america"

replace nationality = "East Asia" if category == "east_asia"
replace nationality = "Middle East and Central Asia" if category == "middle_est_central_asia"
replace nationality = "South Asia" if category == "south_asia"
replace nationality = "South East Asia" if category == "sea"

replace nationality = "North Africa" if category == "north_africa"
replace nationality = "Sub-Saharan Africa" if category == "ssa"

replace nationality = "Oceania" if category == "oceania"

replace nationality = "Other Europe" if category == "other_europe"

*Merge it with the subgroup classifications dataset
rename nationality subgroup
tab category if subgroup == "" // all those are just broader categories, British 
*Or stateless 
drop if subgroup == ""

cd "$master_thesis/Data/Countries"
merge m:1 subgroup date_stata using Subgroups_harmonized.dta
sort subgroup category date_stata
keep if date_stata >= tq(2012q4) & date_stata <= tq(2019q4) // period of analysis
tab subgroup if _merge == 2 // east Asia category does not not exist 
drop if subgroup == "East Africa"

*Encode subgroup
encode subgroup, gen(c_subgroup)
drop _merge

*Safe the structured LTIM dataset
cd "$master_thesis/Data/LTIM"
save LTIM_long.dta, replace
export delimited LTIM, replace 
********************************************************************************

*===============================================================================
*Sample construction
*===============================================================================

cd "$master_thesis/Data/LTIM"
use LTIM_long.dta, replace

*Only keep inflows
keep if flow_dummy == 0
drop flow_dummy quarter year 
rename estimation inflow_
rename ci_interval ci_inflow_
tab mig_reason

*Make mig_reason a variable 
reshape wide inflow_ ci_inflow_, i(category date_stata date_flag subgroup c_subgroup group_iasio ) j(mig_reason)

*Collapse
collapse (firstnm) *inflow* date_flag subgroup c_subgroup group_iasio, ///
	by(category date_stata)
  
*Assign mig_reason value labels as variable labels
foreach t in "inflow_" "ci_inflow_" {
	
	label var `t'1 "All Reasons"
	label var `t'2 "Work Related"
	label var `t'3 "Definite Job"
	label var `t'4 "Looking for Work"
	label var `t'5 "Accompany/Join"
	label var `t'6 "Formal Study"
	label var `t'7 "Going Home to Life"
	label var `t'8 "Other"
	label var `t'9 "No Reason Stated"
}
des
********************************************************************************

*Make it a panel 
keep if subgroup != ""
keep subgroup c_subgroup date_stata *inflow* group_iasio
xtset c_subgroup date_stata
duplicates report c_subgroup date_stata // no duplicates
 
*Gen a log-transformed version of the dependent variable 
forvalues t = 1/9 {
	gen ln_inflow_`t' = ln(inflow_`t')
}

*Save it 
cd "$master_thesis/Data/LTIM"
save LTIM_panel.dta, replace
********************************************************************************

log close 
cd $my_path_do
*===============================================================================
*===============================================================================