********************************************************************************

*NINo Sample Construction
*Simon Zuerneck 
*Stata Version: 18.0
********************************************************************************

clear all
set more off 
capture log close 
cd $master_thesis
log using Log\4_1_NINo_sample_const_Zuerneck.log, text replace 
********************************************************************************

*Load the data
cd "$my_path_in/NINo"
********************************************************************************

*Load Excel data 
import delimited "NINo_Registrations.csv", delim(",")  varnames(1) clear 
des

*Fill up columns 
foreach v in "nationality" "quarterofregistration" "ageatregistration" {
	replace `v' = `v'[_n-1] if `v' == ""
}
rename nationality country 

*Create a data variable in Stata format
tab quarterofregistration
gen year = substr(quarterofregistration, -2, 2) // extract last 2 digits of year
tab year
replace year = "20" + year
replace year = "" if quarterofregistration == "Total"
destring year, replace 
gen quarter = .
forvalues t = 12/19 {
	replace quarter = 1 if quarterofregistration == "Jan-`t' to Mar-`t'"
	replace quarter = 2 if quarterofregistration == "Apr-`t' to Jun-`t'"
	replace quarter = 3 if quarterofregistration == "Jul-`t' to Sep-`t'"
	replace quarter = 4 if quarterofregistration == "Oct-`t' to Dec-`t'"	
}

*Combine year and quarter in one Stata-format data variables 
gen date_stata = yq(year, quarter)
format date_stata %tq
rename year year_stata 
format year %ty

*Amount NINo registrations
tab number 
destring number, replace force

*Do the necessary renaming
replace country = "Antarctica" if country == "Antarctic Territories (British)"
replace country = "Antigua and Barbuda" if country == "Antigua"
replace country = "Aruba" if country == "Aruba and CuraÃ§ao"
replace country = "Brunei Darussalam" if country == "Brunei"
replace country = "Congo, the Democratic Republic of the" if country == "Congo (Democratic Republic)"
replace country = "Curacao" if country == "Curacao"
replace country = "Côte d'Ivoire" if country == "Ivory Coast"
replace country = "Dominican Republic" if country == "Dominican Rep"
replace country = "Timor-Leste" if country == "East Timor"
replace country = "Eswatini" if country == "Swaziland"
replace country = "Falkland Islands (Malvinas)" if country == "Falkland Islands"
replace country = "French Polynesia" if country == "French Polynesia (inc. Tahiti)"
replace country = "Heard Island and McDonald Islands" if country == "Heard Island & McDonald Islands"
replace country = "Macao" if country == "Macau"
replace country = "Micronesia" if country == "Micronesia (Sub Region)"
replace country = "Myanmar" if country == "Burma"
replace country = "Serbia and Montenegro" if country == "Serbia & Montenegro"
replace country = "Sint Maarten" if country == "Sint Maarten (Dutch Part)"
replace country = "South Georgia and the South Sandwich Islands" if country == "South Georgia & South Sandwich Island"
replace country = "South Sudan" if country == "South Sudan"
replace country = "Saint Barthelemy" if country == "Saint Barthelemy"
replace country = "Saint Helena" if country == "St Helena, Ascension and Tristan da Cunha"
replace country = "Saint Kitts and Nevis" if country == "St Kitts and Nevis"
replace country = "Saint Lucia" if country == "St Lucia"
replace country = "Saint Pierre and Miquelon" if country == "St Pierre & Miquelon"
replace country = "Saint Vincent and the Grenadines" if country == "St Vincent & Grenadines"
replace country = "Taiwan, Province of China" if country == "Taiwan"
replace country = "Timor-Leste" if country == "Timor-Leste"
replace country = "Trinidad and Tobago" if country == "Trinidad & Tobago"
replace country = "Turks and Caicos Islands" if country == "Turks & Caicos Islands"
replace country = "United States Minor Outlying Islands" if country == "US Minor Outlying Islands"
replace country = "Virgin Islands, British" if country == "Virgin Islands (British)"
replace country = "Virgin Islands, U.S." if country == "Virgin Islands (USA)"
replace country = "Wallis and Futuna" if country == "Wallis & Futuna"

*Drop the total amounts and other/unknown countries 
drop if quarterofregistration == "Total"
drop if country == "Total"
drop if country == "Other / unknown"

*Try a merge with the country classifications dataset  
cd "$master_thesis/Data/Countries"
preserve
merge m:1 country date_stata using Subgroups_countries_harmonized.dta


*Lets see what got matched and what not 
gen matched = .
replace matched = 1 if _merge == 3 //country matched
replace matched = 2 if _merge == 1 //country only in master dataset
replace matched = 3 if _merge == 2 //country only in using dataset
drop if matched == 1 

collapse (max) matched, by(country)

gen match_status = ""
replace match_status = "Matched" if matched == 1
replace match_status = "Only in Master" if matched == 2
replace match_status = "Only in Using" if matched == 3
list country match_status
restore

*Recode problematic countries
drop if country == "Barbuda" // no registrations --> can be dropped 
drop if country == "Virgin Islands, British" // part of UK, not if interest

*Now do the merge 
merge m:1 country date_stata using Subgroups_countries_harmonized.dta

*Serbia and Montenegro, and Kosovo need to be added 
replace worldregion = "Non-European Union (Other Europe)" if ///
	country == "Serbia and Montenegro" | country == "Kosovo"	
replace subgroup = "Other Europe" if ///
	country == "Serbia and Montenegro" | country == "Kosovo"	
	
*Add them to the respective groups 
replace group_iasio = 0 if subgroup == "Other Europe"
replace group_simon = 0 if country == "Serbia and Montenegro" ///
	| country == "Kosovo"

*Give numerical values to each country/subregion 
foreach var in "worldregion" "subgroup" "country" {
	sort `var'
	encode `var', gen(c_`var') 
	label values c_`var' . 
}
label var c_worldregion "World Region (coded)"
label var c_subgroup "Sub group (coded)"
label var c_country "Nationality (coded)"

drop _merge
*save the data
cd "$master_thesis/Data/NINo"
save NINo_data.dta, replace
********************************************************************************

*===============================================================================
*Dataset construction for the baseline analysis
*===============================================================================

cd "$master_thesis/Data/NINo"
use NINo_data.dta, clear
sort subgroup country date_stata
keep if date_stata != . // only the panel data is needed 
drop quarterofregistration year_stata quarter
des

/*Create the baseline dataset
Heterogeneity analysis by migrant characteristics: 
(Not conducted in the thesis) 
Characteristics of interest: 
- total inflows 
- by gender (male female)
- by agegroup
- by EU-subregion (following the subregions of the LTIM classification?)
*/

tab ageatregistration
drop if ageatregistration == "Unknown"

*Dataset needs to be prepared
replace ageatregistration = "under_18" if ageatregistration == "Less than 18"
replace ageatregistration = "over_59" if ageatregistration == "60 or over"
replace ageatregistration = subinstr(ageatregistration, "-", "_", .)
replace ageatregistration = lower(ageatregistration)
replace gender = lower(gender)

*Some renaming 
rename ageatregistration age
rename number nino_registrations
tab age

*Replace missing NINo registrations with zero (can be done is for this 
*Administrative data: missing means no registrations in that period, and thus 
*zero)
replace nino_registrations = 0 if nino_registrations == .

*Generate age groups
*(three categories to be formed: unitil 29, 30-44, 45-60 or over)

*Only keep age group totals (sum of both genders)
drop if age != "total" & gender == "female" // drops agegroups by female 
drop if age != "total" & gender == "male" // drops agegroups by male 

*Sum age groups to the three categories
gen age_group = " "
replace age_group = "until_29" if inlist(age, "under_18", "18_24", "25_29")
replace age_group = "30_44" if inlist(age, "30_34", "35_39", "40_44")
replace age_group = "45_more" if inlist(age, "45_49", "50_54", "55_59", "over_59")
replace age_group = "total" if age == "total"

collapse (sum) nino_registrations, by(age_group country gender date_stata worldregion subgroup country_code group_clifton group_iasio group_simon c_worldregion c_subgroup c_country)

br country date_stata nino_registrations age_group gender
drop if gender == "" | age_group == ""

*Reshape 
rename nino_registrations n_
reshape wide n_, i(country date_stata age_group) j(gender) string
reshape wide n_*, i(country date_stata) j(age_group) string
foreach var of varlist n_* {
	
	*Express Nino Registrations in thousands 
	replace `var' = `var'/1000 
	
	*Drop those always empty
    quietly count if !missing(`var')
    if r(N) == 0 {
        drop `var'
        di "Dropped variable `var' (all values missing)"
    }
}

*Do some renaming 
rename n_totaluntil_29 nino_until_29
rename n_total30_44 nino_30_44
rename n_total45_more nino_45_more
rename n_femaletotal nino_female
rename n_maletotal nino_male
rename n_totaltotal nino_total

*Do some labeling 
label var nino_until_29 "NINo Registrations: 20 or Younger"
label var nino_30_44 "NINo Registrations: 33-44"
label var nino_45_more "NINo Registrations: 45 or Older"
label var nino_female "NINo Registrations: Female"
label var nino_male "NINo Registrations: Male "
label var nino_total "NINo Registrations: Total"
********************************************************************************

*===============================================================================
*Built the panel
*===============================================================================

tab country // already perfectly balanced
duplicates report c_country date_stata // none - perfect 
duplicates list c_country date_stata

*Pre-treatment trajectory analysis is set to start for all datasets with 2012q4
keep if date_stata >= tq(2012q4) 

*Duplicates are for countries that do not have a c_country number -- drop them 
keep if c_country !=. 

*Make it a panel
xtset c_country date_stata // strongly balanced --> just what is needed!
xtdescribe

*save the data
cd "$master_thesis/Data/NINo"
save NINo_panel.dta, replace
********************************************************************************

log close 
********************************************************************************