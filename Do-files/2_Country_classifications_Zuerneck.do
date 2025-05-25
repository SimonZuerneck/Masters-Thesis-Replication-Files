*===============================================================================
*===============================================================================

*Country Classifications  
*Simon Zuerneck 
*Stata Version: 18.0
********************************************************************************

clear all
set more off 
capture log close 
cd $master_thesis
log using Log\2_Country_classifications_Zuerneck.log, text replace 
********************************************************************************

*Directory globals
global my_path_in "C:/Users/simon/OneDrive/Desktop/Data"
********************************************************************************

*===============================================================================
*Countrynames following ISO Country code
*===============================================================================

/*
https://datahub.io/core/country-list
*/

cd "$my_path_in/Countries"
import excel "ISO_country_names.xlsx", firstrow clear
rename _all, lower 
rename name country
rename code country_code

*Correct country names to a Stata-friendly format
replace country = "Aland Islands" if country == "Ã…land Islands"
replace country = "Côte d'Ivoire" if country == "CÃ´te d'Ivoire"
replace country = "Curacao" if country == "CuraÃ§ao"
replace country = "Reunion" if country == "RÃ©union"
replace country = "Saint Barthelemy" if country == "Saint BarthÃ©lemy"
replace country = "Aruba" if country == "Aruba and CuraÃ§ao"


*Standardizing names to remove unnecessary long forms
replace country = "Bolivia" if country == "Bolivia, Plurinational State of"
replace country = "Iran" if country == "Iran, Islamic Republic of"
replace country = "South Korea" if country == "Korea, Republic of"
replace country = "North Korea" if country == "Korea, Democratic People's Republic of"
replace country = "Laos" if country == "Lao People's Democratic Republic"
replace country = "Moldova" if country == "Moldova, Republic of"
replace country = "Russia" if country == "Russian Federation"
replace country = "Syria" if country == "Syrian Arab Republic"
replace country = "Tanzania" if country == "Tanzania, United Republic of"
replace country = "Venezuela" if country == "Venezuela, Bolivarian Republic of"
replace country = "Vietnam" if country == "Viet Nam"
replace country = "United States" if country == "United States of America"
replace country = "United Kingdom" if country == "United Kingdom of Great Britain and Northern Ireland"

*Correcting alternative names
replace country = "Macedonia" if country == "Macedonia, the Former Yugoslav Republic of"
replace country = "Micronesia" if country == "Micronesia, Federated States of"
replace country = "Myanmar" if country == "Burma"
replace country = "Palestine" if country == "Palestine, State of"
replace country = "Timor-Leste" if country == "East Timor"
replace country = "Sao Tome and Principe" if country == "Sao Tome and Principe"
replace country = "Gambia" if country == "Gambia, The"
replace country = "Bahamas" if country == "Bahamas, The"

*Correcting special territories for consistency
replace country = "Vatican City" if country == "Holy See (Vatican City State)"
replace country = "Saint Martin" if country == "Saint Martin (French part)"
replace country = "Sint Maarten" if country == "Sint Maarten (Dutch part)"
replace country = "Saint Helena" if country == "Saint Helena, Ascension and Tristan da Cunha"
********************************************************************************

*===============================================================================
*Countries used to create treatment and control groups following Clifton-Sprigg 
*and World Bank classification from 2016
*===============================================================================

********************************************************************************
/*Treatment group countries 
Austria, Belgium, Bulgaria, Croatia, Cyprus, Czech Republic, Denmark, Estonia, 
Finland, France, Germany, Greece, Hungary, Ireland, Italy, Latvia, Lithuania, 
Luxembourg, Malta, Netherlands, Poland, Portugal, Romania, Slovakia, Slovenia, 
Spain, Sweden

Control group countries 
American Samoa, Antigua And Barbuda, Argentina, Australia, Azerbaijan, 
Bahamas, Bahrain, Barbados, Belarus, Belize, Bermuda, Botswana, Brazil, 
British Virgin Islands, Brunei, Canada, Cayman Islands, Chile, China, Colombia, 
Costa Rica, Cuba, Dominica, Dominican Republic, Ecuador, Equatorial Guinea, 
Fiji, French Polynesia, Gabon, Grenada, Guyana, Hong Kong, Jamaica, Japan, 
Kazakhstan, Kuwait, Lebanon, Libya, Malaysia, Maldives, Mauritius, Mexico, 
Namibia, New Caledonia, New Zealand, Oman, Panama, Paraguay, Peru, Puerto Rico, 
Qatar, Russia, Saudi Arabia, Seychelles, Singapore, South Africa, South Korea, 
St. Kitts And Nevis, St. Lucia, St. Vincent And The Grenadines, Suriname, 
Taiwan, Thailand, Tonga, Trinidad And Tobago, Turkmenistan, 
Turks And Caicos Islands, Tuvalu, United Arab Emirates, United States, 
Uruguay, US Virgin Islands*/
********************************************************************************

*Check if treatment group countries are missing in the country dataset 
levelsof country, local(countries_in_data)
local treatment_group `" "Austria" "Belgium" "Bulgaria" "Croatia" "Cyprus" "Czech Republic" "Denmark" "Estonia" "Finland" "France" "Germany" "Greece" "Hungary" "Ireland" "Italy" "Latvia" "Lithuania" "Luxembourg" "Malta" "Netherlands" "Poland" "Portugal" "Romania" "Slovakia" "Slovenia" "Spain" "Sweden" "'
local missing_countries ""
foreach c in `treatment_group' {
    local found = 0
    foreach d in `countries_in_data' {
        if "`c'" == "`d'" local found = 1  // Exact match including spaces
    }
    if `found' == 0 local missing_countries "`missing_countries' `c'"
}

*Display missing countries
di "The following treatment group countries are missing from the dataset:"
di "`missing_countries'"
*No countries from the treatment group, as by Clifton-Sprigg are missing 

*Now generate a treated group indicator 
gen treatment_group = regexm(country, ///
    "^(Austria|Belgium|Bulgaria|Croatia|Cyprus|Czech Republic|Denmark|Estonia|Finland|France|Germany|Greece|Hungary|Ireland|Italy|Latvia|Lithuania|Luxembourg|Malta|Netherlands|Poland|Portugal|Romania|Slovakia|Slovenia|Spain|Sweden)$")

tab country if treatment_group == 1 
*27 countries identified as treatment units as wanted!
********************************************************************************

*Check if control group countries are missing in the country dataset 
levelsof country, local(countries_in_data)
local control_group `" "American Samoa" "Antigua and Barbuda" "Argentina" "Australia" "Azerbaijan" "Bahamas" "Bahrain" "Barbados" "Belarus" "Belize" "Bermuda" "Botswana" "Brazil" "Virgin Islands, British" "Brunei Darussalam" "Canada" "Cayman Islands" "Chile" "China" "Colombia" "Costa Rica" "Cuba" "Dominica" "Dominican Republic" "Ecuador" "Equatorial Guinea" "Fiji" "French Polynesia" "Gabon" "Grenada" "Guyana" "Hong Kong" "Jamaica" "Japan" "Kazakhstan" "Kuwait" "Lebanon" "Libya" "Malaysia" "Maldives" "Mauritius" "Mexico" "Namibia" "New Caledonia" "New Zealand" "Oman" "Panama" "Paraguay" "Peru" "Puerto Rico" "Qatar" "Russia" "Saudi Arabia" "Seychelles" "Singapore" "South Africa" "South Korea" "Saint Kitts and Nevis" "Saint Lucia" "Saint Vincent and the Grenadines" "Suriname" "Taiwan, Province of China" "Thailand" "Tonga" "Trinidad and Tobago" "Turkmenistan" "Turks and Caicos Islands" "Tuvalu" "United Arab Emirates" "United States" "Uruguay" "Virgin Islands, U.S." "'
 

local missing_countries ""
foreach c in `control_group' {
    local found = 0
    foreach d in `countries_in_data' {
        if "`c'" == "`d'" local found = 1  // Exact match including spaces
    }
    if `found' == 0 local missing_countries "`missing_countries' `c'"
}

*Display missing countries
di "The following control group countries are missing from the dataset:"
di "`missing_countries'"
*No countries from the control group, as by Clifton-Sprigg are missing 

*Now generate a control_group indicator 
gen control_group = regexm(country, ///
    "^(American Samoa|Antigua and Barbuda|Argentina|Australia|Azerbaijan|Bahamas|Bahrain|Barbados|Belarus|Belize|Bermuda|Botswana|Brazil|Virgin Islands, British|Brunei Darussalam|Canada|Cayman Islands|Chile|China|Colombia|Costa Rica|Cuba|Dominica|Dominican Republic|Ecuador|Equatorial Guinea|Fiji|French Polynesia|Gabon|Grenada|Guyana|Hong Kong|Jamaica|Japan|Kazakhstan|Kuwait|Lebanon|Libya|Malaysia|Maldives|Mauritius|Mexico|Namibia|New Caledonia|New Zealand|Oman|Panama|Paraguay|Peru|Puerto Rico|Qatar|Russia|Saudi Arabia|Seychelles|Singapore|South Africa|South Korea|Saint Kitts and Nevis|Saint Lucia|Saint Vincent and the Grenadines|Suriname|Taiwan, Province of China|Thailand|Tonga|Trinidad and Tobago|Turkmenistan|Turks and Caicos Islands|Tuvalu|United Arab Emirates|United States|Uruguay|Virgin Islands, U.S.)$")
tab country if control_group == 1
*72 countries identified as control units as wanted
********************************************************************************

*Generate one dummy for the treated and control group following Clifton
gen group_clifton = . 
replace group_clifton = 1 if treatment_group == 1 
replace group_clifton = 0 if control_group == 1
drop *_group
********************************************************************************
*Save it 
cd "$master_thesis/Data/Countries"
save Countries_ISO.dta, replace
cd "$master_thesis/Data/Countries"
use Countries_ISO.dta, clear
********************************************************************************

*===============================================================================
*Subgregion classification for NINo
*===============================================================================

*World area reporting structure – subgroups and nationalities

*Generate a dataset for these subgroups and nationalities
/*
https://www.gov.uk/government/publications/nino-allocations-to-adult-overseas-nationals-entering-the-uk-background-information/xx#world-area-reporting-structure--subgroups-and-nationalities
*/ 

*Create a dataset out of the subgroups and nationalities there copied into an 
*Excel file (Subgroups_nationalities_NINo)
cd "$my_path_in/Countries
import excel "Subgroups_nationalities_NINo.xlsx", sheet("Sheet1") ///
firstrow clear
rename _all, lower
rename nationality country
des
********************************************************************************

*Try a merge with the Countries_ISO datafile
preserve 
merge 1:1 country using Countries_ISO.dta

*Lets see what got matched and what not 
gen matched = .
replace matched = 1 if _merge == 3 //country matched
replace matched = 2 if _merge == 1 //country only in master dataset
replace matched = 3 if _merge == 2 //country only in using dataset
drop if matched == 1 

cap collapse (max) matched, by(country)

gen match_status = ""
replace match_status = "Matched" if matched == 1
replace match_status = "Only in Master" if matched == 2
replace match_status = "Only in Using" if matched == 3
list country match_status
list country match_status, sepby(match_status)

restore 

*Match the country names to the ones in the Countries_ISO datafile
replace country = "Antarctica" if country == "Antarctic Territories (British)"
replace country = "Antigua and Barbuda" if country == "Antigua"
replace country = "Aruba" if country == "Aruba and Curaçao"
replace country = "Brunei Darussalam" if country == "Brunei"
replace country = "Congo, the Democratic Republic of the" if country == "Congo (Democratic Republic)"
replace country = "Dominican Republic" if country == "Dominican Rep"
replace country = "Timor-Leste" if country == "East Timor"
replace country = "Eswatini" if country == "Swaziland"
replace country = "Falkland Islands (Malvinas)" if country == "Falkland Islands"
replace country = "French Polynesia" if country == "French Polynesia (inc. Tahiti)"
replace country = "Heard Island and McDonald Islands" if country == "Heard Island & McDonald Islands"
replace country = "Côte d'Ivoire" if country == "Ivory Coast"
replace country = "Macao" if country == "Macau"
replace country = "Micronesia" if country == "Federated States of Micronesia"
replace country = "Myanmar" if country == "Burma"
replace country = "Serbia" if country == "Serbia & Montenegro"
replace country = "Sint Maarten" if country == "Sint Maarten (Dutch Part)"
replace country = "South Georgia and the South Sandwich Islands" if country == "South Georgia & South Sandwich Island"
replace country = "Saint Helena" if country == "St Helena"
replace country = "Saint Kitts and Nevis" if country == "St Kitts and Nevis"
replace country = "Saint Lucia" if country == "St Lucia"
replace country = "Saint Pierre and Miquelon" if country == "St Pierre & Miquelon"
replace country = "Saint Vincent and the Grenadines" if country == "St Vincent & Grenadines"
replace country = "Taiwan, Province of China" if country == "Taiwan"
replace country = "Trinidad and Tobago" if country == "Trinidad & Tobago"
replace country = "Turks and Caicos Islands" if country == "Turks & Caicos Islands"
replace country = "United States Minor Outlying Islands" if country == "US Minor Outlying Islands"
replace country = "Venezuela" if country == "Venezuala"
replace country = "Virgin Islands, British" if country == "Virgin Islands (British)"
replace country = "Virgin Islands, U.S." if country == "Virgin Islands (USA)"
replace country = "Wallis and Futuna" if country == "Wallis & Futuna"

*Now do the merge again
merge 1:1 country using Countries_ISO.dta

*Lets see what got matched and what not 
preserve
gen matched = .
replace matched = 1 if _merge == 3 //country matched
replace matched = 2 if _merge == 1 //country only in master dataset
replace matched = 3 if _merge == 2 //country only in using dataset
drop if matched == 1 

cap collapse (max) matched, by(country)

gen match_status = ""
replace match_status = "Matched" if matched == 1
replace match_status = "Only in Master" if matched == 2
replace match_status = "Only in Using" if matched == 3
list country match_status
list country match_status, sepby(match_status) // works better
restore 

*Some countries are still not merged - need to be handled manually 

*1. Can be dropped from master 
drop if country == "Sarawak" // part of Malaysia 
drop if country == "Sharjah" // part of UAE 
drop if country == "Barbuda" // part of Antigua and Barbuda
drop if country == "Sabah" // part of Malaysia 

*2. Can be dropped from using 
drop if country == "Saint Barthelemy" // part of France and EU 
drop if country == "Aland Islands" // to Finland 
drop if country == "Svalbard and Jan Mayen" // to Norway 
drop if country == "Saint Barthelemy" // Part of EU (France)

*3. Everything related to the UK can be dropped 
drop if country == "UK" | country == "Gibraltar" | country == "Isle of Man" ///
	| country == "Jersey" | country == "United Kingdom" ///
	| country == "Guernsey" | country == "Virgin Islands, British"

*4. Need world region 
replace worldregion = "Rest of the World" if country == "Kiribati" ///
	| country == "Curacau" | country == "Northern Mariana Islands" ///
	| country == "Marshall Islands" | country == "Guam" ///
	| country == "Curacao" | country == "Bonaire, Sint Eustatius and Saba" ///
	| country == "Palau" | country == "Nauru" | country == "Saint Martin" ///
	| country == "South Sudan"
replace worldregion = "Non-European Union (Other Europe)" if ///
	country == "Montenegro" 
replace worldregion = "Asia" if country == "Palestine" | country == "" 

*Need subgroup 
replace subgroup = "Central and South America" if country == "Curacau" ///
	| country == "Curacao" | country == "Bonaire, Sint Eustatius and Saba" ///
	| country == "Saint Martin"
replace subgroup = "Other Europe" if country == "Montenegro" 
replace subgroup = "East Africa" if country == "South Sudan"
replace subgroup = "Oceania" if country == "Northern Mariana Islands" ///
	| country == "Marshall Islands" | country == "Guam" ///
	| country == "Palau" | country == "Nauru" | country == "Kiribati"
replace subgroup = "Middle East and Central Asia" if country == "Palestine"

*Need a country code
replace country_code = "AN" if country == "Antilles (Netherlands)" // not EU or Schengen 

drop _merge 
********************************************************************************

*===============================================================================
*Subregions used to create treatment and control groups following DiIasio
*Based on subgroups (not included in the thesis)
*===============================================================================

*Generate one dummy for treatment and control group 
gen group_iasio = . 
replace group_iasio = 0 if subgroup != ""
replace group_iasio = 1 if subgroup == "European Union EU15" | ///
	subgroup == "European Union EU2" | ///
	subgroup == "European Union EU8" | ///
	subgroup == "European Union Other"
********************************************************************************


*===============================================================================
*Countries used to create treatment and control groups 
*Exclude certain countries 
*Lets call this composition: group_simon
*===============================================================================

*Generate one dummy for treatment and control group 
gen group_simon = group_iasio
tab country if group_simon == 1
tab country if group_simon == 0

*Exclude Ireland as Irish migrants were not affected by any changes in the
*freedom of movement (less uncertainty)
replace group_simon = . if country == "Ireland"

*Include EU regions that are outside of Europe 
replace group_simon = 1 if country == "French Guiana"

*Include countries that are "de facto" European Union 
replace group_simon = 1 if country == "Vatican City" | country == "Monaco" ///
	| country == "Andorra"
	
*Exclude EFTA and Schengen Countries countries 
replace group_simon = . if country == "Norway" | country == "Switzerland" | ///
	country == "Liechtenstein" | country == "Iceland"

*Save it 
cd "$master_thesis/Data/Countries"
save Subgroups_countries_harmonized.dta, replace
********************************************************************************

*===============================================================================
*Add years and quarters to it
*===============================================================================

*Quarter and year indicators
clear
set obs 32  
gen date_stata = tq(2012q1) + _n - 1
format date_stata %tq
gen year_stata = year(dofq(date_stata))
format year_stata %ty

*Bring it back together 
cross using Subgroups_countries_harmonized.dta
collapse (first) year worldregion subgroup country_code ///
	group_clifton group_iasio group_simon, by(date_stata country)
sort country date_stata

*Label the groups 
foreach v in "clifton" "iasio" "simon" {
	label var group_`v' "Treatment and Control Group Indicator"
	cap label def Group_`v' 1 "Treated" 0 "Control"
	label val group_`v' Group_`v'	
}

*Save it 
cd "$master_thesis/Data/Countries"
save Subgroups_countries_harmonized.dta, replace
cd "$master_thesis/Data/Countries"
use Subgroups_countries_harmonized.dta, clear
********************************************************************************

*===============================================================================
*Make the dataset only with subgroups 
*===============================================================================

cd "$master_thesis/Data/Countries"
use Subgroups_countries_harmonized.dta, clear

drop *worldregion *country* group_clifton group_simon 
keep if subgroup != ""
collapse (first) group_iasio, by(subgroup date_stata)

*Do the labeling again 
label val group_iasio Group_iasio

*Save a version with only subgroups
cd "$master_thesis/Data/Countries"
save Subgroups_harmonized.dta, replace
********************************************************************************

log close 
cd $my_path_do
*===============================================================================
*===============================================================================