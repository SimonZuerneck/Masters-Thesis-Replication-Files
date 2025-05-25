*===============================================================================
*===============================================================================

*UKLFS Sample Construction  
*Simon Zuerneck 
*Stata Version: 18.0
********************************************************************************

clear all
set more off 
capture log close 
cd $master_thesis
log using Log\5_1_UKLFS_sample_const_Zuerneck.log, text replace 
********************************************************************************

*Load the data
cd "$my_path_in/UKLFS"

*List all files 
local UKLFS : dir . files "*.dta"
di `UKLFS'
********************************************************************************

*File naming conventions: 

*lfsp = labour force survey personal data

*eul = end user license 
*eul_pwt = person-weighted teaching dataset 
*end_user = end user dataset 

*jm = January-March (Q1)
*aj = April-June (Q2)
*js = July-September (Q3)
*od = October-December (Q4)
*YY = year 

*Data from jm 2012q4 to 2021q4 to capture inflows until 2019q4 based on month of 
*arrival

*File name example: lfsp_aj12_end_user.dta
********************************************************************************

*===============================================================================
*Cross-sectional data (four datasets each year) --> needs to be appended 
*===============================================================================

*For variables of interest, use a 2016 dataset, as this is the most important one 
*then check for the other quarters whether all relevant variables of interest are 
*included and whether the variables have different names
use lfsp_js16_eul_pwt18.dta, clear

*Change all variable's names to lowercase 
rename _all, lower
des

/*Variables of interest: 

Survey relevant variables: 
- casenop (case number - pseudonymous person identifier, cannot be used to link 
people across periods, available on eul)
- caseno (Same but not available on eul)
- week (week no. when the interview took place)
- w1yr (year that address entered survey)
- qrtr (quarter that the address first entered the survey)
- hhld (household number)
- wavfnd (wave at which household was first found) (1 to 5) (allows a change of 
the household to be identified)
- thiswv (wave to which data refers)
- refwkd (reference day for current interview)
- refwkm (reference month for current interview)
- refwky (reference year for current interview)
- respno (person number of the person who completed the interview)
- pwt18 (person weight)

Migrant identifiers:
- cryo7 (country of birth)
- cryox7 (country of birth like cryo7 but all UK grouped together)
- nato7 (nationality with not all coverage)
- natox7 (nationality with all coverage)
- cameyr2 (year of last arrival to UK)
- cameyr (year of first arrival to UK)
- camemt (month of most recent arrival to the UK)

Proxies for work-related migration:
- conmon (month started current job)
- conmpy (year started working with current employer) (work migrants should have 
recent job start dates)
- stucur (whether full-time student) (exclude students)
- stat (employment status) (look also for other variables related to that)
- inde07m (industry sector in the main job) (identify industries with high migrant 
employment

Possible variables of interest: 
	Age
	- age
	Sex
	- sex 
	Education 
	- hiqual15 hiqual15d (Highest qualification / detailed grouping)
	- hiqual11 hiqual11d (in some datasets instead of hiqual15/hiqual15d)
	- soc10m (occupation in main job)
	- edage (age when completed full-time education)
	- sc10mmj (major occupation group (main job))
	- levqul11 levqul15 (Level of highest qualification held)
*/
********************************************************************************

*Starting with lfsp_js17_eul_pwt18.dta (2017Q3), cryo7 cryox7 nato7 natox7 are 
*missing as country-level migrant information are not assessable anymore. Hence 
*the introduced broader categories need to be used: 
*(cryox7_eul_main cryox7_eul_sub natox7_eul_main natox7_eul_sub)

*Let's start appending
*(2012Q2 - 2019Q4 own time horizon)
*(This is rather complicated due to the naming conventions of the datasets and
*changes in variable names and variable existence over time)
*Create a temporary file combining the cross-sections one after another 
clear
tempfile combined
save `combined', emptyok

*Keep variables of interest 
local survey_ident "casenop caseno week w1yr qrtr wavfnd hhld thiswv refwkd refwkm refwky respno pwt18"
*Note caseno* to cover both caseno and casenop 
*(pseudonymised case identifier)
local mig_ident "cryo7 cryox7 nato7 natox7 cameyr2 cameyr camemt"
local mig_ident_grouped "cryox7_eul_main cryox7_eul_sub natox7_eul_main natox7_eul_sub"
local prox_mig_work "conmon conmpy stucur stat inde07m"
local interest_var "age sex hiqual15 hiqul15d hiqual11 hiqul11d soc10m edage sc10mmj levqul11 levqul15 testdummy"
*Testdummy included to check whether the code works 

*Have all variables of interest in one local
local keep_var "`survey_ident' `mig_ident' `prox_mig_work' `mig_ident_grouped' `interest_var' "

*Generate year counter
local count_y = 11

*Year loop 
foreach year in 12 13 14 15 16 17 18 19 20 21 {

*foreach year in 12 {
	
	*Update year counter 
	local count_y = `count_y' + 1 
	di "Survey year `count_y'"
	
	*Generate quarter counter
	local count_q = 0 
	
	*Quarter loop 
    foreach quarter in jm aj js od {
 
		*Update quarter counter 
		local count_q  = `count_q' + 1 
		di "survey quarter `count_q'"
		
		*Get all .dta files in the diectory
        local files : dir . files "*.dta"
		
		*Loop through all files and check for matches
        foreach f in `files' {
            
            *Check if the file name matches the pattern
            if strpos("`f'", "lfsp_`quarter'`year'_") {
                di "Matching file: `f'"

                *Load the respective file 
                use "`f'", clear
				
				*Change all variables names to lower case 
				rename _all, lower
				
				*Check that the variables are in all the quarterly datasets that 
				*will be appended. If not, report the variable but keep the code 
				*running and keeping relevant variables
				quietly ds 
				local existing `r(varlist)'
				local missing : list keep_var - existing
				if "`missing'" != "" {
					*di as error "Warning: The following vars are missing in 
					*`f': `missing'"
				}
				
				*Create placeholders for each missing variable 
				foreach m of local missing {
					quietly gen `m' = .
					*des `m' // remove * to see which variables are kept 
				}
				keep `keep_var'
				
				*Now drop those variables again  (necessary *step to keep the 
				*labels) however, keep possible string variables
				foreach v of varlist `keep_var' {
					
				*Check if the variable is numeric 
				capture confirm numeric variable `v'
				
				*Proceed if numeric 
				if _rc == 0 {
					quietly summarize `v', meanonly		
						if r(N) == 0 {
							drop `v'
							di as error "Dropped `v' in `f' (no observations for `v')"
						}
						else if r(N) == r(miss) {
							drop `v'
							di as error "Dropped `v' in `f' (all observations for `v' missing)"
						}
					}
					else {
						di as txt "`v' is a string variable; skipping."
					}
				}
				
				*Generate the survey date dummies
				gen quarter = `count_q' 
				gen year = `count_y'
				
				*One additional dummy to be sure 
				gen survey_date = "`count_y'_`count_q'"
				
				*Take another look to see that the loop actually works 
				di "The date of this survey wave is: `count_y'_`count_q'"
				tab year 
				tab quarter 
				
				*Append waves
                quietly append using `combined' 
                save `combined', replace
			}		
		}
	}
}

*Combine year and quarter in one stata-format data variables 
replace year = year + 2000 
gen date_survey = yq(year, quarter)
format date_survey %tq
drop quarter year

*Add labels
label var date_survey "Survey Date in Stata Quarterly Format"

*Pre-treatment trajectory analysis is set to start for all datasets with 2012q4
keep if date_survey >= tq(2012q4) // CHECK THIS AGAIN! 
*But for date of arrival and not date of survey

*Convert casenumber to numeric
destring casenop, replace

*Save 
cd "$master_thesis/Data/UKLFS"
save UKLFS_combined.dta, replace
********************************************************************************

*===============================================================================
*keep only respondents with a migration background who moved to the UK in the 
*period of interest in the dataset (largely following Iasio)
*===============================================================================

cd "$master_thesis/Data/UKLFS"
use UKLFS_combined.dta, clear

*Keep respondents for which month and year of arrival are applicable
tab cameyr, nolabel // year of first arrival 
tab cameyr2, nolabel // year of last arrival
keep if camemt > 0 // keep if the year of first arrival to the UK is applicable 
replace cameyr2 = 0 if cameyr2 == -9 // -9: does not apply 
replace cameyr = cameyr2 if cameyr2 > cameyr // only capture the last arrival
keep if cameyr >= 2012 // period of pre-treatment analysis start in 2012
tab cameyr2

sort date_survey

*Generate an arrival quarterly date variable
tab camemt
tab camemt, m // perfect - no missings 
gen quarter = .
replace quarter = 1 if inrange(camemt, 1, 3)
replace quarter = 2 if inrange(camemt, 4, 6)
replace quarter = 3 if inrange(camemt, 7, 9)
replace quarter = 4 if inrange(camemt, 10, 12)
gen date_stata = yq(cameyr, quarter)
format date_stata %tq
label var date_stata "Arrival Date in Stata Quarterly Format"
tab date_stata, m // perfect - no missings 
drop cameyr cameyr2 camemt

*Period of inflow analysis starts with 2012q4 and ends with 2019q4 
keep if date_stata >= tq(2012q4) & date_stata <= tq(2019q4)

*Drop if information on highest qualification does not apply (following di Iasio)
*This step ensures that only individuals having reached a certain age (16) are
*kept in the sample
tab hiqul15d
tab hiqul15d, nolabel
drop if hiqul15d == -9

tab hiqul11d
tab hiqul11d, nolabel
drop if hiqul11d == -9

*Include all respondents of relevance for the first quarter of analysis (2019q4)
*(Following the paper by Iasio)
*For all subsequent quarters: keep only respondents in their first wave to avoid
*double counting
tab thiswv
drop if thiswv != 1 & date_survey != tq(2012q4)
tab date_stata if thiswv != 1 //worked 
********************************************************************************

*Now the trouble begins: 
*<= 2017q2: nato7 natox7 cryo7 cryox7 (country level origin info) available
*> 2017q2: only natox7_eul_main natox7_eul_sub cryox7_eul_main cryox7_eul_sub
*(region level oring info) available 

*Two datasets will be built: 
*detailed country level dataset (2012q4 - 2017q2)
*region level dataset (2012q4 - 2019q4)

*===============================================================================
*Country-level data
*===============================================================================

*Use country/region of birth rather than nationality to 
drop nato*


*Use the more detailed version of the birth identifier 
drop cryox7_eul_main cryo7
********************************************************************************

*merge it with the country classifications dataset 
*for <= 2017q2 based on country
decode cryox7, gen(country)
tab cryox7
tab cryox7, nolabel
drop cryox7

*for > 2017q2 based on subregion
decode cryox7_eul_sub, gen(subgroup)
tab cryox7_eul_sub
tab cryox7_eul_sub, nolabel
drop cryox7_eul_sub

*Change country names to correct lower and upper cases 
replace country = lower(country)
replace country = proper(country)
replace country = subinstr(country, " And ", " and ", .)
replace country = subinstr(country, " & ", " and ", .)
replace country = subinstr(country, " Of ", " of ", .)
replace country = subinstr(country, "Inc.", "inc.", .)
replace country = subinstr(country, "Usa", "USA", .)
replace country = subinstr(country, "Us", "US", .)

*Try a merge with the country classifications data 
preserve 
cd "$master_thesis/Data/Countries"
merge m:1 country date_stata using Subgroups_countries_harmonized.dta

*Let's see what got matched and what not 
gen matched = .
replace matched = 1 if _merge == 3 //country matched
replace matched = 2 if _merge == 1 //country only in master dataset
replace matched = 3 if _merge == 2 //country only in using dataset
drop if matched == 1 

collapse (max) matched, by(country)

gen match_status = ""
*replace match_status = "Matched" if matched == 1
replace match_status = "Only in Master" if matched == 2
replace match_status = "Only in Using" if matched == 3

sort match_status country

list country match_status
*list country match_status, sepby(match_status)
restore 

*1. Can be solved by renaming 
replace country = "Brunei Darussalam" if country == "Brunei"
replace country = "Myanmar" if country == "Burma"
replace country = "Taiwan, Province of China" if country == "China (Taiwan)"
replace country = "Congo, the Democratic Republic of the" if country == "Congo (Democratic Republic)"
replace country = "Timor-Leste" if country == "East Timor"
replace country = "Gambia" if country == "Gambia, The"
replace country = "Hong Kong" if country == "Hong Kong (Special Administrative Region"
replace country = "Côte d'Ivoire" if country == "Ivory Coast"
replace country = "North Korea" if country == "Korea (North)"
replace country = "South Korea" if country == "Korea (South)"
replace country = "Reunion" if country == "R?Nion"
replace country = "Saint Vincent and the Grenadines" if country == "St Vincent and The Grenadines"
replace country = "Virgin Islands, U.S." if country == "United States Virgin Islands"
replace country = "Palestine" if country == "West Bank (Including East Jerusalem) And"
replace country = "Yemen" if country == "Yeman"

*2. Can be subsumed 
replace country = "Spain" if country == "Canary Islands"
replace country = "Cyprus" if country == "Cyprus (European Union)"
replace country = "Cyprus" if country == "Cyprus (Not Otherwise Specified)"
replace country = "Czech Republic" if country == "Czechoslovakia Not Otherwise Specified" // both EU - does not matter 
replace country = "Spain" if country == "Spain (Except Canary Islands)"
replace country = "Spain" if country == "Spain Not Otherwise Specified"
replace country = "China" if country == "Macao (Special Administrative Region Of"

*3. Is only a subgroup 
replace subgroup = "Central and South America" if country == "Caribbean Not Otherwise Specified"
replace country = "" if country == "Caribbean Not Otherwise Specified"
replace subgroup = "Central and South America"  if country == "Central America Not Otherwise Specified"
replace country = "" if country == "Central America Not Otherwise Specified"
replace subgroup = "North America" if country == "North America Not Otherwise Specified"
replace country = "" if country == "North America Not Otherwise Specified"

*4. Needs to be dropped because not part of the analysis 
drop if country == "No Answer"
drop if country == "United Kingdom Not Otherwise Specified"
********************************************************************************

*Split the dataset up to do the merge separately by country and subgroup 
preserve 
keep if subgroup != ""
tempfile UKLFS_subgroup
save `UKLFS_subgroup', replace
restore 
keep if subgroup == ""
drop subgroup

*Now do the merge 
cd "$master_thesis/Data/Countries"
merge m:1 country date_stata using Subgroups_countries_harmonized.dta

*Do a check-up 
*br if _merge == 1 & date_stata <= tq(2017q2) //only those countrynames that are
*a subgroup --> so its fine

*Do a clean-up
drop if _merge == 2
drop _merge
tempfile UKLFS_country
save `UKLFS_country', replace
********************************************************************************

*===============================================================================
*Subregion level data
*===============================================================================

*for > 2017q2 based on subregion 
*Try a merge with the subgroup classifications data 
use `UKLFS_subgroup', clear

cd "$master_thesis/Data/Countries"
merge m:1 subgroup date_stata using Subgroups_harmonized.dta

*Let's see what got matched and what did not and clean up 
*br if _merge == 1 //only UK did not get matched -- that is fine 
drop if subgroup == "UK"
drop if _merge ==2
drop _merge
********************************************************************************

*Put it all together 
append using `UKLFS_country'
sort date_stata country subgroup

*Give numerical values to each country/subregion 
foreach var in "worldregion" "subgroup" "country" {
	sort `var'
	encode `var', gen(c_`var') 
	label values c_`var' . 
}
label var c_worldregion "World Region (coded)"
label var c_subgroup "Sub group (coded)"
label var c_country "Nationality (coded)"


*Save it
cd "$master_thesis/Data/UKLFS"
save UKLFS_merged.dta, replace
********************************************************************************

*===============================================================================
*Sample construction for the baseline analysis
*===============================================================================

cd "$master_thesis/Data/UKLFS"
use UKLFS_merged.dta, clear

*Inflow estimation based on the UKFLS
gen lfs_total = 1

/*Heterogeneity analysis by migrant characteristics:  (not conducted in thesis)
Characteristics of interest: 
- total inflows 
- by gender (male female)
- by agegroup
- by education 
- by EU-subregion (following the subregions of the LTIM classification?)

--> Add those variables in wide format!
*/

*Keep relevant variables
keep lfs_total date_survey date_stata country subgroup ///
	worldregion c_country c_subgroup group_iasio group_clifton group_simon ///
	pwt18 age sex hiqual15 hiqul15d hiqul11 hiqul11d sc10mmj soc10m edage ///
	levqul11 levqul15 
	
*** MAYBE USE A WEIGHTED VERSION AS WELL USING THE PERSON WEIGHT PWT18 ***
	
*Age groups
tab age, m // no missings 
gen lfs_until_29 = (inrange(age, 16, 29))
gen lfs_30_44 = (inrange(age, 30, 44))
gen lfs_over_45 = (inrange(age, 45, 71))
drop age 

*Gender 
tab sex, m // no missings 
gen lfs_male = (sex == 1)
gen lfs_female = (sex == 2)
drop sex 

*Education (grouping follows Iasio)
*detailed: hiqual15 hiqul15d 
*grouped hiqul11 hiqul11d
tab hiqual15 //too detailed - not be used 
tab hiqul15d
tab hiqul11
tab hiqul11d // same as hiqul11
tab soc10m // to detailed - not be used 
tab sc10mmj
tab edage
tab levqul11 // perfect for doing the grouping! 
tab levqul15 // same as levqul

*Missings? 
tab lfs_total if levqul15 == . & levqul11 == . // no 

*Conduct the grouping
gen lfs_high_edu = (levqul11 == 1 | levqul15 == 1) if levqul11 !=. | levqul15 != .
gen lfs_low_edu = (levqul11 != 1 & levqul11 != . & levqul11 != -8)
replace lfs_low_edu = 1 if levqul15 != 1 & levqul15 != . & levqul15 != -8 
drop hiqual15 hiqul15d hiqul11 hiqul11d hiqul11 hiqul11d soc10m sc10mmj ///
	edage levqul11 levqul15
		
*Save it
cd "$master_thesis/Data/UKLFS"
save UKLFS_cleaned.dta, replace
********************************************************************************

*===============================================================================
*Build the panel (unit of observation: subregion)
*===============================================================================

*Baseline panel (subgroups)
cd "$master_thesis/Data/UKLFS"
use UKLFS_cleaned.dta, clear
des

*Drop all country-related variables
drop country c_country group_clifton group_simon

collapse (sum) lfs_total lfs_until_29 lfs_30_44 lfs_over_45 lfs_male ///
	lfs_female lfs_high_edu lfs_low_edu (first) group_iasio subgroup, ///
	by(c_subgroup date_stata)
	
sort subgroup date_stata
	
tab subgroup
*panel is unbalanced --> missing observations for some quarters for: 
*East Asia
*European Union Other
*North Africa
*Other Europe 

/*
Balanced panel datasets have the same number of observations for all groups.
Unbalanced panel datasets have missing values at some time observations for some 
of the groups.
*/

fillin subgroup date_stata 
foreach var of varlist lfs_* {
	
	*Missing values are replaced with zeros
	replace `var' = 0 if _fillin == 1	
	
	*Make a log-transformed version in line with Iasio
	gen ln_`var' = ln(1+`var')
}

*Fill up the other group and country identifiers
foreach var in "c_subgroup" "group_iasio" {
	bysort subgroup (_fillin): replace `var' = `var'[_n-1] ///
		if missing(`var')
}

*Basic labels
label var lfs_total "Non-Log-Transformed Inflows"
label var ln_lfs_total "Log-Transformed Inflows"

*Save it
cd "$master_thesis/Data/UKLFS"
save UKLFS_panel.dta, replace
********************************************************************************

*===============================================================================
*Check a potential panel with unit of observation: country
*===============================================================================

cd "$master_thesis/Data/UKLFS"
use UKLFS_cleaned.dta, clear

keep if country != ""
tab date_stata // problematic last arrival capture in 2017q2 number strongly 
*Decreasing after 2016q3 --> Only take a look at the immediate period after the 
*referendum 
keep if date_stata <= tq(2016q3)

tab country // extremely unbalanced 
********************************************************************************

log close 
cd $my_path_do
*===============================================================================
*===============================================================================
