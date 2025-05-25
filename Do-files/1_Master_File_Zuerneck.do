*===============================================================================
*===============================================================================

*Master File Thesis 
*Simon Zuerneck 
*Stata Version: 18.0
********************************************************************************

clear all
set more off 
capture log close 
********************************************************************************

*===============================================================================
*Path to directory globals (insert own path here)
*===============================================================================

*Simon Zuerneck's path to all thesis files (insert own path to file folder here) 
global master_thesis "C:/Users/simon/OneDrive/Studium/Master/Semester_4/Master_Thesis"
 
*Set the directory 
cd $master_thesis

*Path to downloaded data (adjust this path as well)
global my_path_in "C:/Users/simon/OneDrive/Desktop/Data"

*Path to formatted data
global my_path_out "$master_thesis/Data/"

*Path to do-files 
global my_path_do "$master_thesis/DO/"

*Path to ado-files 
global my_path_ado "$master_thesis/Ado/"
********************************************************************************

*===============================================================================
*Install necessary .ado files 
*===============================================================================

*Remove the upcoming slash and asterisk to install necessary .ado files

/*
ssc install tabout 
ssc install gr0034.pkg
ssc install diff
ssc install sdid
ssc install sdid_event
ssc install unique 
ssc install boottest
ssc install estout, replace
ssc install outreg2
ssc install coefplot
ssc install xttest3

net install grc1leg2, from ("http://digital.cgdev.org/doc/stata/MO/Misc/") ///
replace
net from http://www.stata-journal.com/software/sj3-2/
net describe st0039
net install st0039
*/

********************************************************************************

*===============================================================================
*Modified Ado-files 
*(need to run for proper designed graphs, not part of the replication files ) 
*===============================================================================

cd $my_path_ado
do "SDiD_modified_Zuerneck.do"
********************************************************************************

*===============================================================================
*Do-files 
*===============================================================================

cd $my_path_do

*Remove the upcoming slash and asterisk to run the do-files from here:  

/*
*Creating the groups 
do "2_Country_classifications_Zuerneck.do"

*Long-Term Migration Estimates (LTIM)
do "3_1_LTIM_sample_const_Zuerneck.do"
do "3_2_LTIM_stats_trajectory_Zuerneck.do"
do "3_3_LTIM_empirical_analysis_Zuerneck.do"
do "3_4_LTIM_robustness_checks_Zuerneck.do"

*National Insurance Number Registrations (NINo)
do "4_1_NINo_sample_const_Zuerneck.do"
do "4_2_NINo_stats_trajectory_Zuerneck.do"
do "4_3_NINo_empirical_analysis_Zuerneck.do"
do "3_4_NINo_robustness_checks_Zuerneck.do"

*UK Labour Force Survey (UKLFS)
do "5_1_UKLFS_sample_const_Zuerneck.do"
do "5_2_UKLFS_stats_trajectory_Zuerneck.do"
do "5_3_UKLFS_empirical_analysis_Zuerneck.do"
*/
********************************************************************************

clear 
*===============================================================================
*===============================================================================
 