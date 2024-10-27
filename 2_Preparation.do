

*Felix Bittmann, 2024


use "Data/SC6_D_14-0-0/Stata\SC6_xTargetCompetencies_D_14-0-0.dta", clear
keep ID_t dga7_sc3b dga7_sc3a
rename dga7_sc3b dgcf7_sum
rename dga7_sc3a dgcf7_speed
nepsmiss
sum *
tempfile comp
save `comp', replace



*** Arbeitsmarktdaten ***
use "Data/SC6_D_14-0-0/Stata\SC6_spEmp_D_14-0-0.dta", clear
nepsmiss ts23901
bysort ID_t wave: egen nowork = max(ts23901)
replace nowork = nowork - 1
egen tagger = tag(ID_t wave)
keep if tagger == 1
isid ID_t wave
keep ID_t wave nowork
label var nowork "Reported episode of not working"
count
compress
tempfile emp
save `emp', replace



use "Data/SC6_D_14-0-0/Stata\SC6_CohortProfile_D_14-0-0.dta", clear
merge 1:1 ID_t wave using "Data/SC6_D_14-0-0/Stata\SC6_pTarget_D_14-0-0.dta", nogen
merge m:1 ID_t using "Data/SC6_D_14-0-0/Stata\SC6_Weights_D_14-0-0.dta", nogen
merge m:1 ID_t using "Data/SC6_D_14-0-0/Stata\SC6_Basics_D_14-0-0.dta", nogen
merge m:1 ID_t using `comp', nogen
merge 1:1 ID_t wave using `emp', nogen
drop if wave == 1		//No ALWA


plnepsmiss, threads(4)
compress
save "Data/SC6.dta", replace
