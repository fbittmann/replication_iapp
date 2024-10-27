
*** ALTERNATIVE IMPUTATION WITH INCOME ***


quiet do "Dos/Opera.do"

*** Sample selection ***
keep if inrange(wave, 4, 13)		//Wellen 4 bis 13
recode tx80220 (1=1 "Yes") (2 3 = 0 "No"), gen(teilnahme)
fre teilnahme
bysort ID_t: egen totalteilnahmen = sum(teilnahme)
fre totalteilnahmen
keep if totalteilnahmen >= 4
keep if tage2011 <= 60				//Jünger als 60 am Anfang
keep if inrange(bmi, 18.5, 45) | missing(bmi)
drop if missing(female, casmin)


*** Reshaping ***
keep ID_t wave happy bmi age tage2011 female qweight casmin ///
	dgcf7_sum dgcf7_speed medianhappy health nowork satishealth east position ///
	partner lang origin city logincome
reshape wide bmi happy age qweight health nowork satishealth partner east logincome, i(ID_t) j(wave)
count


*** Imputation ***
mi set flong
mi register imputed happy* bmi* dgcf7_sum dgcf7_speed qweight* age* health* nowork* ///
	satishealth* east* position partner* lang logincome*
mi impute chained (pmm, knn(6)) happy* dgcf7_sum dgcf7_speed age* ///
	health* satishealth* position nowork* partner* lang east* ///
	bmi4 bmi7 bmi10 bmi12 bmi13 logincome* ///
	= c.tage2011##c.tage2011 medianhappy c.casmin origin i.city ///
	, add(15) burnin(20) rseed(49991) dots by(female)
summarize * if _mi_m > 0
compress
save "Data/imputed_test_income.dta", replace

********************************************************************************


