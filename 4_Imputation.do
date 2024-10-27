

quiet do "Dos/Opera.do"

*** Sample selection ***
keep if inrange(wave, 4, 13)		//Wellen 4 bis 13
recode tx80220 (1=1 "Yes") (2 3 = 0 "No"), gen(teilnahme)
fre teilnahme
bysort ID_t: egen totalteilnahmen = sum(teilnahme)
fre totalteilnahmen
keep if totalteilnahmen >= 4
keep if tage2011 <= 60				//Jünger als 60 am Anfang
count if !missing(bmi)
count if bmi < 18.5
count if bmi > 45 & !missing(bmi)
keep if inrange(bmi, 18.5, 45) | missing(bmi)
drop if missing(female, casmin)


*** Preparation for R ***
preserve
label var happy "Happiness"
keep ID_t wave happy bmi age tage2011 female qweight casmin ///
	dgcf7_sum dgcf7_speed medianhappy health nowork satishealth east position ///
	partner lang origin city
reshape wide bmi happy age qweight health nowork satishealth partner east, i(ID_t) j(wave)
compress
save "Data/DatafileR.dta", replace
restore


*** Reshaping ***
keep ID_t wave happy bmi age tage2011 female qweight casmin ///
	dgcf7_sum dgcf7_speed medianhappy health nowork satishealth east position ///
	partner lang origin city
reshape wide bmi happy age qweight health nowork satishealth partner east, i(ID_t) j(wave)
count


*** Imputation ***
mi set flong
mi register imputed happy* bmi* dgcf7_sum dgcf7_speed qweight* age* health* nowork* ///
	satishealth* east* position partner* lang origin
mi impute chained (pmm, knn(6)) happy* dgcf7_sum dgcf7_speed age* ///
	health* satishealth* position nowork* partner* lang east* ///
	bmi4 bmi7 bmi10 bmi12 bmi13 origin ///
	= c.tage2011##c.tage2011 medianhappy c.casmin i.city ///
	, add(50) burnin(20) rseed(49991) dots by(female)
summarize * if _mi_m > 0
compress
save "Data/imputed_rev.dta", replace


********************************************************************************


