
*Felix Bittmann, 2024


use "Data/imputed_test.dta", clear
qui mi reshape long happy bmi qweight age health nowork partner ///
	satishealth east, i(ID_t) j(wave)
label define east 0 "West Germany" 1 "East Germany"
label values east east
label define partner 0 "No partner" 1 "Living with partner"
label values partner partner


*** BMI Interpolation ***
bysort ID_t _mi_m (wave): ipolate bmi age, gen(bmi2)
label var bmi2 "BMI"
label var happy "Happiness"
list ID_t _mi_m wave age bmi2 bmi in 6000/6500 , sepby(ID_t _mi_m)


*** Agegroup categories ***
cap drop agegroup
recode tage2011 (20/40 = 0 "24-40") (40/50 = 1 "40-50") (50/60 = 2 "50-60"), gen(agegroup)
tab tage2011 agegroup
label var agegroup "Age in 2011"
fre agegroup
	

*** BMI categories ***
cap drop bmicat
recode bmi2 (18.5/22 = 0 "18.5-22") (22/25 = 1 "22-25") (25/30 = 2 "25-30") ///
	(30/35 = 3 "30-35") (35/45 = 4 "35-45") (45/99 = .a) (10/18.5 = .a), gen(bmicat)
label var bmicat "BMI classification"
fre bmicat



********************************************************************************

*** Happy - Age ***
twoway (lpoly happy age if female == 0 & _mi_m > 0, bwidth(1.9)) ///
	(lpoly happy age if female == 1 & _mi_m > 0, bwidth(1.9)) ///
	, legend(position(6) order(1 "Men" 2 "Women") row(1)) ///
	xtitle("Age") ytitle("Happiness") name(g1, replace)
	
*** BMI - Age ***
twoway (lpoly bmi2 age if female == 0 & _mi_m > 0, bwidth(1.9)) ///
	(lpoly bmi2 age if female == 1 & _mi_m > 0, bwidth(1.9)) ///
	, legend(position(6) order(1 "Men" 2 "Women") row(1)) ///
	xtitle("Age") ytitle("BMI") name(g2, replace)
graph combine g1 g2
graph save "Output/age-happy-bmi", replace
graph export "Output/age-happy-bmi.png", replace as(png) width(2500)


*** Happiness - BMI ***
twoway (lpoly happy bmi2 if female == 0 & _mi_m > 0, bwidth(1.9)) ///
	(lpoly happy bmi2 if female == 1 & _mi_m > 0, bwidth(1.9)) ///
	, legend(position(6) order(1 "Men" 2 "Women") row(1)) ///
	xtitle("BMI") ytitle("Happiness")
graph save "Output/happy-bmi", replace
graph export "Output/happy-bmi.png", replace as(png) width(2500)


*** Within-Person Variation checken ***
bysort ID_t _mi_m: egen sd_happy = sd(happy) if _mi_m > 0
bysort ID_t _mi_m: egen sd_bmi = sd(bmi2) if _mi_m > 0

histogram sd_happy if _mi_m > 0, nodraw name(g1, replace) xtitle("SD Happiness")
histogram sd_bmi if _mi_m > 0, nodraw name(g2, replace) xtitle("SD BMI")
graph combine g1 g2
graph save "Output/variationcheck", replace
graph export "Output/variationcheck.png", replace as(png) width(2500)

sum sd_happy sd_bmi
bysort ID_t _mi_m: egen min_bmi = min(bmi2)
bysort ID_t _mi_m: egen max_bmi = max(bmi2)
gen diff_bmi = max_bmi - min_bmi
sum diff_bmi, det
centile diff_bmi, centile(2.5 97.5)


*** Deskription ***
label var happy "Happiness"
histogram happy if _mi_m > 0, disc name(g1, replace) ytitle("") nodraw
histogram bmi2 if _mi_m > 0, name(g2, replace) ytitle("") nodraw bin(30)
histogram tage2011 if _mi_m > 0, name(g3, replace) ytitle("") nodraw bin(30)
graph combine g1 g2 g3, row(1) fysize(70)
graph save "Output/histograms", replace
graph export "Output/histograms.png", replace as(png) width(2500)


cap drop educ
recode casmin (0 1 2 = 0 "Low") (3 4 = 1 "Intermediate") (5 6 = 2 "HEE") (7 = 3 "UAS") ///
	(8 = 4 "Tertiary"), gen(educ)
fre educ

tab educ, gen(e_)
label var e_1 "Low"
label var e_2 "Intermediate"
label var e_3 "HEE"
label var e_4 "UAS"
label var e_5 "Tertiary"
eststo M1: estpost summarize happy bmi2 age east health nowork partner female lang e_? ///
	if _mi_m > 0 & wave == 7, det
esttab M1 using "Output/Deskription.rtf", cells("mean(fmt(a2)) p50 sd min max") rtf replace label


pwcorr happy bmi2 if _mi_m > 0
mibeta happy bmi2
pwcorr happy age if _mi_m > 0
mibeta happy age
pwcorr bmi2 age if _mi_m > 0
mibeta bmi2 age

********************************************************************************
	
	
	

*** XTREG MAIN ***
mi xtset ID_t wave	
global controls i.nowork i.partner c.health i.east
eststo G1: mi estimate, post: xtreg happy i.bmicat c.age##c.age $controls if female == 0 ///
	, fe vce(robust)
eststo G2: mi estimate, post: xtreg happy i.bmicat c.age##c.age $controls if female == 1 ///
	, fe vce(robust)
esttab G1 G2 using "Output/xtreg_main.rtf", replace label b(3) ci(3) nogaps nobase rtf ///
	mtitles("Men" "Women")
	
	
*** R2 ***
mi describe
local mtotal = r(M)
local r2_total = 0
local r2_within = 0
global controls i.nowork i.partner c.health i.east
forvalues i = 1/`mtotal' {
	qui xtreg happy i.bmicat c.age##c.age $controls if female == 0 & _mi_m == `i' ///
	, fe vce(robust)
	local r2_total = `r2_total' + e(r2_o)
	local r2_within = `r2_within' + e(r2)
	di "DONE!"
}
di `r2_total' / `mtotal'
di `r2_within' / `mtotal'
local r2_total = 0
local r2_within = 0
forvalues i = 1/`mtotal' {
	qui xtreg happy i.bmicat c.age##c.age $controls if female == 1 & _mi_m == `i' ///
	, fe vce(robust)
	local r2_total = `r2_total' + e(r2_o)
	local r2_within = `r2_within' + e(r2)
	di "DONE!"
}
di `r2_total' / `mtotal'
di `r2_within' / `mtotal'

	
*** XTREG + ALTER CAT ***
mi xtset ID_t wave
global controls i.nowork i.partner c.health i.east
eststo M1: mi estimate: xtreg happy i.bmicat $controls if female == 0 & agegroup == 0 ///
	, fe vce(robust)
eststo M2: mi estimate: xtreg happy i.bmicat $controls if female == 0 & agegroup == 1 ///
	, fe vce(robust)
eststo M3: mi estimate: xtreg happy i.bmicat $controls if female == 0 & agegroup == 2 ///
	, fe vce(robust)
coefplot (M1, label("24-40")) ///
	(M2, label("40-50")) ///
	(M3, label("50-60")) ///
	, drop(_cons 1.nowork 1.partner health 1.east) yline(0) ///
	legend(title("Agegroup (2011)") position(6) row(1)) ///
	title("Men") ytitle("") xtitle("BMI category") vert name(g1, replace)
	
eststo F1: mi estimate: xtreg happy i.bmicat $controls if female == 1 & agegroup == 0 ///
	, fe vce(robust)
eststo F2: mi estimate: xtreg happy i.bmicat $controls if female == 1 & agegroup == 1 ///
	, fe vce(robust)
eststo F3: mi estimate: xtreg happy i.bmicat $controls if female == 1 & agegroup == 2 ///
	, fe vce(robust)
coefplot (F1, label("24-40")) ///
	(F2, label("40-50")) ///
	(F3, label("50-60")) ///
	, drop(_cons 1.nowork 1.partner health 1.east) yline(0) ///
	legend(title("Agegroup (2011)") position(6) row(1)) ///
	title("Women") ytitle("") xtitle("BMI category") vert name(g2, replace)
graph combine g1 g2, ycommon
graph save "Output/xtreg_byage", replace
graph export "Output/xtreg_byage.png", replace as(png) width(2500)



*** BMI as continuous ***
mi xtset ID_t wave
global controls i.nowork i.partner c.health i.east
cap drop samp
mi estimate, esample(samp) saving(test, replace): ///
	xtreg happy c.bmi##c.bmi##c.bmi c.age##c.age $controls if female == 0, fe vce(robust)
mimrgns using test, esample(samp) at(bmi=(19(1)35)) cmdmargins post predict(xb)
marginsplot, xtitle("BMI") ytitle("Predicted happiness") title("Men") name(t1, replace) ///
	recast(line) recastci(rarea) ciopts(color(%40)) xlabel(19(2)35)


cap drop samp
mi estimate, esample(samp) saving(test, replace): ///
	xtreg happy c.bmi##c.bmi##c.bmi c.age##c.age $controls if female == 1, fe vce(robust)
mimrgns using test, esample(samp) at(bmi=(19(1)35)) cmdmargins post predict(xb)
marginsplot, xtitle("BMI") ytitle("Predicted happiness") title("Women") name(t2, replace) ///
	recast(line) recastci(rarea) ciopts(color(%40)) xlabel(19(2)35)
graph combine t1 t2, ycommon
graph save "Output/xtreg_cont", replace
graph export "Output/xtreg_cont.png", replace as(png) width(2500)
	
	

	
*** Initial BMI ***
cap drop bmistart bmistartcat
gen temp = bmi2 if wave == 4
bysort ID_t: egen bmistart = median(temp)
drop temp
recode bmistart (18.5/22 = 0 "18.5-22") (22/25 = 1 "22-25") (25/30 = 2 "25-30") ///
	(30/35 = 3 "30-35") (35/45 = 4 "35-45") (45/99 = .a) (10/18.5 = .a), gen(bmistartcat)
label var bmicat "BMI in 2011/12"
fre bmistartcat

	
*** XTREG + STARTING BMI ***
mi xtset ID_t wave
global controls i.nowork i.partner c.health i.east c.age##c.age
eststo M1: mi estimate: xtreg happy c.bmi2 $controls if female == 0 & bmistartcat == 0 ///
	, fe vce(robust)
eststo M2: mi estimate: xtreg happy c.bmi2 $controls if female == 0 & bmistartcat == 1 ///
	, fe vce(robust)
eststo M3: mi estimate: xtreg happy c.bmi2 $controls if female == 0 & bmistartcat == 2 ///
	, fe vce(robust)
eststo M4: mi estimate: xtreg happy c.bmi2 $controls if female == 0 & bmistartcat == 3 ///
	, fe vce(robust)
eststo M5: mi estimate: xtreg happy c.bmi2 $controls if female == 0 & bmistartcat == 4 ///
	, fe vce(robust)
coefplot (M1, label("18-5.22")) ///
	(M2, label("22-25")) ///
	(M3, label("25-30")) ///
	(M4, label("30-35")) ///
	(M5, label("35-45")) ///
	, drop(_cons 1.nowork 1.partner health 1.east age c.age#c.age) yline(0) ///
	legend(title("BMI (2011)") position(6) row(1)) xtitle("") ///
	title("Men") ytitle("") vert name(g1, replace)
	
eststo F1: mi estimate: xtreg happy c.bmi2 $controls if female == 1 & bmistartcat == 0 ///
	, fe vce(robust) 
eststo F2: mi estimate: xtreg happy c.bmi2 $controls if female == 1 & bmistartcat == 1 ///
	, fe vce(robust)
eststo F3: mi estimate: xtreg happy c.bmi2 $controls if female == 1 & bmistartcat == 2 ///
	, fe vce(robust)
eststo F4: mi estimate: xtreg happy c.bmi2 $controls if female == 1 & bmistartcat == 3 ///
	, fe vce(robust)
eststo F5: mi estimate: xtreg happy c.bmi2 $controls if female == 1 & bmistartcat == 4 ///
	, fe vce(robust)
coefplot (F1, label("18-5.22")) ///
	(F2, label("22-25")) ///
	(F3, label("25-30")) ///
	(F4, label("30-35")) ///
	(F5, label("35-45")) ///
	, drop(_cons 1.nowork 1.partner health 1.east age c.age#c.age) yline(0) ///
	legend(title("BMI (2011)") position(6) row(1)) ///
	title("Women") ytitle("") xtitle("") vert name(g2, replace)
graph combine g1 g2, ycommon
graph save "Output/bystartbmi", replace
graph export "Output/bystartbmi.png", replace as(png) width(2500)	





*** What happens if BMI is selectively under-reported ***
preserve
replace bmi2 = bmi2 - bmi2 * 0.025 if bmicat == 0
replace bmi2 = bmi2 - bmi2 * 0.050 if bmicat == 1
replace bmi2 = bmi2 - bmi2 * 0.075 if bmicat == 2
replace bmi2 = bmi2 - bmi2 * 0.100 if bmicat == 3
replace bmi2 = bmi2 - bmi2 * 0.125 if bmicat == 4

mi xtset ID_t wave	
global controls i.nowork i.partner c.health i.east
eststo G1: mi estimate, post: xtreg happy c.bmi2 c.age##c.age $controls if female == 0 ///
	, fe vce(robust)
eststo G2: mi estimate, post: xtreg happy c.bmi2 c.age##c.age $controls if female == 1 ///
	, fe vce(robust)
esttab G1 G2, replace label ci(2) nogaps nobase
restore


*** Lag ***
mi xtset ID_t wave	
global controls i.nowork i.partner c.health i.east
eststo G1: mi estimate, post: xtreg happy L.bmi2 c.age##c.age $controls if female == 0 ///
	, fe vce(robust)
eststo G2: mi estimate, post: xtreg happy L.bmi2 c.age##c.age $controls if female == 1 ///
	, fe vce(robust)
	
	
*** Not controlling for health ***	
mi xtset ID_t wave	
global controls i.nowork i.partner i.east
eststo G1: mi estimate, post: xtreg happy c.bmi2 c.age##c.age $controls if female == 0 ///
	, fe vce(robust)
eststo G2: mi estimate, post: xtreg happy c.bmi2 c.age##c.age $controls if female == 1 ///
	, fe vce(robust)
esttab G1 G2, replace label ci(2) nogaps nobase
