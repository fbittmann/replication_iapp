

use "Data/SC6.dta", clear


*** Zentrale Vars ***
clonevar happy = t514001
replace happy = . if missing(happy)
label var happy "Life satisfaction"
sum happy, det

clonevar weight = t520000
replace weight = . if !inrange(weight, 50, 170)
label var weight "Weight [kg]"
sum weight, det

bysort ID_t: egen height = median(t520001)
sum height, det
replace height = . if !inrange(height, 150, 205)
replace height = height / 100
label var height "Height [m]"
sum height, det

gen bmi = weight / (height^2)
label var bmi "BMI"
sum bmi, det
tabstat weight height, by(wave)


*** Alter ***
bysort ID_t: egen gebjahr = mode(t70000y), maxmode
bysort ID_t: egen gebmonth = mode(t70000m), maxmode
gen tbirth = mdy(gebmonth, 1, gebjahr)
gen tint = mdy(tx8600m, tx8600d, tx8600y)
gen age = (tint - tbirth) / 365
label var age "Age in years"
sum age
gen temp = (mdy(1, 1, 2011) - tbirth) / 365
bysort ID_t: egen tage2011 = max(temp)
drop temp
label var tage2011 "Age in 2011"
sum tage2011


*** Geschlecht ***
bysort ID_t: egen temp = mode(t700001), maxmode
gen female = temp - 1
drop temp
label var female "Female respondent"
fre female


*** Gewichtung ***
gen qweight = .
forvalues i = 2/14 {
	replace qweight = w_t`i'_cal if wave == `i'
}
label var qweight "Census calibrated weight"
sum qweight
replace qweight = . if qweight == 0		//Missings kodieren


*** Bildung ***
bysort ID_t: egen casmin = max(tx28101)
label var casmin "Education level"
label values casmin de1957
fre casmin


*** Osten ***
clonevar east = t751001_g1
replace east = east - 1
label var east "Living in East Germany"
fre east


*** Gesundheit ***
clonevar health = t521000
clonevar satishealth = t514003
label var nowork "Not working in wave"


*** Positionsgenerator ***
alpha t32600a t32600b t32600c t32600d t32600e t32600f t32600g t32600h t32600k ///
	t32600l t32600m t32600n t32600o if wave == 6, gen(temp)
bysort ID_t: egen position = max(temp)
drop temp
label var position "Positionsgenerator"
sum position


*** Zusammenwohnen mit Partner ***
clonevar partner = t743021
label var partner "Living together with partner"
fre partner


*** Muttersprache ***
bysort ID_t: egen lang = mode(tx29003), maxmode
replace lang = lang - 1
label var lang "Mother tongue not German"
fre lang


*** Not working ***
replace nowork = 1 if inrange(th32218, 2, 7)
replace nowork = 1 if missing(nowork) & inrange(age, 65, 99)
fre nowork



foreach VAR of varlist dgcf7_sum dgcf7_speed health satishealth partner {
	replace `VAR' = . if missing(`VAR')
}

bysort ID_t: egen medianhappy = median(happy)
clonevar city = tx80103
label var city "City size"



*** Gesundheit ***
replace health = 6 - health
label define health 1 "Very bad" 5 "Very good"
label values health health
label var health "Self-reported health status"
sum health


*** Logincome ***
replace t510010 = . if t510010 < 500
replace t510010 = 13000 if t510010 > 13000 & !missing(t510010)
bysort ID_t: egen income = median(t510010)
gen logincome = log(income)


*** ISEI ***
bysort ID_t: egen isei = median(tx29063)
sum isei if wave == 4

*** Soziale Herkunft ***
bysort ID_t: egen jahre_vater = mode(t731351_g3), maxmode
bysort ID_t: egen jahre_mutter = mode(t731301_g3), maxmode
bysort ID_t: egen casmin_mutter = mode(t731301_g2), maxmode
bysort ID_t: egen casmin_vater = mode(t731351_g2), maxmode
bysort ID_t: egen isei_vater = mode(t731453_g5), maxmode
bysort ID_t: egen isei_mutter = mode(t731403_g5), maxmode

foreach VAR of varlist jahre_* casmin_* isei_* {
	replace `VAR' = . if missing(`VAR')
}

gsem (Origin -> isei isei_vater isei_mutter logincome) ///
	(Origin -> casmin casmin_vater casmin_mutter, ologit) if wave == 4
predict temp if wave == 4 & e(sample), latent
sum temp if wave == 4
bysort ID_t: egen origin = max(temp)
drop temp
label var origin "Social origin indicator"
sum origin

