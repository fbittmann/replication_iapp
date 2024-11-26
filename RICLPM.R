#install.packages("lavaan", dependencies = TRUE)
require(lavaan)
mydata <- haven::read_stata('Data/DatafileR.dta')


RICLPM <- '
  # Create between components (random intercepts)
  RIhappy =~ 1*happy4 + 1*happy5 + 1*happy6 + 1*happy7 + 1*happy8 + 1*happy9 + 1*happy10 + 1*happy11 + 1*happy12 + 1*happy13
  RIbmi =~ 1*bmi4 + 1*bmi7 + 1*bmi10 + 1*bmi12 + 1*bmi13
  
  # Create within-person centered variables
  whappy4 =~ 1*happy4
  whappy5 =~ 1*happy5
  whappy6 =~ 1*happy6  
  whappy7 =~ 1*happy7  
  whappy8 =~ 1*happy8
  whappy9 =~ 1*happy9  
  whappy10 =~ 1*happy10  
  whappy11 =~ 1*happy11 
  whappy12 =~ 1*happy12 
  whappy13 =~ 1*happy13 
  
  wbmi4 =~ 1*bmi4
  wbmi7 =~ 1*bmi7  
  wbmi10 =~ 1*bmi10  
  wbmi12 =~ 1*bmi12 
  wbmi13 =~ 1*bmi13 


  # Estimate lagged effects between within-person centered variables
  whappy13 ~ whappy12 + wbmi12
  whappy12 ~ whappy11
  whappy11 ~ whappy10 + wbmi10
  whappy10 ~ whappy9
  whappy9 ~ whappy8
  whappy8 ~ whappy7 + wbmi7
  whappy7 ~ whappy6
  whappy6 ~ whappy5
  whappy5 ~ whappy4 + wbmi4
  
  wbmi13 ~ wbmi12 + whappy12
  wbmi12 ~ wbmi10 + whappy11
  wbmi10 ~ wbmi7 + whappy9
  wbmi7 ~ wbmi4 + whappy6

  #Regressions (Control variables)
  happy4 ~ nowork4 + partner4 + age4 + east4 + health4 + female + lang + origin
  happy5 ~ nowork5 + partner5 + age5 + east5 + health5 + female + lang + origin
  happy6 ~ nowork6 + partner6 + age6 + east6 + health6 + female + lang + origin
  happy7 ~ nowork7 + partner7 + age7 + east7 + health7 + female + lang + origin
  happy8 ~ nowork8 + partner8 + age8 + east8 + health8 + female + lang + origin
  happy9 ~ nowork9 + partner9 + age9 + east9 + health9 + female + lang + origin
  happy10 ~ nowork9 + partner9 + age9 + east9 + health9 + female + lang + origin
  happy11 ~ nowork9 + partner9 + age9 + east9 + health9 + female + lang + origin
  happy12 ~ nowork9 + partner9 + age9 + east9 + health9 + female + lang + origin
  happy13 ~ nowork9 + partner9 + age9 + east9 + health9 + female + lang + origin
  
  bmi4 ~ nowork4 + partner4 + age4 + east4 + health4 + female + lang + origin
  bmi7 ~ nowork7 + partner7 + age7 + east7 + health7 + female + lang + origin
  bmi10 ~ nowork7 + partner7 + age7 + east7 + health7 + female + lang + origin
  bmi12 ~ nowork7 + partner7 + age7 + east7 + health7 + female + lang + origin
  bmi13 ~ nowork7 + partner7 + age7 + east7 + health7 + female + lang + origin

  # Estimate covariance between within-person centered variables at first wave
  whappy4 ~~ wbmi4 # Covariance
  
  # Estimate covariances between residuals of within-person centered variables 
  # (i.e., innovations)
  whappy13 ~~ wbmi13
  whappy12 ~~ wbmi12
  whappy10 ~~ wbmi10
  whappy7 ~~ wbmi7

  
  # Estimate variance and covariance of random intercepts
  RIhappy ~~ RIhappy
  RIbmi ~~ RIbmi
  RIhappy ~~ RIbmi

  # Estimate (residual) variance of within-person centered variables
  whappy4 ~~ whappy4 # Variances
  whappy5 ~~ whappy5
  whappy6 ~~ whappy6
  whappy7 ~~ whappy7
  whappy8 ~~ whappy8
  whappy9 ~~ whappy9
  whappy10 ~~ whappy10
  whappy11 ~~ whappy11
  whappy12 ~~ whappy12
  whappy13 ~~ whappy13
  
  wbmi4 ~~ wbmi4 # Variances
  wbmi7 ~~ wbmi7
  wbmi10 ~~ wbmi10
  wbmi12 ~~ wbmi12
  wbmi13 ~~ wbmi13
'

RICLPM.fit <- lavaan(RICLPM, data = mydata, meanstructure = T, int.ov.free = T, missing = 'fiml.x')
summary(RICLPM.fit, standardized = TRUE, fit.measures = TRUE)

