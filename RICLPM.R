#Felix Bittmann, 2024
#install.packages("lavaan", dependencies = TRUE)
require(lavaan)
mydata <- haven::read_stata('Data/DatafileR.dta')


RICLPM <- '
  # Create between components (random intercepts)
  RIhappy =~ 1*happy4 + 1*happy5 + 1*happy6 + 1*happy7 + 1*happy8 + 1*happy9 + 1*happy10 + 1*happy11 + 1*happy12 + 1*happy13
  RIbmi =~ 1*bmi4 + 1*bmi5 + 1*bmi6 + 1*bmi7 + 1*bmi8 + 1*bmi9 + 1*bmi10 + 1*bmi11 + 1*bmi12 + 1*bmi13
  
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
  wbmi5 =~ 1*bmi5
  wbmi6 =~ 1*bmi6  
  wbmi7 =~ 1*bmi7  
  wbmi8 =~ 1*bmi8
  wbmi9 =~ 1*bmi9  
  wbmi10 =~ 1*bmi10  
  wbmi11 =~ 1*bmi11 
  wbmi12 =~ 1*bmi12 
  wbmi13 =~ 1*bmi13 


  # Estimate lagged effects between within-person centered variables
  whappy13 + wbmi13 ~ whappy12 + wbmi12
  whappy12 + wbmi12 ~ whappy11 + wbmi11
  whappy11 + wbmi11 ~ whappy10 + wbmi10
  whappy10 + wbmi10 ~ whappy9 + wbmi9
  whappy9 + wbmi9 ~ whappy8 + wbmi8
  whappy8 + wbmi8 ~ whappy7 + wbmi7
  whappy7 + wbmi7 ~ whappy6 + wbmi6
  whappy6 + wbmi6 ~ whappy5 + wbmi5
  whappy5 + wbmi5 ~ whappy4 + wbmi4

  #Regressions
#happy4 + happy5 + happy6 + happy7 + happy8 + happy9 + happy10 + happy11 + happy12 + happy13 ~ nowork4 + age4 + east4 + health4 + partner4
#bmi4 + bmi5 + bmi6 + bmi7 + bmi8 + bmi9 + bmi10 + bmi11 + bmi12 + bmi13 ~ nowork4 + age4 + east4 + health4 + partner4 
  


  # Estimate covariance between within-person centered variables at first wave
  whappy4 ~~ wbmi4 # Covariance
  
  # Estimate covariances between residuals of within-person centered variables 
  # (i.e., innovations)
  whappy13 ~~ wbmi13
  whappy12 ~~ wbmi12
  whappy11 ~~ wbmi11
  whappy10 ~~ wbmi10
  whappy9 ~~ wbmi9
  whappy8 ~~ wbmi8
  whappy7 ~~ wbmi7
  whappy6 ~~ wbmi6
  whappy5 ~~ wbmi5

  
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
  wbmi5 ~~ wbmi5
  wbmi6 ~~ wbmi6
  wbmi7 ~~ wbmi7
  wbmi8 ~~ wbmi8
  wbmi9 ~~ wbmi9
  wbmi10 ~~ wbmi10
  wbmi11 ~~ wbmi11
  wbmi12 ~~ wbmi12
  wbmi13 ~~ wbmi13
'

RICLPM.fit <- lavaan(RICLPM, data = mydata, meanstructure = T, int.ov.free = T, missing = 'fiml.x')
summary(RICLPM.fit, standardized = TRUE, fit.measures = TRUE)

