library(survey)
library(tidyverse)

pums_xtabs <- readRDS('./pums_xtabs.RDS')

INR <- read.table("./Poverty_PhaseI_StudyOne_Mturk_INR.csv", header=TRUE, sep=",")
params <- read.table("./mCTB_params.csv", header=TRUE, sep=",")
names(params)[names(params)=="subject_id"] <- "subject_ID"
data <- left_join(INR, params, by="subject_ID")
data$beedee <- data$b*data$d

#'
#' # Planned regression
#' 
#' The model we will be estimating is:
#'   
#' $$
#' \text{DELAYDISCOUNT_PARAM} = \beta_0 + \beta_1\text{ITNR} + \beta_2\text{EDUC}+\beta_3\text{C}+\beta_4\text{IMPULS}+\beta_5\text{PLANFUL}+\epsilon.
#' $$
#' 

dataSVY <- svydesign(ids = ~1, data = data)
dataPostStratSVY <- postStratify(design = dataSVY,
                                 strata = ~Sex + Race + Education + Income,
                                 population = pums_xtabs)

summary(weights(dataPostStratSVY)) #see blog post about trimming
summary(dataPostStratSVY)

summary(svyMod <- svyglm(d ~ (1 + INR + PScore + ConsciScore + BISscore), design = dataPostStratSVY))
summary(glmMod <- glm(d ~ 1 + INR + PScore + ConsciScore + BISscore, data = data))

cat('Absolute deviation of coefficients:')
round((coef(glmMod) - coef(svyMod)),2)
cat('Proportional deviation of coefficients:')
round((coef(glmMod) - coef(svyMod))/coef(svyMod),2)