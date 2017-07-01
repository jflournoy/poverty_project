library(survey)
library(tidyverse)

pums_xtabs <- readRDS('~/code_new/poverty/pums_xtabs_no_race.RDS')

INR <- read.table("./Poverty_PhaseI_StudyOne_Mturk_INR", header=TRUE, sep=",")
data <- INR
# params <- read.table("./mCTB_params.csv", header=TRUE, sep=",")
# names(params)[names(params)=="subject_id"] <- "subject_ID"
# data <- left_join(INR, params, by="subject_ID")
data$beedee <- data$b*data$d
data$d10 <- scale(data$d)

census_breaks <- c(-Inf, 22800, 43511, 72001, 117002, Inf)
data$Income_raw <- data$Income
data$Income <- cut(data$Income_raw,
                   breaks = census_breaks, 
                   dig.lab = -1)

data$Education_raw <- data$Education
data$Education <- as.numeric(c(`1`=1, `2`=2, `3`=3, `4`=4, `5`=4, `6`=4, `7`=4, `8`=4)[as.character(data$Education_raw)])

xtabs(~Sex+Education+Income, data = data)

#'
#' # Planned regression
#' 
#' The model we will be estimating is:
#'   
#' $$
#' \text{DELAYDISCOUNT_PARAM} = \beta_0 + \beta_1\text{ITNR} + \beta_2\text{EDUC}+\beta_3\text{C}+\beta_4\text{IMPULS}+\beta_5\text{PLANFUL}+\epsilon.
#' $$
#' 

data_c <- within(data,{
  INR = INR - 2.75
  PScore = PScore - mean(PScore)
  ConsciScore = ConsciScore- mean(ConsciScore)
  BISscore = BISscore - mean(BISscore)
})

dataSVY <- svydesign(ids = ~1, data = data_c)
dataPostStratSVY <- postStratify(design = dataSVY,
                                 strata = ~Sex + Education+Income,
                                 population = pums_xtabs,
                                 partial = T)

summary(weights(dataPostStratSVY)) #see blog post about trimming

summary(svyMod <- svyglm(d10 ~ (1 + INR), design = dataPostStratSVY))
summary(svyMod <- svyglm(d10 ~ (1 + INR + PScore), design = dataPostStratSVY))
summary(svyMod <- svyglm(d10 ~ (1 + INR + ConsciScore), design = dataPostStratSVY))
summary(svyMod <- svyglm(d10 ~ (1 + INR + BISscore), design = dataPostStratSVY))
summary(svyMod <- svyglm(d10 ~ (1 + INR + PScore + ConsciScore), design = dataPostStratSVY))
summary(svyMod <- svyglm(d10 ~ (1 + INR + PScore + ConsciScore + BISscore), design = dataPostStratSVY))

summary(glmMod <- glm(d10 ~ (1), data = data_c))
summary(glmMod <- glm(d10 ~ (1 + INR), data = data_c))
summary(glmMod <- glm(d10 ~ (1 + INR + PScore), data = data_c))
summary(glmMod <- glm(d10 ~ (1 + INR + ConsciScore), data = data_c))
summary(glmMod <- glm(d10 ~ (1 + INR + BISscore), data = data_c))
summary(glmMod <- glm(d10 ~ (1 + INR + PScore + ConsciScore), data = data_c))
summary(glmMod <- glm(d10 ~ (1 + INR + PScore + ConsciScore + BISscore), data = data_c))


cat('Absolute deviation of coefficients:')
round((coef(glmMod) - coef(svyMod)),4)
cat('Proportional deviation of coefficients:')
round((coef(glmMod) - coef(svyMod))/coef(svyMod),2)
