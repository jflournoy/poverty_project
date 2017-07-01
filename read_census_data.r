library(data.table)
library(tidyverse)
library(ggplot2)

#SEX 1 - male 2 - femaile
#AGEP - 00 - nder 1 year, 1-99 (top coded)
#SCHL - 

pus_a <- rbindlist(list(fread('~/code_new/poverty/census/ss15pusa.csv'),fread('~/code_new/poverty/census/ss15pusb.csv')))

pus_shortened <- pus_a[,.(SERIALNO,SEX,AGEP,SCHL,RAC1P,PWGTP,ADJINC)]
rm(pus_a);gc()

convert_RAC1P <- function(RAC1P){
  #white = 1
  #black = 2
  #indian/alaskan = 3
  #asian/hawaiian = 4
  #other = 5
  ifelse(RAC1P == 1, 1,
         ifelse(RAC1P == 2, 2,
                ifelse(RAC1P %in% c(3,5), 3,
                       ifelse(RAC1P %in% c(6,7), 4,
                              ifelse(RAC1P %in% c(8,9), 5, NA)))))
}

# SCHL
# Educational attainment
# bb .N/A (less than 3 years old)
# 01 .No schooling completed
# 02 .Nursery school, preschool
# 03 .Kindergarten
# 04 .Grade 1
# 05 .Grade 2
# 06 .Grade 3
# 07 .Grade 4
# 08 .Grade 5
# 09 .Grade 6
# 10 .Grade 7
# 11 .Grade 8
# 12 .Grade 9
# 13 .Grade 10
# 14 .Grade 11
# 15 .12th grade - no diploma
# 16 .Regular high school diploma
# 17 .GED or alternative credential
# 18 .Some college, but less than 1 year
# 19 .1 or more years of college credit, no degree
# 20 .Associate's degree
# 21 .Bachelor's degree
# 22 .Master's degree
# 23 .Professional degree beyond a bachelor's degree
# 24 .Doctorate degree

convert_SCHL <- function(SCHL){
  ifelse(SCHL %in% 1:15, 1, #Less than HS
         ifelse(SCHL %in% 16:17, 2, #HS Only
                ifelse(SCHL %in% 18:19, 3, #Some College
                       ifelse(SCHL %in% 20:24, 4, NA)))) #Completed higher ed
}
convert_HINCP <- function(HINCP){
  ifelse(HINCP < 10000, 5000,
         ifelse(HINCP < 200000, HINCP %/% 10000 * 10000 + 10000/2,
                ifelse(HINCP < 500000, HINCP %/% 50000 * 50000 + 50000/2,
                       ifelse(HINCP < 1e6, 750000,
                              ifelse(HINCP >= 1e6, 1e6, NA)))))
}

hus_a <- rbindlist(list(fread('~/code_new/poverty/census/ss15husa.csv'),fread('~/code_new/poverty/census/ss15husb.csv')))
hus_shortened <- hus_a[,.(SERIALNO,HINCP,WGTP,ADJINC)]
rm(hus_a);gc()

setkey(pus_shortened, SERIALNO)
setkey(hus_shortened, SERIALNO)

#Matches expected estimates as reported by census:
#SEX=1: 158,134,332
#SEX=2: 163,284,489
xtabs(PWGTP~SEX, data = pus_shortened)
#Total population: 321,418,821
sum(pus_shortened$PWGTP)

pums_data <- hus_shortened[pus_shortened,]
setnames(pums_data, 'SEX', 'Sex')
pums_data[, Education := convert_SCHL(SCHL)]
pums_data[, Race := convert_RAC1P(RAC1P)]
#Census quintiles, upper ranges:
#22,800	43,511	72,001	117,002
pums_data[, Income_adj := convert_HINCP(HINCP*(ADJINC*1e-6))]
census_breaks <- c(-Inf, 22800, 43511, 72001, 117002, Inf)
# acs_breaks <- c(-Inf, 22834, 43576, 70323, 112145, Inf)
pums_data[, Income := cut(Income_adj, 
                          breaks = census_breaks, 
                          dig.lab = -1)]

#write.csv(pums_data, '~/code_new/poverty/pums_data_short.csv')

pums_xtabs <- xtabs(PWGTP~Sex + Race + Education + Income, pums_data)
saveRDS(pums_xtabs, '~/code_new/poverty/pums_xtabs.RDS')

pums_xtabs_no_race <- xtabs(PWGTP~Sex + Education + Income, pums_data)
saveRDS(pums_xtabs_no_race, '~/code_new/poverty/pums_xtabs_no_race.RDS')

pums_xtabs_no_inc <- xtabs(PWGTP~Sex + Race + Education, pums_data)
saveRDS(pums_xtabs_no_inc, '~/code_new/poverty/pums_xtabs_no_inc.RDS')

pums_xtabs_no_inc_no_ed <- xtabs(PWGTP~Sex + Race, pums_data)
saveRDS(pums_xtabs_no_inc_no_ed, '~/code_new/poverty/pums_xtabs_no_inc_no_ed.RDS')

