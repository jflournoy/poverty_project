library(data.table)
library(tidyverse)
library(ggplot2)

#SEX 1 - male 2 - femaile
#AGEP - 00 - nder 1 year, 1-99 (top coded)
#SCHL - 

pus_a <- rbindlist(list(fread('~/code_new/poverty/census/ss15pusa.csv'),fread('~/code_new/poverty/census/ss15pusb.csv')))

pus_shortened <- pus_a[,.(SERIALNO,SEX,AGEP,SCHL,RAC1P)]
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
convert_SCHL <- function(SCHL){
  ifelse(SCHL %in% 1:15, 1,
         ifelse(SCHL %in% 16:17, 2,
                ifelse(SCHL %in% 18:19, 3, SCHL - 16)))
}
convert_HINCP <- function(HINCP){
  ifelse(HINCP < 10000, 5000,
         ifelse(HINCP < 200000, HINCP %/% 1000 * 10000 + 10000/2,
                ifelse(HINCP < 500000, HINCP %/% 50000 * 50000 + 50000/2,
                       ifelse(HINCP < 1e6, 750000,
                              ifelse(HINCP >= 1e6, 1e6, NA)))))
}

hus_a <- rbindlist(list(fread('~/code_new/poverty/census/ss15husa.csv'),fread('~/code_new/poverty/census/ss15husb.csv')))
hus_shortened <- hus_a[,.(SERIALNO,HINCP)]
rm(hus_a);gc()

setkey(pus_shortened, SERIALNO)
setkey(hus_shortened, SERIALNO)

pums_data <- hus_shortened[pus_shortened,]
setnames(pums_data, 'SEX', 'Sex')
pums_data[, Education := convert_SCHL(SCHL)]
pums_data[, Race := convert_RAC1P(RAC1P)]
pums_data[, Income := convert_HINCP(HINCP)]

write.csv(pums_data, '~/code_new/poverty/pums_data_short.csv')

pums_xtabs <- xtabs(~Sex + Race + Education + Income, pums_data)
saveRDS(pums_xtabs, '~/code_new/poverty/pums_xtabs.RDS')

pums_xtabs_no_inc <- xtabs(~Sex + Race + Education, pums_data)
saveRDS(pums_xtabs_no_inc, '~/code_new/poverty/pums_xtabs_no_inc.RDS')

pums_xtabs_no_inc_no_ed <- xtabs(~Sex + Race, pums_data)
saveRDS(pums_xtabs_no_inc_no_ed, '~/code_new/poverty/pums_xtabs_no_inc_no_ed.RDS')

