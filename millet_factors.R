#FUNCTION TO GET MILLET FACTORS SUMMARY##
library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)
source('C:\\SenegalGIS\\crop_model_result\\fromsharecomputer\\soil_summary.R')
source('C:\\SenegalGIS\\crop_model_result\\fromsharecomputer\\weather_summary.R')

###################################################
#READ AND SUMMARIZE AQUACROP RESULT FILES
setwd('C:\\SenegalGIS\\crop_model_result\\fromsharecomputer\\')
#setwd('C:\\SenegalGIS\\crop_model_result\\')
millet_summary<-read.csv("millet_FINAL_fullrun.csv")
millet_summary$Harvest.Date..YYYY.MM.DD.<-as.Date(millet_summary$Harvest.Date..YYYY.MM.DD.)
millet_summary$year<-year(millet_summary$Harvest.Date..YYYY.MM.DD.)


######################################################

###TODO: Call weather_summary.R and soil_summary.R

#############################
#MERGE DATASETS
#millet_factors<-merge(millet_flux, millet_growth_summary, by=c('site_id', 'soil_id', 'year', 'planting_date'))
soil<-get_soil()
millet_factors<-merge(millet_summary, soil, by.x='soil_id', by.y = "ID")

millet_factors<-merge(millet_factors, june_summary, by.x=c('site_id', 'year'), by.y = c('zone_id', 'year'))
millet_factors<-merge(millet_factors, july_summary, by.x=c('site_id', 'year'), by.y = c('zone_id', 'year'))
millet_factors<-merge(millet_factors, august_summary, by.x=c('site_id', 'year'), by.y = c('zone_id', 'year'))
millet_factors<-millet_factors%>%select(-one_of('zone.x','zone.y'))
millet_factors<-merge(millet_factors, sept_summary, by.x=c('site_id', 'year'), by.y = c('zone_id', 'year'))

setwd('C:\\SenegalGIS\\crop_model_result\\fromsharecomputer\\')
#setwd('C:\\SenegalGIS\\crop_model_result\\')
write.csv(millet_factors, file='millet_factors_fullrunTEST.csv')
