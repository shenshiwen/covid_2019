############################################################
#### load data and brief EDA for coronavirus data
#### Source: https://github.com/CSSEGISandData/COVID-19
#### Author: Shiwen Shen
#### Last Updated: 02/18/2020
############################################################

## load package
library(tidyverse)
library(data.table)
library(ggpubr)

## source functions
source("functions.R")

## set working directory
setwd("D:/Dropbox/coronavirus/covid_2019")

data_location <- "data_02182020/" 

#### load & process data ####
dat_confirm <- fread(paste0(data_location, "time_series_19-covid-Confirmed.csv"))
dat_death <- fread(paste0(data_location, "time_series_19-covid-Deaths.csv"))
dat_recov <- fread(paste0(data_location, "time_series_19-covid-Recovered.csv"))

## When Province/State is missing, replace NA with Country/Region
## and create indicator for whether Province/State equals to Country/Region
dat_confirm[, country_only := `Province/State` == ""]
dat_confirm[`Province/State` == "", `Province/State` := `Country/Region`]

dat_death[, country_only := `Province/State` == ""]
dat_death[`Province/State` == "", `Province/State` := `Country/Region`]

dat_recov[, country_only := `Province/State` == ""]
dat_recov[`Province/State` == "", `Province/State` := `Country/Region`]

## combine three table together
dat_confirm[, type := "confirm"]
dat_death[, type := "death"]
dat_recov[, type := "recovery"]
dat_comb <- rbindlist(list(dat_confirm, dat_death, dat_recov), use.names = FALSE)
names(dat_comb) <- names(dat_confirm)

## change wide data to long data
dat_comb_long <- melt(dat_comb,
                      id.vars = c("Province/State", "Country/Region", "Lat", "Long", "country_only", "type"),
                      measure.vars = names(dat_comb)[which(!names(dat_comb) %in% c("Province/State", "Country/Region", "Lat", "Long", "country_only", "type"))])

dat_comb_long$time <- as.Date(dat_comb_long$variable, "%m/%d/%y")
dat_comb_long$variable <- NULL
dat_comb_long$count <- dat_comb_long$value
dat_comb_long$value <- NULL
names(dat_comb_long) <- c("province_state", "country_region", "latitude", "longitude", "country_only", "type", "time", "count")

dat <- dat_comb_long

rm(dat_comb, dat_comb_long, dat_confirm, dat_death, dat_recov)

## compute incremental counts
dat_last_day <- dat[,c("province_state", "country_region", "type", "time", "count")]
setnames(dat_last_day, old = "count", new = "count_last_day")
dat_last_day[,time := time + 1]

dat_inc <- merge.data.table(x=dat, y=dat_last_day,
                            all.x=TRUE,
                            by = c("province_state", "country_region", "type", "time"))
dat_inc <- na.omit(dat_inc)
dat_inc[,count_inc := count - count_last_day]
dat_inc[,count_inc_percent := ifelse(count_last_day != 0, 100 * count_inc / count_last_day, 0)]
dat_inc$count <- NULL
dat_inc$count_last_day <- NULL

#### EDA plots ------------------------------
EDA_Plot_1(province_state = "Hebei", dat, dat_inc)



