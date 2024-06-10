##########   StreamMetabolizer GPP from DO and more - single-station?


# load packages
library(tidyverse)
library(streamMetabolizer)
library(here)
library(lubridate)
library(plotly)


# load data
DO.csv <- read_csv(here("field_data_workup/data/data_DO/Cat_659494_2023_11_01_US.csv"))


###############   StreamMetabolizer Calc
start_date <- "2023-08-23"

DO.df <- DO.csv %>%
  mutate(solar_time = calc_solar_time(UTC_datetime, longitude= -113.82))




# Arguments
# depth depth m K
# min_interval time resolution (e.g., 15 min)
# temp Zoo time Series temperature in degrees Celcius (see details)
# DO Zoo time Series Dissolved Oxygen in mg/L (see details)
# day date of the day of interest must be in quotes
# start time of the start of the "day" usually 00:00:00 must be in quotes
# end time of the end of the "day" usually 23:45:00 must be in quotes
# sr time of sunrise in the form 04:22:00 must be in quotes
# ss time of sunset in the form 19:23:00 must be in quotes
# K K at 20 deg. C (1/dt; e.g., 1/15min)
# Details
# The input data has to be a zoo time series constructed with a chron date time object of month/day/year
# hr:min:sec (i.e., 08/18/70 23:15:00)
# sr and ss should be after and before the start and end of the time series, respectively.
# ER is calculated as sum Et (i.e, mean nighttime NEP corrected for the difference in daytime temp
#                             and average nighttime temp)
# GPP is calculated by summing NEP-ERt from sunrise to sunset
# NEP=ER+GPP
# Tested Against Rivermet spreadsheet (Izagirre 2007). The data from station 1 (7/10 - 7/15/2003)
# were used with K=0.07 from "Introduced K". ER, NEP, and GPP are in mg/L*d. The results were
# not identical. When Estimation from rivermet was regressed on estimation from this software, GPP,
# ER, and NEP intercepts did not differ significantly from 0 and slopes were nearly 1: 0.94, 0.91, and
# 0.95, respectively. Further testing is greatly appreciated.

z <- 0.5
min_interval <- '5 minutes'
DO <- DO.df$DO_mgl
temp <- DO.df$Temperature_degC
K <- 
sr <- "06:45:00"
ss <- "20:26:00"
day <- 




BC_GPP <- SM(depth=z, min_interval=min_interval, DO=DO, temp, K, day, sr="00:00:00", ss="23:45:00", start="00:00:00", end="23:45:00")








################## Calc N from GPP

# per Hall & Tank, 2003
# Molar C:N = 20
# Autotrophic R = 0.5 * GPP  (because of the assumption that autotrophic production = 0.5 GPP)
# Heterotrophic R = CR (total R) - 0.5*GPP
# Heterotrophic growth efficiency [HGE]: bracketed w low and moderate growth efficiency values: 0.05 and 0.2
# HGE = heterotrophic production / (heterotrophic production + heterotrophic respiration)
# ==> HGE = HP / (HP + HR)  (from del Giorgio et al.)
# == > HGE * HR = HP - HGE * HP
# ==   > HP(1-HGE) = HGE*HR
# ==     > HP = (HGE*HR)/(1-HGE)
# N_demand = (HP + 0.5*GPP)/20

GPP <- 4 # mg C/L/day
CR <- 

hr <- CR - 0.5*GPP
  
hge_low <- 0.05
hge_hi <- 0.2
  
hp_low <- (hge_low*hr)/(1-hge_low)
hp_hi <- (hge_hi*hr)/(1-hge_hi)


