# part of the space-time-rivers NO3 sensor data project
# download and visualize R data from NEON streams: start with CARI (AK) and/or BIGC (CA)?
# also consider KING (KS), WALK (TN), MART (WA), MAYF (?? - also, needs to change scale)

# Code to DL
#   Nitrate in surface water: DP1.20033.001

# Other useful data: 
#   Continuous discharge:       DP4.00130.001
#   Discharge field collection: DP1.20048.001
#   Reaeration field & lab      DP1.20190.001
#   PAR:                        DP1.00024.001
#   PAR at water surface:       DP1.20042.001
#   PAR below water surface:    DP1.20261.001
#   Precipitation               DP1.00006.001
#   Temp. in surface water:     DP1.20053.001
#   Water quality               DP1.20288.001 (SC, Chl-a, DO content, fDOM concentration, pH, and turbidity)
# 

# load packages
library(neonUtilities)
library(neonOS)
library(raster)
library(tidyverse)
library(here)

# set NEON token (FLBS email one)
NEON_TOKEN <- 'eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJjaHJpc3RhLnRvcnJlbnNAZmxicy51bXQuZWR1Iiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxODMyMzUwMzM5LCJpYXQiOjE2NzQ2NzAzMzksImVtYWlsIjoiY2hyaXN0YS50b3JyZW5zQGZsYnMudW10LmVkdSJ9.9LnYeA5NuTuCSeZkhQwizdh07i9dK4zmUYraXQwhrfkvaF75KNFJnqp04qJNZyhYQZFO3rHhUx7FJIsDY5sVcw'


# Set global option to NOT convert all character variables to factors
options(stringsAsFactors=F)

# download NO3 and related datasets
no3.data <- loadByProduct(dpID = "DP1.20033.001", 
                          site = c("CARI", "BIGC", "KING", "WALK"), 
                          package="expanded", check.size=F, 
                          token = Sys.getenv("NEON_TOKEN"))

precip_data <- loadByProduct(dpID = "DP1.00006.001", 
                            site = c("CARI", "BIGC", "KING", "WALK"), 
                            package="expanded", check.size=F, 
                            token = Sys.getenv("NEON_TOKEN"))

q_data <- loadByProduct(dpID = "DP4.00130.001", 
                        site = c("CARI", "BIGC", "KING", "WALK"), 
                        package="expanded", check.size=F, 
                        token = Sys.getenv("NEON_TOKEN"))

manualq_data <- loadByProduct(dpID = "DP1.20048.001", 
                        site = c("CARI", "BIGC", "KING", "WALK"), 
                        package="expanded", check.size=F, 
                        token = Sys.getenv("NEON_TOKEN"))

reaeration_data <- loadByProduct(dpID = "DP1.20190.001", 
                            site = c("CARI", "BIGC", "KING", "WALK"), 
                            package="expanded", check.size=F, 
                            token = Sys.getenv("NEON_TOKEN"))

wtemp_data <- loadByProduct(dpID = "DP1.20053.001", 
                            site = c("CARI", "BIGC", "KING", "WALK"), 
                            package="expanded", check.size=F, 
                            token = Sys.getenv("NEON_TOKEN"))

 
wqual_data <- loadByProduct(dpID = "DP1.20288.001", 
                            site = c("CARI", "BIGC", "KING", "WALK"), 
                            #site = c("CARI", "BIGC"),
                            #site = "BIGC",
                            package="expanded", check.size=F, 
                            token = Sys.getenv("NEON_TOKEN"))

par_wsurf_data <- loadByProduct(dpID = "DP1.20042.001", 
                                site = c("CARI", "BIGC", "KING", "WALK"),
                                #site = c("CARI", "BIGC"),
                                package="expanded", check.size=F, 
                                token = Sys.getenv("NEON_TOKEN"))



### Save to data_raw ###
save(no3.data, file=here("N_uptake_NEON/data/neon_data_raw/no3_data.Rdata"))
save(precip_data, file=here('N_uptake_NEON/data/neon_data_raw/precip_data.Rdata'))
save(q_data, file= here('N_uptake_NEON/data/neon_data_raw/q_data.Rdata'))
save(manual.q_data, file=here("N_uptake_NEON/data/neon_data_raw/manualq_data.Rdata"))
save(reaeration_data, file=here("N_uptake_NEON/data/neon_data_raw/reaeration_data.Rdata"))
save(wtemp_data, file=here("N_uptake_NEON/data/neon_data_raw/wtemp_data.Rdata"))
save(wqual_data, file=here("N_uptake_NEON/data/neon_data_raw/wqual_data_BIGC.Rdata"))
save(par_wsurf_data, file=here("N_uptake_NEON/data/neon_data_raw/par_wsurf_data.Rdata"))

