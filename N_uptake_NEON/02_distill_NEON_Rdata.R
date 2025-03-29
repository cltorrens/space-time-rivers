# Purpose: to clean the raw NEON datasets: remove NAs?  Select columns, bind, save as derived datasets

# load packages
library(tidyverse) # dplyr
library(neonUtilities)
# library(geoNEON)    #not currently using AOP or other remote sensing data
# library(raster)
library(here)


### Load raw data
load(here("N_uptake_NEON/data/neon_data_raw/no3_data_all_2124.Rdata"))
#load(here("N_uptake_NEON/data/neon_data_raw/q_data.Rdata"))
#load(here("N_uptake_NEON/data/neon_data_raw/wqual_data_CARI.Rdata"))
#load(here("N_uptake_NEON/data/neon_data_raw/precip_data.Rdata"))
#load(here("N_uptake_NEON/data/neon_data_raw/wtemp_data.Rdata"))
#load(here("N_uptake_NEON/data/neon_data_raw/par_wsurf_data.Rdata"))
load(here("N_uptake_NEON/data/neon_data_raw/manualq_data.Rdata"))
load(here("N_uptake_NEON/data/neon_data_raw/reaeration_data.Rdata"))


### Separate out variables (and select rows?)

# NO3
no3_data_sensor <- no3.data.2124$NSW_15_minute
no3_spatial_sensor <- no3.data.2124$sensor_positions_20033
no3_variables <- no3.data.2124$variables_20033

# Q
Q_data_sensor <- q_data$csd_continuousDischarge
Q_spatial_sensor <- q_data$sensor_positions_00130
Q_variables <- q_data$variables_00130


# Field (Manual) Q 
manQ <- manual.q_data$dsc_fieldData
manQ_ADCP <- manual.q_data$dsc_fieldDataADCP
manQ_indiv <- manual.q_data$dsc_individualFieldData

# Water Quality - CARI only
  # unique(wqual_data$ais_maintenance[5]) #CARI only
wqual_data_sensor_CARI <- wqual_data$waq_instantaneous
wqual_spatial_sensor_CARI <- wqual_data$sensor_positions_20288
wqual_variables_CARI <- wqual_data$variables_20288

# Precip
precip_1.precip5_data_sensor <- precip_data$PRIPRE_5min
precip_1.precip30_data_sensor <- precip_data$PRIPRE_30min
precip_2.precip1_data_sensor <- precip_data$SECPRE_1min
precip_2.precip30_data_sensor <- precip_data$SECPRE_30min
precip_through.precip1_data_sensor <- precip_data$THRPRE_1min
precip_through.precip30_data_sensor <- precip_data$THRPRE_30min
precip_spatial_sensor <- precip_data$sensor_positions_00006
precip_variables <- precip_data$variables_00006 
precip_readme <- precip_data$readme_00006

# Water temp
wtemp_data_30m_sensor <- wtemp_data$TSW_30min
wtemp_data_05m_sensor <- wtemp_data$TSW_5min
wtemp_spatial_sensor <- wtemp_data$sensor_positions_20053
wtemp_variables <- wtemp_data$variables_20053
wtemp_readme <- wtemp_data$readme_20053

# PAR @ water surface
par_wsurf_sensor_01m <- par_wsurf_data$PARWS_1min
par_wsurf_sensor_05m <- par_wsurf_data$PARWS_5min
par_wsurf_sensor_30m <- par_wsurf_data$PARWS_30min
par_wsurf_spatial_sensor <- par_wsurf_data$sensor_positions_20042
par_wsurf_variables <- par_wsurf_data$variables_20042
par_wsurf_readme <- par_wsurf_data$readme_20042
  
######################### Save derived data #########################

# NO3
save(no3_data_sensor, file=here("N_uptake_NEON/data/neon_data_derived/no3_dataset_2124.Rdata"))
save(no3_spatial_sensor, file=here('N_uptake_NEON/data/neon_data_derived/no3_spatial_sensor_2124.Rdata'))
save(no3_variables, file=here('N_uptake_NEON/data/neon_data_derived/no3_variables_2124.Rdata'))

# Q
save(Q_data_sensor, file="data_derived/q_dataset.Rdata")
save(Q_spatial_sensor, file='data_derived/q_spatial_sensor.Rdata')
save(Q_variables, file='data_derived/q_variables.Rdata')

# ~/Documents/R_working/Modelscape/space-time-rivers/N_uptake_NEON/data/neon_data_derived
# Field (Manual) Q 
save(manQ, file=here("N_uptake_NEON/data/neon_data_derived/manualQ_dataset.Rdata"))
save(manQ_ADCP, file=here("N_uptake_NEON/data/neon_data_derived/manualQ_ADCP_dataset.Rdata"))  
save(manQ_indiv, file=here("N_uptake_NEON/data/neon_data_derived/manualQ_indiv_dataset.Rdata")) 

# Water Quality - CARI only
save(wqual_data_sensor_CARI, file="data_derived/wqual_dataset_CARI.Rdata")
save(wqual_spatial_sensor_CARI, file='data_derived/wqual_spatial_sensor_CARI.Rdata')
save(wqual_variables_CARI, file='data_derived/wqual_variables_CARI.Rdata')

# Precip
save(precip_1.precip5_data_sensor, file="data_derived/precip_dataset_1.precip05.Rdata")
save(precip_1.precip30_data_sensor, file="data_derived/precip_dataset_1.precip30.Rdata")
save(precip_2.precip1_data_sensor, file="data_derived/precip_dataset_2.precip01.Rdata")
save(precip_2.precip30_data_sensor, file="data_derived/precip_dataset_2.precip30.Rdata")
save(precip_through.precip1_data_sensor, file="data_derived/precip_dataset_through.precip01.Rdata")
save(precip_through.precip30_data_sensor, file="data_derived/precip_dataset_through.precip30.Rdata")
save(precip_spatial_sensor, file='data_derived/precip_spatial_sensor.Rdata')
save(precip_variables, file='data_derived/precip_variables.Rdata')
save(precip_readme, file='data_derived/precip_readme.Rdata')

# Water temp
save(wtemp_data_30m_sensor, file="data_derived/wtemp_dataset_30m.Rdata")
save(wtemp_data_05m_sensor, file="data_derived/wtemp_dataset_05m.Rdata")
save(wtemp_spatial_sensor, file='data_derived/wtemp_spatial_sensor.Rdata')
save(wtemp_variables, file='data_derived/wtemp_variables.Rdata')
save(wtemp_readme, file='data_derived/wtemp_readme.Rdata')

# PAR @ water surface
save(par_wsurf_sensor_01m, file="data_derived/par_wsurf_dataset_01m.Rdata")
save(par_wsurf_sensor_05m, file="data_derived/par_wsurf_dataset_05m.Rdata")
save(par_wsurf_sensor_30m, file="data_derived/par_wsurf_dataset_30m.Rdata")
save(par_wsurf_spatial_sensor, file='data_derived/par_wsurf_spatial_sensor.Rdata')
save(par_wsurf_variables, file='data_derived/par_wsurf_variables.Rdata')
save(par_wsurf_readme, file='data_derived/par_wsurf_readme.Rdata')



