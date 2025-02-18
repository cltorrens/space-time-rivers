# NEON NO3 spatial data info

# this script was created to quickly ID and save current sensor location info to a csv
# variables included = site name, stream name, lat, long, elevation 

# load packages
library(tidyverse)
library(neonUtilities)


# load spatial Rdata 
load(here("N_uptake_NEON/data/neon_data_derived/no3_spatial_sensor.Rdata"))

# (separate file for initial 4)
load(here("N_uptake_NEON/data/neon_data_derived/CARI-KING-BIGC-WALK/no3_spatial_sensor.Rdata"))

datetimeVars <- c("positionStartDateTime", "positionEndDateTime", "referenceLocationIDStartDateTime", "referenceLocationIDEndDateTime")
  
no3_spatial.df <- no3_spatial_sensor %>%
  select(siteID, sensorLocationDescription, positionStartDateTime, positionEndDateTime, 
         referenceLocationIDDescription, referenceLocationIDStartDateTime, referenceLocationIDEndDateTime, 
         locationReferenceLatitude, locationReferenceLongitude, locationReferenceElevation, publicationDate) %>%
  mutate_at(datetimeVars, ymd_hms)

View(no3_spatial.df)

# for the initial 4 sensors

datetimeVars <- c("start", "end", "referenceStart", "referenceEnd")

no3_spatial.df2 <- no3_spatial_sensor %>%
  select(siteID, description, start, end, 
         referenceDescription, referenceStart, referenceEnd, 
         referenceLatitude, referenceLongitude, referenceElevation, publicationDate) %>%
  mutate_at(datetimeVars, ymd_hms) %>%      # rename columns to facilitate rbinding
  rename(sensorLocationDescription = description, positionStartDateTime = start, positionEndDateTime = end,
         referenceLocationIDDescription = referenceDescription, referenceLocationIDStartDateTime = referenceStart, 
         referenceLocationIDEndDateTime = referenceEnd, locationReferenceLatitude = referenceLatitude, 
         locationReferenceLongitude = referenceLongitude, locationReferenceElevation = referenceElevation)

View(no3_spatial.df2)

no3_spatial_all.df <- bind_rows(no3_spatial.df, no3_spatial.df2)

View(no3_spatial_all.df)

write_csv(no3_spatial_all.df, file = here("N_uptake_NEON/data/NEON_no3sensor_locations.csv"))
