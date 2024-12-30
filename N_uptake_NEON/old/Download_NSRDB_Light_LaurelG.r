#
#  
#  The following downloads NSRDB PSM-v3 Light Data.
#  Hourly data for SV, WE, KAT through 2022

#  Laurel.genzoli@gmail.com
#  adapted from: https://github.com/dazhiyang/SEDB-S-1/blob/master/Jupyter-R/download-single-point.ipynb


#  Before running: get an API here: https://developer.nrel.gov/signup/

#######################################################################


## How to cite: NREL National Solar Radiation Database was accessed on DATE from https://registry.opendata.aws/nrel-pds-nsrdb. 
## Laurel's api key: fRTM9nd04Y6M2z5FSlqXcHnsnlrYdejkTF6VbY6f

## Christa's api key: oI3p4xXBwjRlO5IwJpmQWyN3djftQeMC8DlkLIXq

library(httr)
library(tidyr)

## parameter options include:  

#air_temperature, 
#clearsky_dhi, 
#clearsky_dni, 
#clearsky_ghi, 
#cloud_type, 
#dew_point, 
#dhi, 
#dni, 
#fill_flag, 
#ghi, 
#relative_humidity, 
#solar_zenith_angle, 
#surface_albedo, 
#surface_pressure, 
#total_precipitable_water, 
#wind_direction, 
#wind_speed


## Download light data from NSRDB PSM-v3
## I Loop through all dates needed and will just repeat 3 times for my 3 sites


########################
## SV:
########################
setwd('/Users/laurelgenzoli/Dropbox/2018-2022_PHD/2023_Klamath_Metab/Klamath-Metabolism/DATA/Light/SV_light')


for (i in 2005:2022){

# API request parameters, except for longitude and latitude
# Declare all variables as strings. Spaces must be replaced with '+'.
################################################################################
# You must request an NSRDB api key from the link above
api_key <- 'oI3p4xXBwjRlO5IwJpmQWyN3djftQeMC8DlkLIXq' #CT's api
# Set the attributes to extract (e.g., dhi, ghi, etc.), separated by commas.
attributes <- 'clearsky_ghi,ghi,air_temperature,surface_pressure,wind_speed'
# Choose year of data
year = i
# Set leap year to true or false. True will return leap day data if present, false will not.
leap_year = 'true'
# Set time interval in minutes, i.e., '30' is half hour intervals. Valid intervals are 30 & 60.
interval = '60'
# Specify Coordinated Universal Time (UTC), 'true' will use UTC, 'false' will use the local time zone of the data.
utc = 'false'
# Your full name, use '+' instead of spaces.
your_name = 'Christa+Torrens'
# Your reason for using the NSRDB.
reason_for_use = 'research'
# Your affiliation
your_affiliation = 'University+of+Montana'
# Your email address
your_email = 'christa.torrens@flbs.umt.edu'
# Please join our mailing list so we can keep you up-to-date on new developments.
mailing_list = 'false'
################################################################################
lat <- 41.8536
lon <- -123.2319

# Declare url string
URL <- paste0('https://developer.nrel.gov/api/nsrdb/v2/solar/psm3-2-2-download.csv?wkt=POINT(', lon, '+', lat, ')&names=', year, '&leap_day=', leap_year, '&interval=', interval, '&utc=', utc, '&full_name=', your_name, '&email=', your_email, '&affiliation=', your_affiliation, '&mailing_list=', mailing_list, '&reason=', reason_for_use, '&api_key=', api_key, '&attributes=', attributes)


# name the output file
output_file <- paste0(lat, '_', lon, '_', i, '.csv')
# API request and saving
GET(url = URL, write_disk(output_file))
}



## Bind data into 1 file
#########################################
setwd('/Users/laurelgenzoli/Dropbox/2018-2022_PHD/2023_Klamath_Metab/Klamath-Metabolism/DATA/Light')

SV.light <- list.files(path="SV_light", full.names = TRUE)%>%
  lapply(read.csv, skip = 3)%>%
  bind_rows()%>%
  select(1:7)%>%
  rename("year" = 1, "month"= 2, "day"= 3, "hour"= 4, "minute" = 5, "GHI"= 7, "clear.sky.GHI"= 6)%>%  #Laurel init. swapped the last two, now corrected
  mutate(date = as.Date(paste(year, month, day, sep = "-")))%>%
  mutate(time = paste(hour, minute, sep = ":"))

write.csv(SV.light, "SV_light_280922.csv")







######################
#### WE:
######################


setwd('/Users/laurelgenzoli/Dropbox/2018-2022_PHD/2023_Klamath_Metab/Klamath-Metabolism/DATA/Light/WE_light')
for (i in 2007:2022){

  api_key <- 'fRTM9nd04Y6M2z5FSlqXcHnsnlrYdejkTF6VbY6f'
  attributes <- 'ghi,clearsky_ghi'
  year = i
  leap_year = 'true'
  interval = '60'
  utc = 'false'
  your_name = 'Laurel+Genzoli'
  reason_for_use = 'research'
  your_affiliation = 'University+of+Montana'
  your_email = 'laurel.genzoli@gmail.com'
  mailing_list = 'false'
  
  lat <- 41.1858
  lon <- -123.7035
  
  URL <- paste0('https://developer.nrel.gov/api/nsrdb/v2/solar/psm3-2-2-download.csv?wkt=POINT(', lon, '+', lat, ')&names=', year, '&leap_day=', leap_year, '&interval=', interval, '&utc=', utc, '&full_name=', your_name, '&email=', your_email, '&affiliation=', your_affiliation, '&mailing_list=', mailing_list, '&reason=', reason_for_use, '&api_key=', api_key, '&attributes=', attributes)
  output_file <- paste0(lat, '_', lon, '_', i, '.csv')
  GET(url = URL, write_disk(output_file))
}

## Bind data into 1 file
####################################################
setwd('/Users/laurelgenzoli/Dropbox/2018-2022_PHD/2023_Klamath_Metab/Klamath-Metabolism/DATA/Light')

WE.light <- list.files(path="WE_light", full.names = TRUE)%>%
  lapply(read.csv, skip = 3)%>%
  bind_rows()%>%
  select(1:7)%>%
  rename("year" = 1, "month"= 2, "day"= 3, "hour"= 4, "minute" = 5, "GHI"= 6, "clear.sky.GHI"= 7)%>%
  mutate(date = as.Date(paste(year, month, day, sep = "-")))%>%
  mutate(time = paste(hour, minute, sep = ":"))

write.csv(WE.light, "WE_light_280922.csv")



######################
#### KAT:
######################

setwd('/Users/laurelgenzoli/Dropbox/2018-2022_PHD/2023_Klamath_Metab/Klamath-Metabolism/DATA/Light/KAT_light')
for (i in 2007:2022){
  
  api_key <- 'fRTM9nd04Y6M2z5FSlqXcHnsnlrYdejkTF6VbY6f'
  attributes <- 'ghi,clearsky_ghi'
  year = i
  leap_year = 'true'
  interval = '60'
  utc = 'false'
  your_name = 'Laurel+Genzoli'
  reason_for_use = 'research'
  your_affiliation = 'University+of+Montana'
  your_email = 'laurel.genzoli@gmail.com'
  mailing_list = 'false'
  
  lat <- 41.5112
  lon <- -123.9775
  
  URL <- paste0('https://developer.nrel.gov/api/nsrdb/v2/solar/psm3-2-2-download.csv?wkt=POINT(', lon, '+', lat, ')&names=', year, '&leap_day=', leap_year, '&interval=', interval, '&utc=', utc, '&full_name=', your_name, '&email=', your_email, '&affiliation=', your_affiliation, '&mailing_list=', mailing_list, '&reason=', reason_for_use, '&api_key=', api_key, '&attributes=', attributes)
  output_file <- paste0(lat, '_', lon, '_', i, '.csv')
  GET(url = URL, write_disk(output_file))
}



## Bind data into 1 file
####################################################
setwd('/Users/laurelgenzoli/Dropbox/2018-2022_PHD/2023_Klamath_Metab/Klamath-Metabolism/DATA/Light')

KAT.light <- list.files(path="KAT_light", full.names = TRUE)%>%
  lapply(read.csv, skip = 3)%>%
  bind_rows()%>%
  select(1:7)%>%
  rename("year" = 1, "month"= 2, "day"= 3, "hour"= 4, "minute" = 5, "GHI"= 6, "clear.sky.GHI"= 7)%>%
  mutate(date = as.Date(paste(year, month, day, sep = "-")))%>%
  mutate(time = paste(hour, minute, sep = ":"))

write.csv(KAT.light, "KAT_light_280922.csv")


