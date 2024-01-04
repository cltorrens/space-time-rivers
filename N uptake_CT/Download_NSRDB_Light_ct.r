### Downloading NSRDB light data to generate light and sumlight

#### Using satellite data 

# Per Laurel Genzoli and her Klamath colleagues, NSRDB light was the best match to actual light under cloudy/ smoky conditions, compared to CIMIS (intermediate) and NLDAS (worst of the 3). 
# 
# See Laurel's script, "Download_NSRDB_Light.R"
# Also, get an API here: https://developer.nrel.gov/signup/
# 
# My API key: oI3p4xXBwjRlO5IwJpmQWyN3djftQeMC8DlkLIXq
# 
# Parameter options include:  
# 
# air_temperature,    clearsky_dhi,     clearsky_dni,                       clearsky_ghi, 
# cloud_type,         dew_point,        dhi (diffuse horizontal irrad),     dni (direct normal irrad), 
# fill_flag,          ghi (global horizontal irrad),                        relative_humidity, 
# solar_zenith_angle, surface_albedo,   surface_pressure,                   total_precipitable_water, 
# wind_direction,     wind_speed
# 
# Figure out what I want... this link helps: https://www.yellowhaze.in/solar-irradiance/
# I believe I want ghi, global horizontal irradiance, which = direct normal irradiance*cos(solar zenith angle) + diffuse horizontal irrad.


####  download NSRDB satellite-generated light

## The data downloads by 1 site and 1 year

year <- 2019

# API request parameters, except for longitude and latitude
# Declare all variables as strings. Spaces must be replaced with '+'.
################################################################################
# You must request an NSRDB api key from the link above
api_key <- 'oI3p4xXBwjRlO5IwJpmQWyN3djftQeMC8DlkLIXq' #CT's api
# Set the attributes to extract (e.g., dhi, ghi, etc.), separated by commas.
attributes <- 'clearsky_ghi,ghi,air_temperature,surface_pressure,wind_speed'
# Choose year of data
year = year
# Set leap year to true or false. True will return leap day data if present, false will not.
leap_year = 'true'
# Set time interval in minutes, i.e., '30' is half hour intervals. Valid intervals are 30 & 60 for this data source. 
#   2019-21 may have shorter (5-10 min) intervals, would need to change the URL string declaration below (in ~ L62)

interval = '30'
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
lat <- 37.05767
lon <- -119.25538

# Declare url string
URL <- paste0('https://developer.nrel.gov/api/nsrdb/v2/solar/psm3-2-2-download.csv?wkt=POINT(', lon, '+', lat, ')&names=', year, '&leap_day=', leap_year, '&interval=', interval, '&utc=', utc, '&full_name=', your_name, '&email=', your_email, '&affiliation=', your_affiliation, '&mailing_list=', mailing_list, '&reason=', reason_for_use, '&api_key=', api_key, '&attributes=', attributes)


# name the output file
output_file <- paste0(lat, '_', lon, '_', year, '_', interval, '.csv')
# API request and saving
GET(url = URL, write_disk(output_file))


##### Get light data for the period of interest 
   ## this code is also in the FAKEdata and NEONdata scripts

# Read in light CSV - NSRDB
# light units for GHI (average global horizontal irradiance) = Watt m^-2
# Conversion factor = approx 4.6 umol m^-2 sec ^-1 for each watt m^-2

real_sumlight.df <- read_csv('37.05767_-119.25538_2019_30.csv', skip=2) %>%
  mutate(Datetime = ymd_hm(paste(Year, Month, Day, Hour, Minute)), 
         Time = format(Datetime, format = "%H:%M"), 
         Jday = yday(Datetime))

# ID light NAs
na_count <- sum(is.na(real_sumlight.df$GHI))  #0
na_position <- which(is.na(real_sumlight.df$GHI))

# calculate the daily sumlight by integrating the light-time curve for each day

# AUC <- function(time, light) {
#   integrate(approxfun(time, light), min(time), max(time))$value
# }

real_sumlight <- real_sumlight.df %>%
  filter(Jday >= 176 & Jday <= 206) %>%  #Big Creek currently looking at Jdays 176:206
  group_by(Jday) %>%
  summarize(light.t = sum(GHI != 0),     # gotta divide by the # of non-0 light windows
            sumlight.real = sum(GHI)/light.t)

plot(real_sumlight$Jday, real_sumlight$sumlight.real)


sumlight.real <- real_sumlight$sumlight.real # sumlight.real = auc for that Jday

plot(x=sumlight.real, y=sumlight.ideal, 
     xlab = "true light (satellite)",
     ylab = "modeled light (StreamMetabolizer)",
     main = "Scatter Plot of ideal vs real daily light",
     xlim = c(450,650))


