## Nitrate issues and flags

## Load packages

# library(scales) # scales map data to aesthetics
library(tidyverse) # includes magrittr, as part of dplyr
library(hms) # 'pretty time of day' - no longer loads w tidyverse?
library(lubridate)
library(tidybayes)
library(GGally) # adds functions to ggplot()
library(zoo)
library(neonUtilities)
library(ggpubr) # publication-ready graphs
library(brms)
library(here) # allows project-based file paths
library(pracma) # for 1 type of light AUC calcs
library(dygraphs) # creates interactive ts graphs
library(knitr) # to show full flag and issue logs with kable
library(DT) # interactive table for flag and issue logs



## Load data

load(here("N_uptake_NEON/data/neon_data_derived/no3_flags_2124.Rdata"))
load(here("N_uptake_NEON/data/neon_data_derived/no3_issuelog_2124.Rdata"))
# load(here("N_uptake_NEON/data/neon_data_derived/par_wsurf_readme.Rdata"))
load(here("N_uptake_NEON/data/neon_data_derived/q_flags.Rdata"))
load(here("N_uptake_NEON/data/neon_data_derived/q_issuelog.Rdata"))
load(here("N_uptake_NEON/data/neon_data_derived/q_readme.Rdata"))

# View

# NO3issues <- no3_issuelog_2124 %>%
#   filter(
#     grepl(paste(unique(par_wsurf_sensor_30m$siteID), collapse = "|"), locationAffected, ignore.case = TRUE) |
#       grepl("all", locationAffected, ignore.case = TRUE)
#   )

# view(WSPARissues) truncates cell text. INSTEAD, use DT::datatable()

# datatable(WSPARissues, options = list(pageLength = 20, autoWidth = TRUE))


# Flags by site and sensor

datatable(no3_flags, options = list(pageLength = 20, autoWidth = TRUE))
# 3 total for CARI and OKSR: positive nighttime PAR, out of range data offset

# SYCA 2020-10-19T17:00:00Z	to 2021-02-28T23:59:00Z: Per DQTT0001410, dry conditions exposed sensors
# SYCA 2021-03-01T00:00:00Z	to 2021-04-30T23:59:00Z: Per DQTT0001410, dry conditions exposed sensors
# SYCA 2022-11-30T16:00:00Z	to 2022-12-31T23:59:59Z: Stream is dry. Sensors reading in air.
# SYCA 2023-01-01T00:00:00Z	to 2023-01-02T01:27:00Z: Stream is dry. Sensors reading in air.
# SYCA 2023-01-20T23:30:00Z	to 2023-01-31T23:59:59Z: Sensor is buried or filled with sediment. Values suspect.
# SYCA 2023-02-01T00:00:00Z	to 2023-02-18T17:58:00Z: Sensor is buried or filled with sediment. Values suspect.
# SYCA 2023-04-01T00:00:00Z	to 2023-04-30T23:59:59Z: Sensors buried.
# SYCA 2023-04-04T20:29:00Z	to 2023-04-30T23:59:59Z: Sonde at S2 was impacted by sediment, but reviewed SUNA data and it looks ok.
# SYCA 2023-09-06T16:00:00Z	to 2024-02-03T20:23:00Z: Sensor coming out of the water due to falling water levels. \n: Stream still dry.\n: Stream still dry
# SYCA 2024-06-15T00:00:00Z	to 2024-07-10T15:00:00Z: Sensor failing.

datatable(no3_issuelog, options = list(pageLength = 20, autoWidth = TRUE))
# SYCA 2020-10-19T00:00:00Z	to 2021-04-30T00:00:00Z (?? 2 sets of dates, this is dateRangeStart-dateRangeEnd): 
# issue: Low flow conditions resulting in exposed sensors	
# resolution: Stage came back up and sensors re-submerged on their own.

# SYCA 2022-02-03T00:00:00Z	to 2022-02-03T00:00:00Z: Study reach shifted upstream. Old upstream location became new downstream location.	
# Resolution: sensorPositions file updated to reflect new locations.

# ALL: 2020-03-23T00:00:00Z to	2021-12-31T00:00:00Z: Safety measures to protect personnel during the COVID-19 pandemic resulted in reduced or canceled maintenance activities for extended periods at NEON sites. Data availability and/or quality may be negatively impacted during this time. In addition, the annual refresh of sensors and data acquisition systems (DAS) did not occur according to the typical 1-year schedule for many sites. The annual refresh is where freshly calibrated and verified sensors and DAS replace the units in the field.	
# Resolution: NEON reviewed data from all sites and time periods potentially impacted by COVID-19 safety precautions to identify and manually flag suspect data that escaped automated quality tests. Suspect data are indicated by the final quality flag in the data files, which should be used to inform data filtering prior to use. Data during this time period should be treated as valid unless marked suspect by other quality flags. Data that are overdue for field calibrations are indicated by the validCalQF quality flag.

datatable(Q_flags, options = list(pageLength = 20, autoWidth = TRUE))

datatable(Q_issuelog, options = list(pageLength = 20, autoWidth = TRUE))

datatable(Q_readme, options = list(pageLength = 20, autoWidth = TRUE))
