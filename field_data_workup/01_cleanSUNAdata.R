### Cleaning SUNA data

# This script takes the exported SUNA csvs, collates, them, and saves this as a data file (CSV)


# Add required packages
library(tidyverse)
library(here)
library(lubridate)
library(plotly)

# here::i_am("field_data_workup/02_StreamMetabolizer_and_assim_N.R")

setwd("/Users/torrensc/Documents/R_working/Modelscape/space-time-rivers/field_data_workup/data/data_SUNA_raw")
#setwd("/Users/torrensc/Documents/R_working/Modelscape/space-time-rivers/field_data_workup/data/data_SUNA_raw/testing")

file_list <- list.files(pattern = ".csv") #creates a list from all .csv files in the current working directory 

# Read and combine files, keeping only the first set of headers (row_bind)
# Also, the exported data has multiple reads for each timestamp; the 1st reading is always 0. I am removing the 0 row and averaging all others

combined_data <- bind_rows(lapply(file_list, read_csv)) %>%
  rename(NO3_uM = `NITRATE_UM (μM)`, DateTimeUTCchar = `DateTime (UTC+00:00)`, N_mgL = `NITRATE_MG (mg N/L)`)

#getting time into posix was not working with the combined_data, but seems to work here (?) - maybe there were other column names R disliked
select_data <- combined_data %>%
  select(DateTimeUTCchar, NO3_uM, N_mgL) %>%
#  filter(NITRATE_uM > 0) %>%
  mutate(DateTimeUTC = mdy_hm(DateTimeUTCchar, tz = 'UTC'), 
         jdayUTC = yday(DateTimeUTC)) %>%
  select(!DateTimeUTCchar)

unique(select_data$jdayUTC)  # missing 232; 253, 254; 266, 267, 268

# already specified the tz is UTC in the data collection, just clarifying here
#   filter(NITRATE_uM > 0)

averaged_data <- select_data %>%
 filter(NO3_uM != 0) %>%
  #filter(NITRATE_uM != 0) %>%
  group_by(DateTimeUTC) %>%
  summarize_if(is.numeric, mean, na.rm = TRUE) %>%
  mutate(local_datetime = with_tz(DateTimeUTC, tzone = 'US/Mountain'), 
         jday_local = yday(local_datetime), 
         date_local = date(local_datetime))

unique(averaged_data$jday_local)  # not logged here:#253, 254 - not on SUNA

# identical(sort(unique(select_data$jdayUTC)), sort(unique(averaged_data$jdayUTC))) # not missing any additional or different days

# Visualize the data

dfplot <- ggplot(data=averaged_data, aes(x=local_datetime, y=NO3_uM)) + 
  geom_point() + 
  labs(x = "Time", y = "NO3") + 
  ylim(0,6) + 
  theme_minimal()

ggplotly(dfplot, tooltip = c("x", "y"))

bcplot1 <- averaged_data %>%
  filter(jday_local < 252) %>%
  ggplot(aes(x=local_datetime, y=NO3_uM)) + 
  geom_point() + 
  labs(x = "Time", y = "NO3") + 
  ylim(0,6) + 
  theme_minimal()

bcplot1

bcplot2 <- 



# Save the combined data frame to a new CSV file
write_csv(averaged_data, here::here("field_data_workup/data/SUNA_data_averaged.csv"))

#setwd(here())

############# Remember to reset working directory to project home directory!!  ################



#######   "CALIBRATION" FILES for data correction

cal6.24 <- read_csv2('Calibration_2024166.CSV')
