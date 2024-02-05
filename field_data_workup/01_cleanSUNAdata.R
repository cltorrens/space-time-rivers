### Cleaning SUNA data

# This script takes the exported SUNA csvs, collates, them, and saves this as a data file (CSV)


# Add required packages
library(tidyverse)
library(here)
library(lubridate)

setwd("/Users/torrensc/Documents/R_working/Modelscape/space-time-rivers/field_data_workup/data/data_SUNA_raw")
setwd("/Users/torrensc/Documents/R_working/Modelscape/space-time-rivers/field_data_workup/data/data_SUNA_raw/testing")

file_list <- list.files(pattern = ".csv") #creates a list from all .csv files in the current working directory 

# Read and combine files, keeping only the first set of headers (row_bind)
# Also, the exported data has multiple reads for each timestamp; the 1st reading is always 0. I am removing the 0 row and averaging all others

combined_data <- bind_rows(lapply(file_list, read_csv)) %>%
  rename(NITRATE_uM = `NITRATE_UM (μM)`, DateTimeUTCchar = `DateTime (UTC+00:00)`, N_mgL = `NITRATE_MG (mg N/L)`)

#getting time into posix was not working with the combined_data, but seems to work here (?) - maybe there were other column names R disliked
select_data <- combined_data %>%
  select(DateTimeUTCchar, NITRATE_uM, N_mgL) %>%
#  filter(NITRATE_uM > 0) %>%
  mutate(DateTimeUTC = mdy_hm(DateTimeUTCchar, tz = 'UTC')) 

# %>%  # already specified the tz is UTC in the data collection, just clarifying here
#   filter(NITRATE_uM > 0)

averaged_data <- select_data %>%
 filter(NITRATE_uM > 0) %>%
  #filter(NITRATE_uM != 0) %>%
  group_by(DateTimeUTC) %>%
  summarize_if(is.numeric, mean, na.rm = TRUE)


# Save the combined data frame to a new CSV file
write_csv(combined_data, here::here("field_data_workup/data/combined_SUNA_data.csv"))

head(combined)
