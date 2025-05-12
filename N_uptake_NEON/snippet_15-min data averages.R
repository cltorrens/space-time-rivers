## Code snippet: convert 1-min to 15-min average values
# Easier to deal with, fewer gaps to fill. Per ChatGPT (check it!), 15-min periods where all data is NA will return an NA for maxpostDischarge 

library(dplyr)
library(lubridate)

# Assume your dataframe is called `df` and columns are:
# - 'timestamp': POSIXct or character datetime
# - 'discharge': numeric discharge value

df <- df %>%
  mutate(
    timestamp = as.POSIXct(timestamp),  # ensure proper datetime format
    time_15min = floor_date(timestamp, unit = "15 minutes")  # round down to nearest 15-min
  ) %>%
  group_by(time_15min) %>%
  summarise(discharge_avg = mean(discharge, na.rm = TRUE), .groups = "drop")




