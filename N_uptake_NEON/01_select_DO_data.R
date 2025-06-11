### The purpose of this script is to take datasets provided by Bobby Hensley (NEON), and select 
#   the days that I need to estimate GPP for my nitrate model. This is the link to the folder Bobby shared, 
#   which he is populating with DO **and co-located temperature data** for his collaboration with Kelly Aho. 
#   https://drive.google.com/drive/folders/180khdU7rR65UqWKDQu8WPEwMSo_U0J5E?usp=drive_link



# load packages
library(tidyverse)
library(here)

# Load data: 

# FOR ALL
path <- here("N_uptake_NEON/data/model_output/daily_summary_all.rds")
daily_summary.df <- readRDS(file=path) 


#####  CARI 
cari_do_wy22 <- read_csv(here("N_uptake_NEON/data/DO_data_Hensley/CARI_WY22.csv"))
cari_do_wy23 <- read_csv(here("N_uptake_NEON/data/DO_data_Hensley/CARI_WY23.csv"))
cari_do_wy24 <- read_csv(here("N_uptake_NEON/data/DO_data_Hensley/CARI_WY24.csv"))

# Get object with CARI's yr_jdays 
cari_yrjday <- daily_summary.df %>%
  dplyr::filter(site == "CARI") %>%
  pull(yr_jday) 

# Combine the CARI water year files into 1 file, create a yr_jday column, then select the yr_jdays to match my data
cari_do_all <- bind_rows(cari_do_wy22, cari_do_wy23, cari_do_wy24) %>%
  mutate(Year = year(solar.time), 
         jday = yday(solar.time), 
         jday_pad = str_pad(jday, width=3, pad="0")) %>%
  unite("yr_jday", Year, jday_pad, sep = '_', remove=FALSE) %>%
  select(yr_jday, solar.time, DO.obs, temp.water, discharge) 

cari_do_select <- cari_do_all %>%
  dplyr::filter(yr_jday %in% cari_yrjday)

path <- here("N_uptake_NEON/data/DO_data_model/cari_do.csv")
write_csv(cari_do_select, file=path)


#####  CUPE 
cupe_do_wy22 <- read_csv(here("N_uptake_NEON/data/DO_data_Hensley/CUPE_WY22.csv"))
cupe_do_wy23 <- read_csv(here("N_uptake_NEON/data/DO_data_Hensley/CUPE_WY23.csv"))
cupe_do_wy24 <- read_csv(here("N_uptake_NEON/data/DO_data_Hensley/CUPE_WY24.csv"))

# Get object with CUPE's yr_jdays 
cupe_yrjday <- daily_summary.df %>%
  dplyr::filter(site == "CUPE") %>%
  pull(yr_jday) 

# Combine the CUPE water year files into 1 file, create a yr_jday column, then select the yr_jdays to match my data
cupe_do_all <- bind_rows(cupe_do_wy22, cupe_do_wy23, cupe_do_wy24) %>%
  mutate(Year = year(solar.time), 
         jday = yday(solar.time), 
         jday_pad = str_pad(jday, width=3, pad="0")) %>%
  unite("yr_jday", Year, jday_pad, sep = '_', remove=FALSE) %>%
  select(yr_jday, solar.time, DO.obs, temp.water, discharge) 

cupe_do_select <- cupe_do_all %>%
  dplyr::filter(yr_jday %in% cupe_yrjday)

path <- here("N_uptake_NEON/data/DO_data_model/cupe_do.csv")
write_csv(cupe_do_select, file=path)





#####  WLOU 
wlou_do_wy22 <- read_csv(here("N_uptake_NEON/data/DO_data_Hensley/WLOU_WY22.csv"))
wlou_do_wy23 <- read_csv(here("N_uptake_NEON/data/DO_data_Hensley/WLOU_WY23.csv"))
wlou_do_wy24 <- read_csv(here("N_uptake_NEON/data/DO_data_Hensley/WLOU_WY24.csv"))

# Get object with WLOU's yr_jdays 
wlou_yrjday <- daily_summary.df %>%
  dplyr::filter(site == "WLOU") %>%
  pull(yr_jday) 

# Combine the WLOU water year files into 1 file, create a yr_jday column, then select the yr_jdays to match my data
wlou_do_all <- bind_rows(wlou_do_wy22, wlou_do_wy23, wlou_do_wy24) %>%
  mutate(Year = year(solar.time), 
         jday = yday(solar.time), 
         jday_pad = str_pad(jday, width=3, pad="0")) %>%
  unite("yr_jday", Year, jday_pad, sep = '_', remove=FALSE) %>%
  select(yr_jday, solar.time, DO.obs, temp.water, discharge) 

wlou_do_select <- wlou_do_all %>%
  dplyr::filter(yr_jday %in% wlou_yrjday)

# the days aren't even: 11137 obs = 116 days + 15 min (1 obs)
#wlou_obsPERday <- wlou_do_select %>% count(yr_jday) # Sept 30, 2022 has 97 obs (not 96)

wlou_check <- wlou_do_select %>%
  count(yr_jday) %>%
  filter(n != 96)

wlou_view <- wlou_do_select %>%
  dplyr::filter(yr_jday %in% wlou_check$yr_jday)

# one timestep is duplicated (rows 68 and 69): similar values, same timestep. Removing 68
wlou_do_select <- wlou_do_select %>%
  dplyr::slice(-68)

# save the data
path <- here("N_uptake_NEON/data/DO_data_model/wlou_do.csv")
write_csv(wlou_do_select, file=path)

