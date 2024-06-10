
# Discharge data

# load packages
library(scales)
library(magrittr) # moved from  'light ea day as vector'
library(tidyverse)
library(lubridate)
library(streamMetabolizer)
library(rstan)
library(tidybayes)
library(GGally)
library(shinystan)
library(zoo)
library(neonUtilities)
library(ggpubr)
library(brms)
library(here) # allows project-based file paths
library(pracma) # for 1 type of light AUC calcs
library(httr) # helps w. the NSRDB light download

# load full dataset
load(here("N_uptake_NEON/data/neon_data_raw/manualq_data.Rdata"))

#Load derived data
load(here("N_uptake_NEON/data/neon_data_derived/manualQ_dataset.Rdata")) #manQ
load(here("N_uptake_NEON/data/neon_data_derived/manualQ_ADCP_dataset.Rdata")) #manQ_ADCP


# pluck item #4 (the manual Q dataset) from the list using the purrr fcn
manQ_data <- purrr::pluck(manual.q_data, 4)

# filter by site 


bigc.manQ.df <- manQ_data %>%
  filter(siteID == "BIGC") %>% 
  mutate(local_datetime = with_tz(collectDate, tzone="US/Pacific"), 
         Jday = yday(local_datetime))  #130-365

cari.manQ.df <- manQ_data %>%
  filter(siteID == "CARI") %>% 
  mutate(local_datetime = with_tz(collectDate, tzone="US/Alaska"), 
         Jday = yday(local_datetime)) 

king.manQ.df <- manQ_data %>%
  filter(siteID == "KING") %>% 
  mutate(local_datetime = with_tz(collectDate, tzone="US/Central"), 
         Jday = yday(local_datetime)) 

walk.manQ.df <- manQ_data %>%
  filter(siteID == "WALK") %>% 
  mutate(local_datetime = with_tz(collectDate, tzone="US/Eastern"), ## in Knox Co. = Eastern Time
         Jday = yday(local_datetime)) 


bigc.manQ.df.19 <- bigc.manQ.df %>%
  filter(year(local_datetime) == 2019)
  

bigc.manQ.df.20 <- bigc.manQ.df %>%
  filter(year(local_datetime) == 2020)

cari.manQ.df.19 <- cari.manQ.df %>%
  filter(year(local_datetime) == 2019)

cari.manq.df.21 <- cari.manq.df %>%
  filter(year(local_datetime) == 2021)  #CHECK

king.manq.df.19 <- king.manq.df %>%
  filter(year(local_datetime) == 2019)

king.manq.df.20 <- king.manq.df %>%
  filter(year(local_datetime) == 2020)

walk.manq.df.19 <- walk.manq.df %>%
  filter(year(local_datetime) == 2019)

walk.manq.df.22 <- walk.manq.df %>%
  filter(year(local_datetime) == 2019) 

mean(bigc.manq.df.19$streamStage) # 0.26m
mean(bigc.manq.df.20$streamStage) # 0.22m
mean(cari.manq.df.19$streamStage)  # 0.3m
mean(cari.manq.df.21$streamStage) # 0.31m
mean(king.manq.df.19$streamStage, na.rm=TRUE) #0.53m
mean(king.manq.df.20$streamStage) #0.62m
mean(walk.manq.df.19$streamStage) #0.13m
mean(walk.manq.df.22$streamStage) #0.13m

