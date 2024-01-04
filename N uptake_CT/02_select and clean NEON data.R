##### Select and clean NEON data for the pooled-L model


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
library(pracma) # for 1 type of light AUC calcs
library(httr) # helps w. the NSRDB light download

# load NO3 data for all 4 streams
load("~/Documents/R_working/Modelscape/space-time-rivers_git/NEON/data_derived/no3_dataset.Rdata")  #no3_data_sensor is the object name
View(no3_data_sensor)

# Filter by site ID  ("KING" "WALK" "BIGC" "CARI") and set appropriate time zone (default for NEON is UTC)
# grep("USA", OlsonNames(), value=TRUE)

#################################### BIGC: Big Creek, CA
bigc.df <- no3_data_sensor %>%
  filter(siteID == "BIGC") %>% 
  mutate(local_datetime = with_tz(startDateTime, tzone="America/Los_Angeles"), 
         Jday = yday(local_datetime))  #130-365

## check years to use for training
bigc.df %>%
  filter(year(local_datetime) == 2019) %>%
  ggplot(aes(x=local_datetime, y = surfWaterNitrateMean)) + 
  geom_point() + 
  theme_bw()

#### 2019 looks good: Filter to 39 days w/o NA

# Pull more days so we can eliminate NAs
bigc.df.19 <- bigc.df %>%
  # filter(local_datetime >= ymd_hms('20190621 040000', tz='America/Los_Angeles') & startDateTime <= ymd_hms('20190801 035500', tz='America/Los_Angeles')) 
  filter(local_datetime >= ymd_hms('20190315 040000', tz='America/Los_Angeles') & startDateTime <= ymd_hms('20190815 035500', tz='America/Los_Angeles')) 

bigc.df.19 %>%
  ggplot(aes(x=local_datetime, y = surfWaterNitrateMean)) + 
  geom_point() + 
  theme_bw()

## select hourly no3 measurements to match what we've done
bigc.df.19h <- bigc.df.19 %>%
  filter(minute(local_datetime) == 0) %>%
  select(siteID, local_datetime, Jday, startDateTime:surfWaterNitrateStdErMean) %>%
  mutate(N_mean_mgm3 = surfWaterNitrateMean * 14.0067) # new col w. N units = mg m^-3

plot(bigc.df.19h$local_datetime, bigc.df.19h$N_mean_mgm3, type = 'l')  # used 'l' for better visualization of diel patterns
#lines(bigc.df.19h$local_datetime, bigc.df.19h$surfWaterNitrateMean, type = 'l', col='blue')

N_b <- mean(bigc.df.19h$N_mean_mgm3, na.rm = TRUE) # background N: here, = 51.9 mg/L 



##### Eliminate days w NAs

################################# 39 day timeseries ####################
# Where are the NAs?
which(is.na(bigc.df.19h$N_mean_mgm3)) 
#273 274 275 276 277 278 279 280 281 282 283 284 285 286 287 288 289 290 291 292 293 294 295 296 297 298 299 300 659 660 661

# 295  607  608  609  968  969 1256 1257 1258 1259 1617 1618 2000 2001 2002 2003 2004 2223 2228 2232 2245 2247
# [23] 2248 2249 2257 2261 2262 2335 2336 2337 2338 2625 2626 2627 2628 2629 2630 2631 2632 2633 2634 2635 2636 2637
# [45] 2638 2639 2640 2641 2642 2643 2644 2645 2646 2647 2648 2649 2650 2651 2652 3011 3012 3013 3345 3346 3347 3348
# [67] 3349 3637 3638 3655

#Double-check this: 
bigc.df.19h[273, 2:6] #local_dateime  2019-07-02 12:00:00 (19:00:00 UTC) Jday 183
bigc.df.19h[300, 2:6] #local_dateime 2019-07-03 15:00:00 (22:00:00 UTC) Jday 184
bigc.df.19h[659, 2:6] #local_dateime 2019-07-18 14:00:00 (21:00:00 UTC) Jday 199

#### for smaller NA chunks, e.g. 659-661 , we will interpolate using zoo()
# first see where in the curve this gap falls: (now 611:613)
plot(bigc.df.19h$local_datetime[605:620], bigc.df.19h$N_mean_mgm3[605:620], type = 'l')  #OK, linear seems reasonable

#ggplot(bigc.df.19h, )

bigc.df.19h$N_mean_mgm3 <- na.approx(bigc.df.19h$N_mean_mgm3, maxgap = 6)  # maxgap = max # of NAs to fill
bigc.df.19h$surfWaterNitrateMean <- na.approx(bigc.df.19h$surfWaterNitrateMean, maxgap = 6)


#### For large NA chunks (eg 273-300), we will remove the entire day.
# Our model day starts at 4a, so need to create additional columns to easily excise days w NAs
bigc.df.19h <- bigc.df.19h %>%
  mutate(model_datetime = local_datetime - hours(4), 
         model_day = yday(model_datetime))


bigc.df.19h$model_day[273] #183
bigc.df.19h$model_day[300] #184

# delete model_day == 183:184
bigc.df.19h <- bigc.df.19h[!(bigc.df.19h$model_day %in% c(183, 184)),]

# check that all NAs are removed
which(is.na(bigc.df.19h$N_mean_mgm3)) 


################################# Longer BIGC timeseries

# Where are the NAs?
which(is.na(bigc.df.19h$N_mean_mgm3)) 

# 295  607  608  609  968  969 1256 1257 1258 1259 1617 1618 2000 2001 2002 2003 2004 2223 2228 2232 2245 2247
# [23] 2248 2249 2257 2261 2262 2335 2336 2337 2338 2625 2626 2627 2628 2629 2630 2631 2632 2633 2634 2635 2636 2637
# [45] 2638 2639 2640 2641 2642 2643 2644 2645 2646 2647 2648 2649 2650 2651 2652 3011 3012 3013 3345 3346 3347 3348
# [67] 3349 3637 3638 3655

bigc.df.19h[295, 2:6] #local_dateime  2019-03-27 10:00:00  Jday 86
bigc.df.19h[607:609, 2:6] #local_dateime 2019-04-09 10:00:00 to 12:00:00, Jday 99
bigc.df.19h[968:969, 2:6] #local_dateime 2019-07-18 14:00:00 (21:00:00 UTC) Jday 199
bigc.df.19h[1256:1259, 2:6]
bigc.df.19h[1617:1618, 2:6]
bigc.df.19h[2000:2004, 2:6]
bigc.df.19h[2223, 2:6]
bigc.df.19h[2228, 2:6]
bigc.df.19h[2232, 2:6]
bigc.df.19h[2245, 2:6]
bigc.df.19h[2247:2249, 2:6]
bigc.df.19h[2257, 2:6]
bigc.df.19h[2261:2262, 2:6]
bigc.df.19h[2335:2338, 2:6]
bigc.df.19h[2625:2652, 2:6] # BIG
bigc.df.19h[3011:3013, 2:6]
bigc.df.19h[3345:3349, 2:6]
bigc.df.19h[3637:3638, 2:6]
bigc.df.19h[3655, 2:6]

###### Small gaps

# Fill small gaps (here, <= 6 NAs) using zoo::na.approx
bigc.df.19h$N_mean_mgm3 <- na.approx(bigc.df.19h$N_mean_mgm3, maxgap = 6)  # maxgap = max # of NAs to fill
bigc.df.19h$surfWaterNitrateMean <- na.approx(bigc.df.19h$surfWaterNitrateMean, maxgap = 6)

###### Large gaps

# check Jdays 
bigc.df.19h[2625, 2:6] # start --> local datetime 2019-07-02 12:00:00, Jday 183
bigc.df.19h[2652, 2:6] # end --> local datetime 2019-07-03 15:00:00, Jday 184

# shift so each model day starts at midnight and full Jdays can be easily removed
bigc.df.19h <- bigc.df.19h %>%
  mutate(model_datetime = local_datetime - hours(4), 
         model_day = yday(model_datetime))

# remove model days with big gaps
bigc.df.19h <- bigc.df.19h[!(bigc.df.19h$model_day %in% c(183, 184)),]

# re-check NAs
which(is.na(bigc.df.19h$N_mean_mgm3)) # None left!

min(bigc.df.19h$model_day) # 74
max(bigc.df.19h$model_day) # 226

## Add this in to the pooled-L NEON data model 
