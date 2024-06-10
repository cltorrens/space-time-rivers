
library(dplyr)
library(readr)
library(lubridate)
library(streamMetabolizer)
library(deSolve)
library(here) 
library(zoo)


####function returns mm of Hg
bpcalc<- function(bpst, alt) {
  bpst*25.4*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}
#end of function
osat<- function(temp, bp) {
  
  tstd<-log((298.15-temp) / (273.15 + temp))
  
  a0<-2.00907
  a1<-3.22014
  a2<-4.0501
  a3<-4.94457
  a4<- -0.256847
  a5<- 3.88767
  
  u<-10^(8.10765-(1750.286/(235+temp)))
  
  sato<-(exp(a0 + a1*tstd + a2*tstd^2 + a3*tstd^3 + a4*tstd^4+a5*tstd^5))*((bp-u)/(760-u))*1.42905
  sato
}

# calculate mmhg at Nyack
mmhg <- bpcalc(29.9, 3350/3.28) # bp in mmhg using elevation at Nyack

#bpcalc(30, 2900/3.28) #686.1688 

#all_oxy<-read.csv("./Data//miniDOT-data_Processed/2023_minidots_processed_fewer_rows.csv", fileEncoding = 'UTF-8-BOM')
all_oxy <- read_csv(here("field_data_workup/data/data_DO/Cat_659494_2023_11_01_US.csv"))

# check for complete dataset
even_spacing <- all_oxy %>% 
  mutate(diff = MST_datetime - lag(MST_datetime)) %>%
  filter(!is.na(diff)) %>%
  summarize(is_even = all(diff == first(diff)))

# gaps exist - ID where

expected_interval <- minutes(5)

# Identify gaps in the time series
gaps <- all_oxy %>%
  mutate(
    diff = MST_datetime - lag(MST_datetime),
    row_number = row_number()
  ) %>%
  filter(!is.na(diff)) %>%
  filter(diff > expected_interval) %>%
  mutate(
    previous_timestamp = lag(MST_datetime, default = first(MST_datetime)),
    gap_start = lag(row_number),
    gap_end = row_number
  ) %>%
  select(gap_start, gap_end, previous_timestamp, MST_datetime, diff)

# Display the gaps
print(gaps)

# gap_start gap_end previous_timestamp  MST_datetime        diff   
#       <int>   <int> <dttm>              <dttm>              <drtn> 
# 1        NA   22452 2023-07-08 12:31:00 2023-07-08 12:31:00 12 mins
# 2     22452   27081 2023-07-08 12:31:00 2023-07-24 14:24:00 13 mins
# 3     27081   35960 2023-07-24 14:24:00 2023-08-24 10:27:00 13 mins
# 4     35960   43493 2023-08-24 10:27:00 2023-09-19 14:32:00 25 mins
# 5     43493   45495 2023-09-19 14:32:00 2023-09-26 13:31:00 14 mins
# 6     45495   48408 2023-09-26 13:31:00 2023-10-06 16:24:00 13 mins
# 7     48408   52692 2023-10-06 16:24:00 2023-10-21 13:42:00 23 mins


all_oxy$time<-as_datetime(all_oxy$Unix_Timestamp_sec)

all_oxy$solar.time<-convert_UTC_to_solartime(all_oxy$time, longitude= -113, time.type="mean solar")


all_oxy$light<- calc_light(all_oxy$solar.time, latitude=48.5, longitude=-113, max.PAR =2326, attach.units = F)

all_oxy$osat<- osat(all_oxy$Temperature_degC, mmhg)


###################  Filter and clean data

################### August -------------------------------------------------------------
all_oxy_aug23 <- all_oxy %>%
  mutate(jday = yday(MST_datetime), 
         model_datetime = MST_datetime-hours(4), 
         model_jday = yday(model_datetime)) %>%
  filter(model_jday >= 230 & model_jday <= 251)  # 18 August - 08 September

even_spacing <- all_oxy_aug23 %>% 
  mutate(diff = MST_datetime - lag(MST_datetime)) %>%
  filter(!is.na(diff)) %>%
  summarize(is_even = all(diff == first(diff)))

# Nope, 13-min gap on 2023-08-24, 10:14:00-10:27:00, row 35960

write_csv(all_oxy_aug23, here("field_data_workup/data/data_DO/all_oxy_aug23.csv"))
### Visualize August dataset
quartz()
aug2023 <- all_oxy_aug23 %>%
  #filter(jday == 236) %>%  # use to view outliers and wobbles
  ggplot(aes(x=MST_datetime, y=DO_mgl)) + 
  geom_point() + 
  labs( x="Date", y=expression("DO"~(mg~L^-1))) + 
  ggtitle("Beaver Creek DO - August 2023") + 
  theme_bw()

aug2023

which(is.na(all_oxy_aug23$DO_mgl)) # 0 NAs
which(all_oxy_aug23$DO_mgl < 5)  # row 4838 = outlier

jday241 <- all_oxy_aug23 %>% # Aug 29
  filter(jday == 241) 

which(jday241$DO_mgl > 8.5)  #173 196 200 201 202 203 204 205 206

jday247 <- all_oxy_aug23 %>% # Sept 4
  filter(jday >= 246 & jday <=247) 

which(jday247$DO_mgl < 6.45) # 280 281 291 292


# Manually NA the bad data, bc not sure how to do it on here... then reload and fill the gaps
all_oxy_aug23 <- read_csv(here("field_data_workup/data/data_DO/all_oxy_aug23_gap.csv")) 
# datetime tzs are all set to "UTC"...

all_oxy_aug23$DO_mgl <- na.spline(all_oxy_aug23$DO_mgl)

which(is.na(all_oxy_aug23$DO_mgl))

write_csv(all_oxy_aug23, here("field_data_workup/data/data_DO/all_oxy_aug23_filled.csv"))

################# October-----------------------------------------------------

all_oxy_oct23 <- all_oxy %>%
  mutate(jday = yday(MST_datetime), 
         model_datetime = MST_datetime-hours(4), 
         model_jday = yday(model_datetime)) %>%
  filter(model_jday >= 270 & model_jday <= 284) # 27 September - 11 October

which(is.na(all_oxy_oct23$DO_mgl)) # none

even_spacing <- all_oxy_oct23 %>% 
  mutate(diff = MST_datetime - lag(MST_datetime)) %>%
  filter(!is.na(diff)) %>%
  summarize(is_even = all(diff == first(diff)))

# Nope, 13-min gap on 2023-10-06, 16:11:00-16:24:00 starts at row [48408,] ==> see above
# Also large wobbles on sept 28-29 (271-72); oct 1-2 (274-75); oct 5-6 (278-79)

quartz()
oct2023 <- all_oxy_oct23 %>%
  #filter(jday >= 278 & jday <= 279) %>%  # use to view outliers and wobbles
  #filter(jday == 274) %>%
  ggplot(aes(x=MST_datetime, y=DO_mgl)) + 
  geom_point() + 
  labs( x="Date", y=expression("DO"~(mg~L^-1))) + 
  ggtitle("Beaver Creek DO - October 2023") + 
  theme_bw()

oct2023

write_csv(all_oxy_oct23, here("field_data_workup/data/data_DO/all_oxy_oct23_gap.csv"))
# manual munging
all_oxy_oct23 <- read_csv( here("field_data_workup/data/data_DO/all_oxy_oct23_gap.csv"))
all_oxy_oct23$DO_mgl <- na.spline(all_oxy_oct23$DO_mgl)

write_csv(all_oxy_oct23, here("field_data_workup/data/data_DO/all_oxy_oct23_filled.csv"))

################################  VISUALIZE DO DATA #################################
all_oxy_bc <- bind_rows(all_oxy_aug23, all_oxy_oct23)

DOsatplot <- all_oxy_bc %>%
  filter(model_jday >= 239 & model_jday <= 241) %>%
  #mutate(season = ifelse(mod_day<260, "summer", "autumn")) %>%
  ggplot(aes(x=MST_datetime, y=DO_sat_perc)) + 
    geom_point() +
    labs(x="Date", y="DO (% sat)") + 
    theme_bw()

quartz()
DOsatplot


################################  MODEL GPP  ########################################

mf_name <- mm_name(type='bayes', pool_K600='normal', err_obs_iid = T, err_proc_iid =T)
mf_specs <- specs(mf_name, K600_daily_meanlog_meanlog=2, K600_daily_meanlog_sdlog=0.7, K600_daily_sdlog_sigma=0.1, burnin_steps=1000, 
                  saved_steps=1000)


z = 0.3 # estimation / to match N model\

##################### August days - Beaver Cr. 

augBC_data_sm<-data.frame(DO.obs=all_oxy_aug23$DO_mgl, DO.sat=all_oxy_aug23$osat, 
                          temp.water=all_oxy_aug23$Temperature_degC, depth=rep(z,length(all_oxy_aug23$Temperature_degC)), 
                          light=all_oxy_aug23$light, solar.time=all_oxy_aug23$solar.time)


augBC_fit <- metab(mf_specs, data=augBC_data_sm, info=c(site='BC Aug 2023', source='CT'))
save(augBC_fit, file=here("field_data_workup/GPP/augBC_fit.RData"))

quartz()
plot_DO_preds(predict_DO(augBC_fit))
plot_metab_preds(predict_metab(augBC_fit))
params_aug23 <- get_params(augBC_fit, uncertainty='ci')

# Checking relationship between K and ER - linear = equifinality
quartz()
ggplot(data = params_aug_v1, aes(x=K600.daily, y=ER.daily)) + 
  geom_point() +
  labs(x= "daily K600", y= "daily ER") + 
  ggtitle("Beaver Creek - August ER vs K600") +
  theme_bw()

er.lm <- lm(params_aug_v1$K600.daily~ params_aug_v1$ER.daily)
summary(er.lm)  # R^2 ER-K600 = 0.97


# Also between K and GPP
quartz()
ggplot(data = params_aug_v1, aes(x=K600.daily, y=GPP.daily)) + 
  geom_point() +
  labs(x= "daily K600", y= "daily GPP") + 
  ggtitle("Beaver Creek - August GPP vs K600") +
  ylim(0, 3) + 
  theme_bw()

gpp.lm <- lm(params_aug_v1$K600.daily~ params_aug_v1$GPP.daily)
summary(gpp.lm) # R^2 GPP-K600 = 0.57

######################## October days - Beaver Cr -----------------------

octBC_data_sm<-data.frame(DO.obs=all_oxy_oct23$DO_mgl, DO.sat=all_oxy_oct23$osat, 
                          temp.water=all_oxy_oct23$Temperature_degC, depth=rep(z,length(all_oxy_oct23$Temperature_degC)), 
                          light=all_oxy_oct23$light, solar.time=all_oxy_oct23$solar.time)


octBC_fit <- metab(mf_specs, data=octBC_data_sm, info=c(site='BC Oct 2023', source='CT'))
save(octBC_fit, file=here("field_data_workup/GPP/octBC_fit.RData"))

quartz()
plot_DO_preds(predict_DO(octBC_fit))
plot_metab_preds(predict_metab(octBC_fit))
params_oct23 <- get_params(octBC_fit , uncertainty='ci')


# Checking relationship between K and ER - linear = equifinality
quartz()
ggplot(data = params_oct23, aes(x=K600.daily, y=ER.daily)) + 
  geom_point() +
  labs(x= "daily K600", y= "daily ER") + 
  ggtitle("Beaver Creek - October ER vs K600") +
  theme_bw()

er.lm <- lm(params_oct23$K600.daily~ params_oct23$ER.daily)
summary(er.lm)  # R^2 ER-K600 = 0.97


# Also between K and GPP
quartz()
ggplot(data = params_oct23, aes(x=K600.daily, y=GPP.daily)) + 
  geom_point() +
  labs(x= "daily K600", y= "daily GPP") + 
  ggtitle("Beaver Creek - October GPP vs K600") +
  ylim(0, 3) + 
  theme_bw()

gpp.lm <- lm(params_oct23$K600.daily~ params_oct23$GPP.daily)
summary(gpp.lm) # R^2 GPP-K600 = 0.57


params_all2023 <- bind_rows(params_aug23, params_oct23) %>%
  mutate(jday = yday(date))

write_csv(params_all2023, here("field_data_workup/data/data_DO/params_alloxy23.csv"))


##############  plot DO sat data vs model

print(octBC_fit)
octBC_fit
