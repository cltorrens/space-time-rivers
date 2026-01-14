#WLOU metabolism

library(tidyverse)
library(lubridate)
library(here)
library(streamMetabolizer)


# load("/Users/bob.hall/Dropbox/Docs - Christa Torrens/neon_metab/")
# load("")

## Function

bpcalc_atm<- function(bpst, alt) {
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}


## load data
aho_k<- read.csv(here("N_uptake_NEON/neon_metab_bob/gasExchange_ds_v2.csv"))

# ## load WLOU do data
# wlou_do <- read.csv(here("N_uptake_NEON/neon_metab_bob/wlou_do.csv")) %>%
#   mutate(solar.time = ymd_hms(solar.time))
# 
# ## select WLOU aho_k
# wlou<-aho_k[aho_k$siteID=="WLOU", ]
# 
# plot(wlou_do$solar.time,wlou_do$DO.obs)
# 
# wlou_do$time<- wlou_do$solar.time
# 
# bpcalc_atm(bpst=1013, alt=2902) #718 mbar
# wlou_do$osat<- calc_DO_sat(wlou_do$temp.water, 718 )  ##mbar!
# 
# plot(wlou_do$time,wlou_do$DO.obs)
# 
# wlou_do_june<- wlou_do[wlou_do$time > ymd_hms("2022-06-01 04:00:00") & wlou_do$time < ymd_hms("2022-07-10  04:00:00"), ]
# wlou_do_june$persat<- 100*wlou_do_june$DO.obs/wlou_do_june$osat
# plot(wlou_do_june$time, 100*wlou_do_june$DO.obs/wlou_do_june$osat)
#  


####CARI
#65.153224, -147.50397
cari_do <- read.csv(here("N_uptake_NEON/neon_metab_bob/cari_do.csv")) %>%
  mutate(solar.time = ymd_hms(solar.time))

cari<-aho_k[aho_k$siteID=="CARI", ]

###there is a duplicate time stamp at row 9369.  Delete it (still there!)
cari_do[9366:9372, ] # checking

cari_do<- cari_do[-9369,] # deleting - but now there is an incomplete day somewhere... 


#luncomment the below to see model fits for just 10 days or so
#cari_do<-cari_do[1:1000,]


# calc barometric pressure (bp) using standard bp and site elevation
bpcalc_atm(bpst=1013, alt=240)  # 984.5823

# create data for StreamMetabolizer
cari_do$time<- ymd_hms(cari_do$solar.time)
cari_do$osat<- calc_DO_sat(cari_do$temp.water, 984.5 )  ##mbar!
cari_do$light <- calc_light(cari_do$time, latitude=65.15, longitude = -147.5)
cari_do$depth<-0.5*cari_do$discharge^0.49  ##from Aho paper


plot(cari_do$time,cari_do$DO.obs)

dailyK_cari <- cari_do %>%
  mutate(date = as.Date(time)) %>%
  group_by(date) %>%
  summarize(
    discharge = mean(discharge, na.rm = TRUE))%>%
  mutate(K600.daily = exp(6.5 - 0.32 * log(discharge*86400)))%>%
  select(-discharge)


# MLE
#mle_name_Q <- mm_name(type='mle', err_proc_iid = T)
mle_proc_err<-"m_np_pi_tr_plrckm.nlm"
mle_specs_Q <- specs(mle_proc_err)
cari_met_data<-data.frame(DO.obs=cari_do$DO.obs, DO.sat=cari_do$osat, 
                           temp.water=cari_do$temp.water, depth=cari_do$depth, 
                           light=cari_do$light, solar.time=cari_do$time)

cari_out<- metab(specs = mle_specs_Q, data=cari_met_data, data_daily = dailyK_cari, info = NULL) # run model

cari_params<-get_params(cari_out)
plot_DO_preds(predict_DO(cari_out))
plot_metab_preds(predict_metab(cari_out))

###CUPE
#18.11352, -66.98676
cupe_do<- read.csv(here("N_uptake_NEON/neon_metab_bob/cupe_do.csv"))

cupe<-aho_k[aho_k$siteID=="CUPE", ]


#cupe_do<-cupe_do[1:1000,]

bpcalc_atm(bpst=1013, alt=170) # 992.7874


cupe_do$time<- ymd_hms(cupe_do$solar.time)
cupe_do$osat<- calc_DO_sat(cupe_do$temp.water, 992 )  ##mbar!
cupe_do$light <- calc_light(cupe_do$time, latitude=18.11, longitude = -66.98)
cupe_do$depth<-0.32*cupe_do$discharge^0.55  ##from Aho paper


plot(cupe_do$time,cupe_do$DO.obs)

dailyK_cupe <- cupe_do %>%
  mutate(date = as.Date(time)) %>%
  group_by(date) %>%
  summarize(
    discharge = mean(discharge, na.rm = TRUE))%>%
  mutate(K600.daily = exp(4.84 - 0.03 * log(discharge*86400)))%>%
  select(-discharge)


# MLE
#mle_name_Q <- mm_name(type='mle', err_proc_iid = T)
mle_proc_err<-"m_np_pi_tr_plrckm.nlm"
mle_specs_Q <- specs(mle_proc_err)
cupe_met_data<-data.frame(DO.obs=cupe_do$DO.obs, DO.sat=cupe_do$osat, 
                          temp.water=cupe_do$temp.water, depth=cupe_do$depth, 
                          light=cupe_do$light, solar.time=cupe_do$time)

cupe_out<- metab(specs = mle_specs_Q, data=cupe_met_data, data_daily = dailyK_cupe, info = NULL) # run model

cupe_params<-get_params(cupe_out)
plot_DO_preds(predict_DO(cupe_out))
plot_metab_preds(predict_metab(cupe_out))



#####


###WLOU
#39.891366 -105.915395
wlou_do <- read.csv(here("N_uptake_NEON/neon_metab_bob/wlou_do.csv")) %>%
  mutate(solar.time = ymd_hms(solar.time))

###there is a duplicate time stamp at row 9381.  Delete it
wlou_do<- wlou_do[-9381,]

## select WLOU aho_k
wlou<-aho_k[aho_k$siteID=="WLOU", ]


bpcalc_atm(bpst=1013, alt=2902) # 718.1046



wlou_do$time<- ymd_hms(wlou_do$solar.time)
wlou_do$osat<- calc_DO_sat(wlou_do$temp.water, bpcalc_atm(bpst=1013, alt=2902) )  ##mbar!
wlou_do$light <- calc_light(wlou_do$time, latitude=39.89, longitude = -105.9)
wlou_do$depth<-0.35*wlou_do$discharge^0.32  ##from Aho paper


plot(wlou_do$time,wlou_do$DO.obs)

dailyK_wlou <- wlou_do %>%
  mutate(date = as.Date(time)) %>%
  group_by(date) %>%
  summarize(
    discharge = mean(discharge, na.rm = TRUE))%>%
  mutate(K600.daily = exp(2.48 + 0.33 * log(discharge*86400)))%>%
  select(-discharge)


# MLE
#mle_name_Q <- mm_name(type='mle', err_proc_iid = T)
mle_proc_err<-"m_np_pi_tr_plrckm.nlm"
mle_specs_Q <- specs(mle_proc_err)
wlou_met_data<-data.frame(DO.obs=wlou_do$DO.obs, DO.sat=wlou_do$osat, 
                          temp.water=wlou_do$temp.water, depth=wlou_do$depth, 
                          light=wlou_do$light, solar.time=wlou_do$time)

wlou_out<- metab(specs = mle_specs_Q, data=wlou_met_data, data_daily = dailyK_wlou, info = NULL) # run model

wlou_params<-get_params(wlou_out)
plot_DO_preds(predict_DO(wlou_out))
plot_metab_preds(predict_metab(wlou_out))





#########
#exploratory code, ignore


####depth

plot(wlou$meanQ_lps, wlou$meanDepth_m)
plot(wlou$meanQ_lps, wlou$meanDepth_m, log='xy')

summary( lm( log(wlou$meanDepth_m)~ log(wlou$meanQ_lps/100000)))

0.35- (-1.09)

##depth intercept 1.44 off.  exponetiate, 4.22.  Mistake pervades. 

###


plot(wlou$meanQ_lps, wlou$best_k600_mPerDay, log="xy")
plot(wlou$meanQ_lps, wlou$best_K600_perDay, log="xy")

logQ<- log(wlou$meanQ_lps)
logk<-log(wlou$best_k600_mPerDay)


summary(lm(log(wlou$best_k600_mPerDay)~log(wlou$meanQ_lps*86400/1000)))
0.7- 2.15


summary(lm(log(wlou$best_K600_perDay)~log(wlou$meanQ_lps*86400/1000)))

exp(2.43+log(20)*0.33)
exp(2.43)*20^0.33

###Kelly's intercepts are 1.45 low, which is 4.26 (4.31) low What mistake throws a 4.26 error?

####FOund it, she used units of Q that are in m3/d.  Ugh.

calc_k<- wlou$meanDepth_m* wlou$best_K600_perDay

plot(calc_k, wlou$best_k600_mPerDay)
#### k to K conversion is fine,  Whew.


####Now O2