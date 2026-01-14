#WLOU metabolism

library(tidyverse)
library(lubridate)
library(streamMetabolizer)
#load("/Users/bob.hall/Dropbox/Docs - Christa Torrens/neon_metab/")
#load("")


aho_k<- read.csv("/Users/bob.hall/Dropbox/Docs - Christa Torrens/neon_metab/gasExchange_ds_v2.csv")

wlou_do<- read.csv("/Users/bob.hall/Dropbox/Docs - Christa Torrens/neon_metab/wlou_do.csv")




wlou<-aho_k[aho_k$siteID=="WLOU", ]



bpcalc_atm<- function(bpst, alt) {
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}
bpcalc_atm(bpst=1013, alt=2902)



wlou_do$time<- ymd_hms(wlou_do$solar.time)
wlou_do$osat<- calc_DO_sat(wlou_do$temp.water, 718 )  ##mbar!


plot(wlou_do$time,wlou_do$DO.obs)

wlou_do_june<- wlou_do[wlou_do$time > ymd_hms("2022-06-01 04:00:00") & wlou_do$time < ymd_hms("2022-07-10  04:00:00"), ]
wlou_do_june$persat<- 100*wlou_do_june$DO.obs/wlou_do_june$osat
plot(wlou_do_june$time, 100*wlou_do_june$DO.obs/wlou_do_june$osat)
 


####CARI
#65.153224, -147.50397
cari_do<- read.csv("/Users/bob.hall/Dropbox/Docs - Christa Torrens/neon_metab/cari_do.csv")

cari<-aho_k[aho_k$siteID=="CARI", ]

###there is a duplicate time stamp at row 9369.  Delete it
cari_do<- cari_do[-9369,]

#luncomment the below to see model fits for just 10 days or so
#cari_do<-cari_do[1:1000,]


bpcalc_atm<- function(bpst, alt) {
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}
bpcalc_atm(bpst=1013, alt=240)


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

cari_params$site<- "cari"


###CUPE
#18.11352, -66.98676
cupe_do<- read.csv("/Users/bob.hall/Dropbox/Docs - Christa Torrens/neon_metab/cupe_do.csv")

cupe<-aho_k[aho_k$siteID=="CUPE", ]


#cupe_do<-cupe_do[1:1000,]


bpcalc_atm<- function(bpst, alt) {
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}
bpcalc_atm(bpst=1013, alt=170)


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

cupe_params$site<- "cupe"

#####


###WLOU
#39.891366 -105.915395
wlou_do<- read.csv("/Users/bob.hall/Dropbox/Docs - Christa Torrens/neon_metab/wlou_do.csv")

###there is a duplicate time stamp at row 9381.  Delete it
wlou_do<- wlou_do[-9381,]

bpcalc_atm<- function(bpst, alt) {
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}
bpcalc_atm(bpst=1013, alt=2902)



wlou_do$time<- ymd_hms(wlou_do$solar.time)
wlou_do$osat<- calc_DO_sat(wlou_do$temp.water, bpcalc_atm(bpst=1013, alt=2902) )  ##mbar!
wlou_do$light <- calc_light(wlou_do$time, latitude=39.89, longitude = -105.9)
wlou_do$depth<-0.35*wlou_do$discharge^0.32  ##from Aho paper


head(wlou_)

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

wlou_params$site<- "wlou"


###SYCA
#33.750993, -111.50809
syca_do<- read.csv("/Users/bob.hall/Dropbox/Docs - Christa Torrens/neon_metab/syca_do.csv")
syca_do<- syca_do[!is.na(syca_do$DO.obs), ]



bpcalc_atm<- function(bpst, alt) {
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}
bpcalc_atm(bpst=1013, alt=648)
##938

head(syca_do)

syca_do$time<- ymd_hms(syca_do$solar.time)
syca_do$osat<- calc_DO_sat(syca_do$temp.water, bpcalc_atm(bpst=1013, alt=938) )  ##mbar!
syca_do$light <- calc_light(syca_do$time, latitude=33.75, longitude = -111.5081)
syca_do$depth<-0.22*syca_do$discharge_cms^0.36  ##from Aho paper


syca_do<- syca_do[syca_do$discharge_lps >1, ]

hist(log10(syca_do$discharge_lps))
hist((syca_do$discharge_lps))


####something awry with Q data

plot(syca_do$time,syca_do$DO.obs)

dailyK_syca <- syca_do %>%
  mutate(date = as.Date(time)) %>%
  group_by(date) %>%
  summarize(
    discharge_cms = mean(discharge_cms, na.rm = TRUE))%>%
  mutate(K600.daily = exp(5.15 - 0.25 * log(discharge_cms*86400)))
 
  
  dailyK_syca <- dailyK_syca[,-2]

hist(dailyK_syca$K600.daily, na.rm=T)

# MLE
#mle_name_Q <- mm_name(type='mle', err_proc_iid = T)
mle_proc_err<-"m_np_pi_tr_plrckm.nlm"
mle_specs_Q <- specs(mle_proc_err)
syca_met_data<-data.frame(DO.obs=syca_do$DO.obs, DO.sat=syca_do$osat, 
                          temp.water=syca_do$temp.water, depth=syca_do$depth, 
                          light=syca_do$light, solar.time=syca_do$time)

syca_out<- metab(specs = mle_specs_Q, data=syca_met_data, data_daily = dailyK_syca, info = NULL) # run model

syca_params<-get_params(syca_out)
plot_DO_preds(predict_DO(syca_out))
plot_metab_preds(predict_metab(syca_out))

syca_params$site<- "syca"

#################


###BIGC
#37.059719, -119.257549
bigc_do<- read.csv("/Users/bob.hall/Dropbox/Docs - Christa Torrens/neon_metab/bigc_do.csv")
bigc_do<- bigc_do[!is.na(bigc_do$DO.obs), ]




bpcalc_atm<- function(bpst, alt) {
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}
bpcalc_atm(bpst=1013, alt=1139)
##885

head(bigc_do)

bigc_do$time<- ymd_hms(bigc_do$solar.time)
bigc_do$osat<- calc_DO_sat(bigc_do$temp.water, bpcalc_atm(bpst=1013, alt=885) )  ##mbar!
bigc_do$light <- calc_light(bigc_do$time, latitude=37.0597, longitude = -119.257)
bigc_do$depth<-0.30*bigc_do$discharge_cms^0.27  ##from Aho paper


hist(log10(bigc_do$discharge_lps))



plot(bigc_do$time,bigc_do$DO.obs)

dailyK_bigc <- bigc_do %>%
  mutate(date = as.Date(time)) %>%
  group_by(date) %>%
  summarize(
    discharge_cms = mean(discharge_cms, na.rm = TRUE))%>%
  mutate(K600.daily = exp(3.55 - 0.10 * log(discharge_cms*86400)))
#select(-discharge_cms)
  
  dailyK_bigc <- dailyK_bigc[,-2]

###2 wonky Q day, interpolate
dailyK_bigc$K600.daily[71]<- 21
dailyK_bigc$K600.daily[73]<- 21

hist(dailyK_bigc$K600.daily, na.rm=T)

# MLE
#mle_name_Q <- mm_name(type='mle', err_proc_iid = T)
mle_proc_err<-"m_np_pi_tr_plrckm.nlm"
mle_specs_Q <- specs(mle_proc_err)
bigc_met_data<-data.frame(DO.obs=bigc_do$DO.obs, DO.sat=bigc_do$osat, 
                          temp.water=bigc_do$temp.water, depth=bigc_do$depth, 
                          light=bigc_do$light, solar.time=bigc_do$time)

bigc_out<- metab(specs = mle_specs_Q, data=bigc_met_data, data_daily = dailyK_bigc, info = NULL) # run model

bigc_params<-get_params(bigc_out)

plot_DO_preds(predict_DO(bigc_out))
plot_metab_preds(predict_metab(bigc_out))

bigc_params$site<- "bigc"


#################


###PRIN
#33.378517, -97.782312
prin_do<- read.csv("/Users/bob.hall/Dropbox/Docs - Christa Torrens/neon_metab/prin_do.csv")
prin_do<- prin_do[!is.na(prin_do$DO.obs), ]




bpcalc_atm<- function(bpst, alt) {
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}
bpcalc_atm(bpst=1013, alt=255)
##982

head(prin_do)

prin_do$time<- ymd_hms(prin_do$solar.time)
prin_do$osat<- calc_DO_sat(prin_do$temp.water, bpcalc_atm(bpst=1013, alt=982) )  ##mbar!
prin_do$light <- calc_light(prin_do$time, latitude=33.378, longitude = -97.782)
prin_do$depth<-0.33*prin_do$discharge_cms^0.31  ##from Aho paper


hist(log10(prin_do$discharge_lps))



plot(prin_do$time,prin_do$DO.obs)

dailyK_prin <- prin_do %>%
  mutate(date = as.Date(time)) %>%
  group_by(date) %>%
  summarize(
    discharge_cms = mean(discharge_cms, na.rm = TRUE))%>%
  mutate(K600.daily = exp(2.37 - 0.05 * log(discharge_cms*86400)))


plot(dailyK_prin$discharge_cms,dailyK_prin$K600.daily)
dailyK_prin <- dailyK_prin[,-2]

plo

hist(dailyK_prin$K600.daily, na.rm=T)

# MLE
#mle_name_Q <- mm_name(type='mle', err_obs_iid = F, err_proc_iid = T)
mle_proc_err<-"m_np_pi_tr_plrckm.nlm"
mle_obs_err<-"m_np_oi_tr_plrckm.nlm"
mle_specs_Q <- specs(mle_proc_err)
mle_specs_Q_obs <- specs(mle_obs_err)
prin_met_data<-data.frame(DO.obs=prin_do$DO.obs, DO.sat=prin_do$osat, 
                          temp.water=prin_do$temp.water, depth=prin_do$depth, 
                          light=prin_do$light, solar.time=prin_do$time)

prin_out<- metab(specs = mle_specs_Q, data=prin_met_data, data_daily = dailyK_prin, info = NULL) # run model
prin_out_obs<- metab(specs = mle_specs_Q_obs, data=prin_met_data, data_daily = dailyK_prin, info = NULL) # run model


prin_params<-get_params(prin_out)
plot_DO_preds(predict_DO(prin_out))
plot_metab_preds(predict_metab(prin_out))

prin_params$site<- "prin"

obs_params<-get_params(prin_out_obs)
proc_params<- prin_params

plot(obs_params$GPP.daily, proc_params$GPP.daily)
lines(obs_params$GPP.daily,obs_params$GPP.daily)

plot(obs_params$ER.daily, proc_params$ER.daily)
lines(obs_params$ER.daily,obs_params$ER.daily)

######
#make one big datafram

neon_metab<- rbind(cari_params, cupe_params,wlou_params,bigc_params,syca_params,prin_params)
write.csv(neon_metab, file="/Users/bob.hall/Dropbox/Docs - Christa Torrens/neon_metab/neon_metab.csv")


###Trust but verify.  Use Hall and Hotchkiss mle code

## This function calculates KO2 at any given tempertaure from K600. via schmidt number scaling.  The scaling equation if From Jähne et al. (1987), Schmidt number conversions from Wanninkhof et al. 1992.
Kcor<-function (temp,K600) {
  K600/(600/(1800.6-(temp*120.1)+(3.7818*temp^2)-(0.047608*temp^3)))^-0.5
}

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

## Here is same function but for fixed and known K600 from propane, SF6, nighttime regression, or other method
rivermetab<-function(o2file, z, bp, ts, K){
  
  ##pull data out of loaded o2file (subset in CALL function) and give it a name. 
  temp<-o2file$temp
  oxy<-o2file$oxy
  light<-o2file$light
  
  ##calculate metabolism by non linear minimization of MLE function (below)
  river.mle<-nlm(onestationmle, p=c(3,-5), oxy=oxy, z=z,temp=temp,light=light, bp=bp, ts=ts, K=K)
  
  ##plot data; uses same plot function as given for rivermetabK above (relisted below to keep each model as own unit)
  onestationplot(GPP=river.mle$estimate[1], ER=river.mle$estimate[2],oxy=oxy,z=z,temp=temp,light=light, K=K, bp=bp, ts=ts)
  
  ##return GPP, ER, and MLE value
  b<-list(GPP= river.mle$estimate[1], ER= river.mle$estimate[2],  neglogL= river.mle$minimum[1])
  b
}
# end of function

# function returns the likelihood value given O2 data and estimates of GPP, ER (which is vector MET); is included in master rivermetab function above; K600 is fixed
onestationmle<-function(MET,temp, oxy, light, z, bp, ts, K) {
  
  oxy.mod<-numeric(length(data))
  oxy.mod[1]<-oxy[1]
  
  # this is the metabolism equation as in Van de Bogert et al 2007 L&OMethods
  for (i in 2:length(oxy)) {oxy.mod[i]<-oxy.mod[i-1]+((MET[1]/z)*(light[i]/sum(light)))+ MET[2]*ts/z+(Kcor(temp[i],K))*ts*(osat(temp[i],bp)-oxy.mod[i-1]) }
  
  ## below is MLE calculation; output is likelihood
  # diff in measured and modeled O2
  sqdiff<-(oxy-oxy.mod)^2 
  # likelihood function
  L <- length(oxy)*(log(((sum(sqdiff)/length(oxy))^0.5)) +0.5*log(6.28)) + ((2*sum(sqdiff)/length(oxy))^-1)*sum(sqdiff)
  L
}
#end of function

# (As in rivermetabK)
# this function plots modeled O2 and O2 data from estimates of daily GPP and ER; is included in master rivermetab function above
# Calls same metabolism equation as in mle function, but plots modeled O2 as a function of GPP, ER estimates from mle
# use this to visually assess your model estimates (should be good agreement between modeled and measured O2)
onestationplot<-function(GPP, ER, oxy, z, temp, K, light, bp, ts) {
  
  oxy.mod<-numeric(length(oxy))
  oxy.mod[1]<-oxy[1]
  
  # this is the metabolism equation as in Van de Bogert et al (2007) L&OMethods
  for (i in 2:length(oxy)) { oxy.mod[i]<-oxy.mod[i-1]+((GPP/z)*(light[i]/sum(light)))+ ER*ts/z+(Kcor(temp[i],K))*ts*(osat(temp[i],bp)-oxy.mod[i-1]) }
  
  plot(seq(1:length(oxy)),oxy.mod, type="l",xlab="Time", ylab="Dissolved oxygen  (mg/L)", cex.lab=1.5, cex.axis=1.5, lwd=2 )
  points(seq(1:length(oxy)),oxy)
}
# end of function


####### END loading rivermetab function ####### 


prin21apr<-prin_met_data[4943:5038,]
prinO2file<-data.frame(oxy=prin21apr$DO.obs,  temp=prin21apr$temp.water, light=prin21apr$light)

rivermetab(o2file=prinO2file
           , z=0.1, bp=736.5, K=7.34, ts=1/96)



rivermetabK<-function(o2file, z, bp, ts){
  
  ##pull data out of loaded o2file (subset in CALL function) and give it a name. 
  temp<-o2file$temp
  oxy<-o2file$oxy
  light<-o2file$light
  
  ##This calls onestationmleK (below) to calculate metabolism by non-linear minimization. We use nlm() function in R stats package; for more information see "help(nlm)"  The first argument is the function to be minimized, the second defines the starting values.  The function that is minimized (onestationmleK, see below) always has as its first argument, a vector of parameters (MET), and second argument a vector of starting values for those parameters, p=c(3,-5, 10).
  
  river.mle<-nlm(onestationmleK, p=c(3,-5, 10), oxy=oxy, z=z,temp=temp,light=light, bp=bp, ts=ts)
  
  ##plot modeled and measaured O2 given MLE estimates of GPP, ER, and K600.  It calls a function below onestationplot()
  
  onestationplot(GPP=river.mle$estimate[1], ER=river.mle$estimate[2],oxy=oxy,z=z,temp=temp,light=light, K=river.mle$estimate[3], bp=bp, ts=ts)
  
  ##return GPP, ER, K600, and MLE values
  b <- list(GPP=river.mle$estimate[1], ER=river.mle$estimate[2], K600=river.mle$estimate[3], neglogL=river.mle$minimum[1])
  b
}
# end of function


# This function returns the negative log likelihood value given O2 data and estimates of GPP, ER, and K (which is vector MET); is included in master rivermetabK function above
onestationmleK<-function(MET,temp, oxy, light, z, bp, ts) {
  
  # create new vector for modeled O2
  oxy.mod<-numeric(length(data))
  # give starting value from oxygen data; this is the only time O2 data is used to model GPP and ER
  oxy.mod[1]<-oxy[1]
  
  # this is the metabolism equation as in Van de Bogert et al 2007 L&OMethods
  for (i in 2:length(oxy)) {oxy.mod[i]<-oxy.mod[i-1]+((MET[1]/z)*(light[i]/sum(light)))+ MET[2]*ts/z+(Kcor(temp[i],MET[3]))*ts*(osat(temp[i],bp)-oxy.mod[i-1]) }
  
  ##below is MLE calculation; output is -log likelihood
  # diff in measured and modeled O2
  sqdiff<-(oxy-oxy.mod)^2 
  # likelihood function
  L <- length(oxy) * (log(((sum(sqdiff)/length(oxy))^0.5))+0.5*log(6.28)) + ((2*sum(sqdiff)/length(oxy))^-1) * sum(sqdiff)
  L
}
# end of function

# this function plots modeled O2 and O2 data from estimates of daily GPP, ER, and K; is included in master rivermetabK function above
# Calls same metabolism equation as in mle function, but plots modeled O2 as a function of GPP, ER, and K estimates from mle
# use this to visually assess your model estimates (should be good agreement between modeled and measured O2)
onestationplot<-function(GPP, ER, oxy, z, temp, K, light, bp, ts) {
  
  oxy.mod<-numeric(length(oxy))
  oxy.mod[1]<-oxy[1]
  
  # this is the metabolism equation as in Van de Bogert et al (2007) L&OMethods
  for (i in 2:length(oxy)) { oxy.mod[i]<-oxy.mod[i-1]+((GPP/z)*(light[i]/sum(light)))+ ER*ts/z+(Kcor(temp[i],K))*ts*(osat(temp[i],bp)-oxy.mod[i-1]) }
  
  plot(seq(1:length(oxy)),oxy.mod, type="l",xlab="Time", ylab="Dissolved oxygen  (mg/L)", cex.lab=1.5, cex.axis=1.5, lwd=2 )
  points(seq(1:length(oxy)),oxy)
}
# end of function

####### END LOADING rivermetabK function ####### 


###########################################
# [7b] CALL RIVERMETABK function
###########################################

## Call as:  z is depth in m, bp is im mmHg, ts is time steps in days.

# for spring creek data; 10/28/14
rivermetabK(o2file=prinO2file
           , z=0.1, bp=736.5, ts=1/96)


##Summary

##21 apr-l
sM gpp=






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