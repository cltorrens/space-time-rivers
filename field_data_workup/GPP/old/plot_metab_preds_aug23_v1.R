
library(dplyr)
library(readr)
library(lubridate)
library(streamMetabolizer)
library(deSolve)
library(here)



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

all_oxy$time<-as_datetime(all_oxy$Unix_Timestamp_sec)

all_oxy$solar.time<-convert_UTC_to_solartime(all_oxy$time, longitude= -113, time.type="mean solar")


all_oxy$light<- calc_light(all_oxy$solar.time, latitude=48.5, longitude=-113, max.PAR =2326, attach.units = F)

all_oxy$osat<- osat(all_oxy$Temperature_degC, mmhg)



# # dividing oxy by site - ANOTHER PROJECT
# paola_oxy<- all_oxy[all_oxy$site_num==2, ]
# casc_oxy<- all_oxy[all_oxy$Stream=="CASC", ]
# beaver_oxy<- all_oxy[all_oxy$Stream=="BEAVER", ]
# sc_oxy<- all_oxy[all_oxy$Stream=="SC", ]
# upwell_oxy<- all_oxy[all_oxy$Stream=="UPWELL", ]

#install.packages("neonUtilities")
#remotes::install_github('appling/unitted')
#remotes::install_github("USGS-R/streamMetabolizer")

all_oxy_aug23 <- all_oxy %>%
  mutate(jday = yday(MST_datetime), 
         model_datetime = MST_datetime-hours(4), 
         model_jday = yday(model_datetime)) %>%
  filter(model_jday >= 230 & model_jday <= 251)

all_oxy_oct23 <- all_oxy %>%
  mutate(jday = yday(MST_datetime), 
         model_datetime = MST_datetime-hours(4), 
         model_jday = yday(model_datetime)) %>%
  filter(model_jday >= 270 & model_jday <= 284)

mf_name <- mm_name(type='bayes', pool_K600='normal', err_obs_iid = T, err_proc_iid =T)
mf_specs <- specs(mf_name, K600_daily_meanlog_meanlog=2, K600_daily_meanlog_sdlog=0.7, K600_daily_sdlog_sigma=0.1, burnin_steps=1000, 
                  saved_steps=1000)


z = 0.3 # estimation / to match N model\
Q_aug =     # both from single-day manual discharge measurements  
Q_oct = 

### August week - Beaver Cr. 

augBC_data_sm<-data.frame(DO.obs=all_oxy_aug23$DO_mgl, DO.sat=all_oxy_aug23$osat, 
                          temp.water=all_oxy_aug23$Temperature_degC, depth=rep(z,length(all_oxy_aug23$Temperature_degC)), 
                          light=all_oxy_aug23$light, solar.time=all_oxy_aug23$solar.time)


augBC_fit <- metab(mf_specs, data=augBC_data_sm, info=c(site='BC Aug 2023', source='CT'))
save(augBC_fit, file=here("field_data_workup/GPP/augBC_fit.RData"))

plot_DO_preds(predict_DO(augBC_fit))
plot_metab_preds(predict_metab(augBC_fit))
params_aug_v1 <- get_params(augBC_fit, uncertainty='ci')

# Checking relationship between K and ER - linear = equifinality
ggplot(data = params_aug_v1, aes(x=K600.daily, y=ER.daily)) + 
  geom_point() +
  labs(x= "daily K600", y= "daily ER") + 
  
  theme_bw()


### October week - Beaver Cr

octBC_data_sm<-data.frame(DO.obs=all_oxy_oct23$DO_mgl, DO.sat=all_oxy_oct23$osat, 
                          temp.water=all_oxy_oct23$Temperature_degC, depth=rep(z,length(all_oxy_oct23$Temperature_degC)), 
                          light=all_oxy_oct23$light, solar.time=all_oxy_oct23$solar.time)


octBC_fit <- metab(mf_specs, data=octBC_data_sm, info=c(site='BC Oct 2023', source='CT'))
#save(octBC_fit, file="octBC_fit.RData")

plot_DO_preds(predict_DO(octBC_fit))
plot_metab_preds(predict_metab(octBC_fit))
get_params(octBC_fit , uncertainty='ci')


octGPPavg <- (2.723725+3.198911 + 2.312929+ 2.415243+2.649883+2.694739+2.618775)/7

###Paola

paola_data_sm<-data.frame(DO.obs=paola_oxy$DO.obs, DO.sat=paola_oxy$osat, 
                           temp.water=paola_oxy$temp.water, depth=rep(0.85,length(paola_oxy$temp.water)), 
                           light=paola_oxy$light, solar.time=paola_oxy$solar.time)



paola_fit <- metab(mf_specs, data=paola_data_sm, info=c(site='paola', source='Bob Hall'))
save(paola_fit, file="paola_fit.RData")

plot_DO_preds(predict_DO(paola_fit))
plot_metab_preds(predict_metab(paola_fit))
get_params(paola_fit , uncertainty='ci')

###Upwell

upwell_data_sm<-data.frame(DO.obs=upwell_oxy$Dissolved_Oxygen, DO.sat=upwell_oxy$osat, 
                              temp.water=upwell_oxy$Temperature, depth=rep(1.05,length(upwell_oxy$Temperature)), 
                              light=upwell_oxy$light, solar.time=upwell_oxy$solar.time)
head(upwell_data_sm)


upwelling_fit <- metab(mf_specs, data=upwell_data_sm, info=c(site='upwelling', source='Bob Hall'))
save(upwelling_fit, file="upwelling_fit.RData")

plot_DO_preds(predict_DO(upwelling_fit))
plot_metab_preds(predict_metab(upwelling_fit))
get_params(upwelling_fit , uncertainty='ci')





