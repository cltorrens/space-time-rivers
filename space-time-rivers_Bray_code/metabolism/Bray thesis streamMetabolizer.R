install.packages("deSolve")
library(deSolve)

install.packages("library")
install.packages("streamMetabolizer", dependencies=TRUE, 
                 repos=c("https://owi.usgs.gov/R","https://cran.rstudio.com"))
update.packages(oldPkgs=c("streamMetabolizer","unitted"), dependencies=TRUE,
                repos=c("https://owi.usgs.gov/R", "https://cran.rstudio.com"))
devtools::find_rtools()
install.packages("Rtools")
library(streamMetabolizer)
install.packages('dplyr', repos="http://cran.cnr.berkeley.edu/")
library(dplyr)
install.packages('tidyr', repos="http://cran.cnr.berkeley.edu/")
library(tidyr)
install.packages('ggplot2', repos="http://cran.cnr.berkeley.edu/")
library(ggplot2)

#what is everything labeled in streamMetabolizer? What are the units?
?mm_data

#import and inspect data
#time should be in Unix units (output from miniDOT)

dat <- read.csv("Miller Creek metabolism input.csv",head=T,sep=",") # load csv data
head(dat)


#convert miniDOT unix time to solar time
dat$solar.time<-as.POSIXct(dat$solar.time, origin="1970-01-01")
dat$solar.time <- lubridate::force_tz(dat$solar.time, "UTC")
head(dat)
dat

#view time series of data
#DO percent saturation and concentration
dat %>% unitted::v() %>%
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

labels <- c(DO.obs='DO concentration\n(mg/L)', DO.sat='Do saturation(%)')
dat %>% unitted::v() %>%
  select(solar.time, DO.obs, DO.sat) %>%
  gather(type, value, DO.obs, DO.sat) %>%
  mutate(type=ordered(type, levels=c('DO.obs','DO.sat')),
         units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line(color='black') + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('black')


#depth,water temperature, and light
labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
dat %>% unitted::v() %>%
  select(solar.time, depth, temp.water, light) %>%
  gather(type, value, depth, temp.water, light) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')


###########estimating k600 prior
S<-0.006 #(m/m)
V<-0.4 #m/s
g<-10 #m/s^2
z<-0.2 #m
gsv<-S*V*g
log(gsv)

logk600<-3.10 + 0.35*log(gsv) #from Amber's paper
k600<-exp(logk600)
K600<- k600/z #convert gas exchange velocity to a rate
logK600<-log(K600)
k600
#prior is lognormal (logK600, 0.75)


#########model 1: Bayesian without pooling
bayes_name <- mm_name(type='bayes', pool_K600='none', err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_name

#specifications of model (i.e. priors)
bayes_specs <- specs(bayes_name,K600_daily_meanlog=log(30),K600_daily_sdlog =.75)


#######model 2: Bayesian with linear partial pooling of K600 with discharge
bayes_name <- mm_name(type='bayes', pool_K600='linear', err_obs_iid=TRUE, err_proc_iid=TRUE)


####linear pooling with log Q (0.05)
bayes_specs <- specs(bayes_name,K600_daily_sigma_sigma = .05)
logQJO <- metab(bayes_specs, data=dat)
outputlogQJO<-predict_metab(logQJO)
outputlogQJO%>% print(n = Inf)
plot_metab_preds(outputlogQJO)
summary(outputlogQJO)

####linear pooling with log Q (0.1)
bayes_specs2 <- specs(bayes_name,K600_daily_sigma_sigma = .1)
logQJO2 <- metab(bayes_specs2, data=dat)
outputlogQJO2<-predict_metab(logQJO2)
outputlogQJO2%>% print(n = Inf)
plot_metab_preds(logQJO2)
summary(outputlogQJO2)

####linear pooling with log Q (0.2)
bayes_specs3 <- specs(bayes_name,K600_daily_sigma_sigma = .2)
logQJO3<- metab(bayes_specs3, data=dat)
outputlogQJO3<-predict_metab(logQJO3)
outputlogQJO3%>% print(n = Inf)
plot_metab_preds(logQJO3)
summary(outputlogQJO3)



#check for collinearity between ER and K600 for each model fit
outputlogQJO<-get_params(logQJO)
par(mar = c(5,5,1,1))
plot<-plot(outputlogQJO$K600.daily,outputlogQJO$ER.daily,xlab =expression("Daily"~K[600]~(d^{-1})),
           pch=16,cex=1.5,col='black',
           ylab=expression("Daily ER" ~ (g ~ O[2]~m^{-2} ~d^{-1})),
           cex.lab=1.2)

outputlogQJO2<-get_params(logQJO2)
par(mar = c(5,5,1,1))
plot<-plot(outputlogQJO2$K600.daily,outputlogQJO2$ER.daily,xlab =expression("Daily"~K[600]~(d^{-1})),
           pch=16,cex=1.5,col='black',
           ylab=expression("Daily ER" ~ (g ~ O[2]~m^{-2} ~d^{-1})),
           cex.lab=1.2)


outputlogQJO3<-get_params(logQJO3)
par(mar = c(5,5,1,1))
plot<-plot(outputlogQJO3$K600.daily,outputlogQJO3$ER.daily,xlab =expression("Daily"~K[600]~(d^{-1})),
           pch=16,cex=1.5,col='black',
           ylab=expression("Daily ER" ~ (g ~ O[2]~m^{-2} ~d^{-1})),
           cex.lab=1.2)

