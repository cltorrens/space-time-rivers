
library(tidyverse)
library(brms)
library(here)


depth<-read.csv(here("./Data/Transect-data/transect_data.csv"))

mean_depth<- depth %>% group_by(site,transect) %>% summarize(depth=mean(depth_m))


depth_fit<- brm(depth ~ 0 + Intercept + (1 | site), data=mean_depth)
summary(depth_fit)
depth_dev<-ranef(depth_fit)
depth_dev<-as.data.frame(depth_dev)
depth_summ<-0.86+depth_dev$site.Estimate.Intercept


mean_river_depth<- mean_depth %>% group_by(site) %>% summarize(depth=mean(depth, na.rm=T))


plot(depth$width_m,depth$depth_m, log='xy')
