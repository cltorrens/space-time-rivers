######################################################################################################################## 
#' @title prepNEON_streamMetabolizer_HOPB24

#' @author Bobby Hensley \email{hensley@battelleecology.org} \cr 

#' @description This script downloads dissolved oxygen, water temperature, 
#' discharge and light data from Lower Hop Brook for Water Year 2024, 
#' and formats it for the streamMetabolizer single station metabolism model.

#' @return This script produces a .csv file 

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Bobby Hensley (12/4/2020)
#     Original script created
######################################################################################################################## 
library(neonUtilities)
library(streamMetabolizer)
library(dplyr)
library(tidyr)
library(ggplot2)

######################################################################################################################## 

#Pulls required NEON data ##############################################################################################
#' Set site and date range
  siteName="HOPB"
  siteLong=-72.33 #site longitude (for calculating local solar time and modeling light)
  siteLat=42.47 #site latitude (for modeling light)
  startDate="2023-10"
  endDate="2024-09"

#' Pulls L1 water quality data
  waq<-neonUtilities::loadByProduct(dpID="DP1.20288.001", site=siteName, startdate=startDate, 
              enddate=endDate, package="expanded", include.provisional=T, check.size = F)
  list2env(waq, .GlobalEnv)
  #' Separates downstream (s2) data
  waqS2<-waq_instantaneous[(waq_instantaneous$horizontalPosition=="112"),]
  #' Keeps necessary columns
  waqS2<-waqS2[,c("startDateTime","dissolvedOxygen","dissolvedOxygenFinalQF")]
  #' Removes quality flagged measurements
  for (i in 1:nrow(waqS2)){if(waqS2[i,3]!=0)(waqS2[i,2]=NA)}
  #' Adjusts shifts
  startAdjust<-as.POSIXct("2023-11-30 18:00",tz="UTC")
  endAdjust<-as.POSIXct("2024-03-13 18:00",tz="UTC")  
  for (i in 1:nrow(waqS2)){if(waqS2[i,1]>startAdjust&waqS2[i,1]<endAdjust)(waqS2[i,2]=waqS2[i,2]*(97.7/99.7))} #' Offset from 2024-03-13 cal
  startAdjust<-as.POSIXct("2024-07-03 16:00",tz="UTC") #' Drifts for first few days then stabilized
  endAdjust<-as.POSIXct("2024-07-06 00:00",tz="UTC")  
  for(i in 1:nrow(waqS2)){
    if(waqS2$startDateTime[i]>startAdjust&
       waqS2$startDateTime[i]<endAdjust){
      waqS2$dissolvedOxygen[i] <- 
        waqS2$dissolvedOxygen[i]-
        ((waqS2$dissolvedOxygen[i]*(1-(97.2/101.8)))*(as.numeric(difftime(waqS2$startDateTime[i],
                                    startAdjust,units="secs"))/
                  as.numeric(difftime(endAdjust,
                                      startAdjust,units="secs"))))
    }}
  startAdjust<-as.POSIXct("2024-07-06 00:00",tz="UTC")
  endAdjust<-as.POSIXct("2024-08-20 16:00",tz="UTC")  
  for (i in 1:nrow(waqS2)){if(waqS2[i,1]>=startAdjust&waqS2[i,1]<endAdjust)(waqS2[i,2]=waqS2[i,2]*(97.2/101.8))} #' Offset from 2024-08-20 cal
  
  #' Calculates 15 minute averages
  waqS2$startDateTime<-lubridate::round_date(waqS2$startDateTime,unit="15 minute")
  waqS2<-plyr::ddply(waqS2,c("startDateTime"),summarise,dissolvedOxygen=mean(dissolvedOxygen))
  #' Fills data gaps using spline curve.  Max gap to fill set to 6 hrs 
  waqS2$dissolvedOxygen<-zoo::na.spline(waqS2$dissolvedOxygen,maxgap=24) 
  
#' Pulls L1 PRT water temperature data
  tsw<-neonUtilities::loadByProduct(dpID="DP1.20053.001", site=siteName, startdate=startDate,
              enddate=endDate, package="basic", include.provisional=T, check.size = F)
  list2env(tsw, .GlobalEnv)
  #' Separates downstream (s2) data
  tswS2<-TSW_5min[(TSW_5min$horizontalPosition=="112"),]
  #' Keeps necessary columns
  tswS2<-tswS2[,c("startDateTime","surfWaterTempMean","finalQF")]
  #' Removes quality flagged measurements
  for (i in 1:nrow(tswS2)){if(tswS2[i,3]!=0)(tswS2[i,2]=NA)}
  #' Replaces with actual sonde temp from saved csv files
  tswS2<-read.csv(file="HOPBtemp24.csv")
  tswS2$startDateTime<-as.POSIXct(tswS2$startDateTime, tz="GMT", format="%Y-%m-%d %H:%M")
  tswS2$surfWaterTempMean<-tswS2$temperature
  tswS2<-tswS2[(tswS2$surfWaterTempMean<26),]
  #' Calculates 15 minute averages
  tswS2$startDateTime<-lubridate::round_date(tswS2$startDateTime,unit="15 minute")
  tswS2<-plyr::ddply(tswS2,c("startDateTime"),summarise,waterTemp=mean(surfWaterTempMean))
  #' Fills data gaps using spline curve.  Max gap to fill set to 6 hrs 
  tswS2$waterTemp<-zoo::na.spline(tswS2$waterTemp,maxgap=24)
  
  #' Pulls L4 continuous discharge data
  csd<-neonUtilities::loadByProduct(dpID="DP4.00130.001", site=siteName, startdate=startDate, 
              enddate=endDate, package="basic", include.provisional = T, check.size = F)
  list2env(csd, .GlobalEnv)
  #' Keeps necessary columns
  csd<-csd_continuousDischarge[,c("endDate","continuousDischarge","dischargeFinalQF")]
  #' Removes quality flagged measurements
  for (i in 1:nrow(csd)){if(csd[i,3]!=0)(csd[i,2]=NA)}
  #' Converts L/s to m3/s
  csd$continuousDischarge<-csd$continuousDischarge/1000
  #' Calculates 15 minute averages
  csd$startDateTime<-lubridate::round_date(csd$endDate,unit="15 minute")
  csd<-plyr::ddply(csd,c("startDateTime"),summarise,discharge=mean(continuousDischarge))
  #' Fills data gaps using spline curve.  Max gap to fill set to 6 hrs 
  csd$discharge<-zoo::na.spline(csd$discharge,maxgap=24)
  
#' Pulls L1 Met station light data
  par<-neonUtilities::loadByProduct(dpID="DP1.00024.001", site=siteName, startdate=startDate, 
              enddate=endDate, package="basic", include.provisional=T, check.size = F)
  list2env(par, .GlobalEnv)
  par<-PARPAR_1min[,c("startDateTime","PARMean","PARFinalQF")]
  #' Removes quality flagged measurements
  for (i in 1:nrow(par)){if(par[i,3]!=0)(par[i,2]=NA)}
  #' Calculates 15 minute averages
  par$startDateTime<-lubridate::round_date(par$startDateTime,unit="15 minute")
  par<-plyr::ddply(par,c("startDateTime"),summarise,PAR=mean(PARMean))
  #' Fills data gaps using spline curve.  Max gap to fill set to 6 hrs 
  par$PAR<-zoo::na.spline(par$PAR,maxgap=24)
  
#' Pulls L1 Met station barometric pressure data 
  bp<-neonUtilities::loadByProduct(dpID="DP1.00004.001", site=siteName, startdate=startDate, 
              enddate=endDate, package="basic", include.provisional=T, check.size = F)
  list2env(bp, .GlobalEnv)
  bp<-BP_1min[,c("startDateTime","corPres","corPresFinalQF")]
  #' Removes quality flagged measurements
  for (i in 1:nrow(bp)){if(bp[i,3]!=0)(bp[i,2]=NA)}
  #' Converts from kPa to millibars
  bp$corPres<-bp$corPres*10
  #' Calculates 15 minute averages
  bp$startDateTime<-lubridate::round_date(bp$startDateTime,unit="15 minute")
  bp<-plyr::ddply(bp,c("startDateTime"),summarise,barPres=mean(corPres))
  #' Fills data gaps using spline curve.  Max gap to fill set to 6 hrs 
  bp$barPres<-zoo::na.spline(bp$barPres,maxgap=24)
  
########################################################################################################################  

  #Preps data for streamMetabolizer ###################################################################################### 
  #' Merges data using time stamps  
  mergedData<-merge(waqS2,tswS2,by.x="startDateTime",by.y="startDateTime",all.x=T,all.y=T)
  mergedData<-merge(mergedData,csd,by.x="startDateTime",by.y="startDateTime",all.x=T,all.y=T)
  mergedData<-merge(mergedData,par,by.x="startDateTime",by.y="startDateTime",all.x=T,all.y=T)
  mergedData<-merge(mergedData,bp,by.x="startDateTime",by.y="startDateTime",all.x=T,all.y=T)
  #' Fills temp data gaps using spline curve.  Max gap to fill set to 6 hrs (Need to do this again because temp came from sonde csv files with no entries for missing data)
  mergedData$waterTemp<-zoo::na.spline(mergedData$waterTemp,maxgap=24)
  
  #' Calculates DO saturation using Temp and Pres
  mergedData$satDO<-streamMetabolizer::calc_DO_sat(mergedData$waterTemp,mergedData$barPres,model="garcia-benson")
  mergedData$pctSat<-mergedData$dissolvedOxygen/mergedData$satDO
  
  #' Calculates mean depth from discharge using power function derived from reaeration dataset
  mergedData$depth<-streamMetabolizer::calc_depth(mergedData$discharge,c=0.24,f=0.29)  
  
  #' Convert UTC to local solar time
  mergedData$startDateTime<-streamMetabolizer::calc_solar_time(mergedData$startDateTime,longitude=siteLong)
  
  #' Option to replace measured PAR with modeled PAR
  # mergedData$PAR<-streamMetabolizer::calc_light(mergedData$startDateTime,siteLat,siteLong,max.PAR=2000)
  
  #' Formats data into correct columns and headers
  mergedData<-mergedData[,c("startDateTime","dissolvedOxygen","satDO","discharge","depth","waterTemp","PAR")]
  names(mergedData)<-c("solar.time","DO.obs","DO.sat","discharge","depth","temp.water","light")
  
write.csv(mergedData,file="HOPB_WY24.csv")  
  
  
  

  
  
  
  
  
  
    
  
  
#'Select start and end date of model run (if less than period of data)
  modelStart=as.POSIXct("2023-10-01 00:00", tz="UTC")
  modelEnd=as.POSIXct("2023-11-01 00:00", tz="UTC")
  mergedData<-mergedData[(mergedData$solar.time>=modelStart),]
  mergedData<-mergedData[(mergedData$solar.time<=modelEnd),]
  
    
#' Plots data for visual inspection  
  mergedData %>% unitted::v() %>%
    mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
    select(solar.time, starts_with('DO')) %>%
    gather(type, DO.value, starts_with('DO')) %>%
    mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
    ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() +facet_grid(units ~ ., scale='free_y') + theme_bw() + scale_color_discrete('variable')
  labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
  mergedData %>% unitted::v() %>%
    select(solar.time, depth, temp.water, light) %>%
    gather(type, value, depth, temp.water, light) %>%
    mutate(
      type=ordered(type, levels=c('depth','temp.water','light')),
      units=ordered(labels[type], unname(labels))) %>%
    ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() +
    facet_grid(units ~ ., scale='free_y') + theme_bw() +
    scale_color_discrete('variable')
