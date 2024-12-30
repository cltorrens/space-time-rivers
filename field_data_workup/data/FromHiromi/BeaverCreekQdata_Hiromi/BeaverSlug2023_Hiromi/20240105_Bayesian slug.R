

################################
###BAYSIAN method coded with Bob 20240104
###application by Hiromi 20240106

##
library(tidyverse)
library(lubridate)
library(brms)
library(mgcv)

setwd("C:/Users/hirom/Dropbox/Hiromi Drop box/###Montana/Beaver slug 202223")

#NH4 slug
lowerN2022July<-read.csv("202207NH4slug_lowerBeaver.csv")
upperN2022July<-read.csv("202207NH4slug_upperBeaver.csv")

lowerN2023Apr<-read.csv("20230425NH4slug_lowerBeaver.csv")
upperN2023Apr<-read.csv("20230426NH4slug_upperBeaver.csv")

lowerN2023JulyE<-read.csv("20230707NH4slug_lowerBeaver.csv")
upperN2023JulyE<-read.csv("20230708NH4slug_upperBeaver.csv")

lowerN2023JulyL<-read.csv("20230722NH4slug_lowerBeaver.csv")
upperN2023JulyL<-read.csv("20230722NH4slug_upperBeaver.csv")

lowerN2023Aug<-read.csv("20230820NH4slug_lowerBeaver.csv")
upperN2023Aug<-read.csv("20230824NH4slug_upperBeaver.csv")





######################################
########### SRP slug ########### 
###################################

#SRP slug
lowerP2023Apr<-read.csv("20230425SRPslug_lowerBeaver.csv")
upperP2023Apr<-read.csv("20230426SRPslug_upperBeaver.csv")

lowerP2023JulyE<-read.csv("20230707SRPslug_lowerBeaver.csv")
upperP2023JulyE<-read.csv("20230708SRPslug_upperBeaver.csv")

lowerP2023JulyL<-read.csv("20230716SRPslug_lowerBeaver.csv")
upperP2023JulyL<-read.csv("20230722SRPslug_upperBeaver.csv")

lowerP2023Aug<-read.csv("20230820SRPslug_lowerBeaver.csv")
upperP2023Aug<-read.csv("20230824SRPslug_upperBeaver.csv")



###################################
##INPUT Sampling specific data
##
#NH4
##lower Beaver

for(i in 1:18){

  if(i==1){
#lowerN2022July
dat<-lowerN2022July
dat<-na.omit(dat)
dat$nh4re<-dat$nh4
dat<-na.omit(dat)
title<-"lowerN2022July"
NH4Cl<-400 #g
nh4add<-NH4Cl*14*1e6/(54.5)  #ug added N as NH4Cl
width<-15.5 #(m)
reach_length<-650
#special adjustment for background drift
lmspc<-lm(dat$spc[c(1:4,29:31)]~dat$minutes[c(1:4,29:31)])
est.bkgspc<-lmspc$coefficients[2]*(dat$mi)+lmspc$coefficients[1]
lmnh4<-lm(dat$nh4re[c(1:4,29:31)]~dat$minutes[c(1:4,29:31)])
est.bkgnh4<-lmnh4$coefficients[2]*(dat$mi)+lmnh4$coefficients[1]
bkgspc<-est.bkgspc
bkgnh4<-est.bkgnh4
mean(bkgnh4[1:4])
}

  if(i==2){
#lowerN2023Apr
dat<-lowerN2023Apr
dat<-na.omit(dat)
dat$nh4re<-dat$nh4/1.2356
title<-"lowerN2023Apr"
NH4Cl<-400 #g
nh4add<-NH4Cl*14*1e6/(54.5)  #ug added N as NH4Cl
width<-15.5 #(m)
reach_length<-650
bkgnh4<-mean(dat$nh4re[1:4]) 
bkgspc<-mean(dat$spc[1:4])  
  }
  

if(i==3){

#lowerN2023JulyE
dat<-lowerN2023JulyE
dat<-na.omit(dat)
dat$nh4re<-dat$nh4/1.2356
title<-"lowerN2023JulyE"
NH4Cl<-240.5 #g
nh4add<-NH4Cl*14*1e6/(54.5)  #ug added N as NH4Cl
width<-15.5 #(m)
reach_length<-550
bkgnh4<-mean(dat$nh4re[2:10]) 
bkgspc<-mean(dat$spc[2:10])  
}
  
if(i==4){
#lowerN2023JulyL
dat<-lowerN2023JulyL
dat<-na.omit(dat)
title<-"lowerN2023JulyL"
dat$nh4re<-dat$nh4/1.2356
NH42SO4<-250 #g
nh4add<-NH42SO4*14*2*1e6/(132)  #ug added N as NH42SO4
width<-15.5 #(m)
reach_length<-550
bkgnh4<-mean(dat$nh4re[1:3]) 
bkgspc<-mean(dat$spc[1:3])  
}

  if(i==5){
#lowerN2023Aug
dat<-lowerN2023Aug
dat<-na.omit(dat)
dat$nh4re<-dat$nh4/1.2356
title<-"lowerN2023Aug"
NH42SO4<-250 #g
nh4add<-NH42SO4*14*2*1e6/(132)  #ug added N as NH42SO4
width<-15.5 #(m)
reach_length<-550
bkgnh4<-mean(dat$nh4re[1:4]) 
bkgspc<-mean(dat$spc[1:4])   
}

##upper Beaver

  if(i==6){
#upperN2022July
dat<-upperN2022July
dat<-na.omit(dat)
dat$nh4re<-dat$nh4
title<-"upperN2022July"
NH4Cl<-400 #g
nh4add<-NH4Cl*14*1e6/(54.5)  #ug added N as NH4Cl
width<-17.6 #(m)
reach_length<-550
bkgnh4<-mean(dat$nh4re[2:5]) 
bkgspc<-mean(dat$spc[2:5])   
  }
  

  if(i==7){
#upperN2023Apr
dat<-upperN2023Apr
dat<-na.omit(dat)
dat$nh4re<-dat$nh4/1.2356
title<-"upperN2023Apr"
NH4Cl<-400 #g
nh4add<-NH4Cl*14*1e6/(54.5)  #ug added N as NH4Cl
width<-17.6 #(m)
reach_length<-550
bkgnh4<-mean(dat$nh4re[1:4]) 
bkgspc<-mean(dat$spc[1:4])   
  }
  

  if(i==8){
#upperN2023JulyE
dat<-upperN2023JulyE
dat<-na.omit(dat)
dat$nh4re<-dat$nh4/1.2356
title<-"upperN2023JulyE"
NH4Cl<-240.5 #g
nh4add<-NH4Cl*14*1e6/(54.5)  #ug added N as NH4Cl
width<-17.6 #(m)
reach_length<-500
bkgnh4<-mean(dat$nh4re[2:6]) 
bkgspc<-mean(dat$spc[2:6])  
  }
  
  if(i==9){
#upperN2023JulyL
dat<-upperN2023JulyL
dat<-na.omit(dat)
dat$nh4re<-dat$nh4/1.2356
title<-"upperN2023JulyL"
NH42SO4<-250 #g
nh4add<-NH42SO4*14*2*1e6/(132)  #ug added N as NH42SO4
width<-17.6 #(m)
reach_length<-500
bkgnh4<-mean(dat$nh4re[1:6]) 
bkgspc<-mean(dat$spc[1:6])  
  }
  
  if(i==10){
#upperN2023Aug
dat<-upperN2023Aug
dat<-na.omit(dat)
dat$nh4re<-dat$nh4/1.2356
title<-"upperN2023Aug"
NH42SO4<-250 #g
nh4add<-NH42SO4*14*2*1e6/(132)  #ug added N as NH42SO4
width<-17.6 #(m)
reach_length<-500

bkgnh4<-mean(dat$nh4re[1:5]) 
bkgspc<-mean(dat$spc[1:5])   
  }
  
if(i==11){
##INPUT Sampling specific data
##
#lowerP2023Apr
dat<-lowerP2023Apr
title<-"lowerP2023Apr"
dat$SRP<-replace(dat$SRP,dat$SRP=="< 0.8","0.3")
head(dat)
KH2PO4<-150 #g
srpadd<-KH2PO4*31*1e6/(136)  #ug added P as KH2PO4
width<-15.5 #(m)
reach_length<-650
nh4add<-srpadd
dat$nh4re<-dat$SRP
bkgnh4<-mean(dat$nh4re[1:3]) 
bkgspc<-mean(dat$spc[1:3])   
}

if(i==12){
#lowerP2023JulyE
dat<-lowerP2023JulyE
title<-"lowerP2023JulyE"
head(dat)
dat$SRP<-replace(dat$SRP,dat$SRP=="< 0.8","0.3")
dat<-na.omit(dat)
dat$SRP<-as.numeric(dat$SRP)
KH2PO4<-150 #g
srpadd<-KH2PO4*31*1e6/(136)  #ug added P as KH2PO4
width<-15.5 #(m)
reach_length<-550
nh4add<-srpadd
dat$nh4re<-dat$SRP
bkgnh4<-mean(dat$nh4re[1:2]) 
bkgspc<-mean(dat$spc[1:2])   
}

if(i==13){
#lowerP2023JulyL
dat<-lowerP2023JulyL
title<-"lowerP2023JulyL"
dat$SRP<-replace(dat$SRP,dat$SRP=="< 0.8","0.3")
dat<-na.omit(dat)
dat$SRP<-as.numeric(dat$SRP)
KH2PO4<-150 #g
srpadd<-KH2PO4*31*1e6/(136)  #ug added P as KH2PO4
width<-15.5 #(m)
reach_length<-550
nh4add<-srpadd
dat$nh4re<-dat$SRP
bkgnh4<-mean(dat$nh4re[1:4]) 
bkgspc<-mean(dat$spc[1:4])   
}

if(i==14){
#lowerP2023Aug
dat<-lowerP2023Aug
title<-"lowerP2023Aug"
dat$SRP<-replace(dat$SRP,dat$SRP=="< 0.8","0.3")
dat<-na.omit(dat)
dat$SRP<-as.numeric(dat$SRP)
KH2PO4<-150 #g
srpadd<-KH2PO4*31*1e6/(136)  #ug added P as KH2PO4
width<-15.5 #(m)
reach_length<-550
nh4add<-srpadd
dat$nh4re<-dat$SRP
bkgnh4<-mean(dat$nh4re[1:5]) 
bkgspc<-mean(dat$spc[1:5])   
}

if(i==15){
#SRP
#upperP2023Apr
dat<-upperP2023Apr
title<-"upperP2023Apr"
dat$SRP<-replace(dat$SRP,dat$SRP=="< 0.8","0.3")
dat<-na.omit(dat)
dat$SRP<-as.numeric(dat$SRP)
KH2PO4<-150 #g
srpadd<-KH2PO4*31*1e6/(136)  #ug added P as KH2PO4
width<-17.6 #(m)
reach_length<-550
nh4add<-srpadd
dat$nh4re<-dat$SRP
bkgnh4<-mean(dat$nh4re[1:4]) 
bkgspc<-mean(dat$spc[1:4])   
}

if(i==16){
#upperP2023JulyE
dat<-upperP2023JulyE
title<-"upperP2023JulyE"
dat$SRP<-replace(dat$SRP,dat$SRP=="< 0.8","0.3")
dat<-na.omit(dat)
dat$SRP<-as.numeric(dat$SRP)
KH2PO4<-150 #g
srpadd<-KH2PO4*31*1e6/(136)  #ug added P as KH2PO4
width<-17.6 #(m)
reach_length<-500
nh4add<-srpadd
dat$nh4re<-dat$SRP
bkgnh4<-mean(dat$nh4re[1:4]) 
bkgspc<-mean(dat$spc[1:4])   
}

if(i==17){

#upperP2023JulyL
dat<-upperP2023JulyL
title<-"upperP2023JulyL"
dat$SRP<-replace(dat$SRP,dat$SRP=="< 0.8","0.3")
dat<-na.omit(dat)
dat$SRP<-as.numeric(dat$SRP)
KH2PO4<-150 #g
srpadd<-KH2PO4*31*1e6/(136)  #ug added P as KH2PO4
width<-17.6 #(m)
reach_length<-500
nh4add<-srpadd
dat$nh4re<-dat$SRP
bkgnh4<-mean(dat$nh4re[1:5])
bkgspc<-mean(dat$spc[1:5])
}

if(i==18){

#upperP2023Aug
dat<-upperP2023Aug
title<-"upperP2023Aug"
dat$SRP<-replace(dat$SRP,dat$SRP=="< 0.8","0.3")
dat<-na.omit(dat)
dat$SRP<-as.numeric(dat$SRP)
KH2PO4<-150 #g
srpadd<-KH2PO4*31*1e6/(136)  #ug added P as KH2PO4
width<-17.6 #(m)
reach_length<-500
nh4add<-srpadd
dat$nh4re<-dat$SRP
bkgnh4<-mean(dat$nh4re[1:4]) 
bkgspc<-mean(dat$spc[1:4]) 
}

#slug.bayese<-function(dat,title,nh4add,width,reach_length,bkg.s,bkg.e){

################START from here

spc_add<- 50*454*2060 # 50 lb salt, convert to grams and then concuctivity based on measured 2060 uS/cm / g/L
add_ratio<- nh4add/spc_add

dat$nh4_bkg<- dat$nh4re-bkgnh4  #subtract background
dat$spc_bkg<- dat$spc-bkgspc


fit_b_n_nh4<- m <- brm(bf(nh4_bkg ~ s(minutes, k=20)),
                       data = dat, family = gaussian() )

fit_b_n_spc<- m <- brm(bf(spc_bkg ~ s(minutes, k=20)),
                       data = dat, family = gaussian() )

pred_n<- predict(fit_b_n_nh4)
pred_spc<- predict(fit_b_n_spc)

err_n<- posterior_predict(fit_b_n_nh4, ndraws = 1000)  #draw 1000 relaizations from joint distribution of model parameters
err_spc<- posterior_predict(fit_b_n_spc, ndraws = 1000)

err_spc_norm<- err_spc*add_ratio

####Graph###
#| fig-cap: Normalized specific conductivity (dark blue) with 100 relaizations of GAM fit derived from joint distribution of the parameters (light blue).  The same for NH4 but in green
#| 

#select NH4 or SRP for yaxis
#plot(dat$minutes, dat$nh4_bkg, xlab="minutes from release", ylab=" NH4 bkg corr", ylim=c(-2,100),main=title)
plot(dat$minutes, dat$nh4_bkg, xlab="minutes from release", ylab=" SRP bkg corr", ylim=c(-2,50),main=title)


for (i in 1:100){
  lines (dat$minutes, t(err_n[i,]), col="light green")
}

for (i in 1:100){
  lines (dat$minutes, t(err_spc_norm[i,]), col="lightblue")
}

points(dat$minutes, dat$nh4_bkg, pch=16, col="darkgreen")
lines(dat$minutes, pred_n[,1])

points(dat$minutes, dat$spc_bkg*add_ratio, pch=16, col="blue")
lines(dat$minutes, pred_spc[,1]*add_ratio)


##modeling
b_n_new_min <- data.frame(minutes = seq(min(dat$minutes),max(dat$minutes),1))  # new x data

new_pred_b_n_nh4 <- posterior_predict(fit_b_n_nh4, newdata = b_n_new_min, ndraws = 1000)  #1000 new BTC
new_pred_b_n_spc <- posterior_predict(fit_b_n_spc, newdata = b_n_new_min, ndraws = 1000)

b_n_nh4_int<-rowSums(new_pred_b_n_nh4)  #integral is easy, add them up!
b_n_spc_int<-rowSums(new_pred_b_n_spc)

mean(b_n_nh4_int) 
mean(b_n_spc_int)

##now calculate metrics############
b_n_ratio_int<- b_n_nh4_int/b_n_spc_int  # here is the ratio of N found vs spc found
Rx<- b_n_ratio_int/add_ratio #NH4 recapture ratio
quantile(Rx, c(0.05,0.5,0.95))
Q<-spc_add/b_n_spc_int*1440/1000 # m3 day-1
quantile(Q, c(0.05,0.5,0.95))
K_n<- -(log(b_n_ratio_int/add_ratio)) /reach_length # the model equation
quantile(K_n, c(0.05,0.5,0.95))
S_w<- 1/K_n #uptake length m
quantile(S_w, c(0.05,0.5,0.95))
vf_n<- Q*K_n/width #uptake velocity m/day
quantile(vf_n, c(0.05,0.5,0.95))


if(i==1){
  uday<- vf_n*dat$nh4re[c(1:4)] # for lowerN2022July 
  }else{
uday<- vf_n*bkgnh4 # mg m-2 d-1
}
quantile(uday, c(0.05,0.5,0.95))

data.sum<-data.frame(b_n_ratio_int,Rx,Q,K_n,S_w,vf_n,uday)
write.csv(data.sum,sprintf("%s.data.sum.csv",title),row.names=FALSE)
####################################################

}





####################################################
Season.list<-c("2022July","2023Apr","2023JulyE","2023JulyL","2023Aug")
Section.list<-c("lower","upper")
Nutrient.list<-c("N","P")

slug.summary<-data.frame()
for(i in 1:5){
  for(j in 1:2){
Season<-Season.list[i]
Section<-Section.list[j]
Nutrient<-Nutrient.list[1]
slug.data<-read.csv(sprintf("%s%s%s.data.sum.csv",Section,Nutrient,Season))
data<-data.frame(Season=Season,Section=Section,Nutrient=Nutrient,slug.data)
slug.summary<-rbind(slug.summary,data)     
    }
  }

for(i in 2:5){
  for(j in 1:2){
    Season<-Season.list[i]
    Section<-Section.list[j]
    Nutrient<-Nutrient.list[2]
    slug.data<-read.csv(sprintf("%s%s%s.data.sum.csv",Section,Nutrient,Season))
    data<-data.frame(Season=Season,Section=Section,Nutrient=Nutrient,slug.data)
    slug.summary<-rbind(slug.summary,data)     
  }
}

nrow(slug.summary)
write.csv(slug.summary,"slug.summary.csv",row.names=FALSE)

