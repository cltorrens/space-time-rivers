
library(rstan)
library(shinystan)
#install.packages('dplyr', repos="http://cran.cnr.berkeley.edu/")
library(dplyr)
#install.packages('tidyr', repos="http://cran.cnr.berkeley.edu/")
library(tidyr)
library("bayesplot")
library("ggplot2")
library("rstanarm")
library(scales)
library(magrittr) # moved from  'light ea day as vector'

######################### entire dataset

datain<-read.csv("Miller Creek N uptake input.csv")
dim(datain)  #dimensions

head(datain)

####create list of variables 

###light at each timestep as matrix (col=days, row= hours/day)
lightMA <- matrix(unlist(datain$light), ncol = 111, byrow = FALSE)

###nitrate conc each timestep as matrix (col=days, row= hours/day)
concMA <- matrix(unlist(datain$conc), ncol = 111, byrow = FALSE)

###sum total light for each day as vector

sumlightgrouped<-datain %>% group_by(Jday) %>%
  summarize(sumlight = sum(light))

sumlight<-(sumlight=sumlightgrouped$sumlight)
head(sumlight)
sumlight

#check sum light
plot(sumlight)

###depth aat each timestep as matrix (col=days, row= hours/day)
zMA<- matrix(unlist(datain$z), ncol = 111, byrow = FALSE)


###merge matrices, vectors,and real numbers into single list
#T=number of hours in a day, D= number of days of sampling, deltat=change in time between time steps (in units of day)
data <- list(lightMA=lightMA,concMA=concMA,deltat=1/24,sumlight=sumlight,zMA=zMA,T=24,D=111)

########################################unpooled (all days)####################################################################

sink("unpooled.stan")

cat("
    
    data {
    int <lower = 1> T;   //number of hours each day is an integer
    int <lower = 1> D;   //number of total days is an integer
    real deltat;     //time increment of sampling (in d) is a real number
    matrix[T,D] lightMA;    //light at each timestep is a matrix
    vector[D] sumlight; // total summed daily light is a vector
    matrix[T,D] zMA;//depth /channel depth is a matrix
    matrix[T,D] concMA;//nitrate concentration is a matrix
    }
    
    parameters {
    real<lower = 0> sigma; //  standard deviation
    vector<lower=0>[D] K;        // Nitrate rate
    vector <lower=0> [D] N_b;// Background nitrate concentration
    vector[D] U;           // assimilatory autotrophic nitrate uptake
    }

   model {
    matrix[T,D] conc_hat;
    for (d in 1:D) {
    conc_hat[1,d] = concMA[1,d]; 
    for (i in 2:T){
    conc_hat[i,d]= concMA[i-1,d] -(U[d]*lightMA[i,d])/(zMA[i,d]*sumlight[d])+K[d]*(N_b[d]-concMA[i-1,d])*deltat;
    concMA[i,d]~ normal (conc_hat[i,d], sigma);
    }
    
    }
    K~lognormal(2.25,1.5);
    N_b~normal(57.16,11.98);
    }
    "
    ,fill=TRUE)
sink()


#run the MCMC

fit <- stan("unpooled.stan", data = data,  iter = 1000, chains = 4,control = list(max_treedepth = 15))


output<-print(fit)

#view chains
traceplot(fit,pars="U")
traceplot(fit,pars="K")
traceplot(fit,pars="N_b")

###daily mean, se, sd, percentiles, n_eff, and Rhat
d<-summary(fit)
d

#export summary stats
write.csv(d,file="MCNR Bayesian N uptake output_Bray.csv")

output1<-read.csv("MCNR Bayesian N uptake output_Bray.csv")
head(output1)
dim(output1)

##create data frame of mean K, confidence interval of K, N_b, and U
#K
K1<-output1$summary.mean[2:112]
end(K1)

#K confidence interval
K_int_high<-output1$summary.97.5.[2:112]
K_int_low<-output1$summary.2.5.[2:112]

K_conf_int1<-(K_int_high-K_int_low)/2
head(K_conf_int1)

#K SD
K1_SD<-output1$summary.sd[2:112]
end(K1)


#N_b
N_b1<-output1$summary.mean[113:223]
end(N_b1)

#U
U1<-output1$summary.mean[224:334]
end(U1)

#date
Date <- seq(as.Date('2019-06-25'), as.Date('2019-10-13'), by = 'days')
Date

#dataframe
output_average<-data.frame(Date,K1,N_b1,U1,K_conf_int1,K1_SD)
dim(output_average)
head(output_average)
output_average

###plot K vs. U to check  colinearity
plot(output_average$K1,output_average$U1, xlab="K", ylab="U",main="Unpooled")

#plot U over the study
plot(Date,output_average$U1,xlab="Date",ylab="U",main="Unpooled")
lines(Date,output_average$U1,xlab="Date",ylab="U",main="Unpooled")

#Plot K over study
plot(Date,output_average$K1,xlab="Date",ylab="K",main="Unpooled")
lines(Date,output_average$K1,xlab="Date",ylab="K",main="Unpooled")


# u  versus sumlight with model estimates of slope and intercept
b1<-1.1138
b0<--2.803

sumlight_dat<-data.frame(Date,sumlight)
sumlight
plot(Date,sumlight)

pooled_line<-b0+b1*sumlight*1e-6
plot(Date,pooled_line)

reg<-lm(pooled_line~Date)

plot(sumlight,output_average$U1,xlab="Sum daily light",ylab="U",main="Unpooled")
lines(sumlight,pooled_line,xlab="Date",ylab="U",main="Unpooled", col="red")


####check model fit by comparing modeled concentration to observed
###use model outputs to estimate concentration
dt<-1/24


U<-output_average$U1  # this is flux in unit mg N m-2 d-1
K<-output_average$K1
N_b<-output_average$N_b1

d<-1:111
N<-NA

N<-matrix(nrow = 24, ncol = 111)

quartz()
par(mfrow=c(2,2))  #before the for looP to see 4 days at a time
dev.off()

for (d in 1:111){
  N[1,d]<-N_b[d]
  
  # start at equilibrium for each day
  
  for (i in 2:24){
    
    N[i,d]<-N[i-1,d] - (U[d]*lightMA[i,d])/(zMA[i,d]*sumlight[d])+ K[d]*(N_b[d] - N[i-1,d])*dt
    
  }
  plot(seq(1:24),concMA[,d], main=d, xlab="Hour", ylab="concentration")
  lines(seq(1:24), N[,d], col="red")
}


########################################pooled with light (all days)####################################################################

sink("pooled.stan")

cat("
    
    data {
    int <lower = 1> T;   //number of hours each day (24) is an integer
    int <lower = 1> D;   //number of total days (5) is an integer
    real deltat;     //time increment of sampling (in d) is a real number
    matrix[T,D] lightMA;    //light at each timestep is a matrix
    vector[D] sumlight; // total summed daily light is a vector
    matrix[T,D] zMA;//depth /channel depth is a matrix
    matrix[T,D] concMA;//nitrate concentration is a matrix
    }
    
    parameters {
    real<lower = 0> sigma; //  standard deviation
    vector<lower=0>[D] K;        // Nitrate rate
    vector <lower=0> [D] N_b;// Background nitrate concentration
    vector[D] U;           // assimilatory autotrophic nitrate uptake
    real<lower = 0> sigma_U; //  standard deviation of mean assimilatory autotrophic nitrate uptake over entire study
    real b0;//intercept of linear relationship between uptake and sum daily light
    real<lower=0> b1;//slope of linear relationship between uptake and sum daily light
    }

   model {
    matrix[T,D] conc_hat;
    for (d in 1:D) {
    U[d] ~ normal(b0 + b1*sumlight[d]*1e-6, sigma_U);//linear relationship between light and uptake
    conc_hat[1,d] = concMA[1,d]; 
    for (i in 2:T){
    conc_hat[i,d]= concMA[i-1,d] -(U[d]*lightMA[i,d])/(zMA[i,d]*sumlight[d])+K[d]*(N_b[d]-concMA[i-1,d])*deltat;
    concMA[i,d]~ normal (conc_hat[i,d], sigma);
    }
    
    }
    b0~normal(0,4.57);
    b1~normal(0,10);
    K~lognormal(2.25,1.5);
    N_b~normal(57.16,11.98);
    sigma_U~normal(0,4.57);//half normal distribution
    }
    "
    ,fill=TRUE)
sink()


fit2 <- stan("pooled.stan", data = data,  iter = 1000, chains = 4,control = list(max_treedepth = 15))
output<-print(fit2)

#view chains
traceplot(fit2,pars="U")
traceplot(fit2,pars="K")
traceplot(fit2,pars="N_b")

###daily mean, se, sd, percentiles, n_eff, and Rhat
d2<-summary(fit2)

#export summary stats
write.csv(d2,file="MCNR pooled Bayesian N uptake output 2_Bray.csv")

output2<-read.csv("MCNR pooled Bayesian N uptake output 2_Bray.csv")
head(output2)
dim(output2)

#K
K2<-output2$summary.mean[2:112]
end(K2)

#K confidence interval
K_int_high2<-output2$summary.97.5.[2:112]
K_int_low2<-output2$summary.2.5.[2:112]

K_conf_int2<-(K_int_high2-K_int_low2)/2
head(K_conf_int2)

#K SD
K2_SD<-output2$summary.sd[2:112]
end(K2)


#N_b
N_b2<-output2$summary.mean[113:223]
end(N_b2)

#U
U2<-output2$summary.mean[224:334]
end(U2)

#date
Date <- seq(as.Date('2019-06-25'), as.Date('2019-10-13'), by = 'days')
Date

#dataframe
output_average2<-data.frame(Date,K2,N_b2,U2,K_conf_int2,K2_SD)
dim(output_average2)
head(output_average2)
output_average2

###plot K vs. U to check colinearity
plot(output_average2$K2,output_average2$U2, xlab="K", ylab="U",main="Pooled")
#pooling U with light reduced linearity between K and U

#Plot U and K over study
plot(Date,output_average2$U2,xlab="Date",ylab="U",main="Pooled")
lines(Date,output_average2$U2,xlab="Date",ylab="U",main="Pooled")

plot(Date,output_average2$K2,xlab="Date",ylab="K",main="Pooled")
lines(Date,output_average2$K2,xlab="Date",ylab="K",main="Pooled")


# u  versus sumlight with model estimates of slope and intercept
b1<-1.1138
b0<--2.803

sumlight_dat<-data.frame(Date,sumlight)
sumlight
plot(Date,sumlight)

pooled_line<-b0+b1*sumlight*1e-6
plot(Date,pooled_line)


plot(sumlight,output_average2$U2,xlab="Sum daily light",ylab="U",main="Pooled")
lines(sumlight,pooled_line,xlab="Date",ylab="U",main="Pooled", col="red")


####check model fit by comparing modeled concentration to observed
###use model outputs to estimate concentration
dt<-1/24


U2<-output_average2$U2  # this is flux in unit mg N m-2 d-1
K2<-output_average2$K2
N_b2<-output_average2$N_b2

d<-1:111

N2<-matrix(nrow = 24, ncol = 111)

quartz()
par(mfrow=c(2,2))  #before the for looP to see 4 days at a time
dev.off()

for (d in 1:111){
  N2[1,d]<-N_b2[d]
  
  # start at equilibrium for each day
  
  for (i in 2:24){
    
    N2[i,d]<-N2[i-1,d] - (U2[d]*lightMA[i,d])/(zMA[i,d]*sumlight[d])+ K2[d]*(N_b2[d] - N2[i-1,d])*dt
    
  }
  plot(seq(1:24),concMA[,d], main=d, xlab="Hour", ylab="concentration")
  lines(seq(1:24), N2[,d], col="blue")
}




##Compare modeled concentration from pooled and unpooled models to observed concentration
par(mfrow=c(5,4)) 

for (d in 1:111){
  
  plot(seq(1:24),concMA[,d], main=d, xlab="Hour", ylab="concentration")
  lines(seq(1:24), N[,d], col="red")
  lines(seq(1:24), N2[,d], col="blue")
  
}
