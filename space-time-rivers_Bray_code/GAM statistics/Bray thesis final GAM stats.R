library("mgcv")
library("scam")
library("ggplot2")
library("cowplot")
library("tidyr")
library("gratia")


data<- read.csv("Bray_Miller_Creek_daily.csv",head=T,sep=",") # load csv data

head(data)
dim(data)  

#############################################1) Assess changes in time series using generalized additive models and derivatives#######################

###########################################################Downstream discharge (with storm event outliers removed)#############################################################################


#####fit simple GAM with REML
gam_mod_Q<-gamm(Q.dwn~ s(julian.day,k=100), data = data,method='REML')

summary(gam_mod_Q$gam)
gam.check(gam_mod_Q$gam)

acf(residuals(gam_mod_Q$gam))
pacf(residuals(gam_mod_Q$gam))
#autocorrelation

## fit GAM using gamm() with CAR
## ...so fit the AR1
car1_Q <- gamm(Q.dwn~ s(date,k=100), data = data,
               correlation = corARMA(form = ~ julian.day, p = 1),method='REML')

summary(car1_Q$gam)
set.seed(1)
gam.check(car1_Q$gam)

acf(residuals(car1_Q$gam))
pacf(residuals(car1_Q$gam))

## ...and fit the AR2
car2_Q<- gamm(Q.dwn ~ s(julian.day, k = 100), data = data,
              correlation = corARMA(form = ~ julian.day, p = 2),method='REML')
summary(car2_Q$gam)
gam.check(car2_Q$gam)

acf(residuals(car2_Q$gam))
pacf(residuals(car2_Q$gam))

#fit AR(3)
car3_Q<- gamm(Q.dwn ~ s(julian.day, k = 100), data = data,
              correlation = corARMA(form = ~ julian.day, p = 3),method='REML')
summary(car3_Q$gam)
gam.check(car3_Q$gam)

acf(residuals(car3_Q$gam))
pacf(residuals(car3_Q$gam))

#compare model fits with AIC
anova(gam_mod_Q$lme,car1_Q$lme, car2_Q$lme,car3_Q$lme)
#AR(2) provides the best fit



############################plot GAM model output
N <- 300 # number of points at which to evaluate the smooth

## create new data to predict at; 200 evenly-spaced values over `Year`
newYear <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                  length.out = 200)))
## Predict from the fitted model; note we predict from the $gam part
newYear <- cbind(newYear,
                 data.frame(predict(car1_Q$gam, newYear, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(car1_Q$gam))
newYear <- transform(newYear,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))
## Plot estimated trend
Q_fitted <- ggplot(newYear, aes(x = julian.day, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = julian.day), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") +
  geom_point(data = data, mapping = aes(x = julian.day, y = Q.dwn),
             inherit.aes = FALSE) +
  geom_line() +
  labs(y ="Q", x = "Julian day")
Q_fitted



## simulate data from model
set.seed(1) # set the random seed to make this reproducible
nsim <- 20 # how many simulations to draw
## do the simulations
sims <- simulate(car1_Q, nsim = nsim, newdata = newYear, unconditional = TRUE)

## rearrange the output into a long/tidy format
colnames(sims) <- paste0("sim", seq_len(nsim))
sims <- setNames(stack(as.data.frame(sims)), c("simulated", "run"))
sims <- transform(sims, Year = rep(newYear$julian.day, nsim),
                  simulated = simulated)

## Plot simulated trends
smallSim.plt <- ggplot(newYear, aes(x = julian.day, y = fit)) +
  geom_line(data = sims,
            mapping = aes(y = simulated, x = year, group = run),
            colour = "grey80") +
  geom_line(lwd = 1)

smallSim.plt



#confidence and simultaneous intervals
#compute across-the-fuction and simultaneous confidence intervals
cint <- confint(car1_Q, parm = "julian.day", newdata = newYear,
                type = "confidence")

sint <- confint(car1_Q, parm = "Year", newdata = newYear,
                type = "simultaneous")
head(sw.sint)




###derivatives of estimated trend
Q.d <- fderiv(car1_Q, newdata = newYear, n = N)
Q.sint <- with(newYear,
               cbind(confint(Q.d, nsim = nsim,
                             type = "simultaneous"),
                     julian.day = julian.day))
Q_deriv_plt <- ggplot(Q.sint, aes(x = julian.day, y = est)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  labs(x = "Julian day CE", y = "First derivative")

#plot derivatives
plot_grid(Q_deriv_plt, ncol = 1, labels = "auto",
          align = "hv", axis = "lr")

print(Q.sint)


###########################################################Groundwater discharge (with storm event outliers removed)#############################################################################


#####fit simple GAM with REML
gam_mod_Qgw<- gamm(Q.gw~ s(julian.day,k=50), data = data,method='REML')

summary(gam_mod_Qgw$gam)
gam.check(gam_mod_Qgw$gam)

acf(residuals(gam_mod_Qgw$gam))
pacf(residuals(gam_mod_Qgw$gam))

#plot the residuals
plot(gam_mod_Qgw$gam, residuals = TRUE, pch = 1)

## fit GAM using gamm() with a CAR(1)
car1_Qgw<- gamm(Q.gw~ s(julian.day,k=50), data = data,
                ccorrelation = corARMA(form = ~ julian.day,p=1),method='REML')

summary(car1_Qgw$gam)
set.seed(1)
gam.check(car1_Qgw$gam)

#plot the residuals
plot(car1_Qgw$gam, residuals = TRUE, pch = 1)

## fit GAM using gamm() with a CAR(2)
car2_Qgw<- gamm(Q.gw~ s(julian.day,k=50), data = data,
                ccorrelation = corARMA(form = ~ julian.day,p=2),method='REML')

summary(car2_Qgw$gam)
set.seed(1)
gam.check(car2_Qgw$gam)

## fit GAM using gamm() with a CAR(2)
car3_Qgw<- gamm(Q.gw~ s(julian.day,k=50), data = data,
                ccorrelation = corARMA(form = ~ julian.day,p=3),method='REML')

summary(car3_Qgw$gam)
set.seed(1)
gam.check(car3_Qgw$gam)

acf(residuals(car3_Qgw$gam))
pacf(residuals(car3_Qgw$gam))

anova(gam_mod_Qgw$lme,car1_Qgw$lme, car2_Qgw$lme,car3_Qgw$lme)
#AR(1) provides the best fit


##Take derivative to find points of change

############################plot GAM model output
N <- 300 # number of points at which to evaluate the smooth
x_label <- expression(Q~gw) #x axis label

## create new data to predict at; 200 evenly-spaced values over `Year`
newYear <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                  length.out = 200)))
## Predict from the fitted model; note we predict from the $gam part
newYear <- cbind(newYear,
                 data.frame(predict(car1_Qgw$gam, newYear, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(car1_Qgw$gam))
newYear <- transform(newYear,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))
## Plot estimated trend
Qgw_fitted <- ggplot(newYear, aes(x = julian.day, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = julian.day), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") +
  geom_point(data = data, mapping = aes(x = julian.day, y = Q.gw),
             inherit.aes = FALSE) +
  geom_line() +
  labs(y = x_label, x = "Julian day")
Qgw_fitted


## simulate data from model
set.seed(1) # set the random seed to make this reproducible
nsim <- 20 # how many simulations to draw
## do the simulations
sims <- simulate(car1_Qgw, nsim = nsim, newdata = newYear, unconditional = TRUE)

## rearrange the output into a long/tidy format
colnames(sims) <- paste0("sim", seq_len(nsim))
sims <- setNames(stack(as.data.frame(sims)), c("simulated", "run"))
sims <- transform(sims, julian.day = rep(newYear$julian.day, nsim),
                  simulated = simulated)

## Plot simulated trends
QgwSim.plt <- ggplot(newYear, aes(x = julian.day, y = fit)) +
  geom_line(data = sims,
            mapping = aes(y = simulated, x =julian.day, group = run),
            colour = "grey80") +
  geom_line(lwd = 1)

QgwSim.plt


#confidence and simultaneous intervals
#compute across-the-fuction and simultaneous confidence intervals
cint <- confint(car1_Qgw, parm = "julian.day", newdata = newYear,
                type = "confidence")

sint <- confint(car1_Qgw, parm = "Year", newdata = newYear,
                type = "simultaneous")
head(sw.sint)


###derivatives of estimated trend
Qgw.d <- fderiv(car1_Qgw, newdata = newYear, n = N)
Qgw.sint <- with(newYear,
                 cbind(confint(Qgw.d, nsim = nsim,
                               type = "simultaneous"),
                       julian.day = julian.day))
Qgw_deriv_plt <- ggplot(Qgw.sint, aes(x = julian.day, y = est)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  labs(x = "Julian day", y = "First derivative")

#plot derivatives
plot_grid(Qgw_deriv_plt, ncol = 1, labels = "auto",
          align = "hv", axis = "lr")

print(Qgw.sint)


###########################################################Downstream discharge diel magnitude############################################################################################

#####fit simple GAM with REML
gam_mod_Qamp<-gamm(Q.dwn.mag~ s(julian.day,k=120), data = data,method='REML',family=gaussian)

summary(gam_mod_Qamp$gam)
gam.check(gam_mod_Qamp$gam)

acf(residuals(gam_mod_Qamp$gam))
pacf(residuals(gam_mod_Qamp$gam))
#autocorrelation

## fit GAM using gamm() with CAR
## ...so fit the AR1
car1_Qamp <- gamm(Q.dwn.mag~ s(julian.day,k=120), data = data,
                  correlation = corARMA(form = ~ julian.day, p = 1),method='REML')

summary(car1_Qamp$gam)
set.seed(1)
gam.check(car1_Qamp$gam)

acf(residuals(car1_Qamp$gam))
pacf(residuals(car1_Qamp$gam))

## ...and fit the AR2
car2_Qamp<- gamm(Q.dwn.mag ~ s(julian.day, k = 100), data = data,
                 correlation = corARMA(form = ~ julian.day, p = 2),method='REML')
summary(car2_Qamp$gam)
gam.check(car2_Qamp$gam)

acf(residuals(car2_Qamp$gam))
pacf(residuals(car2_Qamp$gam))

#fit AR(3)
car3_Qamp<- gamm(Q.dwn.mag ~ s(julian.day, k = 100), data = data,
                 correlation = corARMA(form = ~ julian.day, p = 3),method='REML')
summary(car3_Qamp$gam)
gam.check(car3_Qamp$gam)

acf(residuals(car3_Qamp$gam))
pacf(residuals(car3_Qamp$gam))


anova(gam_mod_Qamp$lme,car1_Qamp$lme, car2_Qamp$lme,car3_Qamp$lme)
#CAR(1) provides the best fit



##Take derivative to find points of change

############################plot GAM model output
N <- 300 # number of points at which to evaluate the smooth
x_label <- expression(Q~magnitude) #x axis label

## create new data to predict at; 200 evenly-spaced values over `Year`
newYear <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                  length.out = 200)))

## Predict from the fitted model; note we predict from the $gam part
newYear <- cbind(newYear,
                 data.frame(predict(car1_Qamp$gam, newYear, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(car1_Qamp$gam))
newYear <- transform(newYear,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))
## Plot estimated trend
Qamp_fitted <- ggplot(newYear, aes(x = julian.day, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = julian.day), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") +
  geom_point(data = data, mapping = aes(x = julian.day, y = Q.dwn.mag),
             inherit.aes = FALSE) +
  geom_line() +
  labs(y = x_label, x = "Julian day")
Qamp_fitted



###derivatives of estimated trend
Qamp.d <- fderiv(car1_Qamp, newdata = newYear, n = N)
Qamp.sint <- with(newYear,
                  cbind(confint(Qamp.d, nsim = nsim,
                                type = "simultaneous"),
                        julian.day = julian.day))
Qamp_deriv_plt <- ggplot(Qamp.sint, aes(x = julian.day, y = est)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  labs(x = "Julian day CE", y = "First derivative")

#plot derivatives
plot_grid(Qamp_deriv_plt, ncol = 1, labels = "auto",
          align = "hv", axis = "lr")

print(Qamp.sint)


###########################################################Groundwater discharge diel magnitude#############################################################################


plot(data$julian.day,data$Q.gw.mag)

#####fit simple GAM with REML
gam_mod_Qgwamp<-gamm(Q.gw.mag~ s(julian.day,k=110), data = data,method='REML',family=gaussian)

summary(gam_mod_Qgwamp$gam)
gam.check(gam_mod_Qgwamp$gam)

acf(residuals(gam_mod_Qgwamp$gam))
pacf(residuals(gam_mod_Qgwamp$gam))
#autocorrelation

## fit GAM using gamm() with CAR
## ...so fit the AR1
car1_Qgwamp <- gamm(Q.gw.mag~ s(julian.day,k=110), data = data,
                    correlation = corARMA(form = ~ julian.day, p = 1),method='REML')

summary(car1_Qgwamp$gam)
set.seed(1)
gam.check(car1_Qgwamp$gam)

acf(residuals(car1_Qgwamp$gam))
pacf(residuals(car1_Qgwamp$gam))

## ...and fit the AR2
car2_Qgwamp<- gamm(Q.gw.mag ~ s(julian.day, k = 100), data = data,
                   correlation = corARMA(form = ~ julian.day, p = 2),method='REML')
summary(car2_Qgwamp$gam)
gam.check(car2_Qgwamp$gam)

acf(residuals(car2_Qgwamp$gam))
pacf(residuals(car2_Qgwamp$gam))

#fit AR(3)
car3_Qgwamp<- gamm(Q.gw.mag ~ s(julian.day, k = 100), data = data,
                   correlation = corARMA(form = ~ julian.day, p = 3),method='REML')
summary(car3_Qgwamp$gam)
gam.check(car3_Qgwamp$gam)

acf(residuals(car3_Qgwamp$gam))
pacf(residuals(car3_Qgwamp$gam))


anova(gam_mod_Qgwamp$lme,car1_Qgwamp$lme, car2_Qgwamp$lme,car3_Qgwamp$lme)
#AR(1) provides the best fit


############################plot GAM model output
N <- 300 # number of points at which to evaluate the smooth

## create new data to predict at; 200 evenly-spaced values over `julian.day`
newYear <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                  length.out = 200)))

## Predict from the fitted model; note we predict from the $gam part
newYear <- cbind(newYear,
                 data.frame(predict(car1_Qgwamp$gam, newYear, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(car1_Qgwamp$gam))
newYear <- transform(newYear,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))
## Plot estimated trend
Qgwamp_fitted <- ggplot(newYear, aes(x = julian.day, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = julian.day), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") +
  geom_point(data = data, mapping = aes(x = julian.day, y = Q.gw.mag),
             inherit.aes = FALSE) +
  geom_line() +
  labs(y = "Qgw amp", x = "Julian day")
Qgwamp_fitted


###derivatives of estimated trend
Qgwamp.d <- fderiv(car1_Qgwamp, newdata = newYear, n = N)
Qgwamp.sint <- with(newYear,
                    cbind(confint(Qgwamp.d, nsim = nsim,
                                  type = "simultaneous"),
                          julian.day = julian.day))
Qgwamp_deriv_plt <- ggplot(Qgwamp.sint, aes(x = julian.day, y = est)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  labs(x = "Julian day", y = "First derivative")

#plot derivatives
plot_grid(Qgwamp_deriv_plt, ncol = 1, labels = "auto",
          align = "hv", axis = "lr")

print(Qgwamp.sint)



###########################################################Downstream discharge diel minimum timing#############################################################################
head(data)
dim(data)

plot(data$julian.day,data$Q.dwn.min)

#####fit simple GAM with REML
gam_mod_Qmin<-gamm(Q.dwn.min~ s(julian.day,k=100), data = data,method='REML',family=gaussian)

summary(gam_mod_Qmin$gam)
gam.check(gam_mod_Qmin$gam)

acf(residuals(gam_mod_Qmin$gam))
pacf(residuals(gam_mod_Qmin$gam))
#autocorrelation

## fit GAM using gamm() with CAR
## ...so fit the AR1
car1_Qmin <- gamm(Q.dwn.min~ s(julian.day,k=100), data = data,
                  correlation = corARMA(form = ~ julian.day, p = 1),method='REML')

summary(car1_Qmin$gam)
set.seed(1)
gam.check(car1_Qmin$gam)

acf(residuals(car1_Qmin$gam))
pacf(residuals(car1_Qmin$gam))

## ...and fit the AR2
car2_Qmin<- gamm(Q.dwn.min ~ s(julian.day, k = 100), data = data,
                 correlation = corARMA(form = ~ julian.day, p = 2),method='REML')
summary(car2_Qmin$gam)
gam.check(car2_Qmin$gam)

acf(residuals(car2_Qmin$gam))
pacf(residuals(car2_Qmin$gam))

#fit AR(3)
car3_Qmin<- gamm(Q.dwn.min ~ s(julian.day, k = 100), data = data,
                 correlation = corARMA(form = ~ julian.day, p = 3),method='REML')
summary(car3_Qmin$gam)
gam.check(car3_Qmin$gam)

acf(residuals(car3_Qmin$gam))
pacf(residuals(car3_Qmin$gam))


anova(gam_mod_Qmin$lme,car1_Qmin$lme, car2_Qmin$lme,car3_Qmin$lme)
#Gam without AR provides the best fit












#############################################################Daily dissolved oxygen concentration##########################################################

###############fit simple GAM
gam_mod_DO<- gamm(DO.dwn ~ s(julian.day,k=42), data = data,family=gaussian,method = "REML")

summary(gam_mod_DO$gam)
gam.check(gam_mod_DO$gam)


## fit GAM using gamm() with a CAR(1)
car1_DO<- gamm(DO.dwn~ s(julian.day,k=42), data = data,
               correlation = corARMA(form = ~ julian.day, p = 1),method='REML')

summary(car1_DO$gam)
set.seed(1)
gam.check(car1_DO$gam)

acf(residuals(car1_DO$gam))
pacf(residuals(car1_DO$gam))



## ...and fit the AR2
car2_DO<- gamm(DO.dwn ~ s(julian.day, k = 42), data = data,
               correlation = corARMA(form = ~ julian.day, p = 2),method='REML')
summary(car2_DO$gam)
gam.check(car2_DO$gam)

acf(residuals(car2_DO$gam))
pacf(residuals(car2_DO$gam))

#fit AR(3)
car3_DO<- gamm(DO.dwn~ s(julian.day, k = 42), data = data,
               correlation = corARMA(form = ~ julian.day, p = 3),method='REML')
summary(car3_DO$gam)
gam.check(car3_DO$gam)

acf(residuals(car3_DO$gam))
pacf(residuals(car3_DO$gam))


anova(gam_mod_DO$lme,car1_DO$lme, car2_DO$lme,car3_DO$lme)
#REML without CAR provides the best fit


##Take derivative to find points of change

############################plot GAM model output
N <- 300 # number of points at which to evaluate the smooth
x_label <- expression(DO~concentration) #x axis label

## create new data to predict at; 200 evenly-spaced values over `Year`
newYear <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                  length.out = 200)))
## Predict from the fitted model; note we predict from the $gam part
newYear <- cbind(newYear,
                 data.frame(predict(car1_DO, newYear, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(car1_DO))
newYear <- transform(newYear,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))
## Plot estimated trend
small_fitted <- ggplot(newYear, aes(x = julian.day, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = julian.day), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") +
  geom_point(data = data, mapping = aes(x = julian.day, y = DO.dwn),
             inherit.aes = FALSE) +
  geom_line() +
  labs(y = x_label, x = "Julian day")
small_fitted




## simulate data from model
set.seed(1) # set the random seed to make this reproducible
nsim <- 20 # how many simulations to draw
## do the simulations
sims <- simulate(car1_DO, nsim = nsim, newdata = newYear, unconditional = TRUE)

## rearrange the output into a long/tidy format
colnames(sims) <- paste0("sim", seq_len(nsim))
sims <- setNames(stack(as.data.frame(sims)), c("simulated", "run"))
sims <- transform(sims, julian.day = rep(newYear$julian.day, nsim),
                  simulated = simulated)


## Plot simulated trends
smallSim.plt <- ggplot(newYear, aes(x = julian.day, y = fit)) +
  geom_line(data = sims,
            mapping = aes(y = simulated, x = julian.day, group = run),
            colour = "grey80") +
  geom_line(lwd = 1)

smallSim.plt



#confidence and simultaneous intervals
#compute across-the-fuction and simultaneous confidence intervals
cint <- confint(car1_DO, parm = "julian.day", newdata = newYear,
                type = "confidence")

sint <- confint(car1_DO, parm = "Year", newdata = newYear,
                type = "simultaneous")
head(sw.sint)




###derivatives of estimated trend
small.d <- fderiv(car1_DO, newdata = newYear, n = N)
small.sint <- with(newYear,
                   cbind(confint(small.d, nsim = nsim,
                                 type = "simultaneous"),
                         julian.day = julian.day))
small_deriv_plt <- ggplot(small.sint, aes(x = julian.day, y = est)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  labs(x = "Julian day CE", y = "First derivative")

#plot derivatives
plot_grid(small_deriv_plt, ncol = 1, labels = "auto",
          align = "hv", axis = "lr")

print(small.sint)


#######################################Daily NO3-N concentration###########################################################################################################

###############fit simple GAM
gam_mod_nitrate<- gam(nitrate.dwn ~ s(julian.day,k=42), data = data,method = "REML")

summary(gam_mod_nitrate$gam)
gam.check(gam_mod_nitrate$gam)

acf(residuals(gam_mod_nitrate$gam))
pacf(residuals(gam_mod_nitrate$gam))

## fit GAM using gamm() with a CAR(1)
car1_nitrate<- gamm(nitrate.dwn ~ s(julian.day, k = 42), data = data,
                    correlation = corARMA(form = ~ julian.day, p = 1),method='REML')

summary(car1_nitrate$gam)
set.seed(1)
gam.check(car1_nitrate$gam)

acf(residuals(car1_nitrate$gam))
pacf(residuals(car1_nitrate$gam))

## ...and fit the AR2
car2_nitrate<- gamm(nitrate.dwn ~ s(julian.day, k = 42), data = data,
                    correlation = corARMA(form = ~ julian.day, p = 2),method='REML')
summary(car2_nitrate$gam)
gam.check(car2_nitrate$gam)

acf(residuals(car2_nitrate$gam))
pacf(residuals(car2_nitrate$gam))

#fit AR(3)
car3_nitrate<- gamm(nitrate.dwn~ s(julian.day, k = 42), data = data,
                    correlation = corARMA(form = ~ julian.day, p = 3),method='REML')
summary(car3_nitrate$gam)
gam.check(car3_nitrate$gam)

acf(residuals(car3_nitrate$gam))
pacf(residuals(car3_nitrate$gam))


anova(gam_mod_nitrate$lme,car1_nitrate$lme, car2_nitrate$lme,car3_nitrate$lme)
#use REML; something odd happening with CAAR models (EDF =1??)


##Take derivative to find points of change

############################plot GAM model output
N <- 300 # number of points at which to evaluate the smooth
x_label <- expression(nitrate~concentration) #x axis label

## create new data to predict at; 200 evenly-spaced values over `Year`
newYear <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                  length.out = 200)))
## Predict from the fitted model; note we predict from the $gam part
newYear <- cbind(newYear,
                 data.frame(predict(gam_mod_nitrate, newYear, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(gam_mod_nitrate))
newYear <- transform(newYear,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))
## Plot estimated trend
small_fitted <- ggplot(newYear, aes(x = julian.day, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = julian.day), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") +
  geom_point(data = data, mapping = aes(x = julian.day, y = nitrate.dwn),
             inherit.aes = FALSE) +
  geom_line() +
  labs(y = x_label, x = "Julian day")
small_fitted




## simulate data from model
set.seed(1) # set the random seed to make this reproducible
nsim <- 20 # how many simulations to draw
## do the simulations
sims <- simulate(gam_mod_nitrate, nsim = nsim, newdata = newYear, unconditional = TRUE)

## rearrange the output into a long/tidy format
colnames(sims) <- paste0("sim", seq_len(nsim))
sims <- setNames(stack(as.data.frame(sims)), c("simulated", "run"))
sims <- transform(sims, Year = rep(newYear$julian.day, nsim),
                  simulated = simulated)




#confidence and simultaneous intervals
#compute across-the-fuction and simultaneous confidence intervals
cint <- confint(gam_mod_nitrate, parm = "julian.day", newdata = newYear,
                type = "confidence")

sint <- confint(gam_mod_nitrate, parm = "Year", newdata = newYear,
                type = "simultaneous")
head(sw.sint)




###derivatives of estimated trend
small.d <- fderiv(gam_mod_nitrate, newdata = newYear, n = N)
small.sint <- with(newYear,
                   cbind(confint(small.d, nsim = nsim,
                                 type = "simultaneous"),
                         julian.day = julian.day))
small_deriv_plt <- ggplot(small.sint, aes(x = julian.day, y = est)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  labs(x = "Julian day CE", y = "First derivative")

#plot derivatives
plot_grid(small_deriv_plt, ncol = 1, labels = "auto",
          align = "hv", axis = "lr")

print(small.sint)




###########################################################GPP#############################################################################


#####fit simple GAM with REML
gam_mod_GPP<- gamm(GPP~ s(julian.day,k=10), data = data,method='REML')

summary(gam_mod$gam)
gam.check(gam_mod$gam)

#check for residual aoutocorrelation
acf(residuals(gam_mod$gam))
pacf(residuals(gam_mod$gam))
#residual autocorrelation not present


## fit GAM using gamm() with a CAR(1)
car1_GPP<- gamm(GPP~ s(julian.day,k=10), data = data,
                correlation = corCAR1(form = ~ julian.day),method='REML')

summary(car1_GPP$gam)
set.seed(1)
gam.check(car1_GPP$gam)

acf(residuals(car1_GPP$gam))
pacf(residuals(car1_GPP$gam))

## ...and fit the AR2
car2_GPP<- gamm(GPP~ s(julian.day, k =10), data = data,
                correlation = corARMA(form = ~ julian.day, p = 2),method='REML')
summary(car2_GPP$gam)
gam.check(car2_GPP$gam)

acf(residuals(car2_GPP$gam))
pacf(residuals(car2_GPP$gam))


#fit AR(3)
car3_GPP<- gamm(GPP ~ s(julian.day, k = 10), data = data,
                correlation = corARMA(form = ~ julian.day, p = 3),method='REML')
summary(car3_GPP$gam)
gam.check(car3_GPP$gam)

acf(residuals(car3_GPP$gam))
pacf(residuals(car3_GPP$gam))


anova(gam_mod_GPP$lme,car1_GPP$lme, car2_GPP$lme,car3_GPP$lme)
# AR(1) provides best fit



############################plot GAM model output
N <- 300 # number of points at which to evaluate the smooth

## create new data to predict at; 200 evenly-spaced values over `Year`
newYear <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                  length.out = 200)))
## Predict from the fitted model; note we predict from the $gam part
newYear <- cbind(newYear,
                 data.frame(predict(car1_GPP$gam, newYear, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(car1_GPP$gam))
newYear <- transform(newYear,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))
## Plot estimated trend
GPP_fitted <- ggplot(newYear, aes(x = julian.day, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = julian.day), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") +
  geom_point(data = data, mapping = aes(x = julian.day, y =GPP),
             inherit.aes = FALSE) +
  geom_line() +
  labs(y = "GPP", x = "Julian day")
GPP_fitted

head(newYear)

## simulate data from model
set.seed(1) # set the random seed to make this reproducible
nsim <- 20 # how many simulations to draw
## do the simulations
sims<- simulate(car1_GPP, nsim = nsim, newdata = newYear, unconditional = TRUE)

## rearrange the output into a long/tidy format
colnames(sims) <- paste0("sim", seq_len(nsim))
sims <- setNames(stack(as.data.frame(sims)), c("simulated", "run"))
sims <- transform(sims, julian.day = rep(newYear$julian.day, nsim),
                  simulated = simulated)


#confidence and simultaneous intervals
#compute across-the-fuction and simultaneous confidence intervals
cint <- confint(car1_GPP, parm = "julian.day", newdata = newYear,
                type = "confidence")

sint <- confint(car1_GPP, parm = "julian.day", newdata = newYear,
                type = "simultaneous")



###derivatives of estimated trend
GPP.d <- fderiv(car1_GPP, newdata = newYear, n = N)
GPP.sint <- with(newYear,
                 cbind(confint(GPP.d, nsim = nsim,
                               type = "simultaneous"),
                       julian.day = julian.day))
GPP_deriv_plt <- ggplot(GPP.sint, aes(x = julian.day, y = est)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  labs(x = "Julian day CE", y = "First derivative")

#plot derivatives
plot_grid(GPP_deriv_plt, ncol = 1, labels = "auto",
          align = "hv", axis = "lr")

print(GPP.sint)

#################################################################ER########################################################################

#########################################################fit simple GAM
gam_mod_ER <- gamm(ER~ s(julian.day,k=120), data = data,method='REML')

summary(gam_mod_ER$gam)
gam.check(gam_mod_ER$gam)


acf(residuals(gam_mod_ER$gam))
pacf(residuals(gam_mod_ER$gam))

## fit GAM using gamm() with CAR
## ...so fit the AR1
car1_ER <- gamm(ER ~ s(julian.day, k = 120), data = data,
                correlation = corARMA(form = ~ julian.day, p = 1),method='REML')

summary(car1_ER$gam)
set.seed(1)
gam.check(car1_ER$gam)

acf(residuals(car1_ER$gam))
pacf(residuals(car1_ER$gam))

## ...and fit the AR2
car2_ER <- gamm(ER ~ s(julian.day, k = 120), data = data,
                correlation = corARMA(form = ~ julian.day, p = 2),method='REML')
summary(car2_ER$gam)
gam.check(car2_ER$gam)

acf(residuals(car2_ER$gam))
pacf(residuals(car2_ER$gam))

#fit AR(3)
car3_ER<- gamm(ER ~ s(julian.day, k = 120), data = data,
               correlation = corARMA(form = ~ julian.day, p = 3),method='REML')
summary(car3_ER$gam)
gam.check(car3_ER$gam)

acf(residuals(car3_ER$gam))
pacf(residuals(car3_ER$gam))


anova(gam_mod_ER$lme,car1_ER$lme, car2_ER$lme,car3_ER$lme)
#AR1 provides best fit



############################plot GAM model output
N <- 300 # number of points at which to evaluate the smooth

## create new data to predict at; 200 evenly-spaced values over `Year`
newYear <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                  length.out = 200)))
## Predict from the fitted model; note we predict from the $gam part
newYear <- cbind(newYear,
                 data.frame(predict(car1_ER$gam, newYear, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(car1_ER$gam))
newYear <- transform(newYear,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))
## Plot estimated trend
ER_fitted <- ggplot(newYear, aes(x = julian.day, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = julian.day), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") +
  geom_point(data = data, mapping = aes(x = julian.day, y =ER),
             inherit.aes = FALSE) +
  geom_line() +
  labs(y = "ER", x = "Julian day")
ER_fitted




## simulate data from model
set.seed(1) # set the random seed to make this reproducible
nsim <- 20 # how many simulations to draw
## do the simulations
sims <- simulate(gam_mod_ER, nsim = nsim, newdata = newYear, unconditional = TRUE)

## rearrange the output into a long/tidy format
colnames(sims) <- paste0("sim", seq_len(nsim))
sims <- setNames(stack(as.data.frame(sims)), c("simulated", "run"))
sims <- transform(sims, julian.day = rep(newYear$julian.day, nsim),
                  simulated = simulated)


#confidence and simultaneous intervals
#compute across-the-fuction and simultaneous confidence intervals
cint <- confint(car1_ER, parm = "julian.day", newdata = newYear,
                type = "confidence")

sint <- confint(car1_ER, parm = "julian.day", newdata = newYear,
                type = "simultaneous")



###derivatives of estimated trend
ER.d <- fderiv(car1_ER, newdata = newYear, n = N)
ER.sint <- with(newYear,
                cbind(confint(ER.d, nsim = nsim,
                              type = "simultaneous"),
                      julian.day = julian.day))
ER_deriv_plt <- ggplot(ER.sint, aes(x = julian.day, y = est)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  labs(x = "Julian day", y = "First derivative")

#plot derivatives
plot_grid(ER_deriv_plt, ncol = 1, labels = "auto",
          align = "hv", axis = "lr")

print(ER.sint)


#########################################Bayesian assimilatory autotrophic NO3-N uptake##############################################################################################################


#########################################################fit simple GAM
gam_mod_Nup<- gamm(Nupbayes~ s(julian.day,k=110), data = data,method='REML')

summary(gam_mod_Nup$gam)
gam.check(gam_mod_Nup$gam)


acf(residuals(gam_mod_Nup$gam))
pacf(residuals(gam_mod_Nup$gam))

## fit GAM using gamm() with CAR
## ...so fit the AR1
car1_Nup<- gamm(Nupbayes~ s(julian.day, k = 110), data = data,
                correlation = corARMA(form = ~ julian.day, p = 1),method='REML')

summary(car1_Nup$gam)
set.seed(1)
gam.check(car1_Nup$gam)

acf(residuals(car1_Nup$gam))
pacf(residuals(car1_Nup$gam))

## ...and fit the AR2
car2_Nup<- gamm(Nupbayes ~ s(julian.day, k = 110), data = data,
                correlation = corARMA(form = ~ julian.day, p = 2),method='REML')
summary(car2_Nup$gam)
gam.check(car2_Nup$gam)

acf(residuals(car2_Nup$gam))
pacf(residuals(car2_Nup$gam))

#fit AR(3)
car3_Nup<- gamm(Nupbayes ~ s(julian.day, k = 110), data = data,
                correlation = corARMA(form = ~ julian.day, p = 3),method='REML')
summary(car3_Nup$gam)
gam.check(car3_Nup$gam)

acf(residuals(car3_Nup$gam))
pacf(residuals(car3_Nup$gam))


anova(gam_mod_Nup$lme,car1_Nup$lme, car2_Nup$lme,car3_Nup$lme)
#AR1 provides best fit



############################plot GAM model output
N <- 300 # number of points at which to evaluate the smooth

## create new data to predict at; 200 evenly-spaced values over `Year`
newYear <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                  length.out = 200)))
## Predict from the fitted model; note we predict from the $gam part
newYear <- cbind(newYear,
                 data.frame(predict(car1_Nup$gam, newYear, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(car1_Nup$gam))
newYear <- transform(newYear,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))
## Plot estimated trend
Nup_fitted <- ggplot(newYear, aes(x = julian.day, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = julian.day), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") +
  geom_point(data = data, mapping = aes(x = julian.day, y =Nupbayes),
             inherit.aes = FALSE) +
  geom_line() +
  labs(y = "Nitrate uptake", x = "Julian day")
Nup_fitted



## simulate data from model
set.seed(1) # set the random seed to make this reproducible
nsim <- 20 # how many simulations to draw
## do the simulations
sims <- simulate(car1_Nup, nsim = nsim, newdata = newYear, unconditional = TRUE)

## rearrange the output into a long/tidy format
colnames(sims) <- paste0("sim", seq_len(nsim))
sims <- setNames(stack(as.data.frame(sims)), c("simulated", "run"))
sims <- transform(sims, julian.day = rep(newYear$julian.day, nsim),
                  simulated = simulated)


#confidence and simultaneous intervals
#compute across-the-fuction and simultaneous confidence intervals
cint <- confint(car1_Nup, parm = "julian.day", newdata = newYear,
                type = "confidence")

sint <- confint(car1_Nup, parm = "julian.day", newdata = newYear,
                type = "simultaneous")



###derivatives of estimated trend
Nup.d <- fderiv(car1_Nup, newdata = newYear, n = N)
Nup.sint <- with(newYear,
                 cbind(confint(Nup.d, nsim = nsim,
                               type = "simultaneous"),
                       julian.day = julian.day))
Nup_deriv_plt <- ggplot(Nup.sint, aes(x = julian.day, y = est)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  labs(x = "Julian day", y = "First derivative")

#plot derivatives
plot_grid(Nup_deriv_plt, ncol = 1, labels = "auto",
          align = "hv", axis = "lr")

print(Nup.sint)

############################2) Assess relationships among variables using univariate and multivariate predictors######################################################

############Daily nitrate############

head(daily)

#Try one with predictors expected to affect daily nitrate conc
nitrate_mod1 <- gam(nitrate.dwn~ s(temp.dwn) + s(Q.dwn)+s(PAR.NLDAS)+s(DO.dwn)+s(nitrate.mag)+s(Q.gw),
                    data = data, method = "REML")
summary(nitrate_mod1)
gam.check(nitrate_mod1)


#Remove light,nitrate, mag, and Q gw
nitrate_mod2 <- gam(nitrate.dwn~ s(temp.dwn) + s(Q.dwn)+s(DO.dwn),
                    data = data, method = "REML")
summary(nitrate_mod2)
gam.check(nitrate_mod2)
plot(nitrate_mod2)

#remove DO.dwn
nitrate_mod3 <- gam(nitrate.dwn~ s(temp.dwn) + s(Q.dwn),
                    data = data, method = "REML")
summary(nitrate_mod3)
gam.check(nitrate_mod3)
plot(nitrate_mod3)



###############GPP##############
head(data)

#Try one with predictors expected to affect GPP
GPP_mod1 <- gam(GPP~ s(temp.dwn) + s(Q.dwn)+s(PAR.NLDAS)+s(Nupbayes),
                data = data, method = "REML")
summary(GPP_mod1)
gam.check(GPP_mod1$gam)


#Remove N uptake
GPP_mod2 <- gam(GPP~ s(temp.dwn) + s(Q.dwn)+s(PAR.NLDAS),
                data = data, method = "REML")
summary(GPP_mod2)
gam.check(GPP_mod2)


###############ER########
head(data)

#Try one with predictors expected to affect ER
ER_mod1 <- gam(ER~ s(temp.dwn) + s(Q.dwn)+s(GPP),
               data = data, method = "REML")
summary(ER_mod1)
gam.check(ER_mod1)

plot(ER_mod1$gam)

#Remove temp
ER_mod2 <- gam(ER~ s(Q.dwn) +s(GPP),
               data = data, method = "REML")
summary(ER_mod2)
gam.check(ER_mod2)

plot(ER_mod2$gam)

#remove Gpp
ER_mod3 <- gam(ER~ s(temp.dwn) + s(Q.dwn),
               data = data, method = "REML")
summary(ER_mod3)
gam.check(ER_mod2)

#just K600
ER_mod4 <- gam(ER~ s(k600),
               data = data, method = "REML")
summary(ER_mod4)
plot(data$k600,data$ER)

head(data)


##############Bayesian NO3-N uptake#######
head(data)

#Try one with all expected predictors 
Nup_mod1 <- gam(Nupbayes~ s(temp.dwn) + s(Q.dwn)+s(GPP)+s(PAR.NLDAS)+s(ER),
                data = data, method = "REML")
summary(Nup_mod1)
gam.check(Nup_mod1)

#Remove ER
Nup_mod2 <- gam(Nupbayes~ s(temp.dwn) + s(Q.dwn)+s(PAR.NLDAS)+s(GPP),
                data = data, method = "REML")
summary(Nup_mod2)
gam.check(Nup_mod2)


#remove GPP
Nup_mod3 <- gam(Nupbayes~ s(temp.dwn) + s(Q.dwn)+s(PAR.NLDAS),
                data = data, method = "REML")
summary(Nup_mod3)


plot(data$PAR.NLDAS,data$Nupbayes)

##only GPP
Nup_mod4 <- gam(Nupbayes~ s(GPP),
                data = data, method = "REML")
summary(Nup_mod4)


########Diel NO3-N concentration minimum timing######
head(data)

#Try univariate with diel DO max timing
Nitratemin_mod1 <- gam(nitrate.min~ s(DO.max),
                       data = data, method = "REML")
summary(Nitratemin_mod1)
gam.check(Nitratemin_mod1)
#DO diel magnitude doesn't predict nitrate diel magnitude

#Try univariate with diel corrected DO magnitude
Nitratemin_mod2 <- gam(nitrate.min~ s(cor.DO.max),
                       data = data, method = "REML")
summary(Nitratemin_mod2)
gam.check(Nitratemin_mod2)
#DO diel magnitude doesn't predict nitrate diel magnitude


##ry univariate with diel Q max timing
Nitratemin_mod2 <- gam(nitrate.min~ s(Q.dwn),
                       data = data, method = "REML")
summary(Nitratemin_mod2)
gam.check(Nitratemin_mod2)


###########Lag time between diel NO3-N minimum and DO maximum########

#benthic footprint diff?
head(data)
lag_mod1 <- gam(Domax.nitmin~ s(footprint.dif),
                data = data, method = "REML")
summary(lag_mod1)
gam.check(lag_mod1)
plot(data$footprint.dif,data$Domax.nitmin)


#benthic footprint diff?
head(data)
lag_mod2 <- gam(corDOmax.nitratemin~ s(footprint.dif),
                data = data, method = "REML")
summary(lag_mod2)
gam.check(lag_mod2)


###########Diel NO3-N concentration magnitude#######
head(data)

#Try univariate with diel DO magnitude
Nitrateamp_mod1 <- gam(nitrate.mag~ s(DO.mag),
                       data = data, method = "REML")
summary(Nitrateamp_mod1)
gam.check(Nitrateamp_mod1)
#DO diel magnitude doesn't predict nitrate diel magnitude

#Try univariate with diel corrected DO magnitude
Nitrateamp_mod2<- gam(nitrate.mag~ s(cor.DO.mag),
                      data = data, method = "REML")
summary(Nitrateamp_mod2)
gam.check(Nitrateamp_mod2)
#DO diel magnitude doesn't predict nitrate diel magnitude


#try other predictors
Nitrateamp_mod2 <- gam(nitrate.mag~ s(Q.dwn.mag),
                       data = data, method = "REML")
summary(Nitrateamp_mod2)
gam.check(Nitrateamp_mod2)

