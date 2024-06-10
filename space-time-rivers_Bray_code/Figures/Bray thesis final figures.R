library("mgcv")
library("scam")
library("ggplot2")
library("cowplot")
library("tidyr")
library("gratia")


#####################################################################Fig.1: surface water Q######################################################

data <- read.csv("Bray_Miller_Creek_daily.csv",head=T,sep=",") # load csv data

head(data)
dim(data) 
data$date<-as.Date(data$julian.day, origin=as.Date("2018-12-31"))
head(data$date)

#####fit simple GAM with REML
gam_mod_Q<-gamm(Q.dwn~ s(julian.day,k=100), data = data,method='REML')

summary(gam_mod_Q$gam)
gam.check(gam_mod_Q$gam)

acf(residuals(gam_mod_Q$gam))
pacf(residuals(gam_mod_Q$gam))
#autocorrelation

## fit GAM using gamm() with CAR
## ...so fit the AR1
car1_Q <- gamm(Q.dwn~ s(julian.day,k=100), data = data,
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
N <- 126 # number of points at which to evaluate the smooth
length(data$julian.day)


## create new data to predict at; 200 evenly-spaced values over `Year`
newYear <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                  length.out = 126)))
## Predict from the fitted model; note we predict from the $gam part
newYear <- cbind(newYear,
                 data.frame(predict(car1_Q$gam, newYear, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(car1_Q$gam))
newYear <- transform(newYear,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))

head(newYear)
newYear$date<-as.Date(newYear$julian.day, origin=as.Date("2018-12-31"))
head(newYear)
head(data)



## Plot estimated trend
Q_fitted <- ggplot(newYear, aes(x = date, y = fit)) +
  geom_ribbon(data=newYear,aes(ymin = lower, ymax = upper, x = date), alpha = 0.2,
              fill = "black") +
  geom_point(data = data, aes(x = date, y = Q.dwn),fill="white",color="black",shape=21,size=2) +
  geom_line(size=.5) +
  ylab(bquote('Q (L'~s^-1*')')) +   
  xlab(bquote("Date")) +
  scale_y_continuous(expand = c(0,0),limits=c(0,400),breaks = seq(0,400, by = 100)) + 
  scale_x_date(expand = c(0,0),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-14"),by="14 days")) +
  theme(axis.text.x=element_blank(),axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(1,1,-0.2,1), "cm")) + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"),
        legend.title = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="a")+theme(plot.tag.position = 'topleft',
                                                                                             plot.tag = element_text(vjust = -3.5, hjust = 0))
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

print(Q.sint)


Q.sint$date<-as.Date(newYear$julian.day, origin=as.Date("2018-12-31"))
head(Q.sint)



head(Q.sint)


Q_deriv_plt <- ggplot(Q.sint, aes(x = date, y = est)) +
  geom_hline(yintercept=0,linetype="dashed")+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  ggtitle("") +
  ylab(bquote('Q derivative (L'~s^-1*~d^-1*')')) +   
  xlab(bquote("Date")) +
  scale_x_date(expand = c(0,0),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-14"),by="14 days")) +
  scale_y_continuous(expand = c(0,0),limits=c(-15,5),breaks = seq(-15,5, by = 5)) + 
  ggtitle("") +
  theme(axis.text.x=element_text(size=11,angle=45,color="black", vjust=1, hjust=1), axis.title.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(-0.2,1,1,1), "cm")) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"))
Q_deriv_plt<-Q_deriv_plt+ theme(legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="b")+theme(plot.tag.position = 'topleft',
                                                                                                                                                     plot.tag = element_text(vjust = -3.5, hjust = 0))
Q_deriv_plt


print(Q.sint)


##convert to grob object and export figure
g1<- ggplotGrob(Q_fitted)
g2<- ggplotGrob(Q_deriv_plt)

grid::grid.newpage()
grid::grid.draw(rbind(g1,g2))


pdf(file="Bray thesis final Fig.1.pdf",paper="special",width=6.5,height=5.5)
grid::grid.newpage()
grid::grid.draw(rbind(g1,g2))
dev.off()


jpeg(file="Bray thesis final Fig.1.jpeg",width=6.5,height=5.5,units="in",res=1200)
grid::grid.newpage()
grid::grid.draw(rbind(g1,g2))
dev.off()


###########################################################Fig.2: metabolism##########################################################################

##############################################GPP


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


set.seed(1)
############################plot GAM model output
N <- 126 # number of points at which to evaluate the smooth

## create new data to predict at; 200 evenly-spaced values over `Year`
newYear <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                  length.out = 126)))
## Predict from the fitted model; note we predict from the $gam part
newYear <- cbind(newYear,
                 data.frame(predict(car1_GPP$gam, newYear, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(car1_GPP$gam))
newYear <- transform(newYear,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))

head(newYear)
newYear$date<-as.Date(newYear$julian.day, origin=as.Date("2018-12-31"))
head(newYear)

## Plot estimated trend
GPP_fitted <- ggplot(newYear, aes(x = date, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = date), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") +
  geom_point(data = data, aes(x = date, y = GPP),fill="white",color="black",shape=21,size=2) +
  geom_line(size=.5) +
  ylab(bquote('GPP (g'~'O'[2]~m^-2*~d^-1*')')) +   
  xlab(bquote("")) +
  scale_x_date(expand = c(0,0),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-14"),by="14 days")) +
  theme(axis.text.x=element_blank())+
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(1,1,-0.2,1), "cm")) + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"),
        legend.title = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="a")+theme(plot.tag.position = 'topleft',
                                                                                             plot.tag = element_text(vjust = -3.5, hjust = 0))
GPP_fitted



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

GPP.sint$date<-as.Date(newYear$julian.day, origin=as.Date("2018-12-31"))


GPP_deriv_plt <- ggplot(GPP.sint, aes(x = date, y = est)) +
  geom_hline(yintercept=0,linetype="dashed")+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  ggtitle("") +
  ylab(bquote('GPP derivative g'~'O'[2]~m^-2*~d^-2*')')) +   
  xlab(bquote("")) +
  scale_x_date(expand = c(0,0),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-14"),by="14 days")) +
  ggtitle("") +
  scale_y_continuous(expand = c(0,0),limits=c(-0.03,0.02),breaks = seq(-0.03,0.02, by = 0.01)) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(-0.2,1,-0.2,1), "cm")) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"))
GPP_deriv_plt<-GPP_deriv_plt+ theme(legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="b")+theme(plot.tag.position = 'topleft',
                                                                                                                                                         plot.tag = element_text(vjust = -3.5, hjust = 0))
GPP_deriv_plt


print(GPP.sint)



#######################################ER


#####fit simple GAM with REML
gam_mod_ER<- gamm(ER~ s(julian.day,k=10), data = data,method='REML')

summary(gam_mod_ER$gam)
gam.check(gam_mod_ER$gam)

#check for residual aoutocorrelation
acf(residuals(gam_mod_ER$gam))
pacf(residuals(gam_mod_ER$gam))
#residual autocorrelation not present


## fit GAM using gamm() with a CAR(1)
car1_ER<- gamm(ER~ s(julian.day,k=10), data = data,
               correlation = corCAR1(form = ~ julian.day),method='REML')

summary(car1_ER$gam)
set.seed(1)
gam.check(car1_ER$gam)

acf(residuals(car1_ER$gam))
pacf(residuals(car1_ER$gam))

## ...and fit the AR2
car2_ER<- gamm(ER~ s(julian.day, k =10), data = data,
               correlation = corARMA(form = ~ julian.day, p = 2),method='REML')
summary(car2_ER$gam)
gam.check(car2_ER$gam)

acf(residuals(car2_ER$gam))
pacf(residuals(car2_ER$gam))


#fit AR(3)
car3_ER<- gamm(ER ~ s(julian.day, k = 10), data = data,
               correlation = corARMA(form = ~ julian.day, p = 3),method='REML')
summary(car3_ER$gam)
gam.check(car3_ER$gam)

acf(residuals(car3_ER$gam))
pacf(residuals(car3_ER$gam))


anova(gam_mod_ER$lme,car1_ER$lme, car2_ER$lme,car3_ER$lme)
# AR(1) provides best fit



############################plot GAM model output
set.seed(1)
N <- 126 # number of points at which to evaluate the smooth

## create new data to predict at; 200 evenly-spaced values over `Year`
newYear2 <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                   length.out = 126)))
## Predict from the fitted model; note we predict from the $gam part
newYear2 <- cbind(newYear2,
                  data.frame(predict(car1_ER$gam, newYear2, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(car1_ER$gam))
newYear2 <- transform(newYear2,
                      upper = fit + (crit.t * se.fit),
                      lower = fit - (crit.t * se.fit))

head(newYear2)
newYear2$date<-as.Date(newYear2$julian.day, origin=as.Date("2018-12-31"))
head(newYear2)

## Plot estimated trend
ER_fitted <- ggplot(newYear2, aes(x = date, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = date), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") +
  geom_point(data = data, aes(x = date, y = ER),fill="white",color="black",shape=21,size=2) +
  geom_line(size=.5) +
  ylab(bquote('ER (g'~'O'[2]~m^-2*~d^-1*')')) +   
  xlab(bquote("")) +
  scale_x_date(expand = c(0,0),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-14"),by="14 days")) +
  theme(axis.text.x=element_blank())+
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5), plot.margin=unit(c(-0.2,1,1,1), "cm")) + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"),
        legend.title = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="b")+theme(plot.tag.position = 'topleft',
                                                                                             plot.tag = element_text(vjust = -3.5, hjust = 0))
ER_fitted



#confidence and simultaneous intervals
#compute across-the-fuction and simultaneous confidence intervals
cint <- confint(car1_ER, parm = "julian.day", newdata = newYear22,
                type = "confidence")

sint <- confint(car1_ER, parm = "julian.day", newdata = newYear22,
                type = "simultaneous")



###derivatives of estimated trend
ER.d <- fderiv(car1_ER, newdata = newYear2, n = N)
ER.sint <- with(newYear2,
                cbind(confint(ER.d, nsim = nsim,
                              type = "simultaneous"),
                      julian.day = julian.day))

ER.sint$date<-as.Date(newYear2$julian.day, origin=as.Date("2018-12-31"))


ER_deriv_plt <- ggplot(ER.sint, aes(x = date, y = est)) +
  geom_hline(yintercept=0,linetype="dashed")+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  ggtitle("") +
  ylab(bquote('ER derivative g'~'O'[2]~m^-2*~d^-2*')')) +   
  xlab(bquote("Date")) +
  scale_x_date(expand = c(0,0),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-14"),by="14 days")) +
  ggtitle("") +
  scale_y_continuous(expand = c(0,0),limits=c(-0.050,0.075),breaks = seq(-0.05,0.075, by = 0.025)) +
  theme(axis.text.x=element_text(size=11,angle=45,color="black", vjust=1, hjust=1), axis.title.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(-0.2,1,1,1), "cm")) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"))
ER_deriv_plt<-ER_deriv_plt+ theme(legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="c")+theme(plot.tag.position = 'topleft',
                                                                                                                                                       plot.tag = element_text(vjust = -3.5, hjust = 0))
ER_deriv_plt


print(ER.sint)





###########################Fig.2a: metabolism together
met_fitted <- ggplot() +
  geom_hline(yintercept=0,size=.5)+
  geom_point(data = data, aes(x = date, y = GPP),fill="gray",color="black",shape=21,size=2) +
  geom_ribbon(data=newYear,aes(ymin = lower, ymax = upper, x = date), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") +
  geom_line(data=newYear, aes(x = date, y = fit),size=.5) +
  geom_ribbon(data=newYear2,aes(ymin = lower, ymax = upper, x = date), alpha = 0.3,
              inherit.aes = FALSE, fill = "black") +
  geom_point(data = data, aes(x = date, y = ER),fill="white",color="black",shape=21,size=2) +
  geom_line(data=newYear2, aes(x = date, y = fit),size=.5) +
  ylab(bquote('Metabolism (g'~'O'[2]~m^-2*~d^-1*')')) +   
  xlab(bquote("")) +
  scale_y_continuous(expand=c(0,0),limits=c(-8,2),breaks = seq(-8,2, by = 2))+
  scale_x_date(expand = c(0,0),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-14"),by="14 days")) +
  theme(axis.text.x=element_blank())+
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(1,1,-0.2,1), "cm")) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"),
        legend.title = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="a")+theme(plot.tag.position = 'topleft',
                                                                                             plot.tag = element_text(vjust = -3.5, hjust = 0))
met_fitted


##convert to grob object and export figure
g3<- ggplotGrob(GPP_deriv_plt)
g4<- ggplotGrob(ER_deriv_plt)
g5<- ggplotGrob(met_fitted)

grid::grid.newpage()
grid::grid.draw(rbind(g5,g3,g4))



pdf(file="Bray thesis final Fig.2.pdf",paper="special",width=6.5,height=9)
grid::grid.newpage()
grid::grid.draw(rbind(g3,g1,g2))
dev.off()


jpeg(file="Bray thesis final Fig.2.jpeg",width=6.5,height=9,units="in",res=1200)
grid::grid.newpage()
grid::grid.draw(rbind(g3,g1,g2))
dev.off()

################################################Fig. 3: Modeled N uptake outputs#############################################################################

##################Fig. 3a: Pooled N uptake versus light

fig3a<-ggplot(data = data, aes(x =Nup.light, y =Nupbayes)) +
  geom_point(size = 2,shape=1,show.legend = F,color ="black") + 
  geom_smooth(method=lm,se=FALSE,color="black",size=.5)+
  scale_y_continuous(expand=c(0,0),limits=c(0,12.0),breaks = seq(0,12.0, by = 4.0)) +
  scale_x_continuous(expand=c(0,0),limits=c(30,80),breaks = seq(30,80, by = 10)) +
  xlab(bquote(Sigma*'PPF'*D[u]~'(mol'~m^-2*~d^-1*')')) + 
  #ylab(bquote('')) +
  ylab(bquote(U[ANO3]~'(mg'~'N'~m^-2*~d^-1*')')) +
  ggtitle("") +
  theme(axis.text.x=element_text(size=11,color="black"), axis.title.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(.5,.5,0.5,.2), "cm")) + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(.2, "cm"),
        axis.line = element_line(color = "black"))
fig3a<-fig3a+theme(panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="a")+theme(plot.tag.position = 'topleft',
                                                                                                        plot.tag = element_text(vjust = -3.5, hjust = 0))
fig3a

############Fig.3b: Unpooled and pooled N uptake
head(data)

fig3b<-ggplot() +
  geom_line(data = data, aes(x =date, y =Nupunpooled),color ="black")+
  geom_point(data = data, aes(x =date, y =Nupunpooled),size = 2,shape=21,show.legend = F,color ="black",fill="gray") + 
  geom_point(data = data, aes(x =date, y =Nupbayes),size = 2,shape=1,show.legend = F,color ="black",fill="white") + 
  scale_x_date(expand = c(0,0),limits = c(as.Date("2019-06-11"), as.Date("2019-10-14")),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-14"),by="14 days")) +
  scale_y_continuous(expand=c(0,0),limits=c(0,36.0),breaks = seq(0,36.0, by = 12.0)) +
  xlab(bquote("Date")) +
  #ylab(bquote('')) +
  ylab(bquote(U[ANO3]~'(mg'~'N'~m^-2*~d^-1*')')) +
  ggtitle("") +
  theme(axis.text.x=element_text(size=11,angle=45,color="black", vjust=1, hjust=1), axis.title.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5), plot.margin=unit(c(0.5,.2,0.5,.5), "cm")) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"),
        legend.title = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="b")+theme(plot.tag.position = 'topleft',
                                                                                             plot.tag = element_text(vjust = -3.5, hjust = 0))
fig3b

########Fig.3c: pooled N uptake versus K
head(data)

fig3c<-ggplot() +
  geom_point(data = data, aes(x =Knit, y =Nupbayes),size = 2,shape=1,show.legend = F,color ="black") + 
  scale_x_continuous(expand=c(0,0),limits=c(0,20.0),breaks = seq(0,20.0, by = 5.0)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,12.0),breaks = seq(0,12.0, by =4.0)) +
  xlab(bquote(K[nit]~'('*d^-1*')')) +
  #ylab(bquote('')) +
  ylab(bquote(U[ANO3]~'(mg'~'N'~m^-2*~d^-1*')')) +
  ggtitle("") +
  theme(axis.text.x=element_text(size=11, color="black"), axis.title.y=element_text(size=12))+
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5), plot.margin=unit(c(0.5,.5,.2,.5), "cm")) + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"),
        legend.title = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="c")+theme(plot.tag.position = 'topleft',
                                                                                             plot.tag = element_text(vjust = -3.5, hjust =0))
fig3c



####Fig.3d: Histogram of K prior probability and parameter estimates
head(data)



n_obs<-length(data$Knit)


fig3d <- ggplot(data=data) + 
  scale_x_continuous(expand=c(0,0),limits=c(0,20.0),breaks = seq(0,20.0, by = 5.0)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,20),breaks = seq(0,20,by=5)) +
  geom_density(binwidth = 0.8, aes(x = Knit, ..count..),color="steelblue",fill="steelblue",alpha=0.3,size=0.5) + 
  #geom_histogram(binwidth = 0.8, aes(x = Knit, ..count..),color="black",fill="gray") + 
  #stat_function(fun=dlnorm, args = list(meanlog = 2.25, sdlog = 1.5), 
  #color = "black",size=1,geom="area",fill="white",shape=1)+
  xlab(bquote(K[nit]~'('*d^-1*')')) +
  ylab(bquote('Frequency')) +
  ggtitle("") +
  theme(axis.text.x=element_text(size=11, color="black"), axis.title.y=element_text(size=12))+
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5), plot.margin=unit(c(0.5,.2,.5,.5), "cm")) + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"),
        legend.title = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="d")+theme(plot.tag.position = 'topleft',
                                                                                             plot.tag = element_text(vjust = -3.5, hjust =0))
fig3d<-fig3d+stat_function(fun = function(x) 
  dlnorm(x, meanlog = 2.25, sdlog = 1.5) * n_obs,geom="area",color="darkgray",fill="darkgray",alpha=0.3,size=0.5)
fig3d

##convert to grob object and export figure
library(cowplot)
g6<- ggplotGrob(fig3a)
g7<- ggplotGrob(fig3b)
g8<- ggplotGrob(fig3c)
g9<- ggplotGrob(fig3d)


plot_grid(g6,g7,g8,g9,nrow=2,align = "h")



pdf(file="Bray thesis final Fig.3.pdf",paper="special",width=6.5,height=6.5,pointsize=12)
plot_grid(g1,g2,g3,g4,nrow=2,align = "h")
dev.off()


jpeg(file="Bray thesis final Fig.3.jpeg",width=6.5,height=6.5,units="in",res=1200,pointsize=12)
plot_grid(g1,g2,g3,g4,nrow=2,align = "h")
dev.off()


#####################Fig.4: Modeled N uptake vs stoichiometric N uptake & GPP####################################################################################

##########Fig.4a: modeled N uptake vs. stoichiometric N uptake
fig4a<-ggplot(data = data, aes(x=NupGPP, y = Nupbayes)) +
  geom_point(data = data, aes(x=NupGPP, y = Nupbayes),size = 3,shape=1,show.legend = F,color ="black") + 
  geom_abline(slope=1, intercept=0, linetype=2, size=0.5)+
  scale_x_continuous(expand=c(0,0),limits=c(0,12.0),breaks = seq(0,12.0, by =4)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,12.0),breaks = seq(0,12.0, by = 4.0)) +
  xlab(bquote(U[A]~'(mg'~'N'~m^-2*~d^-1*')')) +
  ylab(bquote('')) +
  #ylab(bquote(U[ANO3]~'(mg'~'N'~m^-2*~d^-1*')')) +
  ggtitle("") +
  theme(axis.text.x=element_text(size=11), axis.title.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5), plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm")) + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(.2, "cm"),
        axis.line = element_line(color = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="a")+theme(plot.tag.position = 'topleft',
                                                                                             plot.tag = element_text(vjust = -3.5, hjust = 0))
fig4a


##########Fig.4b:N uptake vs. GPP

Nup_mod4 <- gamm(Nupbayes~ s(GPP),
                 data = data, method = "REML")
summary(Nup_mod4$gam)


############################plot GAM model output
set.seed(1)
length(data$Nupbayes)
head(data)

N <- 111 # number of points at which to evaluate the smooth
dim(data)
## create new data to predict at; 200 evenly-spaced values over `Year`
newYear3 <- with(data, data.frame(GPP= seq(min(GPP), max(GPP),
                                           length.out = 111)))
head(newYear3)

## Predict from the fitted model; note we predict from the $gam part
newYear3 <- cbind(newYear3,
                  data.frame(predict(Nup_mod4$gam, newYear3, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(Nup_mod4$gam))
newYear3 <- transform(newYear3,
                      upper = fit + (crit.t * se.fit),
                      lower = fit - (crit.t * se.fit))

## Plot estimated trend
NupvsGPP_fitted <- ggplot(newYear3, aes(x = GPP, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, x = GPP), alpha = 0.2,
              inherit.aes = FALSE, fill = "black") +
  geom_point(data = data, aes(x = GPP, y = Nupbayes),fill="white",color="black",shape=1,size=3) +
  geom_line(size=.5) +
  xlab(bquote('GPP (g'~'O'[2]~m^-2*~d^-1*')')) +   
  ylab(bquote('')) +
  #ylab(bquote(U[ANO3]~'(mg'~'N'~m^-2*~d^-1*')')) +
  scale_x_continuous(expand=c(0,0),limits=c(0,0.9),breaks = seq(0,0.9, by = .3)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,12.0),breaks = seq(0,12.0, by = 4.0)) +
  theme(axis.text.x=element_text(size=11), axis.title.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5), plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm")) + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(.2, "cm"),
        axis.line = element_line(color = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="b")+theme(plot.tag.position = 'topleft',
                                                                                             plot.tag = element_text(vjust = -3.5, hjust = 0))
NupvsGPP_fitted

##convert to grob object and export figure
g10<- ggplotGrob(fig4a)
g11<- ggplotGrob(NupvsGPP_fitted)


grid::grid.newpage()
grid::grid.draw(rbind(g10,g11))

library(gridExtra)
library( ggpubr)

library(gridExtra)
#with one y axis title

library(gridExtra)
library(grid)

grid.arrange(
  fig4a,
  NupvsGPP_fitted,
  nrow = 2,
  left=textGrob(U[ANO3]~'(mg'~'N'~m^-2*~d^-1*')',
                rot = 90, vjust = 1,gp=gpar(fontsize=12))
)



pdf(file="Bray thesis final Fig.4.pdf",paper="special",width=3.5,height=7)
grid.arrange(
  sig4,
  NupvsGPP_fitted,
  nrow = 2,
  left=textGrob(U[ANO3]~'(mg'~'N'~m^-2*~d^-1*')',
                rot = 90, vjust = 1,gp=gpar(fontsize=12))
)
dev.off()


jpeg(file="Bray thesis final Fig.4.jpeg",width=3.5,height=7,units="in",res=1200)
grid.arrange(
  sig4,
  NupvsGPP_fitted,
  nrow = 2,
  left=textGrob(U[ANO3]~'(mg'~'N'~m^-2*~d^-1*')',
                rot = 90, vjust = 1,gp=gpar(fontsize=12))
)
dev.off()



########################Fig.5: Diel Do characterization vs. diel No3-N characterization

#####Fig.5a: Diel DO magnitude vs nitrate-N magnitude

fig5a<-ggplot(data = data, aes(x=DO.mag, y = nitrate.mag)) +
  geom_point(data = data, aes(x=DO.mag, y = nitrate.mag),size = 4,shape=1,show.legend = F,color ="black") + 
  scale_y_continuous(expand=c(0,0),limits=c(10,50.0),breaks = seq(10,50.0, by =10)) +
  scale_x_continuous(expand=c(0,0),limits=c(0,2.0),breaks = seq(0,2.0, by = 0.5)) +
  xlab(expression('Diel DO amplitude ('*m*'g'~O[2]~L^-1~')')) +
  ylab(expression('Diel N'~O[3]~'-N amplitude ('~mu*'g'~N~L^-1~')')) +
  ggtitle("") +
  theme(axis.text.x=element_text(size=11,color="black"), axis.title.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5), plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm")) + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(.2, "cm"),
        axis.line = element_line(color = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="a")+theme(plot.tag.position = 'topleft',
                                                                                             plot.tag = element_text(vjust = -3.5, hjust = 0))
fig5a


####Fig. 5b: Diel Do maximum timing vs NO3 minimum timing

fig5b<-ggplot(data = data, aes(x=DO.max, y = nitrate.min)) +
  geom_point(data = data, aes(x=DO.max, y = nitrate.min),size = 4,shape=1,show.legend = F,color ="black") + 
  scale_y_continuous(expand=c(0,0),limits=c(0,25),breaks = seq(0,25, by =5)) +
  scale_x_continuous(expand=c(0,0),limits=c(8,16),breaks = seq(8,16, by = 2)) +
  xlab(expression('Diel DO max timing (h)')) +
  ylab(expression('Diel N'~O[3]~'-N min timing (h)')) +
  ggtitle("") +
  theme(axis.text.x=element_text(size=11,color="black"), axis.title.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5), plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm")) + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(.2, "cm"),
        axis.line = element_line(color = "black"),
        panel.border = element_rect(colour = "black", fill=NA,size=1))+labs(tag="b")+theme(plot.tag.position = 'topleft',
                                                                                           plot.tag = element_text(vjust = -3.5, hjust = 0))
fig5b

##convert to grob object and export figure
g12<- ggplotGrob(fig5a)
g13<- ggplotGrob(fig5b)


library(gridExtra)
grid.arrange(g1, g2, nrow = 1)

pdf(file="Bray thesis final Fig.5.pdf",paper="special",width=7,height=3.5)
grid.arrange(g1, g2, nrow = 1)
dev.off()


jpeg(file="Bray thesis final Fig.5.jpeg",width=7,height=3.5,units="in",res=1200)
grid.arrange(g1, g2, nrow = 1)
dev.off()


##############################Fig.6: DO and NO3-N signals Aug 17-Aug 20#####################################################################################
##Fig. 6a: NO3-N concentration

############SUNA nitrate on Aug 17  
library(scales)
data2 <- read.csv("MCNR SUNA nitrate 8.17.csv",head=T,sep=",") # load csv data
data2



data2$Unix<-as.POSIXct(data2$Unix, origin="1970-01-01")
data2$Unix<- lubridate::force_tz(data2$Unix,"UTC")
head(data2)    
data2$Unix

fig6<-ggplot(data=data2) +
  geom_line(data = data2, aes(x = Unix, y = nitrate),color="black",size=.5) +
  geom_point(data = data2, aes(x = Unix, y = nitrate),shape=21,color="black",size=2, fill="white",stroke=1) +
  ylab(bquote('N'~O[3]~'-N ('*mu*'g'~N~L^-1*')')) +
  xlab(bquote("Date")) +
  ggtitle("") +
  scale_x_datetime(expand = c(0,0),labels = date_format("%H:%M"),breaks=function(x) seq.POSIXt(from = min(x), to = max(x), by = "12 hours")) +
  scale_y_continuous(expand=c(0,0),limits=c(40,80),breaks = seq(40,80, by = 10.0)) +
  theme(axis.text.x=element_blank(), axis.title.x=element_blank()) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title=element_text(size=12)) +
  theme(plot.title = element_text(size=16, hjust = .5),plot.margin=unit(c(1,1,-0.2,1), "cm")) +  
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(.2, "cm"),
        axis.line = element_line(color = "black"))+theme(panel.border = element_rect(colour = "black", fill=NA, size=.5))   
fig6a<-fig6a+labs(tag="a")+theme(plot.tag.position = 'topleft',
                           plot.tag = element_text(vjust = -4.5, hjust = -.1))
fig6a

####fig.6b:DO concentrataion


data3 <- read.csv("MCNR DO & Temp 8.17.csv",head=T,sep=",") # load csv data
data3


data3$Unix<-as.POSIXct(data3$Unix, origin="1970-01-01")
data3$Unix<- lubridate::force_tz(data3$Unix,"UTC")
head(data3)    



fig6b<-ggplot(data=data3) +
  geom_point(data = data3, aes(x = Unix, y = DO),shape=1,color="black",size=1,stroke=1) +
  ylab(bquote('Observed DO ('*m*'g'~L^-1*')')) +
  xlab(bquote("Time")) +
  ggtitle("") +
  scale_x_datetime(expand = c(0,0),labels = date_format("%H:%M"),breaks=function(x) seq.POSIXt(from = min(x), to = max(x), by = "12 hours")) +
  scale_y_continuous(expand = c(0,0),limits=c(7,10),breaks = seq(7,10, by = 1.0)) +
  theme(axis.text.x=element_blank(), axis.title.x=element_blank()) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title=element_text(size=12)) +
  theme(plot.title = element_text(size=16, hjust = .5),plot.margin=unit(c(-0.2,1,-0.2,1), "cm")) + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(.2, "cm"),
        axis.line = element_line(color = "black"))+theme(panel.border = element_rect(colour = "black", fill=NA, size=.5))   
fig6b<-fig6b+labs(tag="b")+theme(plot.tag.position = 'topleft',
                           plot.tag = element_text(vjust = -4.5, hjust = -.1))

fig6a
#############################################figure5c:DO conc corrected###################################################################################


data4 <- read.csv("MCNR gs ex corrected DO 8.17.csv",head=T,sep=",") # load csv data
data4


data4$Unix<-as.POSIXct(data4$Unix, origin="1970-01-01")
data4$Unix<- lubridate::force_tz(data4$Unix,"UTC")
head(data4)    


fig6c<-ggplot(data=data4) +
  geom_point(data = data4, aes(x = Unix, y = DO),shape=1,color="black",size=1,stroke=1) +
  ylab(bquote('Corrected DO ('*m*'g'~L^-1*')')) +
  xlab(bquote("Time")) +
  ggtitle("") +
  scale_x_datetime(expand = c(0,0),labels = date_format("%H:%M"),breaks=function(x) seq.POSIXt(from = min(x), to = max(x), by = "12 hours")) +
  scale_y_continuous(expand = c(0,0),limits=c(7,10),breaks = seq(7,10, by = 1.0)) +
  theme(axis.text.x=element_text(size=11,angle=0,color="black", vjust=0, hjust=0.5), axis.title.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=12, hjust = .5), plot.margin=unit(c(-0.2,1,1,1), "cm")) + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray"), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"))+theme(panel.border = element_rect(colour = "black", fill=NA, size=.5)) 
fig6c<-fig6c+labs(tag="c")+theme(plot.tag.position = 'topleft',
                           plot.tag = element_text(vjust = -4.5, hjust = -.1))

fig6c

##convert to grob object and export figure
g14<- ggplotGrob(fig6a)
g15<- ggplotGrob(fig6b)
g16<- ggplotGrob(fig6c)

grid::grid.newpage()
grid::grid.draw(rbind(g14,g15,g16))


##############################################################Supporting information#################################################################


###############################################################Fig.A.1: ER vs. K600###############################################################################

###function to force sig figs on x label (ER)
scaleFUN <- function(x) sprintf("%.1f", x)
library(ggplot2)


figa1<-ggplot() +
  geom_point(data = data, aes(x = k600, y = ER),size = 4,shape=1,show.legend = F, fill = "white",color ="black") + 
  scale_y_continuous(expand=c(0,0),limits=c(-6,0.0),breaks = seq(-6,0, by = 2.0),labels=scaleFUN) +
  scale_x_continuous(expand=c(0,0),limits=c(0.0,40),breaks = seq(10,40, by = 10),position="top") +
  xlab(bquote('K600 ('~d^-1*')')) +   
  ylab(bquote('ER (g'~'O'[2]~m^-2*~d^-1*')')) +
  ggtitle("") +
  theme(axis.text.x=element_text(size=11,color="black", vjust=1, hjust=1.1), axis.title.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin = margin(5, 15, 10, 10)) + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(.2, "cm"),
        axis.line = element_line(color = "black",size=.5))

figa1<-figa1+theme(panel.border = element_rect(colour = "black", fill=NA, size=.5))
figa1


pdf(file="Bray thesis final Fig.A.1.pdf",paper="special",width=3.5,height=3.5)
figa1
dev.off()


jpeg(file="Bray thesis final Fig.A.1",width=3.5,height=3.5,units="in",res=1200)
figa1
dev.off()


################################################Fig.A.2: Groundwater discharge and derivative#########


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
N <- 126 # number of points at which to evaluate the smooth
length(data$julian.day)


## create new data to predict at; 200 evenly-spaced values over `Year`
newYear <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                  length.out = 126)))
## Predict from the fitted model; note we predict from the $gam part
newYear <- cbind(newYear,
                 data.frame(predict(car1_Qgw$gam, newYear, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(car1_Qgw$gam))
newYear <- transform(newYear,
                     upper = fit + (crit.t * se.fit),
                     lower = fit - (crit.t * se.fit))

head(newYear)
newYear$date<-as.Date(newYear$julian.day, origin=as.Date("2018-12-31"))
head(newYear)
head(data)


## Plot estimated trend
Qgw_fitted <- ggplot(newYear, aes(x = date, y = fit)) +
  geom_ribbon(data=newYear,aes(ymin = lower, ymax = upper, x = date), alpha = 0.2,
              fill = "black") +
  geom_point(data = data, aes(x = date, y = Q.gw),fill="white",color="black",shape=21,size=2) +
  geom_line(size=.5) +
  ylab(bquote(~Q[gw] ~'(L'~s^-1*')')) +   
  xlab(bquote("")) +
  scale_x_date(expand = c(0,0),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-14"),by="14 days")) +
  scale_y_continuous(expand = c(0,0),limits=c(-40,60),breaks = seq(-40,60, by = 20)) + 
  theme(axis.text.x=element_blank())+
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(1,1,-0.2,1), "cm")) + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"),
        legend.title = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="a")+theme(plot.tag.position = 'topleft',
                                                                                             plot.tag = element_text(vjust = -3.5, hjust = 0))
Qgw_fitted


## simulate data from model
set.seed(1) # set the random seed to make this reproducible
nsim <- 20 # how many simulations to draw



#confidence and simultaneous intervals
#compute across-the-fuction and simultaneous confidence intervals
cint <- confint(car1_Qgw, parm = "julian.day", newdata = newYear,
                type = "confidence")

sint <- confint(car1_Qgw, parm = "Year", newdata = newYear,
                type = "simultaneous")
head(sw.sint)
head(newYear)

###derivatives of estimated trend
Qgw.d <- fderiv(car1_Qgw, newdata = newYear, n = N)
Qgw.sint <- with(newYear,
                 cbind(confint(Qgw.d, nsim = nsim,
                               type = "simultaneous"),
                       julian.day = julian.day))

print(Qgw.sint)


Qgw.sint$date<-as.Date(newYear$julian.day, origin=as.Date("2018-12-31"))
head(Qgw.sint)



Qgw_deriv_plt <- ggplot(Qgw.sint, aes(x = date, y = est)) +
  geom_hline(yintercept=0,linetype="dashed")+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  ggtitle("") +
  ylab(bquote(~Q[gw] ~'derivative (L'~s^-1*~d^-1*')')) +   
  xlab(bquote("Date")) +
  scale_x_date(expand = c(0,0),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-14"),by="14 days")) +
  scale_y_continuous(expand = c(0,0),limits=c(-20,20),breaks = seq(-20,20, by = 10)) + 
  ggtitle("") +
  theme(axis.text.x=element_text(size=11,angle=45,color="black", vjust=1, hjust=1), axis.title.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(-0.2,1,1,1), "cm")) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"))
Qgw_deriv_plt<-Qgw_deriv_plt+ theme(legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="b")+theme(plot.tag.position = 'topleft',
                                                                                                                                                         plot.tag = element_text(vjust = -3.5, hjust = 0))
Qgw_deriv_plt


print(Qgw.sint)

##convert to grob object and export figure
g17<- ggplotGrob(Qgw_fitted)
g18<- ggplotGrob(Qgw_deriv_plt)

grid::grid.newpage()
grid::grid.draw(rbind(g17,g18))

pdf(file="Bray thesis final Fig.A.2.pdf",paper="special",width=6.5,height=5.5)
grid::grid.newpage()
grid::grid.draw(rbind(g3,g4))
dev.off()


jpeg(file="Bray thesis final Fig.A.2",width=6.5,height=5.5,units="in",res=1200)
grid::grid.newpage()
grid::grid.draw(rbind(g3,g4))
dev.off()

########################Fig. A.3: Daily DO and NO3-N concentrations & Fig. A.4: Daily DO and NO3-N concentration deriviatives###################


##Daily DO concentration

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
#REML with CAR 1 provides the best fit


############################plot GAM model output
N <- 126 # number of points at which to evaluate the smooth


## create new data to predict at; 200 evenly-spaced values over `Year`
newYear5 <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                   length.out = 126)))
## Predict from the fitted model; note we predict from the $gam part
newYear5 <- cbind(newYear5,
                  data.frame(predict(gam_mod_DO$gam, newYear5, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(gam_mod_DO$gam))
newYear5 <- transform(newYear5,
                      upper = fit + (crit.t * se.fit),
                      lower = fit - (crit.t * se.fit))

head(newYear5)
newYear5$date<-as.Date(newYear5$julian.day, origin=as.Date("2018-12-31"))
head(newYear5)
head(data)



## Plot estimated trend
DO_fitted <- ggplot(newYear5, aes(x = date, y = fit)) +
  geom_ribbon(data=newYear5,aes(ymin = lower, ymax = upper, x = date), alpha = 0.2,
              fill = "black") +
  geom_point(data = data, aes(x = date, y = DO.dwn),fill="white",color="black",shape=21,size=2) +
  geom_line(size=.5) +
  ylab(expression(atop('DO',paste('('*m*'g'~O[2]~L^-1~')')))) +
  #ylab(bquote('DO ('*m*'g'~O[2]~L^-1*')')) +  
  xlab(bquote("")) +
  scale_y_continuous(expand=c(0,0),limits=c(8.0,11),breaks = seq(8.0,11, by =1)) +
  scale_x_date(expand = c(0,0),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-13"),by="14 days")) +
  theme(axis.text.x=element_blank())+
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(1,1,.2,1), "cm")) + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"),
        legend.title = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="a")+theme(plot.tag.position = 'topleft',
                                                                                             plot.tag = element_text(vjust = -3.5, hjust = 0))
DO_fitted


## simulate data from model
set.seed(1) # set the random seed to make this reproducible
nsim <- 20 # how many simulations to draw

#confidence and simultaneous intervals
#compute across-the-fuction and simultaneous confidence intervals
cint <- confint(gam_mod_DO, parm = "julian.day", newdata = newYear5,
                type = "confidence")

sint <- confint(cgam_mod_DO, parm = "Year", newdata = newYear5,
                type = "simultaneous")
head(sw.sint)


###derivatives of estimated trend
DO.d <- fderiv(gam_mod_DO, newdata = newYear5, n = N)
DO.sint <- with(newYear5,
                cbind(confint(DO.d, nsim = nsim,
                              type = "simultaneous"),
                      julian.day = julian.day))

print(DO.sint)


DO.sint$date<-as.Date(newYear5$julian.day, origin=as.Date("2018-12-31"))
head(DO.sint)


DO_deriv_plt <- ggplot(DO.sint, aes(x = date, y = est)) +
  geom_hline(yintercept=0,linetype="dashed")+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  ggtitle("") +
  #ylab(bquote('DO derivative ('*m*'g'~O[2]~L^-1~d^-1*')')) +  
  ylab(expression(atop('DO derivative',paste('('*m*'g'~O[2]~L^-1~d^-1*')')))) +
  xlab(bquote("")) +
  scale_x_date(expand = c(0,0),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-13"),by="14 days")) +
  ggtitle("") +
  scale_y_continuous(labels =scales::number_format(accuracy = 0.1),expand=c(0,0),limits=c(-0.60,0.40),breaks = seq(-0.60,0.40, by =0.2)) +
  theme(axis.text.x=element_blank())+
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(-0.2,1,-.2,1), "cm")) +  
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"))
DO_deriv_plt<-DO_deriv_plt+ theme(legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="a")+theme(plot.tag.position = 'topleft',
                                                                                                                                                       plot.tag = element_text(vjust = -3.5, hjust = 0))
DO_deriv_plt


print(DO.sint)


###########################Daily nitrate

###############fit simple GAM
gam_mod_nitrate<- gamm(nitrate.dwn ~ s(julian.day,k=42), data = data,family=gaussian,method = "REML")

summary(gam_mod_nitrate$gam)
gam.check(gam_mod_nitrate$gam)


## fit GAM using gamm() with a CAR(1)
car1_nitrate<- gamm(nitrate.dwn~ s(julian.day,k=42), data = data,
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
#REML without CAR


############################plot GAM model output
N <- 112 # number of points at which to evaluate the smooth


## create new data to predict at; 200 evenly-spaced values over `Year`
newYear6 <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                   length.out = 112)))
## Predict from the fitted model; note we predict from the $gam part
newYear6 <- cbind(newYear6,
                  data.frame(predict(gam_mod_nitrate$gam, newYear6, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(gam_mod_nitrate$gam))
newYear6 <- transform(newYear6,
                      upper = fit + (crit.t * se.fit),
                      lower = fit - (crit.t * se.fit))

head(newYear6)
newYear6$date<-as.Date(newYear6$julian.day, origin=as.Date("2018-12-31"))
head(newYear6)
head(data)



## Plot estimated trend
nitrate_fitted <- ggplot(newYear6, aes(x = date, y = fit)) +
  geom_ribbon(data=newYear6,aes(ymin = lower, ymax = upper, x = date), alpha = 0.2,
              fill = "black") +
  geom_point(data = data, aes(x = date, y = nitrate.dwn),fill="white",color="black",shape=21,size=2) +
  geom_line(size=.5) +
  #ylab(bquote('N'~O[3]~'-N'~' ('*mu*'g'~N~L^-1*')')) + 
  ylab(expression(atop('N'~O[3]~'-N',paste('('~mu*'g'~N~L^-1~')')))) +
  xlab(bquote("Date")) +
  scale_y_continuous(expand = c(0,0),limits=c(40,100),breaks = seq(40,100, by =20)) + 
  scale_x_date(expand = c(0,0),limits=c(as.Date("2019-06-11"), as.Date("2019-10-13")),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-13"),by="14 days")) +
  #theme(axis.text.x=element_blank())+
  theme(axis.text.x=element_text(size=11,angle=45,color="black", vjust=1, hjust=1), axis.title.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(-.2,1,1,1), "cm")) +   
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"),
        legend.title = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="b")+theme(plot.tag.position = 'topleft',
                                                                                             plot.tag = element_text(vjust = -3.5, hjust = 0))
nitrate_fitted


## simulate data from model
set.seed(1) # set the rannitratem seed to make this reproducible
nsim <- 20 # how many simulations to draw


###derivatives of estimated trend
nitrate.d <- fderiv(gam_mod_nitrate, newdata = newYear6, n = N)
nitrate.sint <- with(newYear6,
                     cbind(confint(nitrate.d, nsim = nsim,
                                   type = "simultaneous"),
                           julian.day = julian.day))

print(nitrate.sint)


nitrate.sint$date<-as.Date(newYear6$julian.day, origin=as.Date("2018-12-31"))
head(nitrate.sint)


nitrate_deriv_plt <- ggplot(nitrate.sint, aes(x = date, y = est)) +
  geom_hline(yintercept=0,linetype="dashed")+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  ggtitle("") +
  #ylab(bquote('N'~O[3]~'-N'~' derivative ('*mu*'g'~N~L^-1~d^-1*')')) + 
  ylab(expression(atop('N'~O[3]~'-N'~' derivative',paste('('*mu*'g'~N~L^-1~d^-1*')')))) +
  xlab(bquote("Date")) +
  scale_x_date(expand = c(0,0),limits=c(as.Date("2019-06-11"), as.Date("2019-10-13")),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-13"),by="14 days")) +
  scale_y_continuous(expand = c(0,0),limits=c(-8,12),breaks = seq(-8,12, by =4)) + 
  ggtitle("") +
  theme(axis.text.x=element_text(size=11,angle=45,color="black", vjust=1, hjust=1), axis.title.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(-0.2,1,1,1), "cm")) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"))
nitrate_deriv_plt<-nitrate_deriv_plt+ theme(legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="b")+theme(plot.tag.position = 'topleft',
                                                                                                                                                                 plot.tag = element_text(vjust = -3.5, hjust = 0))
nitrate_deriv_plt


print(nitrate.sint)

###daily Do and NO3-N concentrations (Fig.A3) and derivatives (Fig.A4) in separate figures

##convert to grob object and export figure

##daily concentrations
g19<- ggplotGrob(DO_fitted)
g20<- ggplotGrob(nitrate_fitted)


pdf(file="Bray thesis final Fig.A.3.pdf",paper="special",width=6.5,height=5.5)
grid::grid.newpage()
grid::grid.draw(rbind(g19,g20))
dev.off()

jpeg(file="Bray thesis final Fig.A.3.jpeg",width=6.5,height=5.5,units="in",res=1200)
grid::grid.newpage()
grid::grid.draw(rbind(g19,g20))
dev.off()


##derivatives

g21<- ggplotGrob(DO_deriv_plt)
g22<- ggplotGrob(nitrate_deriv_plt)


grid::grid.newpage()
grid::grid.draw(rbind(g21,g22))

pdf(file="Bray thesis final Fig.A.4.pdf",paper="special",width=6.5,height=5.5)
grid::grid.newpage()
grid::grid.draw(rbind(g21,g22))
dev.off()

jpeg(file="Bray thesis final Fig.A.4.jpeg",width=6.5,height=5.5,units="in",res=1200)
grid::grid.newpage()
grid::grid.draw(rbind(g21,g22))
dev.off()



##################Fig.A.5: Ecosystem respiration vs. benthic standing stocks and dissolved organic carbon concentration##########################################
data5 <- read.csv("Bray_Miller_Creek_biweekly.csv",head=T,sep=",") # load csv data

data5$dt<- as.Date(data5$date)
data5$dt


head(data5)


####Fig. A5a: ER vs. chlorophyll a

###function to force sig figs on x label (er)
scaleFUN <- function(x) sprintf("%.1f", x)

m <- lm(ER ~ chla, data5)
summary(m)

figA5a<-ggplot(data=data5) +
  geom_point(data = data5, aes(x = chla, y = ER),size = 4,shape=21,show.legend = F, fill = "white",color ="black") + 
  geom_smooth(data=data5,aes(x = chla, y = ER),method = "lm", color='black',se = FALSE,size=0.5)+
  scale_x_continuous(expand=c(0,0),limits=c(0,60.0),breaks = seq(0,60, by = 20.0),labels=scaleFUN) +
  scale_y_continuous(expand=c(0,0),limits=c(-7,-3.0),breaks = seq(-7,-3, by = 1.0),labels=scaleFUN) +
  #xlab(expression('chl '*italic(a)~'content ('~m*'g'~m^-2*')')) +  
  xlab(expression(atop('chl '*italic(a)~'content',paste('('~m*'g'~m^-2*')')))) +
  ylab(bquote('ER (g'~'O'[2]~m^-2*~d^-1*')')) +
  ggtitle("") +
  theme(axis.text.x=element_text(size=11,color="black"), axis.title.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=16, hjust = .5), plot.margin=unit(c(.25,.5,.25,0.5), "cm")) + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(.2, "cm"),
        axis.line = element_line(color = "black"))
figA5a<-figA5a+theme(panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="a")+theme(plot.tag.position = 'topleft',
                                                                                                        plot.tag = element_text(vjust = -3.5, hjust = 0))
figA5a


####Fig. A5b: ER vs. ash-free dry mass

###function to force sig figs on x label (er)
scaleFUN <- function(x) sprintf("%.1f", x)

m <- lm(ER ~ AFDM, data5)
summary(m)

figA5b<-ggplot(data=data5) +
  geom_point(data = data5, aes(x = AFDM, y = ER),size = 4,shape=21,show.legend = F, fill = "white",color ="black") + 
  geom_smooth(data=data5,aes(x = AFDM, y = ER),method = "lm", color='black',se = FALSE,size=0.5)+
  scale_x_continuous(expand=c(0,0),limits=c(0,20.0),breaks = seq(0,20, by =4.0),labels=scaleFUN) +
  scale_y_continuous(expand=c(0,0),limits=c(-7,-3.0),breaks = seq(-7,-3, by = 1.0),labels=scaleFUN) +
  xlab(expression(atop('Benthic organic matter',paste('(g'~'AFDM'~m^-2*')')))) +
  #xlab(bquote('Benthic organic matter (g'~'AFDM'~m^-2*')')) +    
  ylab(bquote('')) +
  ggtitle("") +
  theme(axis.text.x=element_text(size=11,color="black"), axis.title.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=16, hjust = .5), plot.margin=unit(c(.25,0.5,.2,.5), "cm")) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(.2, "cm"),
        axis.line = element_line(color = "black"))
figA5b<-figA5b+theme(panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="b")+theme(plot.tag.position = 'topleft',
                                                                                                        plot.tag = element_text(vjust = -3.5, hjust = 0))
figA5b


####Fig. A5b: ER vs. dissolved organic carbon concentration

###function to force sig figs on x label (er)
scaleFUN <- function(x) sprintf("%.1f", x)



m <- lm(ER ~ DOC.dwn, data5)
summary(m)

figA5c<-ggplot(data=data5) +
  geom_point(data = data5, aes(x = DOC.dwn, y = ER),size = 4,shape=21,show.legend = F, fill = "white",color ="black") + 
  geom_smooth(data=data5,aes(x = DOC.dwn, y = ER),method = "lm", color='black',se = FALSE,size=0.5)+
  scale_x_continuous(expand=c(0,0),limits=c(0,2.5),breaks = seq(0,2.5, by = .5),labels=scaleFUN) +
  scale_y_continuous(expand=c(0,0),limits=c(-7,-3.0),breaks = seq(-7,-3, by = 1.0),labels=scaleFUN) +
  xlab(expression(atop('DOC',paste('('~m*'g'~L^-1*')')))) +
  #xlab(expression('DOC ('~m*'g'~L^-1*')')) +  
  ylab(bquote('')) +
  ggtitle("") +
  theme(axis.text.x=element_text(size=11,color="black"), axis.title.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=16, hjust = .5), plot.margin=unit(c(.25,.5,.25,.5), "cm")) + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(.2, "cm"),
        axis.line = element_line(color = "black"))
figA5c<-figA5c+theme(panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="c")+theme(plot.tag.position = 'topleft',
                                                                                                        plot.tag = element_text(vjust = -3.5, hjust = 0))
figA5c

##convert to grob object and export figure

g23<- ggplotGrob(figA5a)
g24<- ggplotGrob(figA5b)
g25<- ggplotGrob(figA5c)


library(gridExtra)
grid.arrange(g23,g24,g25, nrow = 1)


pdf(file="Bray thesis final Fig.A.5.pdf",paper="special",width=8.5,height=3.25)
grid.arrange(g23,g24,g25, nrow = 1)
dev.off()


jpeg(file="Bray thesis final Fig.A.5.jpeg",width=8.5,height=3.25,units="in",res=1200)
grid.arrange(g23,g24,g25, nrow = 1)
dev.off()


########################Fig. A.6: Diel DO concentration magnitude and deriviative###################


###############fit simple GAM
gam_mod_DOamp<- gamm(DO.mag ~ s(julian.day,k=42), data = data,family=gaussian,method = "REML")

summary(gam_mod_DOamp$gam)
gam.check(gam_mod_DOamp$gam)


## fit GAM using gamm() with a CAR(1)
car1_DOamp<- gamm(DO.mag~ s(julian.day,k=42), data = data,
                  correlation = corARMA(form = ~ julian.day, p = 1),method='REML')

summary(car1_DOamp$gam)
set.seed(1)
gam.check(car1_DOamp$gam)

acf(residuals(car1_DOamp$gam))
pacf(residuals(car1_DOamp$gam))

## ...and fit the AR2
car2_DOamp<- gamm(DO.mag ~ s(julian.day, k = 42), data = data,
                  correlation = corARMA(form = ~ julian.day, p = 2),method='REML')
summary(car2_DOamp$gam)
gam.check(car2_DOamp$gam)

acf(residuals(car2_DOamp$gam))
pacf(residuals(car2_DOamp$gam))

#fit AR(3)
car3_DOamp<- gamm(DO.mag~ s(julian.day, k = 42), data = data,
                  correlation = corARMA(form = ~ julian.day, p = 3),method='REML')
summary(car3_DOamp$gam)
gam.check(car3_DOamp$gam)

acf(residuals(car3_DOamp$gam))
pacf(residuals(car3_DOamp$gam))

anova(gam_mod_DOamp$lme,car1_DOamp$lme, car2_DOamp$lme,car3_DOamp$lme)
#REMLwith CAR 1

head(data)
############################plot GAM model output
N <- 125 # number of points at which to evaluate the smooth


## create new data to predict at; 200 evenly-spaced values over `Year`
newYear6 <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                   length.out = 125)))
## Predict from the fitted model; note we predict from the $gam part
newYear6 <- cbind(newYear6,
                  data.frame(predict(gam_mod_DOamp$gam, newYear6, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(gam_mod_DOamp$gam))
newYear6 <- transform(newYear6,
                      upper = fit + (crit.t * se.fit),
                      lower = fit - (crit.t * se.fit))

head(newYear6)
newYear6$date<-as.Date(newYear6$julian.day, origin=as.Date("2018-12-31"))
head(newYear6)
head(data)



## Plot estimated trend
DOamp_fitted <- ggplot(newYear6, aes(x = date, y = fit)) +
  geom_ribbon(data=newYear6,aes(ymin = lower, ymax = upper, x = date), alpha = 0.2,
              fill = "black") +
  geom_point(data = data, aes(x = date, y = DO.mag),fill="white",color="black",shape=21,size=2) +
  geom_line(size=.5) +
  ylab(expression(atop('Diel DO amplitude',paste('('*m*'g'~O[2]~L^-1~')')))) +
  #ylab(bquote('DOamp ('*m*'g'~O[2]~L^-1*')')) +  
  xlab(bquote("")) +
  scale_y_continuous(expand=c(0,0),limits=c(0.2,1.8),breaks = seq(0.2,1.8, by =0.4)) +
  scale_x_date(expand = c(0,0),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-14"),by="14 days")) +
  theme(axis.text.x=element_blank())+
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(1,1,-0.2,1), "cm")) + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"),
        legend.title = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="a")+theme(plot.tag.position = 'topleft',
                                                                                             plot.tag = element_text(vjust = -3.5, hjust = 0))
DOamp_fitted


## simulate data from model
set.seed(1) # set the ranDOampm seed to make this reproducible
nsim <- 20 # how many simulations to draw

#confidence and simultaneous intervals
#compute across-the-fuction and simultaneous confidence intervals
cint <- confint(gam_mod_DOamp, parm = "julian.day", newdata = newYear6,
                type = "confidence")

sint <- confint(cgam_mod_DOamp, parm = "Year", newdata = newYear6,
                type = "simultaneous")
head(sw.sint)


###derivatives of estimated trend
DOamp.d <- fderiv(gam_mod_DOamp, newdata = newYear6, n = N)
DOamp.sint <- with(newYear6,
                   cbind(confint(DOamp.d, nsim = nsim,
                                 type = "simultaneous"),
                         julian.day = julian.day))

print(DOamp.sint)


DOamp.sint$date<-as.Date(newYear6$julian.day, origin=as.Date("2018-12-31"))
head(DOamp.sint)


DOamp_deriv_plt <- ggplot(DOamp.sint, aes(x = date, y = est)) +
  geom_hline(yintercept=0,linetype="dashed")+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  ggtitle("") +
  #ylab(bquote('DOamp derivative ('*m*'g'~O[2]~L^-1~d^-1*')')) +  
  ylab(expression(atop('Diel DO amplitude',paste('derivative ('*m*'g'~O[2]~L^-1~d^-1*')')))) +
  xlab(bquote("")) +
  scale_x_date(expand = c(0,0),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-14"),by="14 days")) +
  scale_y_continuous(expand=c(0,0),limits=c(-0.080,0.08),breaks = seq(-0.08,0.08, by =0.04)) +
  ggtitle("") +
  theme(axis.text.x=element_text(size=11,angle=45,color="black", vjust=1, hjust=1), axis.title.x=element_text(size=12))+ 
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(-0.2,1,-0.2,1), "cm")) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"))
DOamp_deriv_plt<-DOamp_deriv_plt+ theme(legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="b")+theme(plot.tag.position = 'topleft',
                                                                                                                                                             plot.tag = element_text(vjust = -3.5, hjust = 0))
DOamp_deriv_plt


print(DOamp.sint)

##convert to grob object and export figure
g26<- ggplotGrob(DOamp_fitted)
g27<- ggplotGrob(DOamp_deriv_plt)

grid::grid.newpage()
grid::grid.draw(rbind(g26,g27))

pdf(file="Bray thesis final Fig.A.6.pdf",paper="special",width=6.5,height=5.5)
grid::grid.newpage()
grid::grid.draw(rbind(g26,g27))
dev.off()


jpeg(file="Bray thesis final Fig.A.6.jpeg",width=6.5,height=5.5,units="in",res=1200)
grid::grid.newpage()
grid::grid.draw(rbind(g26,g27))
dev.off()


####################Fig.A.7: Diel NO3-N concentration magnitude and derivative###########################################

###############fit simple GAM
gam_mod_nitrateamp<- gamm(nitrate.mag ~ s(julian.day,k=42), data = data,family=gaussian,method = "REML")

summary(gam_mod_nitrateamp$gam)
gam.check(gam_mod_nitrateamp$gam)


## fit GAM using gamm() with a CAR(1)
car1_nitrateamp<- gamm(nitrate.mag~ s(julian.day,k=42), data = data,
                       correlation = corARMA(form = ~ julian.day, p = 1),method='REML')

summary(car1_nitrateamp$gam)
set.seed(1)
gam.check(car1_nitrateamp$gam)

acf(residuals(car1_nitrateamp$gam))
pacf(residuals(car1_nitrateamp$gam))

## ...and fit the AR2
car2_nitrateamp<- gamm(nitrate.mag ~ s(julian.day, k = 42), data = data,
                       correlation = corARMA(form = ~ julian.day, p = 2),method='REML')
summary(car2_nitrateamp$gam)
gam.check(car2_nitrateamp$gam)

acf(residuals(car2_nitrateamp$gam))
pacf(residuals(car2_nitrateamp$gam))

#fit AR(3)
car3_nitrateamp<- gamm(nitrate.mag~ s(julian.day, k = 42), data = data,
                       correlation = corARMA(form = ~ julian.day, p = 3),method='REML')
summary(car3_nitrateamp$gam)
gam.check(car3_nitrateamp$gam)

acf(residuals(car3_nitrateamp$gam))
pacf(residuals(car3_nitrateamp$gam))

anova(gam_mod_nitrateamp$lme,car1_nitrateamp$lme, car2_nitrateamp$lme,car3_nitrateamp$lme)
#REMLwith CAR 2

head(data)
############################plot GAM model output
N <- 111 # number of points at which to evaluate the smooth


## create new data to predict at; 200 evenly-spaced values over `Year`
newYear7 <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                   length.out = 111)))
## Predict from the fitted model; note we predict from the $gam part
newYear7 <- cbind(newYear7,
                  data.frame(predict(gam_mod_nitrateamp$gam, newYear7, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(gam_mod_nitrateamp$gam))
newYear7 <- transform(newYear7,
                      upper = fit + (crit.t * se.fit),
                      lower = fit - (crit.t * se.fit))

head(newYear7)
newYear7$date<-as.Date(newYear7$julian.day, origin=as.Date("2018-12-31"))
head(newYear7)
head(data)



## Plot estimated trend
nitrateamp_fitted <- ggplot(newYear7, aes(x = date, y = fit)) +
  geom_ribbon(data=newYear7,aes(ymin = lower, ymax = upper, x = date), alpha = 0.2,
              fill = "black") +
  geom_point(data = data, aes(x = date, y = nitrate.mag),fill="white",color="black",shape=21,size=2) +
  geom_line(size=.5) +
  scale_y_continuous(expand=c(0,0),limits=c(0,60),breaks = seq(0,60, by =20)) +
  ylab(expression(atop('Diel N'~O[3]~'-N amplitude',paste('('~mu*'g'~N~L^-1~')')))) +
  #ylab(bquote('nitrateamp ('*m*'g'~O[2]~L^-1*')')) +  
  xlab(bquote("")) +
  scale_x_date(expand = c(0,0),limit=c(as.Date("2019-06-11"),as.Date("2019-10-13")),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-13"),by="14 days")) +
  theme(axis.text.x=element_blank())+
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(-0.2,1,-0.2,1), "cm")) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"),
        legend.title = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="a")+theme(plot.tag.position = 'topleft',
                                                                                             plot.tag = element_text(vjust = -3.5, hjust = 0))
nitrateamp_fitted


## simulate data from model
set.seed(1) # set the rannitrateampm seed to make this reproducible
nsim <- 20 # how many simulations to draw

#confidence and simultaneous intervals
#compute across-the-fuction and simultaneous confidence intervals
cint <- confint(gam_mod_nitrateamp, parm = "julian.day", newdata = newYear7,
                type = "confidence")

sint <- confint(cgam_mod_nitrateamp, parm = "Year", newdata = newYear7,
                type = "simultaneous")
head(sw.sint)


###derivatives of estimated trend
nitrateamp.d <- fderiv(gam_mod_nitrateamp, newdata = newYear7, n = N)
nitrateamp.sint <- with(newYear7,
                        cbind(confint(nitrateamp.d, nsim = nsim,
                                      type = "simultaneous"),
                              julian.day = julian.day))

print(nitrateamp.sint)


nitrateamp.sint$date<-as.Date(newYear7$julian.day, origin=as.Date("2018-12-31"))
head(nitrateamp.sint)


nitrateamp_deriv_plt <- ggplot(nitrateamp.sint, aes(x = date, y = est)) +
  geom_hline(yintercept=0,linetype="dashed")+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  ggtitle("") +
  #ylab(bquote('nitrateamp derivative ('*m*'g'~O[2]~L^-1~d^-1*')')) +  
  ylab(expression(atop('Diel N'~O[3]~'-N amplitude',paste('derivative ('~mu*'g'~N~L^-1~d^-1~')')))) +
  xlab(bquote("Date")) +
  scale_x_date(expand = c(0,0),limit=c(as.Date("2019-06-11"),as.Date("2019-10-13")),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-13"),by="14 days")) +
  scale_y_continuous(expand=c(0,0),limits=c(-1,1),breaks = seq(-1,1, by =0.5)) +
  theme(axis.text.x=element_text(size=11,angle=45,color="black", vjust=1, hjust=1), axis.title.x=element_text(size=12))+ 
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(-0.2,1,1,1), "cm")) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"))
nitrateamp_deriv_plt<-nitrateamp_deriv_plt+ theme(legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="b")+theme(plot.tag.position = 'topleft',
                                                                                                                                                                       plot.tag = element_text(vjust = -3.5, hjust = 0))
nitrateamp_deriv_plt


print(nitrateamp.sint)

##convert to grob object and export figure
g28<- ggplotGrob(nitrateamp_fitted)
g29<- ggplotGrob(nitrateamp_deriv_plt)


grid::grid.newpage()
grid::grid.draw(rbind(g28,g29))

pdf(file="Bray thesis final Fig.A.7.pdf",paper="special",width=6.5,height=5.5)
grid::grid.newpage()
grid::grid.draw(rbind(g28,g29))
dev.off()


jpeg(file="Bray thesis final Fig.A.7.jpeg",width=6.5,height=5.5,units="in",res=1200)
grid::grid.newpage()
grid::grid.draw(rbind(g28,g29))
dev.off()

######################Fig.A8: Diel DO concentration maximum timing#########################

###############fit simple GAM
gam_mod_DOmax<- gamm(DO.max ~ s(julian.day,k=42), data = data,family=gaussian,method = "REML")

summary(gam_mod_DOmax$gam)
gam.check(gam_mod_DOmax$gam)


## fit GAM using gamm() with a CAR(1)
car1_DOmax<- gamm(DO.max~ s(julian.day,k=42), data = data,
                  correlation = corARMA(form = ~ julian.day, p = 1),method='REML')

summary(car1_DOmax$gam)
set.seed(1)
gam.check(car1_DOmax$gam)

acf(residuals(car1_DOmax$gam))
pacf(residuals(car1_DOmax$gam))

## ...and fit the AR2
car2_DOmax<- gamm(DO.max ~ s(julian.day, k = 42), data = data,
                  correlation = corARMA(form = ~ julian.day, p = 2),method='REML')
summary(car2_DOmax$gam)
gam.check(car2_DOmax$gam)

acf(residuals(car2_DOmax$gam))
pacf(residuals(car2_DOmax$gam))

#fit AR(3)
car3_DOmax<- gamm(DO.max~ s(julian.day, k = 42), data = data,
                  correlation = corARMA(form = ~ julian.day, p = 3),method='REML')
summary(car3_DOmax$gam)
gam.check(car3_DOmax$gam)

acf(residuals(car3_DOmax$gam))
pacf(residuals(car3_DOmax$gam))

anova(gam_mod_DOmax$lme,car1_DOmax$lme, car2_DOmax$lme,car3_DOmax$lme)
#REML without CAR 

head(data)
############################plot GAM model output
N <- 125 # number of points at which to evaluate the smooth


## create new data to predict at; 200 evenly-spaced values over `Year`
newYear7 <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                   length.out = 125)))
## Predict from the fitted model; note we predict from the $gam part
newYear7 <- cbind(newYear7,
                  data.frame(predict(gam_mod_DOmax$gam, newYear7, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(gam_mod_DOmax$gam))
newYear7 <- transform(newYear7,
                      upper = fit + (crit.t * se.fit),
                      lower = fit - (crit.t * se.fit))

head(newYear7)
newYear7$date<-as.Date(newYear7$julian.day, origin=as.Date("2018-12-31"))
head(newYear7)
head(data)



## Plot estimated trend
DOmax_fitted <- ggplot(newYear7, aes(x = date, y = fit)) +
  geom_ribbon(data=newYear7,aes(ymin = lower, ymax = upper, x = date), alpha = 0.2,
              fill = "black") +
  geom_point(data = data, aes(x = date, y = DO.max),fill="white",color="black",shape=21,size=2) +
  geom_line(size=.5) +
  ylab(expression(atop('Diel DO',paste('max timing (h)')))) +
  scale_y_continuous(expand=c(0,0),limits=c(8,16),breaks = seq(8,16, by =2)) +
  #ylab(bquote('DOmax ('*m*'g'~O[2]~L^-1*')')) +  
  xlab(bquote("")) +
  scale_x_date(expand = c(0,0),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-14"),by="14 days")) +
  theme(axis.text.x=element_blank())+
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(1,1,-0.2,1), "cm")) + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"),
        legend.title = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="a")+theme(plot.tag.position = 'topleft',
                                                                                             plot.tag = element_text(vjust = -3.5, hjust = 0))
DOmax_fitted


## simulate data from model
set.seed(1) # set the ranDOmaxm seed to make this reproducible
nsim <- 20 # how many simulations to draw

#confidence and simultaneous intervals
#compute across-the-fuction and simultaneous confidence intervals
cint <- confint(gam_mod_DOmax, parm = "julian.day", newdata = newYear7,
                type = "confidence")

sint <- confint(cgam_mod_DOmax, parm = "Year", newdata = newYear7,
                type = "simultaneous")
head(sw.sint)


###derivatives of estimated trend
DOmax.d <- fderiv(gam_mod_DOmax, newdata = newYear7, n = N)
DOmax.sint <- with(newYear7,
                   cbind(confint(DOmax.d, nsim = nsim,
                                 type = "simultaneous"),
                         julian.day = julian.day))

print(DOmax.sint)


DOmax.sint$date<-as.Date(newYear7$julian.day, origin=as.Date("2018-12-31"))
head(DOmax.sint)


DOmax_deriv_plt <- ggplot(DOmax.sint, aes(x = date, y = est)) +
  geom_hline(yintercept=0,linetype="dashed")+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  ggtitle("") +
  #ylab(bquote('DOmax derivative ('*m*'g'~O[2]~L^-1~d^-1*')')) +  
  ylab(expression(atop('Diel DO max',paste(' timing derivative (h'~d^-1*')')))) +
  xlab(bquote("")) +
  scale_x_date(expand = c(0,0),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-14"),by="14 days")) +
  scale_y_continuous(expand=c(0,0),limits=c(-0.040,0.04),breaks = seq(-0.04,0.04, by =0.04)) +
  ggtitle("") +
  theme(axis.text.x=element_text(size=11,angle=45,color="black", vjust=1, hjust=1), axis.title.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(-0.2,1,-0.2,1), "cm")) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"))
DOmax_deriv_plt<-DOmax_deriv_plt+ theme(legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="b")+theme(plot.tag.position = 'topleft',
                                                                                                                                                             plot.tag = element_text(vjust = -3.5, hjust = 0))
DOmax_deriv_plt


print(DOmax.sint)

##convert to grob object and export figure
g30<- ggplotGrob(DOmax_fitted)
g31<- ggplotGrob(DOmax_deriv_plt)

grid::grid.newpage()
grid::grid.draw(rbind(g30,g31))

pdf(file="Bray thesis final Fig.A.8.pdf",paper="special",width=6.5,height=5.5)
grid::grid.newpage()
grid::grid.draw(rbind(g30,g31))
dev.off()


jpeg(file="Bray thesis final Fig.A.8.jpeg",width=6.5,height=5.5,units="in",res=1200)
grid::grid.newpage()
grid::grid.draw(rbind(g30,g31))
dev.off()

######################Fig.A9: Diel NO3-N concentration minimum timing#########################


###############fit simple GAM
gam_mod_nitratemin<- gamm(nitrate.min ~ s(julian.day,k=42), data = data,family=gaussian,method = "REML")

summary(gam_mod_nitratemin$gam)
gam.check(gam_mod_nitratemin$gam)


## fit GAM using gamm() with a CAR(1)
car1_nitratemin<- gamm(nitrate.min~ s(julian.day,k=42), data = data,
                       correlation = corARMA(form = ~ julian.day, p = 1),method='REML')

summary(car1_nitratemin$gam)
set.seed(1)
gam.check(car1_nitratemin$gam)

acf(residuals(car1_nitratemin$gam))
pacf(residuals(car1_nitratemin$gam))

## ...and fit the AR2
car2_nitratemin<- gamm(nitrate.min ~ s(julian.day, k = 42), data = data,
                       correlation = corARMA(form = ~ julian.day, p = 2),method='REML')
summary(car2_nitratemin$gam)
gam.check(car2_nitratemin$gam)

acf(residuals(car2_nitratemin$gam))
pacf(residuals(car2_nitratemin$gam))

#fit AR(3)
car3_nitratemin<- gamm(nitrate.min~ s(julian.day, k = 42), data = data,
                       correlation = corARMA(form = ~ julian.day, p = 3),method='REML')
summary(car3_nitratemin$gam)
gam.check(car3_nitratemin$gam)

acf(residuals(car3_nitratemin$gam))
pacf(residuals(car3_nitratemin$gam))

anova(gam_mod_nitratemin$lme,car1_nitratemin$lme, car2_nitratemin$lme,car3_nitratemin$lme)
#REML without CAR

head(data)
############################plot GAM model output
N <- 111 # number of points at which to evaluate the smooth


## create new data to predict at; 200 evenly-spaced values over `Year`
newYear8 <- with(data, data.frame(julian.day = seq(min(julian.day), max(julian.day),
                                                   length.out = 111)))
## Predict from the fitted model; note we predict from the $gam part
newYear8 <- cbind(newYear8,
                  data.frame(predict(gam_mod_nitratemin$gam, newYear8, se.fit = TRUE)))
## Create the confidence interval
crit.t <- qt(0.975, df = df.residual(gam_mod_nitratemin$gam))
newYear8 <- transform(newYear8,
                      upper = fit + (crit.t * se.fit),
                      lower = fit - (crit.t * se.fit))

head(newYear8)
newYear8$date<-as.Date(newYear8$julian.day, origin=as.Date("2018-12-31"))
head(newYear8)
head(data)



## Plot estimated trend
nitratemin_fitted <- ggplot(newYear8, aes(x = date, y = fit)) +
  geom_ribbon(data=newYear8,aes(ymin = lower, ymax = upper, x = date), alpha = 0.2,
              fill = "black") +
  geom_point(data = data, aes(x = date, y = nitrate.min),fill="white",color="black",shape=21,size=2) +
  geom_line(size=.5) +
  scale_y_continuous(expand=c(0,0),limits=c(0,24),breaks = seq(0,24, by =8)) +
  ylab(expression(atop('Diel N'~O[3]~'-N min',paste('timing (h)')))) +
  #ylab(bquote('nitratemin ('*m*'g'~O[2]~L^-1*')')) +  
  xlab(bquote("")) +
  scale_x_date(expand = c(0,0),limits=c(as.Date("2019-06-11"), as.Date("2019-10-14")),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-14"),by="14 days")) +
  theme(axis.text.x=element_blank())+
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(1,1,-0.2,1), "cm")) +  
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"),
        legend.title = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="a")+theme(plot.tag.position = 'topleft',
                                                                                             plot.tag = element_text(vjust = -3.5, hjust = 0))
nitratemin_fitted


## simulate data from model
set.seed(1) # set the rannitrateminm seed to make this reproducible
nsim <- 20 # how many simulations to draw

#confidence and simultaneous intervals
#compute across-the-fuction and simultaneous confidence intervals
cint <- confint(gam_mod_nitratemin, parm = "julian.day", newdata = newYear8,
                type = "confidence")

sint <- confint(cgam_mod_nitratemin, parm = "Year", newdata = newYear8,
                type = "simultaneous")
head(sw.sint)


###derivatives of estimated trend
nitratemin.d <- fderiv(gam_mod_nitratemin, newdata = newYear8, n = N)
nitratemin.sint <- with(newYear8,
                        cbind(confint(nitratemin.d, nsim = nsim,
                                      type = "simultaneous"),
                              julian.day = julian.day))

print(nitratemin.sint)


nitratemin.sint$date<-as.Date(newYear8$julian.day, origin=as.Date("2018-12-31"))
head(nitratemin.sint)


nitratemin_deriv_plt <- ggplot(nitratemin.sint, aes(x = date, y = est)) +
  geom_hline(yintercept=0,linetype="dashed")+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2,
              fill = "black") +
  geom_line() +
  ggtitle("") +
  #ylab(bquote('nitratemin derivative ('*m*'g'~O[2]~L^-1~d^-1*')')) +  
  ylab(expression(atop('Diel N'~O[3]~'-N min',paste('timing derivative (h'~d^-1*')')))) +
  xlab(bquote("")) +
  scale_x_date(expand = c(0,0),limits=c(as.Date("2019-06-11"), as.Date("2019-10-14")),
               date_labels = "%b %d",breaks = seq(as.Date("2019-06-11"), as.Date("2019-10-14"),by="14 days")) +
  ggtitle("") +
  scale_y_continuous(expand=c(0,0),limits=c(-1,0.5),breaks = seq(-1,0.5, by =0.5)) +
  theme(axis.text.x=element_text(size=11,angle=45,color="black", vjust=1, hjust=1), axis.title.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=11, color="black"), axis.title.y=element_text(size=12)) +
  theme(plot.title = element_text(size=11, hjust = .5),plot.margin=unit(c(-0.2,1,-0.2,1), "cm")) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(.2, "cm"))
nitratemin_deriv_plt<-nitratemin_deriv_plt+ theme(legend.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))+labs(tag="b")+theme(plot.tag.position = 'topleft',
                                                                                                                                                                       plot.tag = element_text(vjust = -3.5, hjust = 0))
nitratemin_deriv_plt


print(nitratemin.sint)

##convert to grob object and export figure
g32<- ggplotGrob(nitratemin_fitted)
g33<- ggplotGrob(nitratemin_deriv_plt)

grid::grid.newpage()
grid::grid.draw(rbind(g32,g33))

pdf(file="Bray thesis final Fig.A.9.pdf",paper="special",width=6.5,height=5.5)
grid::grid.newpage()
grid::grid.draw(rbind(g32,g33))
dev.off()


jpeg(file="Bray thesis final Fig.A.9.jpeg",width=6.5,height=5.5,units="in",res=1200)
grid::grid.newpage()
grid::grid.draw(rbind(g32,g33))
dev.off()
