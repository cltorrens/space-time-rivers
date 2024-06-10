
library(brms)

chl<-read.csv(here("./Data/chl_2023.csv"))

chl_fit<- brm(log(chla) ~ 0 + Intercept + (1 | site), data=chl, 
              prior=c(set_prior("normal(0,1)", class = "sd", group = "site") ,set_prior("normal(0,1)", class = "b" )))
summary(chl_fit)

chl_dev<-ranef(chl_fit)
chl_dev<-as.data.frame(chl_dev)
chl_summ<-data.frame(site=c("casc","essex","paola", "sc","upwell", "wg"),
                     mean=0.74+chl_dev$site.Estimate.Intercept, 
                     low=0.74+chl_dev$site.Q2.5.Intercept,
                     high=0.74+ chl_dev$site.Q97.5.Intercept)

x<-seq(1:6)
pdf(file="chl.pdf",width=6, height=5)
plot(x,exp(chl_summ$mean), pch=19, col="blue", ylim=c(0,10), axes=F, ylab="chl µg/cm2", xlab="" )
arrows(x0=x, x1=x, y0=exp(chl_summ$mean), y1=exp(chl_summ$low), code=2, 
       col="blue", lwd=2, length=0)
arrows(x0=x, x1=x, y0=exp(chl_summ$mean), y1=exp(chl_summ$high), code=2, 
       col="blue", lwd=2, length=0)
axis(1, at=x, labels = c("casc","essex","paola", "sc","upwell", "wg"))
axis(2)
dev.off()


points(chl_mean$chl, col="red")


chl_mean<-chl %>% group_by(site) %>% summarize(chl=exp(mean(log(chla), na.rm=T)),
                                               logchl=mean(log(chla), na.rm=T),
                                               csd=sd(log(chla), na.rm=T),
                                               cci=2.3*sd(log(chla)/sqrt(5), na.rm=T))
chl_mean<-chl_mean[-1,]
chl_mean$low<-chl_mean$logchl-chl_mean$cci
chl_mean$high<- chl_mean$logchl+chl_mean$cci


##########
#### bug biomass
#####


bug<-read.csv(here("./Data/bugs_2023.csv"))

bug_fit<- brm(log(biomass) ~ 0 + Intercept + (1 | site), data=bug,
              prior=c(set_prior("normal(0,0.5)", class = "sd", group = "site") ,set_prior("normal(-1,1)", class = "b" )))
summary(bug_fit)

bug_dev<-ranef(bug_fit)
bug_dev<-as.data.frame(bug_dev)
bug_summ<-data.frame(site=c("beav","casc","essex","paola", "sc","upwell", "wg"),
                     mean=-1.66+bug_dev$site.Estimate.Intercept, 
                     low=-1.66+bug_dev$site.Q2.5.Intercept,
                     high=-1.66+ bug_dev$site.Q97.5.Intercept)

bug_mean<-bug %>% group_by(site) %>% summarize(biomass=exp(mean(log(biomass), na.rm=T)))





x<-seq(1:7)
pdf(file="bug.pdf",width=6, height=5)


plot(x,exp(bug_summ$mean), pch=19, col="darkgreen", ylim=c(0,0.8), axes=F, ylab="invertebrate biomass g/m2", xlab="" )
arrows(x0=x, x1=x, y0=exp(bug_summ$mean), y1=exp(bug_summ$low), code=2, 
       col="darkgreen", lwd=2, length=0)
arrows(x0=x, x1=x, y0=exp(bug_summ$mean), y1=exp(bug_summ$high), code=2, 
       col="darkgreen", lwd=2, length=0)
axis(1, at=x, labels = c("beav","casc","essex","paola", "sc","upwell", "wg"))
axis(2)

points(bug_mean$biomass, col="red")
dev.off()



