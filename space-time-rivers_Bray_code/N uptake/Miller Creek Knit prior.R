
###with original Hall et al. 2013 Biogeosciences data

data <- read.csv("solutespiraldata.csv",head=T,sep=",") # load csv data
head(data)

levels(data$stream)
levels(data$author)

###Calculate no3vf (no3vf=qw/s) in units of m/d
calc.no3vf<-(data$qw/data$no3s)*60*24 #m/d


###natural log transform no3 vf and concentration
logno3vf<-log(calc.no3vf) #m/d
logno3c<-log(data$no3c)   #ug/L

###create data frame of log transformed values
df <- data.frame(logno3c,logno3vf)
head(df)


####linear model of natural log transformed values
model = lm(logno3vf~logno3c, data=df)
summary(model)

#####estimate Miller Creek K (1/d)
#mean nitrate-N conc measured was 57.16 ug N/L
meanc<-57.16 #ug/L
logMCc<-log(meanc)
#estimate natural log vf for Miller Creek using linear regression from Hall et al. 2013 in units of m/d
logMCvf<-(logMCc*-0.21874)+1.30643 #m/d


#calculate log K (=Vf/z) in units of 1/d
z<-0.16#average depth in Miller Creek = 0.16 m
logz<-log(0.16)#m

logMCK<-((logMCvf-logz))#1/d

logMCK #mean log K for Miller Creek is equal to log(2.254 1/d)  (9.53 d-1) 

#residual standard error of model is log K error
logKres<-1.524



##plot K distribution
K<-seq(0.1,20, 0.1)
plot(K, dlnorm(K, 2.25,1.52))

