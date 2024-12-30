# load required packages
library(tidyverse)
library(here)
library(plotly)
library(ggplot2)


########  FUNCTIONS ##########################

### calculating auc for a concentration curve
# conc = a vector of concentration measurements from a timeseries
# t = a vector of the times for each conc measurement

conc_auc <- function(conc, time) {
  auc <- sum(((conc[-length(conc)] + conc[-1]) / 2) * diff(time))
  #return(auc)
}

### calculating k, uptake rate
# R0 = initial ratio of nutrient/tracer (added ratio)
# Rx = auc ratio of nutrient/tracer
# width = stream width

calc_k <- function(R0, Rx, length) {
  k <- (log(R0)-log(Rx))/length   #log = natural log
}


### returning a vector of x,y values for the line, to subtract from auc values at each timepoint
# xy1 and xy2: the two endpoints of the line


  slope <- (y2-y1)/(x2-x1)
  intercept <- 



### calculating Q from a salt addition (using the auc)
# M = mass of salt (can also use conductance)
# conc = concentration of salt (can also use conductance)




################  CODE ##################################################

### Load data
# ==>> have all data on one csv and subset by date once loaded  <<==

juldf <- read_csv(here("field_data_workup/nutrient addition/data/20240722_ROHclass.csv"))
sep05df <- read_csv(here("field_data_workup/nutrient addition/data/20240905_partial.csv"))
sep15df <- read_csv(here("field_data_workup/nutrient addition/data/20240915_night.csv"))
octdf <- read_csv(here("field_data_workup/nutrient addition/data/20241008_day.csv"))

###### For all

# t <- as.numeric(df$elapsed_time)/60  #as.numeric gets it into seconds, dividing by 60 gets it into minutes
# t2 <-df$elapsed_time  # this creates an "hms" "difftime" object that won't work with the code

# ==>> ###### Move lines 48-58 to indiv sections <<==
# # subtract the background in the code!!!!  
# conc_n <- df$corr_n_conc
# sc_nacl <- df$corr_sc
# 
# # use conc_auc function to calculate aucs
# auc_n <- conc_auc(conc=conc_n, time=t)
# auc_sc <- conc_auc(conc=sc_nacl, time=t)


# Use initial ratio to figure out conservative n, and add to plot
  
###### July 22, 2024 --------------------------------------------------------------

df <- juldf

t <- as.numeric(df$elapsed_time)/60  #as.numeric gets it into seconds, dividing by 60 gets it into minutes
# t2 <-df$elapsed_time  # this creates an "hms" "difftime" object that won't work with the code

# subtract the background in the code!!!!  
conc_n <- df$corr_n_conc
sc_nacl <- df$corr_sc
ratios <- conc_n/sc_nacl

# use conc_auc function to calculate aucs
auc_n <- conc_auc(conc=conc_n, time=t)
auc_sc <- conc_auc(conc=sc_nacl, time=t)

# plot no3 data
plot(t, conc_n, main = "July 22")

n_add <- 1200*0.97*1e6/85 # in g; Hoss Chilean Nitrate is ~ 97% pure
# convert to umol NaNO3 -> 14+16*3+23 = 85g/ mol 

sc_add <- (23.23-0.08)*1000*2060 ## 23.23 kg salt- 0.08 kg empty bag, convert to grams and then conductivity based on measured 2060 uS/cm / g/L
width <- 17.6
reach_length <- 600

ratio_auc <- auc_n/auc_sc
ratio_init <- n_add/sc_add


# plot july22 ratios
plot(t, ratios, main = "July 22 nut/tracer ratios")
abline(h=ratio_init, col='darkorange')

k <- calc_k(R0=ratio_init, Rx=ratio_auc, length=reach_length) # units are m^-1
Sw <- 1/k # units are m
Q <- sc_add/auc_sc  # returns Q in L/min
Q_cms <- sc_add/(auc_sc*1000*60) #converts L/min to cms
Vf <- Q_cms*k*86400/width # units are m/d
# sampledate <- parse_date_time("7/22/2024", "mdy", tz = "America/Denver")

July2024 <- as_tibble(cbind(n_add, sc_add, width, reach_length, Q_cms, k, Sw, Vf))

July2024$sampledate <- parse_date_time("7/22/2024", "mdy", tz = "America/Denver")

### Clip to 90-100 min run time to compare w Sept 5

df_clip <- df %>%
  mutate(t = as.numeric(df$elapsed_time)/60) %>%
  filter(t <= 90) # try 100

t <- df_clip$t
conc_n <- df_clip$corr_n_conc
sc_nacl <- df_clip$corr_sc

plot(t, conc_n, main = "July 22 clip")
# use conc_auc function to calculate clipped aucs
auc_n <- conc_auc(conc=conc_n, time=t)
auc_sc <- conc_auc(conc=sc_nacl, time=t)

# run lines 61-78

# THEN: 
July2024_clip <- as_tibble(cbind(n_add, sc_add, width, reach_length, Q_cms, k, Sw, Vf))

July2024_clip$sampledate <- parse_date_time("7/22/2024", "mdy", tz = "America/Denver")

# travel time ~ 12 m/ min *1440 = /day * k (0.0002) = per day (d^-1) = how close is that to my model? VERY. That's why we're doing this.



###### Septenber 5, 2024 (partial) -------------------------------------------------
df <- sep05df

t <- as.numeric(df$elapsed_time)/60  #as.numeric gets it into seconds, dividing by 60 gets it into minutes
# t2 <-df$elapsed_time  # this creates an "hms" "difftime" object that won't work with the code

# subtract the background in the code!!!!  
conc_n <- df$corr_n_conc
sc_nacl <- df$corr_sc
ratios <- conc_n/sc_nacl

# use conc_auc function to calculate aucs
auc_n <- conc_auc(conc=conc_n, time=t)
auc_sc <- conc_auc(conc=sc_nacl, time=t)

# adjust for slanted baseline: 
#   calculate equation for line

# nutrient XYs : 
x1 <- 
x2 <- 
y1 <- 
y2 <-

slope <- (y2-y1)/(x2-x1)
intercept <- y1-slope*x1

df$baseline <- slope*df$elapsed_time - intercept

auc_n_corr <- auc_n-baseline

# plot no3 timeseries
plot(t, conc_n, main = "Sept 05")


n_add <- 1300*0.97*1e6/85 # Hoss Chilean Nitrate is ~ 97% pure
sc_add <- (23.12-0.08)*1000*2060 ## 23.23 kg salt- 0.08 kg empty bag, convert to grams and then conductivity based on measured 2060 uS/cm / g/L
width <- 17.5
reach_length <- 900

ratio_auc <- auc_n/auc_sc
ratio_init <- n_add/sc_add

# plot sept 05 ratios
plot(t, ratios, main = "September 05 nut/tracer ratios")
abline(h=ratio_init, col='darkorange')

k <- calc_k(R0=ratio_init, Rx=ratio_auc, length=reach_length) # units are m^-1
Sw <- 1/k # units are m
Q <- sc_add/auc_sc  # returns Q in L/min
Q_cms <- sc_add/(auc_sc*1000*60) #converts L/min to cms
Vf <- Q_cms*k*86400/width # units are m/
# sampledate <- parse_date_time("7/22/2024", "mdy", tz = "America/Denver")

Sept2024_05 <- as_tibble(cbind(n_add, sc_add, width, reach_length, Q_cms, k, Sw, Vf))

Sept2024_05$sampledate <- parse_date_time("9/05/2024", "mdy", tz = "America/Denver")



###### Sept 15, 2024 (night) -------------------------------------------------
df <- sep15df

t <- as.numeric(df$elapsed_time)/60  #as.numeric gets it into seconds, dividing by 60 gets it into minutes
# t2 <-df$elapsed_time  # this creates an "hms" "difftime" object that won't work with the code

# subtract the background in the code!!!!  
conc_n <- df$corr_n_conc
sc_nacl <- df$corr_sc
ratios <- conc_n/sc_nacl


# use conc_auc function to calculate aucs
auc_n <- conc_auc(conc=conc_n, time=t)
auc_sc <- conc_auc(conc=sc_nacl, time=t)

# plot no3 timeseries
plot(t, conc_n, main = "Sept 15")

# plot Sept 15 ratios - full
plot(t, ratios, main = "Sept 15 nut/tracer ratios")
abline(h=ratio_init, col='darkorange')

# clip to ~ 180min to compare w Sept 5

### BACKGROUND CHANGED - NEED TO FIGURE OUT HOW TO CODE AUC CALC WITH A SLOPED BASELINE ###
###     Currently, ratio at bottom is > ratio at top; clipping to 300 *barely* gave a + k

# for now, clip off some of the overlong tail... then re-run auc calc
df_clip <- df %>%
  mutate(t = as.numeric(df$elapsed_time)/60) %>%
  filter(t <= 225) # try 200... 40 min past plateau; nope too long
  
t <- df_clip$t
conc_n <- df_clip$corr_n_conc
sc_nacl <- df_clip$corr_sc
ratios <- conc_n/sc_nacl

sept15plot <- plot(t, conc_n, main = "Sept 15 clip 225")

# use conc_auc function to calculate aucs
auc_n <- conc_auc(conc=conc_n, time=t)
auc_sc <- conc_auc(conc=sc_nacl, time=t)

  
n_add <- 1250*0.97*1e6/85 # Hoss Chilean Nitrate is ~ 97% pure
sc_add <- (22.77-0.08)*1000*2060 ## 23.23 kg salt- 0.08 kg empty bag, convert to grams and then conductivity based on measured 2060 uS/cm / g/L
width <- 17.5
reach_length <- 900

ratio_auc <- auc_n/auc_sc
ratio_init <- n_add/sc_add

# plot Sept 15 ratios - clip
plot(t, ratios, main = "Sept 15 (225m) nut/tracer ratios")
abline(h=ratio_init, col='darkorange')


k <- calc_k(R0=ratio_init, Rx=ratio_auc, length=reach_length) # units are m^-1
Sw <- 1/k # units are m
Q <- sc_add/auc_sc  # returns Q in L/min
Q_cms <- sc_add/(auc_sc*1000*60) #converts L/min to cms
Vf <- Q_cms*k*86400/width # units are m/d
# sampledate <- parse_date_time("7/22/2024", "mdy", tz = "America/Denver")

Sept2024_15 <- as_tibble(cbind(n_add, sc_add, width, reach_length, Q_cms, k, Sw, Vf))

Sept2024_15$sampledate <- parse_date_time("9/15/2024", "mdy", tz = "America/Denver")

### CLIPPED DATA

Sept2024_15_clip <- as_tibble(cbind(n_add, sc_add, width, reach_length, Q_cms, k, Sw, Vf))

Sept2024_15_clip$sampledate <- parse_date_time("9/15/2024", "mdy", tz = "America/Denver")

###### October 08, 2024 (day) -------------------------------------------------
df <- octdf

t <- as.numeric(df$elapsed_time)/60  #as.numeric gets it into seconds, dividing by 60 gets it into minutes
# t2 <-df$elapsed_time  # this creates an "hms" "difftime" object that won't work with the code

# subtract the background in the code!!!!  
conc_n <- df$corr_n_conc
sc_nacl <- df$corr_sc
ratios <- conc_n/sc_nacl

# use conc_auc function to calculate aucs
auc_n <- conc_auc(conc=conc_n, time=t)
auc_sc <- conc_auc(conc=sc_nacl, time=t)

# full n timeseries plot
plot(t, conc_n, main = "Oct 08")

n_add <- 2000*0.97*1e6/85 # Hoss Chilean Nitrate is ~ 97% pure
sc_add <- (23.27-0.08)*1000*2060 ## 23.23 kg salt- 0.08 kg empty bag, convert to grams and then conductivity based on measured 2060 uS/cm / g/L
width <- 17.5
reach_length <- 900

ratio_auc <- auc_n/auc_sc
ratio_init <- n_add/sc_add

# plot oct 08 ratios
plot(t, ratios, ylim=c(0, 1), main = "October 8 nut/tracer ratios")
abline(h=ratio_init, col='darkorange')


k <- calc_k(R0=ratio_init, Rx=ratio_auc, length=reach_length) # units are m^-1
Sw <- 1/k # units are m
Q <- sc_add/auc_sc  # returns Q in L/min
Q_cms <- sc_add/(auc_sc*1000*60) #converts L/min to cms
Vf <- Q_cms*k*86400/width # units are m/d
# sampledate <- parse_date_time("7/22/2024", "mdy", tz = "America/Denver")

Oct2024 <- as_tibble(cbind(n_add, sc_add, width, reach_length, Q_cms, k, Sw, Vf))

Oct2024$sampledate <- parse_date_time("10/08/2024", "mdy", tz = "America/Denver")

### Clip to ~ 220 min run time to compare w Sept 5

df_clip <- df %>%
  mutate(t = as.numeric(df$elapsed_time)/60) %>%
  filter(t <= 220)

t <- df_clip$t
conc_n <- df_clip$corr_n_conc
sc_nacl <- df_clip$corr_sc

# use conc_auc function to calculate clipped aucs
auc_n <- conc_auc(conc=conc_n, time=t)
auc_sc <- conc_auc(conc=sc_nacl, time=t)

plot(t, conc_n, main = "Oct 08 clip")

# run lines 190-202

# THEN: 
Oct2024_clip <- as_tibble(cbind(n_add, sc_add, width, reach_length, Q_cms, k, Sw, Vf))

Oct2024_clip$sampledate <- parse_date_time("10/08/2024", "mdy", tz = "America/Denver")


######  Bind all outputs, save as CSV --------------------------------------------------------


uptake2024 <- bind_rows(July2024, Sept2024_05, Sept2024_15, Oct2024)

write_csv(uptake2024, here("field_data_workup/nutrient addition/data/uptake2024.csv"))


######  Compare full additions w Sept 05 (which ended ~ 35 m past peak/plateau)

uptake2024_clip <- bind_rows(July2024_clip, Sept2024_05, Sept2024_15_clip, Oct2024_clip)
write_csv(uptake2024_clip, here("field_data_workup/nutrient addition/data/uptake2024_clip.csv"))

