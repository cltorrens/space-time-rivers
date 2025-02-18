# Use tidybayes to extract stuff from model fits for the BIGC (Big Creek, CA) runs comparing process and observation error
# Created 1/10/2025 by Christa Torrens

# Load libraries
library(tidyverse)
library(tidybayes)
library(here)


# Fit models in "04_run pooled NO3 models_NEON data.qmd"
#     Model fit names are 'fit.bigc19.obsE' and 'fit.bigc19.procE'

# Warning messages generated for the process error model fit:
# Warning messages:
#   1: There were 39 divergent transitions after warmup. See
# https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# to find out why this is a problem and how to eliminate them.
# 2: There were 4 chains where the estimated Bayesian Fraction of Missing Information was low. See
# https://mc-stan.org/misc/warnings.html#bfmi-low
# 3: Examine the pairs() plot to diagnose sampling problems
# 
# 4: The largest R-hat is NA, indicating chains have not mixed.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#r-hat
# 5: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#bulk-ess
# 6: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#tail-ess



###### Extracting values from the OBSERVATION ERROR model fit --------------------------------------------

# What was the modeled uptake?
U_mod <- rstan::extract(fit.bigc19.obsE, pars = "U")$U
U_mod_avg <- apply(U_mod, MARGIN = 2, FUN = mean) 
U_mod_sd <- apply(U_mod, MARGIN = 2, FUN = sd)

# what was modeled K? 

K_mod <- rstan::extract(fit.bigc19.obsE, pars = "K")$K
K_mod_avg <- apply(K_mod, MARGIN = 2, FUN = mean) 
K_mod_sd <- apply(K_mod, MARGIN = 2, FUN = sd)

# Get conc_hat from fit; plot vs concMA
conc_hat <- rstan::extract(fit.bigc19.obsE, pars = "conc_hat")$conc_hat 
#conc_hat.oe <- extract(fit.oe, pars = "conc_hat")$conc_hat 

# > dim(conc_hat)
# [1] 4000   24   39

# Collapse the 4000-layer array to a matrix rows = hours, columns = days - just like concMA
#avg_conc_hat_oeMA <- apply(conc_hat.oe, MARGIN = c(2, 3), FUN = mean) 
avg_conc_hat_MA <- apply(conc_hat, MARGIN = c(2, 3), FUN = mean)  


#avg_conc_hat.oe <- as.vector(c(avg_conc_hat_oeMA))
N_conc_hat <- as.vector(c(avg_conc_hat_MA))

#U_mean <- as.vector(c(U_mod_avg))

N_conc <- bigc.df.19h$surfWaterNitrateMean ##as.vector(c(concMA)) would give the same values

local_datetime <- bigc.df.19h$local_datetime  
model_datetime <- bigc.df.19h$local_datetime - hours(4)
model_day <-bigc.df.19h$model_day
hours <- hour(local_datetime)
mod_hours <- hour(model_datetime)
# find a way to remove day #212

mod_day <- unique(model_day)

N_output.df <- data.frame(local_datetime, hours, mod_hours, model_datetime, model_day, N_conc, N_conc_hat)

#mod_day <- unique(model_day)
U_output.df <- data.frame(mod_day, U_mod_avg, U_mod_sd, sumlight.real, model_datetime) 
K_output.df <- data.frame(mod_day, K_mod_avg, K_mod_sd, sumlight.real, model_datetime)

mean(K_mod_avg) # 3.255  #3.05

# credible intervals for each U
# extract 2.5% and 97.5% values - see Alice's code?

which.min(U_output.df$U_mean)

U_output.df$U_mod_avg

###### N and N-hat over time
N_and_Nhat <- N_output.df %>%
  filter(model_day >= 175 & model_day <= 185) %>%  # to see these better...
  ggplot(aes(x=mod_hours)) +
  geom_point(aes(y=N_conc)) + 
  geom_line(aes(y=N_conc_hat), col='red')+
  labs(
    x="Time (h)", y=expression("N"~(mmol~m^-3))
  ) +
  #ggtitle("N and N_hat over time - Big Creek 2019 pooled model")+
  ggtitle("NEON: Big Creek  - observation error model") +
  facet_wrap(~model_day)+
  #title("N conc vs conc-hat, Big Creek pooled 1 (by mean)")+
  #scale_color_manual(values=c("N_conc" = "black", "N_conc_hat" = "red"), name= "Big Creek N") +
  theme_bw()

quartz()
N_and_Nhat
# Use 'for' loop with matrix version or use hours as the x-axis... 

# datetimeMA <- matrix(local_datetime, nrow=24)
# 
# quartz()
# for (i in 1:nday) {
#   plot (datetimeMA[,i], concMA[,i])  
#  lines(datetimeMA[,i], avg_conc_hat_MA[,i], col='red')
# }


########  N-hat vs N


Nhat_V_N <- ggplot(data = N_output.df, aes(x=N_conc, y=N_conc_hat)) +
  geom_point() + 
  xlab("measured N (umol/L)") + ylab("modeled N (umol/L)") + 
  ggtitle("Measured N vs modeled N, Big Creek 2019 pooled model - observation error model") +
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  theme_bw()

quartz()
Nhat_V_N

##### U over time


U_time <- ggplot(data = U_output.df, aes(x=mod_day, y=U_mod_avg)) +
  geom_point() + 
  # geom_point(y = sumlight.real, color = 'gold') +
  # ADD IN HIGH AND LOW CIs
  xlab("Julian day") + ylab("modeled U (mmol/m2/day)") + 
  #ylim(0,1) +
  #ggtitle("modeled U over time, Big Creek 2019 pooled model w real light") +
  ggtitle("Diel nitrate uptake (modeled), Big Creek 2019 - observation error model") +
  theme_bw()

quartz()
U_time


U_time_clip <- ggplot(data = U_output.df, aes(x=mod_day)) +
  geom_point(y=U_mod_avg) + 
  # geom_point(y = sumlight.real, color = 'gold') +
  # ADD IN HIGH AND LOW CIs
  xlab("Julian day") + ylab("modeled U (mmol/m2/day)") + 
  ylim(0,2) +
  ggtitle("modeled U over time, Big Creek 2019 pooled model w real light - observation error model") +
  theme_bw()

quartz()
U_time_clip

###### U vs sumlight


U_vs_light <- ggplot(data = U_output.df, aes(x=sumlight.real, y=U_mod_avg)) +
  geom_point() + 
  #xlab("true light (satellite)") + ylab("modeled U (mmol/m2/day)") +
  xlab("light (satellite)") + ylab("modeled U (mmol/m2/day)") +
  ggtitle("Big Creek 2019: scatterplot of NO3 uptake and daily light - observation error model") +
  #ylim = c(-0.2, 1) +
  theme_bw()

quartz()
U_vs_light

#######  K over time


K_time <- ggplot(data = K_output.df, aes(x=mod_day)) +
  geom_point(y=K_mod_avg) + 
  # geom_point(y = sumlight.real, color = 'gold') +
  # ADD IN HIGH AND LOW CIs
  xlab("Julian day") + ylab("modeled K (day -1)") + # daily change in N concentration
  ylim(0,20) +
  ggtitle("modeled K over time, Big Creek 2019 pooled model w real light - observation error model") +
  theme_bw()

quartz()
K_time



K_time_clip <- ggplot(data = K_output.df, aes(x=mod_day)) +
  geom_point(y=K_mod_avg) + 
  # geom_point(y = sumlight.real, color = 'gold') +
  # ADD IN HIGH AND LOW CIs
  xlab("Julian day") + ylab("modeled K (umol/day)") + # ??? UNITS?
  ylim(0,10) +
  ggtitle("modeled K over time, Big Creek 2019 pooled model w real light") +
  theme_bw()

quartz()
K_time_clip



# U vs K

plot(U_mod_avg, K_mod_avg)

dev.off()

# write.csv(p.bigc,file="N_output_pooledU_bigc.csv")
# output_bigc <- read_csv("N_output_pooledU_bigc.csv")
# 
# max.print(fit,pars="conc_tilde")





###### Extracting values from the PROCESS ERROR model fit -------------------------------------------------

# What was the modeled uptake?
U_mod <- rstan::extract(fit.bigc19.procE, pars = "U")$U
U_mod_avg <- apply(U_mod, MARGIN = 2, FUN = mean) 
U_mod_sd <- apply(U_mod, MARGIN = 2, FUN = sd)

# what was modeled K? 

K_mod <- rstan::extract(fit.bigc19.procE, pars = "K")$K
K_mod_avg <- apply(K_mod, MARGIN = 2, FUN = mean) 
K_mod_sd <- apply(K_mod, MARGIN = 2, FUN = sd)

# Get conc_hat from fit; plot vs concMA
conc_hat <- rstan::extract(fit.bigc19.procE, pars = "conc_hat")$conc_hat 
#conc_hat.oe <- extract(fit.oe, pars = "conc_hat")$conc_hat 

# > dim(conc_hat)
# [1] 4000   24   39

# Collapse the 4000-layer array to a matrix rows = hours, columns = days - just like concMA
#avg_conc_hat_oeMA <- apply(conc_hat.oe, MARGIN = c(2, 3), FUN = mean) 
avg_conc_hat_MA <- apply(conc_hat, MARGIN = c(2, 3), FUN = mean)  


#avg_conc_hat.oe <- as.vector(c(avg_conc_hat_oeMA))
N_conc_hat <- as.vector(c(avg_conc_hat_MA))

#U_mean <- as.vector(c(U_mod_avg))

N_conc <- bigc.df.19h$surfWaterNitrateMean ##as.vector(c(concMA)) would give the same values

local_datetime <- bigc.df.19h$local_datetime  
model_datetime <- bigc.df.19h$local_datetime - hours(4)
model_day <-bigc.df.19h$model_day
hours <- hour(local_datetime)
mod_hours <- hour(model_datetime)
# find a way to remove day #212

mod_day <- unique(model_day)

N_output.df <- data.frame(local_datetime, hours, mod_hours, model_datetime, model_day, N_conc, N_conc_hat)

#mod_day <- unique(model_day)
U_output.df <- data.frame(mod_day, U_mod_avg, U_mod_sd, sumlight.real, model_datetime) 
K_output.df <- data.frame(mod_day, K_mod_avg, K_mod_sd, sumlight.real, model_datetime)

mean(K_mod_avg) # 3.255  #3.05

# credible intervals for each U
# extract 2.5% and 97.5% values - see Alice's code?

which.min(U_output.df$U_mean)

U_output.df$U_mod_avg

###### N and N-hat over time
N_and_Nhat <- N_output.df %>%
  filter(model_day >= 175 & model_day <= 185) %>%  # to see these better...
  ggplot(aes(x=mod_hours)) +
  geom_point(aes(y=N_conc)) + 
  geom_line(aes(y=N_conc_hat), col='red')+
  labs(
    x="Time (h)", y=expression("N"~(mmol~m^-3))
  ) +
  #ggtitle("N and N_hat over time - Big Creek 2019 pooled model")+
  ggtitle("NEON: Big Creek 2019 pooled model - process error model") +
  facet_wrap(~model_day)+
  #title("N conc vs conc-hat, Big Creek pooled 1 (by mean)")+
  #scale_color_manual(values=c("N_conc" = "black", "N_conc_hat" = "red"), name= "Big Creek N") +
  theme_bw()

quartz()
N_and_Nhat
# Use 'for' loop with matrix version or use hours as the x-axis... 

# datetimeMA <- matrix(local_datetime, nrow=24)
# 
# quartz()
# for (i in 1:nday) {
#   plot (datetimeMA[,i], concMA[,i])  
#  lines(datetimeMA[,i], avg_conc_hat_MA[,i], col='red')
# }


########  N-hat vs N


Nhat_V_N <- ggplot(data = N_output.df, aes(x=N_conc, y=N_conc_hat)) +
  geom_point() + 
  xlab("measured N (umol/L)") + ylab("modeled N (umol/L)") + 
  ggtitle("Measured N vs modeled N, Big Creek 2019 pooled model - process error model") +
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  theme_bw()

quartz()
Nhat_V_N

##### U over time


U_time <- ggplot(data = U_output.df, aes(x=mod_day, y=U_mod_avg)) +
  geom_point() + 
  # geom_point(y = sumlight.real, color = 'gold') +
  # ADD IN HIGH AND LOW CIs
  xlab("Julian day") + ylab("modeled U (mmol/m2/day)") + 
  #ylim(0,1) +
  #ggtitle("modeled U over time, Big Creek 2019 pooled model w real light") +
  ggtitle("Diel nitrate uptake (modeled), Big Creek 2019 - process error model") +
  theme_bw()

quartz()
U_time


U_time_clip <- ggplot(data = U_output.df, aes(x=mod_day)) +
  geom_point(y=U_mod_avg) + 
  # geom_point(y = sumlight.real, color = 'gold') +
  # ADD IN HIGH AND LOW CIs
  xlab("Julian day") + ylab("modeled U (mmol/m2/day)") + 
  ylim(0,2) +
  ggtitle("modeled U over time, Big Creek 2019 pooled model w real light - process error model") +
  theme_bw()

quartz()
U_time_clip

###### U vs sumlight


U_vs_light <- ggplot(data = U_output.df, aes(x=sumlight.real, y=U_mod_avg)) +
  geom_point() + 
  #xlab("true light (satellite)") + ylab("modeled U (mmol/m2/day)") +
  xlab("light (satellite)") + ylab("modeled U (mmol/m2/day)") +
  ggtitle("Big Creek 2019: scatterplot of NO3 uptake and daily light - process error model") +
  #ylim = c(-0.2, 1) +
  theme_bw()

quartz()
U_vs_light

#######  K over time


K_time <- ggplot(data = K_output.df, aes(x=mod_day)) +
  geom_point(y=K_mod_avg) + 
  # geom_point(y = sumlight.real, color = 'gold') +
  # ADD IN HIGH AND LOW CIs
  xlab("Julian day") + ylab("modeled K (day -1)") + # daily change in N concentration
  ylim(0,20) +
  ggtitle("modeled K over time, Big Creek 2019 pooled model w real light - process error model") +
  theme_bw()

quartz()
K_time



K_time_clip <- ggplot(data = K_output.df, aes(x=mod_day)) +
  geom_point(y=K_mod_avg) + 
  # geom_point(y = sumlight.real, color = 'gold') +
  # ADD IN HIGH AND LOW CIs
  xlab("Julian day") + ylab("modeled K (umol/day)") + # ??? UNITS?
  ylim(0,10) +
  ggtitle("modeled K over time, Big Creek 2019 pooled model w real light - process error model") +
  theme_bw()

quartz()
K_time_clip



# U vs K

plot(U_mod_avg, K_mod_avg)

dev.off()

# write.csv(p.bigc,file="N_output_pooledU_bigc.csv")
# output_bigc <- read_csv("N_output_pooledU_bigc.csv")
# 
# max.print(fit,pars="conc_tilde")


