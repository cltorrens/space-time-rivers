## Checking K_nit and residence time (1/K_nit) values from stan model fits
#  mean residence time for all is < 1 day (range of mean tau = 6.5h-15.8h)
# BIGC = 0.39 days (~9.4 h)
# CARI = 0.66 days (~15.8 h)
# CUPE = 0.27 days (~6.5 h)
# PRIN = 0.62 days (~14.9h)
# WLOU = 0.31 days (~ 7.4 h)

#########  BIGC ################
bigc.fit <- readRDS(here("N_uptake_NEON/data/model_fits/bigc.fit.rds"))
bigc.spread <- spread_draws(bigc.fit, K[d], U[d], N_e[d]) 

# mean + QI
bigc.meanqi <- bigc.spread %>%
  mean_qi() %>%
  add_column(site = "BIGC")

range(bigc.meanqi$K)  #  1.796969 9.517582
mean(bigc.meanqi$K)   #  2.577008
median(bigc.meanqi$K) #  2.480447

1/mean(bigc.meanqi$K) #  0.3880469 days


# median + QI
bigc.medqi <- bigc.spread %>%
  median_qi() %>%
  add_column(site = "BIGC")

range(bigc.medqi$K)  #  1.721050 9.480995
mean(bigc.medqi$K)   #  2.411285
median(bigc.medqi$K) #  2.313678


##########  CARI ################
cari.fit <- readRDS(here("N_uptake_NEON/data/model_fits/cari.fit.rds"))
cari.spread <- spread_draws(cari.fit, K[d], U[d], N_e[d]) 

### mean + QI
cari.meanqi <- cari.spread %>%
  mean_qi() %>%
  add_column(site = "CARI")

range(cari.meanqi$K)  #  0.5475838 2.9927245
mean(cari.meanqi$K)   #  1.5179
median(cari.meanqi$K) #  1.512851

1/mean(cari.meanqi$K) #  0.6588048 days

### median + QI
cari.medqi <- cari.spread %>%
  median_qi() %>%
  add_column(site = "CARI")

range(cari.medqi$K)  #  0.5267835 2.6971147
mean(cari.medqi$K)   #  1.43725
median(cari.medqi$K) #  1.429016


#########  CUPE ################
cupe.fit <- readRDS(here("N_uptake_NEON/data/model_fits/cupe.fit.rds"))
cupe.spread <- spread_draws(cupe.fit, K[d], U[d], N_e[d]) 

### mean + QI
cupe.meanqi <- cupe.spread %>%
  mean_qi() %>%
  add_column(site = "CUPE")

range(cupe.meanqi$K)  #  1.944345 23.410399
mean(cupe.meanqi$K)   #  3.674639
median(cupe.meanqi$K) #  3.52047

1/mean(cupe.meanqi$K) #  0.2721356 days 

### median + QI
cupe.medqi <- cupe.spread %>%
  median_qi() %>%
  add_column(site = "CUPE")

range(cupe.medqi$K)  #  1.911134 23.405603
mean(cupe.medqi$K)   #  3.62563
median(cupe.medqi$K) #  3.487091



#########  PRIN ################
prin.fit <- readRDS(here("N_uptake_NEON/data/model_fits/prin.fit.rds"))
prin.spread <- spread_draws(prin.fit, K[d], U[d], N_e[d]) 

### mean + QI
prin.meanqi <- prin.spread %>%
  mean_qi() %>%
  add_column(site = "PRIN")

range(prin.meanqi$K)  #  0.6491682 2.7484695
mean(prin.meanqi$K)   #  1.617306
median(prin.meanqi$K) #  1.556586

1/mean(prin.meanqi$K) #  0.6183121 days

### median + QI
prin.medqi <- prin.spread %>%
  median_qi() %>%
  add_column(site = "PRIN")

range(prin.medqi$K)  #  0.6282701 2.5553823
mean(prin.medqi$K)   #  1.52413
median(prin.medqi$K) #  1.488541


#########  WLOU ################

wlou.fit <- readRDS(here("N_uptake_NEON/data/model_fits/wlou.fit.rds"))
wlou.spread <- spread_draws(wlou.fit, K[d], U[d], N_e[d]) 

### mean + QI
wlou.meanqi <- wlou.spread %>%
  mean_qi() %>%
  add_column(site = "WLOU")

range(wlou.meanqi$K)  #  1.834569 8.163136
mean(wlou.meanqi$K)   #  3.234137
median(wlou.meanqi$K) #  2.999537

1/mean(wlou.meanqi$K) #  0.3092015 days

### median + QI
wlou.medqi <- wlou.spread %>%
  median_qi() %>%
  add_column(site = "WLOU")

range(wlou.medqi$K)  #  1.740871 7.945978
mean(wlou.medqi$K)   #  3.100244
median(wlou.medqi$K) #  2.836234

