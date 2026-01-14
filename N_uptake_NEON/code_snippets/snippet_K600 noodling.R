library(tidyverse)
library(tidybayes)
library(here)


wlou_K600 <- readRDS(here("N_uptake_NEON/data/z_K600_Aho_EDI/WLOU_big_K600_v1.rds"))
gasExchange_ds_v2 <- read_csv(here("N_uptake_NEON/data/z_K600_Aho_EDI/gasExchange_ds_v2") 

gasExch_wlou <- gasExchange_ds_v2 %>%
  dplyr::filter(siteID == "WLOU")

wlou_K600.spread <- spread_draws(wlou_K600, Kd[d], logK600[d], a, b)

wlou_K600.meanqi <- wlou_K600.spread %>%
  mean_qi() %>%
  add_column(site = "WLOU") %>%
  mutate(exp_logK600 = exp(logK600), 
         diff = exp_logK600-Kd, 
         Q_lps = gasExch_wlou$meanQ_lps, 
         K_calc = a*Q_lps^b)

plot(wlou_K600.meanqi$Kd, wlou_K600.meanqi$exp_logK600)
plot(wlou_K600.meanqi$Kd, wlou_K600.meanqi$K_calc)
plot(wlou_K600.meanqi$exp_logK600, wlou_K600.meanqi$K_calc)
plot(wlou_K600.meanqi$logK600, wlou_K600.meanqi$K_calc)
abline(a=0, b=1)
