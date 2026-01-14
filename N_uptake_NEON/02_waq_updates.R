## waq2 changes

# LOAD and EDIT BIGC and PRIN waq from orig file, then remove tsw_sonde days that were NA for waq...
# continue QCing through tsw_15, then 
# reload bigc and prin do files, and re-write tsa_15

bigc_do <- read_csv(here("N_uptake_NEON/GPP_model/DO_data_CLT/bigc_do.csv"))



bigcWT <- bigc_do$temp.water

bigc_do$temp.water <- tsw_15$sonde_wtemp[match(bigc_do$startDateTime, tsw_15$startDateTime)]

bigcWT2 <- bigc_do$temp.water

plot(bigcWT, bigcWT2,
     xlab = "Before replacement",
     ylab = "After replacement",
     main = "Water Temperature: Before vs After",
     pch = 19, col = "steelblue")
abline(0, 1, col = "red", lty = 2)

filepath <- here("N_uptake_NEON/GPP_model/DO_data_CLT/bigc_do_2224.csv")
write_csv(bigc_do,file=filepath) 



prin_do <- read_csv(here("N_uptake_NEON/GPP_model/DO_data_CLT/prin_do_2224.csv"))

prinWT <- prin_do$temp.water

prin_do$temp.water <- tsw_15$sonde_wtemp[match(prin_do$startDateTime, tsw_15$startDateTime)]

prinWT2 <- prin_do$temp.water

plot(prinWT, prinWT2,
     xlab = "Before replacement",
     ylab = "After replacement",
     main = "Water Temperature: Before vs After",
     pch = 19, col = "steelblue")
abline(0, 1, col = "red", lty = 2)

filepath <- here("N_uptake_NEON/GPP_model/DO_data_CLT/prin_do.csv")
write_csv(prin_do,file=filepath) 



############################################ BIGC ############################################

waqS2 <-  makeNAusing_thresholds_and_dates(
  data = waqS2,
  datetime_col = startDateTime,
  target_col = dissolvedOxygen,
  dates = c("2022-07-11"),
  threshold = 8.6,
  operator = ">"
)
waqS2 <-  makeNAusing_thresholds_and_dates(
  data = waqS2,
  datetime_col = startDateTime,
  target_col = dissolvedOxygen,
  dates = c("2023-10-25"),
  threshold = 9.3,
  operator = ">"
)
waqS2 <- makeNAusing_thresholds_and_dates(
  data = waqS2, 
  datetime_col = startDateTime, 
  target_col = dissolvedOxygen, 
  dates = c("2024-02-05"), 
  threshold = 9)

waqS2 <- makeNAusing_thresholds_and_dates(
  data = waqS2, 
  datetime_col = startDateTime, 
  target_col = dissolvedOxygen, 
  dates = c("2024-06-06"), 
  threshold = 8)


### water temp ###
highT_cutoff <- 26

# tsw_sonde <- makeNAusing_thresholds_and_dates(
#   data = tsw_sonde, 
#   datetime_col = startDateTime, 
#   target_col = temperature, 
#   dates = c("2024-07-05"), 
#   threshold = 5)
# 
# bp <- makeNAusing_thresholds_and_dates(
#   data = bp, 
#   datetime_col = startDateTime, 
#   target_col = corPres_mb, 
#   dates = c("2024-01-12"), 
#   operator = ">",
#   threshold = 1031)
# 
# bp <- makeNAusing_thresholds_and_dates(
#   data = bp, 
#   datetime_col = startDateTime, 
#   target_col = corPres_mb, 
#   dates = c("2024-04-06"), 
#   operator = ">",
#   threshold = 1029)


############################################ PRIN ############################################ 

####################### replaces select S2 values with S1 values ####################### 

# function 'in_any_window()': replace_times, starts, ends

# Replacement windows
windows <- tibble(
  start = ymd_hms(c("2022-05-25 00:00:00", "2024-05-29 00:00:00", "2024-07-24 00:00:00")),
  end   = ymd_hms(c("2022-06-09 23:59:59", "2024-06-06 23:59:59", "2024-08-13 23:59:59"))
)

# Match timestamps from S2 to S1
match_idx <- match(waqS2$startDateTime, waqS1$startDateTime)

# Logical vector for replacements:
#   - timestamp matches in S1
#   - falls in a replacement window
#   - S1 has a non-NA DO value

replace_idx <- !is.na(match_idx) &
  in_any_window(waqS2$startDateTime, windows$start, windows$end) &
  !is.na(waqS1$dissolvedOxygen[match_idx])

# Replace S2 values in place
waqS2$dissolvedOxygen[replace_idx] <- waqS1$dissolvedOxygen[match_idx[replace_idx]]

waqS2 <- makeNAusing_thresholds_and_dates(
  data = waqS2,
  datetime_col = startDateTime,
  target_col = dissolvedOxygen,
  dates = c("2022-05-25"),
  threshold = 4)

waqS2 <- makeNAusing_thresholds_and_dates(
  data = waqS2,
  datetime_col = startDateTime,
  target_col = dissolvedOxygen,
  dates = c("2022-05-28"),
  threshold = 6)

waqS2 <- makeNAusing_thresholds_and_dates(
  data = waqS2,
  datetime_col = startDateTime,
  target_col = dissolvedOxygen,
  dates = c("2022-05-26", "2022-05-27"),
  threshold = 7.2)

waqS2 <- makeNAusing_thresholds_and_dates(
  data = waqS2,
  datetime_col = startDateTime,
  target_col = dissolvedOxygen,
  dates = c("2022-06-02"),
  threshold = 6.8)

waqS2 <- makeNAusing_thresholds_and_dates(
  data = waqS2,
  datetime_col = startDateTime,
  target_col = dissolvedOxygen,
  dates = list(start="2023-03-07", end="2023-03-09"),
  threshold = 7)

waqS2 <-  makeNAusing_thresholds_and_dates(
  data = waqS2,
  datetime_col = startDateTime,
  target_col = dissolvedOxygen,
  dates = c("2023-09-23"),
  threshold = 6.8,
  #operator = "<"
)

waqS2 <-  makeNAusing_thresholds_and_dates(
  data = waqS2,
  datetime_col = startDateTime,
  target_col = dissolvedOxygen,
  dates = c("2024-04-24"),
  threshold = 5.6,
  #operator = "<"
)

waqS2 <- waqS2 %>%
  mutate(dissolvedOxygen = if_else(
    between(startDateTime, 
            ymd_hms("2024-05-28 00:00:00", tz = "UTC"), 
            ymd_hms("2024-06-06 23:59:59", tz = "UTC")),
    NA_real_,
    dissolvedOxygen)
  )

waqS2_15 <- waqS2_15 %>%
  mutate(dissolvedOxygen = if_else(
    between(startDateTime, 
            ymd_hms("2024-05-29 00:00:00", tz = "UTC"), 
            ymd_hms("2024-06-03 23:59:59", tz = "UTC")),
    NA_real_,
    temperature)
  )


###########################################################################
#######################  water.temp  #############################  

#######################  Clean sonde data  ####################### 

sondeNAs <- which(is.na(tsw_sonde$temperature))  # no NA values, then 524 w. 24 cutoff  

# highT_cutoff <- 50 # removed extreme outliers: THEN
highT_cutoff <- 33 #  # removed unreasonably high values - checking w. Bobby H re this.
tsw_sonde <- tsw_sonde %>%
  mutate(temperature = if_else(temperature >= highT_cutoff, NA_real_, temperature))

sondeNAs <- which(is.na(tsw_sonde$temperature))   # now 524


#######################  remove obvious outliers  ####################### 

# default operator is "<"
tsw_sonde <- makeNAusing_thresholds_and_dates(
  data = tsw_sonde,
  datetime_col = startDateTime,
  target_col = temperature,
  dates = c("2023-02-22"),
  threshold = 7)

# fill gaps then re-check

#######################  Re-clean sonde data  #######################  

sondeNAs <- which(is.na(tsw_sonde$temperature))  # no NA values, then 524 w. 24 cutoff  

# highT_cutoff <- 50
highT_cutoff <- 36
tsw_sonde_15_filled <- tsw_sonde_15_filled %>%
  mutate(temperature = if_else(temperature >= highT_cutoff, NA_real_, temperature))

tsw_sonde_15_filled$temperature<-zoo::na.approx(tsw_sonde_15_filled$temperature,maxgap=24)

sonde15NAs <- which(is.na(tsw_sonde_15_filled$temperature))   # now 5608


tsw_15 <- tsw_sonde_15_filled %>% # rename so the workflow goes smoothly
  rename(sonde_wtemp = temperature) 



##### ALSO: tswS2 data
# cutoff was 26
tswS2 <- makeNAusing_thresholds_and_dates(
  data = tswS2, 
  datetime_col = startDateTime, 
  target_col = surfWaterTempMean, 
  dates = c("2022-08-08"), 
  operator = ">",
  threshold = 25)


############################################  SYCA ############################################ 

waqS2 <-  makeNAusing_thresholds_and_dates(
  data = waqS2,
  datetime_col = startDateTime,
  target_col = dissolvedOxygen,
  dates = c("2022-08-19"),
  threshold = 3.5,
  operator = ">"
)

waqS2 <-  makeNAusing_thresholds_and_dates(
  data = waqS2,
  datetime_col = startDateTime,
  target_col = dissolvedOxygen,
  dates = c("2022-08-25"),
  threshold = 4,
  operator = ">"
)

waqS2 <-  makeNAusing_thresholds_and_dates(
  data = waqS2,
  datetime_col = startDateTime,
  target_col = dissolvedOxygen,
  dates = c("2022-09-09", "2022-09-11"),
  threshold = 1.8
)

waqS2 <-  makeNAusing_thresholds_and_dates(
  data = waqS2,
  datetime_col = startDateTime,
  target_col = dissolvedOxygen,
  dates = c("2022-09-15"),
  threshold = 2.3
)

waqS2 <-  makeNAusing_thresholds_and_dates(
  data = waqS2,
  datetime_col = startDateTime,
  target_col = dissolvedOxygen,
  dates = list(start="2022-09-19", end="2022-09-22"),
  threshold = 1.6
)

waqS2 <-  makeNAusing_thresholds_and_dates(
  data = waqS2,
  datetime_col = startDateTime,
  target_col = dissolvedOxygen,
  dates = c("2022-09-28"),
  threshold = 1.9
)

