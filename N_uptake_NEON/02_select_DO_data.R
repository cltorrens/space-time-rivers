# ## The purpose of this script is to take datasets provided by Bobby Hensley (NEON) OR that I created (CLT), and select
#   the days that I need to estimate GPP for my nitrate model. This is the link to the folder Bobby shared,
#   which he is populating with DO **and co-located temperature data** for his collaboration with Kelly Aho.
#   https://drive.google.com/drive/folders/180khdU7rR65UqWKDQu8WPEwMSo_U0J5E?usp=drive_link



# load packages
library(tidyverse)
library(here)
library(plotly)

# Load data: 

# FOR BOBBY'S DATA
path <- here("N_uptake_NEON/data/model_output/daily_summary_all.rds")
daily_summary.df <- readRDS(file=path) 


#####  CARI 
cari_do_wy22 <- read_csv(here("N_uptake_NEON/data/DO_data_Hensley/CARI_WY22.csv"))
cari_do_wy23 <- read_csv(here("N_uptake_NEON/data/DO_data_Hensley/CARI_WY23.csv"))
# cari_do_wy24 <- read_csv(here("N_uptake_NEON/data/DO_data_Hensley/CARI_WY24.csv"))

# Get object with CARI's yr_jdays 
cari_yrjday <- daily_summary.df %>%
  dplyr::filter(site == "CARI") %>%
  pull(yr_jday) 

# Combine the CARI water year files into 1 file, create a yr_jday column, then select the yr_jdays to match my data
cari_do_all <- bind_rows(cari_do_wy22, cari_do_wy23) %>%
  mutate(Year = year(solar.time), 
         jday = yday(solar.time), 
         jday_pad = str_pad(jday, width=3, pad="0")) %>%
  unite("yr_jday", Year, jday_pad, sep = '_', remove=FALSE) %>%
  select(yr_jday, solar.time, DO.obs, temp.water, discharge) 

cari_do_select <- cari_do_all %>%
  dplyr::filter(yr_jday %in% cari_yrjday)


# the days aren't even: 17569 obs = 116 days + 15 min (1 obs)
#wlou_obsPERday <- wlou_do_select %>% count(yr_jday) # Sept 30, 2022 has 97 obs (not 96) (2022_273)

cari_check <- cari_do_select %>%
  count(yr_jday) %>%
  filter(n != 96)

cari_view <- cari_do_select %>%
  dplyr::filter(yr_jday %in% cari_check$yr_jday)

# one timestep is duplicated (rows 57 and 58): similar values, same timestep. Removing #57
cari_do_select <- cari_do_select %>%
  dplyr::slice(-57)

path <- here("N_uptake_NEON/data/DO_data_model/cari_do.csv")
write_csv(cari_do_select, file=path)


#####  CUPE 
cupe_do_wy22 <- read_csv(here("N_uptake_NEON/data/DO_data_Hensley/CUPE_WY22.csv"))
cupe_do_wy23 <- read_csv(here("N_uptake_NEON/data/DO_data_Hensley/CUPE_WY23.csv"))
# cupe_do_wy24 <- read_csv(here("N_uptake_NEON/data/DO_data_Hensley/CUPE_WY24.csv"))

# Get object with CUPE's yr_jdays 
cupe_yrjday <- daily_summary.df %>%
  dplyr::filter(site == "CUPE") %>%
  pull(yr_jday) 

# Combine the CUPE water year files into 1 file, create a yr_jday column, then select the yr_jdays to match my data
cupe_do_all <- bind_rows(cupe_do_wy22, cupe_do_wy23) %>%
  mutate(Year = year(solar.time), 
         jday = yday(solar.time), 
         jday_pad = str_pad(jday, width=3, pad="0")) %>%
  unite("yr_jday", Year, jday_pad, sep = '_', remove=FALSE) %>%
  select(yr_jday, solar.time, DO.obs, temp.water, discharge) 

cupe_do_select <- cupe_do_all %>%
  dplyr::filter(yr_jday %in% cupe_yrjday)

path <- here("N_uptake_NEON/data/DO_data_model/cupe_do.csv")
write_csv(cupe_do_select, file=path)





#####  WLOU 
wlou_do_wy22 <- read_csv(here("N_uptake_NEON/data/DO_data_Hensley/WLOU_WY22.csv"))
wlou_do_wy23 <- read_csv(here("N_uptake_NEON/data/DO_data_Hensley/WLOU_WY23.csv"))
wlou_do_wy24 <- read_csv(here("N_uptake_NEON/data/DO_data_Hensley/WLOU_WY24.csv"))

# Get object with WLOU's yr_jdays 
wlou_yrjday <- daily_summary.df %>%
  dplyr::filter(site == "WLOU") %>%
  pull(yr_jday) 

# Combine the WLOU water year files into 1 file, create a yr_jday column, then select the yr_jdays to match my data
wlou_do_all <- bind_rows(wlou_do_wy22, wlou_do_wy23, wlou_do_wy24) %>%
  mutate(Year = year(solar.time), 
         jday = yday(solar.time), 
         jday_pad = str_pad(jday, width=3, pad="0")) %>%
  unite("yr_jday", Year, jday_pad, sep = '_', remove=FALSE) %>%
  select(yr_jday, solar.time, DO.obs, temp.water, discharge) 

wlou_do_select <- wlou_do_all %>%
  dplyr::filter(yr_jday %in% wlou_yrjday)

# the days aren't even: 11137 obs = 116 days + 15 min (1 obs)
#wlou_obsPERday <- wlou_do_select %>% count(yr_jday) # Sept 30, 2022 has 97 obs (not 96)

wlou_check <- wlou_do_select %>%
  count(yr_jday) %>%
  filter(n != 96)

wlou_view <- wlou_do_select %>%
  dplyr::filter(yr_jday %in% wlou_check$yr_jday)

# one timestep is duplicated (rows 68 and 69): similar values, same timestep. Removing 68
wlou_do_select <- wlou_do_select %>%
  dplyr::slice(-68)

# save the data
path <- here("N_uptake_NEON/data/DO_data_model/wlou_do.csv")
write_csv(wlou_do_select, file=path)


#####################################################

# Explore data: 

wlou.do <- read_csv(here("N_uptake_NEON/data/DO_data_model/wlou_do.csv"))

cupe.do <- read_csv(here("N_uptake_NEON/data/DO_data_model/cupe_do.csv"))

cari.do <- read_csv(here("N_uptake_NEON/data/DO_data_model/cari_do.csv"))



wlou.DOplot <- wlou.do %>% 
  ggplot(aes(x=solar.time, y=DO.obs)) + 
  geom_point() + 
  xlab("Solar time") + ylab("Observed DO") +
  ggtitle("West St. Louis Creek DO") + 
  theme_bw()

ggplotly(wlou.DOplot) %>%
  layout(
  xaxis = list(
    rangeslider = list(visible = TRUE)
    )
  )



cupe.DOplot <- cupe.do %>% 
  ggplot(aes(x=solar.time, y=DO.obs)) + 
  geom_point() + 
  xlab("Solar time") + ylab("Observed DO") +
  ggtitle("Rio Cupeyes DO") + 
  theme_bw()

ggplotly(cupe.DOplot) %>%
  layout(
    xaxis = list(
      rangeslider = list(visible = TRUE)
    )
  )


cari.DOplot <- cari.do %>% 
  ggplot(aes(x=solar.time, y=DO.obs)) + 
  geom_point() + 
  xlab("Solar time") + ylab("Observed DO") +
  ggtitle("Caribou Creek DO") + 
  theme_bw()

ggplotly(cari.DOplot) %>%
  layout(
    xaxis = list(
      rangeslider = list(visible = TRUE)
    )
  )




######### FOR MY DATA

######  BIGC DAYS
list.21 <- c(274, 275, 276, 337, 338, 339, 340)

list.22 <- c(22, 23, 24, 25, 26, 27, 28, 38, 41, 42, 48, 49, 51, 52, 58, 59, 68, 69, 70, 82, 83, 84, 85, 91, 92, 93, 94, 95, 100, 103, 104, 105, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 127, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 147, 148, 155, 160, 170, 195, 196, 204, 205, 206, 221, 222, 230, 231, 232, 233, 234, 237, 238, 239, 244, 247, 251, 252, 257, 258, 259, 266, 267, 268)

list.23 <- c(33, 34, 41, 43, 44, 46, 47, 48, 49, 50, 51, 91, 92, 96, 101, 291, 292, 293, 294, 299, 300, 301, 302, 303, 304, 305, 306, 307, 309, 313, 314, 315, 317, 318, 325, 326, 327, 328, 329, 330, 336, 337, 338, 339, 340, 341, 344, 345, 346, 347, 348, 349, 350)

list.24 <- c(25, 26, 27, 28, 29, 30, 31, 74, 75, 76, 80, 81, 82, 93, 94, 99, 100, 137, 138, 142, 143, 146, 153, 154, 155, 163, 168)




######  PRIN DAYS

list.21 <- c(181, 184, 186, 188, 234, 235, 236, 237, 240, 241, 242, 243, 244, 249, 251, 253, 254, 255, 261, 265, 266, 267, 268, 269, 294, 295, 304, 318, 319, 324, 343, 344, 355, 356, 357)

list.22 <- c(4, 5, 6, 10, 11, 12, 13, 16, 17, 18, 19, 25, 27, 29, 30, 31, 38, 39, 40, 41, 44, 45, 61, 68, 74, 75, 76, 77, 78, 84, 85, 86, 87, 90, 91, 104, 105, 106, 107, 110, 111, 112, 130, 131, 132, 134, 135, 136, 137, 138, 139, 140, 182, 304, 305, 306, 310)

list.23 <- c(55, 72, 75, 77, 84, 85, 86, 88, 90, 94, 95, 96, 97, 100, 102, 103, 104, 106, 107, 108, 109, 110, 111, 113, 115, 121, 122, 123, 124, 125, 126, 137, 138, 139, 140, 141, 142, 143, 146, 147, 148, 149, 150, 151, 152, 328, 329, 337)

# 2024 not used yet
# list.24 <- c(48, 61, 69, 71, 72, 73, 74, 80, 95, 97, 118, 139, 140, 141, 142, 144, 145, 147, 159, 160, 161, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 177, 179)




######  SYCA DAYS

# NO WY22 days in 2021

list.22 <- c(34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 49, 50, 51, 52, 53, 56, 57, 58, 59, 60, 61, 62, 64, 65, 66, 67, 68, 69, 70, 71, 350, 351, 352, 356, 357, 358, 359, 360)

list.23 <- c(101, 102, 103, 104, 105, 106, 125, 126, 127, 128, 129, 132, 138, 139, 143, 144, 145, 146, 147, 148, 150, 257, 263, 266, 274, 278, 279, 280, 281, 282)

list.24 <- c(64, 97, 98, 99, 100, 103, 104, 105, 112, 113, 114, 115, 116, 119, 120, 121, 125, 126, 127, 128, 129, 131, 138, 142, 145, 146, 147, 150, 151, 152, 155, 156)


######  CARI DAYS

list.21 <- c(274, 275, 276, 277)

list.22 <- c(130, 131, 135, 136, 137, 138, 139, 140, 141, 142, 144, 145, 146, 147, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 161, 162, 163, 164, 165, 166, 169, 170, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 186, 188, 189, 190, 194, 195, 198, 199, 202, 203, 204, 205, 208, 209, 210, 211, 212, 215, 216, 217, 218, 223, 224, 225, 226, 236, 237, 238, 239, 240, 241, 242, 243, 246, 247, 248, 249, 250, 251, 252, 253, 254, 260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282)


list.23 <- c(132, 133, 134, 139, 140, 141, 158, 159, 160, 161, 162, 163, 164, 165, 166, 170, 171, 172, 173, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 191, 192, 193, 194, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 208, 209, 210, 211, 212, 213,  214, 215, 216, 217, 218, 219, 220, 221, 223, 224, 252, 253, 254, 256, 257, 258, 259, 263, 264, 265, 266, 267, 268, 269, 270, 271, 274, 275, 276, 277, 279, 280, 281, 282, 283, 284, 285, 287, 288, 290, 291)

# 2024 not used
# list.24 <- c(111, 112, 113, 121, 122, 123, 124, 125, 126, 137, 138, 139, 140, 141, 142, 148, 149, 153, 154, 155, 156, 160, 161, 162, 163, 172, 173, 274)  # 28d, ending on Sept 30, 2024


######  CUPE DAYS

list.21 <- c(281, 282, 283, 285, 287, 295, 296, 298, 299, 300, 301, 302, 303, 304, 307, 308, 309, 310, 311, 312, 313, 314, 316, 323, 324, 325, 335, 350, 351, 352, 353, 354, 355, 356, 357, 358, 359, 360, 361, 363, 364)

list.22 <- c(14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 28, 29, 30, 31, 38, 39, 84, 85, 86, 89, 90, 91, 92, 93, 94, 95, 96, 99, 100, 101, 102, 103, 105, 106, 107, 110, 111, 113, 114, 116, 118, 119, 121, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 149, 150, 155, 156, 157, 165, 166, 167, 168, 169, 170, 171, 172, 175, 176, 177, 178, 184, 187, 188, 189, 192, 193, 197, 198, 199, 200, 201, 203, 204, 205, 206, 207, 209, 211, 212, 298, 321, 327, 328, 329, 330, 331, 332, 333, 334, 335, 336, 337, 338, 339, 340, 341, 342, 343, 344, 345, 347, 348, 349, 352, 352, 353, 354, 355, 356, 357, 358, 359, 360, 361, 362)

list.23 <- c(1, 2, 3, 4, 5, 6, 9, 20, 21, 25, 26, 27, 28, 29, 30, 31, 37, 39, 40, 41, 42, 43, 45, 46, 48, 50, 52, 53, 54, 55, 56, 60, 62, 63, 64, 65, 66, 70, 71, 72, 77, 78, 79, 85, 86, 87, 88, 89, 91, 92, 99, 100, 101, 102, 111, 112, 113, 115, 119, 120, 121, 122, 126, 127, 128, 129, 130, 134, 135, 136, 142, 143, 144, 145, 146, 151, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 182, 183, 184, 186, 187, 194, 195, 197, 198, 199, 200, 201, 202, 203, 204, 205, 208, 209, 210, 211, 212, 213, 216, 217, 218, 221, 222, 223, 224, 225, 226, 227, 228, 229, 231, 232, 233, 234, 236, 245, 246, 249, 250, 257, 270, 274, 278, 279, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289, 301, 302, 303, 304, 309, 310, 311, 312, 318, 319, 321, 322, 323, 326, 327, 328, 329, 351, 352, 535, 356, 357, 358, 359, 362, 363, 364, 365)

# 2024 not used yet
# list.24 <- c(1, 2, 3, 9, 11, 12, 13, 14, 15, 16, 17, 18, 20, 26, 27, 28, 32, 33, 34, 35, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 59, 71, 72, 76, 77, 79, 86, 87, 93, 94, 97, 98, 99, 100, 101, 105, 106, 107, 109, 125, 126, 140, 141, 142, 143, 144, 149, 150, 151, 152, 153, 156, 157, 158, 159, 160, 161, 162, 163, 171)

