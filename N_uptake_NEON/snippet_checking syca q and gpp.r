syca.df <- daily_summary_all.df %>% 
  dplyr::filter(site == "SYCA")

sycaDO_Q <- ggplot(data=syca_do, aes(x=startDateTime, y=discharge_cms)) + 
  geom_point()+ 
  theme_bw()

ggplotly(sycaDO_Q)

sycaQ <- ggplot(data=syca.df, aes(x=yr_jday, y=Q)) + 
  geom_point()+ 
  theme_bw()

ggplotly(sycaQ)

sitemeansKU <- daily_summary_all.df %>%
  dplyr::group_by(site) %>%
  dplyr::summarise()