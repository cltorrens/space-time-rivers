### Cleaning and visualizing DO data (from miniH2O sensors)

# load required packages
library(tidyverse)

# load data files, set datetime columns to POSIX

data <- read.csv(file.choose(), header=TRUE) %>%
  mutate(UTC_datetime=ymd_hms(UTC_datetime, tz="UTC"), 
         MST_datetime=ymd_hms(MST_datetime, tz="US/Mountain"), 
         month)
  
head(data)


# exploratory plot of the data

ggplot(data=data, aes(x=MST_datetime, y=DO_mgl))+
  geom_point() + 
  labs(x="Time", y="DO, mg/l", title="DO at Beaver Creek")+
  facet_wrap()
  theme_bw()
  
## 

# write csv with concatenated data

