##################  Working with Beaver Creek SUNA data
###################################################################

# The cleaned script has already been saved; 

# load libraries
library(tidyverse)
library(streamMetabolizer)
library(here)
library(lubridate)
library(plotly)


# load data

NO3.df <- read_csv(here("field_data_workup/data/SUNA_data_averaged.csv"))