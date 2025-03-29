## Removing outlier data based on interquartile range
## Not actually useful/ desirable for the variable NO3 data
## Christa Torrens, 5 March 2024


######### REMOVE OUTLIER(S) BASED IN INTERQUARTILE RANGE ##########

# plot(king.df.20h$local_datetime, king.df.20h$surfWaterNitrateMean)

### The spikes are *not* an error; probably precip events. 

# quartiles <- quantile(king.df.19h$surfWaterNitrateMean, probs=c(.25, .75), na.rm = TRUE)
# IQR <- IQR(king.df.19h$surfWaterNitrateMean, na.rm=TRUE)
#  
# Lower <- quartiles[1] - 1.5*IQR
# Upper <- quartiles[2] + 1.5*IQR 
#  
# king.df.19h <- king.df.19h %>%
#   subset(surfWaterNitrateMean > Lower & surfWaterNitrateMean < Upper)

# which(king.df.20h$surfWaterNitrateMean < Lower | king.df.20h$surfWaterNitrateMean > Upper)

#  27   28   29   
# 45   46   47   48   49   50   51   52   53   54   
# 74   75   76   
# 99  100  
# 116  117  118 119  120  121  122  124  125  126  127  128  129  130  131  132  133  134  135
# 241  242  243  244  245 246  247  248  249  250  251  252  253  254  255  256  257  258 
# 1110 1111 1112


#### OK< need to determine how (or whether?) to deal w these...