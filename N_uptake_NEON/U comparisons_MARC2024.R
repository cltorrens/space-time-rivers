# create a dotplot in R to compare BIGC autotrophic uptake with Bray and Heffernan and Cohen data

library(lattice)

# load data
U.df <- read.csv(here("N_uptake_NEON/data/U comparisons_MARC2024.csv"))

# Create dotplot
quartz()
dotplot(reorder(site, U_mmol_m2_d) ~ U_mmol_m2_d, data=U.df, 
        #scales = list(x=list(log="e")),
        scales = list(x=list(log=10)),
        #main = "Autotrophic uptake comparison", 
        xlab = expression("Uptake"~(mmol~m^-2~d^-1))
)

