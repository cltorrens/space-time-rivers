## Code snippet for remote installations
#    Used this for MuMIn

# Install 'remotes' if you haven't already
install.packages("remotes")

# Install MuMIn version 1.47.5
remotes::install_version("MuMIn", version = "1.47.5")

# Then had to refresh the Packages tab to be able to see it. 

## ALSO, TIL:  This is matrix multiplication in R.    
# %*%

# From the sparse informercial prediction code, e.g.
in_sample_lasso <- model_matrix_included %*% effect_size_lasso

