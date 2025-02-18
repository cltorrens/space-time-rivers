#### Playing with ways to code step_area and conc_auc

## sadly, I did not note down what package they are in and R doesn't know... 

conc <- c(5, 8, 7, 9, 10, 15, 21, 14, 11, 9, 7, 4, 1)  # Example concentration vector
time <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)   # Example time vector

# Calculate step areas using vectorized operations
step_area <- ((conc[-length(conc)] + conc[-1]) / 2) * diff(time)

conc_auc <- sum(((conc[-length(conc)] + conc[-1]) / 2) * diff(time))

# Print step areas
step_area

sum(step_area)