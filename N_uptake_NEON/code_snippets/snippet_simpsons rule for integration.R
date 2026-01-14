##### Simpson's 1/3  Rule for integration

x_values <- c(0, 1, 2, 3, 4, 5)  # x-values
y_values <- c(0, 1, 4, 9, 16, 25)  # Corresponding y-values

simpsons_rule <- function(x, y) {
  h <- diff(x)[1]  # Interval width
  n <- length(x) - 1  # Number of intervals
  
  # Apply Simpson's rule formula
  integral <- (h / 3) * (y[1] + 4 * sum(y[c(TRUE, FALSE)] ) + 2 * sum(y[-c(1, n, n+1)]) + y[n+1])
  return(integral)
}

# Calculate the integral using Simpson's rule
integral_simpson <- simpsons_rule(x_values, y_values)
print(integral_simpson)
