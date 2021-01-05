#install packages

install.packages("aTSA")
library(aTSA)

# Function

Stationarity_test <- function(data, sign_level, lag, print_statiscics) {
  
  group <- as.vector(unlist(data$CLOSE)) # making vector for testing
  c <- as.data.frame(stationary.test(group, method = c("adf", "pp", "kpss"), nlag = lag, type = c("Z_rho", "Z_tau"), lag.short = TRUE, output = FALSE))
  
  # Issue
  
  if (c[lag,3] > sign_level) {
    print("Not stationar")
  } else {
    print("Stationar")
  }
  
  # Statistics output
  
  if (print_statiscics == T) {
    print(c)
  }
}

# Results

Stationarity_test(data = Quotes_result, sign_level = 0.05, lag = 1, print_statiscics = T)
