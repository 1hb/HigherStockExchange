#Installing packages

install.packages("ggpubr")
install.packages("rstatix")
library(ggpubr)
library(rstatix)

# Function

Normality_test <- function(data, sign_level, qqplot, dplot, print_statiscics) {

  df <- shapiro.test(data$CLOSE)
  
  # Issue
  
  if (df[2] > sign_level){
    print("Normal")
  } else {
    print("Not normal")
  }
  
  # Plots
  
  # Quantile Plot
  
  if (qqplot == T) {
    print(ggqqplot(data$CLOSE))
  }
  
  # Density Plot
  
  if (dplot == T) {
    print(ggdensity(data$CLOSE, fill = "lightgray"))
  }
  
  # Statistics output
  
  if (print_statiscics == T) {
    print(df)
  }
}

# Results 

Normality_test(data = Quotes_result, sign_level = 0.05, qqplot = T, dplot = F, print_statiscics = T)
