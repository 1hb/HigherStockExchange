# THIS FUNCTION WILL SHOW YOU DESCRIPTIVE STATISTICS YOUR DATA

# SELECT CODE TILL THE END AND RUN CODE (Ctrl+Enter)

# Function

Descr_stat <- function(data) {
 
df <- data.frame(mean = c(mean(data$OPEN), mean(data$CLOSE), 
                          mean(data$HIGH), mean(data$LOW),
                          mean(data$return), mean(data$Cumulative_yield)),
                 sd = c(sd(data$OPEN), sd(data$CLOSE), 
                        sd(data$HIGH), sd(data$LOW),
                        sd(data$return), sd(data$Cumulative_yield)),
                 median = c(median(data$OPEN), median(data$CLOSE), 
                            median(data$HIGH), median(data$LOW),
                            median(data$return), median(data$Cumulative_yield)),
                 max = c(max(data$OPEN), max(data$CLOSE), 
                         max(data$HIGH), max(data$LOW),
                         max(data$return), max(data$Cumulative_yield)),
                 min = c(min(data$OPEN), min(data$CLOSE), 
                         min(data$HIGH), min(data$LOW),
                         min(data$return), min(data$Cumulative_yield)),
                 row.names = c("OPEN", "CLOSE","HIGH", "LOW", "return", "Cumulative_yield"))
        return(df)
}
 
# Results

Descr_stat(Quotes_result)