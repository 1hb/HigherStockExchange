# Installing required packages

install.packages('data.table')
install.packages('dplyr')
install.packages('lubridate')
install.packages('ggplot2')
library('data.table')
library('dplyr')
library('lubridate')
library('ggplot2')

# Checking the working directory

getwd() # Put the .txt files in the WD

# Downloading data

OrderLog <- fread('OrderLog20150302.txt')

# Look at ticker list

Ticker_list <- as.data.frame(unique(OrderLog$SECCODE))
Ticker_list

# SELECT FUNCTION TILL THE END AND RUN CODE (Ctrl+Enter)

# Function

quotes <- function(data, company_ticker,     # company_ticker = ""
                   time_interval,            # time_interval: 1-60
                   start, finish) {          # start / finish = "%Y-%m-%d %H:%M"
  
  # Choosing company
  OL <- OrderLog %>%
    filter(SECCODE == company_ticker, ACTION == 2)
  
  # Changing time format
  OL$TIME <- strptime(as.character(OL$TIME), "%H%M%S")
  OL$TIME <- as.POSIXct(OL$TIME)
  OL$TIME <- with(OL, TIME - years(as.integer(format(Sys.Date(), "%Y")) - 2015))
  OL$TIME <- with(OL, TIME - months(as.integer(format(Sys.Date(), "%m")) - 3))
  OL$TIME <- with(OL, TIME - days(as.integer(format(Sys.Date(), "%e")) - 2))
  OL$HM <- strftime(OL$TIME, format="%Y-%m-%d %H:%M")
  
  # Filtering data according to chosen time intervals
  OL_red <- OL %>%
    filter(HM >= start, HM < finish)
  
  # Calculating data for 1 minute time interval
  Quotes <- OL_red %>%
    select(SECCODE, HM, TRADEPRICE, VOLUME) %>%
    group_by(SECCODE, HM) %>%
    summarise(OPEN = TRADEPRICE[1], LOW = min(TRADEPRICE), HIGH = max(TRADEPRICE),
              CLOSE = TRADEPRICE[length(TRADEPRICE)], VOLUME = sum(VOLUME) / 2)
  
  # Preparing data for t minute time interval (if needed)
  if (time_interval != 1) {
    delta <- 0
    Quotes$ID <- 0
    
    # Creating ID for each time group
    if (length(Quotes$HM) %% length(seq(1, length(Quotes$HM), time_interval)) == 0) {
      for (i in seq(1, length(Quotes$HM), time_interval)) {
        Quotes$ID[i:(i + time_interval - 1)] <- i - delta
        delta <- delta + (time_interval - 1)
      }
    } else {
      for (i in seq(1, length(Quotes$HM) - time_interval + 1, time_interval) ) {
        Quotes$ID[i:(i + time_interval - 1)] <- i - delta
        delta <- delta + (time_interval - 1)
      }
      Quotes$ID[(i + time_interval):length(Quotes$HM)] <- Quotes$ID[i] + 1
    }
    
    # Calculating data for t minute time interval (if needed)
    Quotes <- Quotes %>%
      group_by(SECCODE, ID) %>%
      summarise(HM = max(HM), OPEN = OPEN[1], LOW = min(LOW), HIGH = max(HIGH),
                CLOSE = CLOSE[length(CLOSE)], VOLUME = sum(VOLUME))
    Quotes$ID <- NULL
    
  }
  
  # Changing date format
  Quotes$HM <- as.POSIXct(Quotes$HM)
  Quotes$HM <- with(Quotes, HM + minutes(1))
  
  # Yield
  
  Quotes$return <- c(0, tail(Quotes$CLOSE, -1)/head(Quotes$CLOSE, -1) -1 )
  
  # Cumulative yield
  
  Quotes$Cumulative_yield <- c(0, tail(Quotes$CLOSE, -1)/Quotes$CLOSE[1] -1 )
  
  return(Quotes)
}

# Results: put necessery ticker, time frame in minutes and start and end period in the form: 

Quotes_result <- quotes(data = OrderLog, company_ticker = 'SBER', time_interval = 10, start = '2015-03-02 10:00', finish = '2015-03-02 15:00')
