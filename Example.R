# Check if you have universal installer package, install if not
if("pacman" %in% rownames(installed.packages()) == FALSE){
  install.packages("pacman")
}

pacman::p_load(AER, sandwich, lmtest, car, stargazer, ggplot2, openintro, OIdata, gdata, doBy,
               plm, ivpack, forecast, readxl, zoo, xts, lubridate, urca, quantmod, dplyr, ggplot2, xts,
               forecast, writexl, hts, fpp, janitor, stringi, purrr, Mcomp, tstools, stats, tseries,
               tidyquant, timetk, sweep, corrplot, vars, astsa, fastmatch, Hmisc,
               shiny, shinydashboard, DT, plotly, knitr, 
               quantmod, e1071, TTR, PerformanceAnalytics,
               reticulate, bizdays, RQuantLib, odbc, varhandle
               #for forecasts, dataset, Mcomp, smooth, tstools, greybox, stats, tseries, bstats
)

load_quantlib_calendars("Russia", from = '2018-01-01', to = '2025-12-31')
bizdays.options$set(default.calendar = 'QuantLib/Russia')

#################
## Let's start ##
#################

#Long-Run
tcs.full <- getSymbols("TCS", auto.assign = F, from = "2018-01-01")

#Split data by years
tcs2018 <- tcs.full['2018']
tcs2019 <- tcs.full['2019']

#Have a fast look
summary(tcs.full)

#Plot
tcs.full %>% ggplot(aes(x = rownames(tcs.full), y = tcs.full$TCS.Close))
