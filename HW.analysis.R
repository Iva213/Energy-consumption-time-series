## HW analysis function 

HW.analysis <- function(sub.var) { #Weekly or monthly data
  
  #Create TS object 
  sub.ts <- ts(sub.var, frequency = 52, start = c(2007, 1)) #Choose frequency (12 / 52)
  ## Decomposition -- trend, seasonal, & remainder
  sub.decom <- decompose(sub.ts)
  #Seasonal adjusting of sub-meters  by subtracting the seasonal component
  sub.ts.adj <- sub.ts - sub.decom$seasonal
  #Holt Winters Exponential Smoothing & Plot
  sub.hw <- HoltWinters(sub.ts.adj, beta = TRUE, gamma = FALSE)
  #forecast <- forecast(sub.hw, h = 25, level = c(80, 90))
  forecast <- predict(sub.hw, n.ahead = 24, prediction.interval = T, level = c(0.8, 0.9)) ## Change n.ahead (24 / 74)
  return(list(time.series = sub.ts, decom.ts = sub.decom, sub.hw = sub.hw, forecast = forecast,
              plot(sub.hw, forecast, ylim = c(0, 15))))

}

