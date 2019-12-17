## Time series analysis function 

time.series.analysis <- function(energy.var) { ## energy consumption variable -- sub 1 / 2 / 3
  energy.merged.ts <- ts(energy.var, frequency = 12, start = c(2007, 1)) ## change frequency of cycles (weekly (52) / monthly (12))
  lm.sub <- tslm(energy.merged.ts ~ trend + season)
  forecast.sub <- forecast(lm.sub, h = 20, level = c(80, 90))
  return(list(time.series = energy.merged.ts, model = lm.sub, forecast = forecast.sub, 
              plot(forecast.sub, ylim = c(0, 10), ylab = "Watt-Hours", xlab = "Time")))
}
