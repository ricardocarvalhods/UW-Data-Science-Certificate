production <- read.csv('CADairyProduction.csv')

milk.ts <- ts(production$Milk.Prod, start=1995, freq=12)

plot(log(milk.ts), main='Milk Production Time Series', ylab='Log of Milk Production')
# – Is this time series stationary?
# No, there is a clear trend!

milk.decomp <- stl(log(milk.ts), s.window = "periodic", t.window = 0.5*length(milk.ts))
plot(milk.decomp, main = paste('Decompositon of Milk Production with lowess span = 0.5'))
# – Is there a significant seasonal component?
# Yes

remainder.ts <- milk.decomp$time.series[,'remainder']
plot.acf <- function(df, col = 'remainder', is.df =TRUE){
  if(is.df) temp <- df[, col]
  else temp <- df
  par(mfrow = c(2,1))
  acf(temp, main = paste('ACF of', col))
  pacf(temp, main = paste('PACF of', col))
  par(mfrow = c(1,1))
}
plot.acf(remainder.ts, is.df=F)

## Model of the remainder
milk.arima = arima(remainder.ts, order = c(1,0,0), include.mean=F)
plot.acf(milk.arima$resid[-1], col = 'Milk ARIMA model residuals', is.df=F)

#---
require(forecast)
ARIMAfit <- auto.arima(log(milk.ts), max.p=3, max.q=3, max.P=2, max.Q=2, max.order=5,
                       max.d=2, max.D=1, start.p=0, start.q=0, start.P=0, start.Q=0)
summary(ARIMAfit)

## Make the forecast for the next year
milk.forecast = forecast(ARIMAfit, h=12)
summary(milk.forecast)
# Are the confidence intervals reasonably small compared to the forecast means.
plot(milk.forecast)






