## seasonal means
# read in dataset 
sales <- read.csv("SeriesReport-Not Seasonally Adjusted Sales(fix2) - Monthly (Millions of Dollars).csv", header = T)
salesTS <- ts(sales, start = c(1992, 1), end = c(1997, 12), freq = 12)/1000
Time <- time(salesTS)
# ACF and PACF of the original data
ACF <- acf2(salesTS)
# estimated the trend
linear_model <- lm(salesTS ~ Time); linear_model
# estimate the seasonality
Month <- factor(rep(month.abb, length.out=length(salesTS)), levels = month.abb)
season_model1 <- lm(salesTS ~ 0 + Time + Month); season_model1
# detrended data
par(mfrow = c(2, 1), mar = c(5, 5, 1, 1), cex = 0.8)
plot(salesTS, main = "monthly sales of foods(brillions of dollar)")
abline(reg = linear_model, col="red", lty=2)
plot(ts(resid(linear_model), start = 1992, end = c(1997, 12), frequency = 12), main = "detrended data", ylab = "Value")
#deseasonalized data
plot(salesTS, ylab="salse", main="monthly salse of food services", type="o")
lines(as.numeric(Time), fitted(season_model1), col="red", lty=2)
z<- ts(resid(season_model1), start=1992, end=c(1997, 12), frequency = 12)
plot(z, ylab="residuals", main = "detrended and deseasonalized")
abline(h=0,lty=3)
# ACF and PACF of detrended and deseasonalize data
acf(z, main="", ylim = c(-1, 1), lag.max = 40)
pacf(z, main="", ylim = c(-1, 1), lag.max = 40)


### ARMA
mod2 <- sarima(resid(season_model1), 2, 0, 2, no.constant = T, details = T); mod2$fit
# mod3 <- sarima(resid(season_model1), 1, 0, 1, no.constant = T, details = F); mod2$fit
# mod4 <- sarima(resid(season_model1), 2, 0, 1, no.constant = T, details = F); mod3$fit
# mod5 <- sarima(resid(season_model1), 1, 0, 2, no.constant = T, details = F); mod4$fit


### forcasting(detrended)
mod2a <- arima(z, order = c(2, 0, 2), include.mean = F)
# forcast out to December 1998
mod2.pr <- predict(mod2a, n.ahead = 36)
zhat <- mod2.pr$pr
pi.z.upper <- mod2.pr$pr + 2*mod2.pr$se
pi.z.lower <- mod2.pr$pr - 2*mod2.pr$se
plot(z, ylab="residuals", xlim = c(1992, 2000), main = expression("Forcasting "~z[t]))
lines(zhat, col="red", lty= 2)
lines(pi.z.upper, lty = 2, col = "blue")
lines(pi.z.lower, lty = 2, col = "blue")


### forcasting(original)
newTime <- seq(1998+0/12, 2000+11/12, by=1/12)
newMonth <- factor(rep(1:12, 3))
levels(newMonth) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
trend <- predict(season_model1, newdata = data.frame(Time = newTime, Month = newMonth))

yhat <- trend + zhat 
pi.y.lower <- trend + pi.z.lower  
pi.y.upper <- trend + pi.z.upper 

plot(salesTS, ylab = "Monthly Sales of Food(trillions of dollars)", xlim = c(1992, 2001), ylim = c(140, 310), 
     main=expression("Forecasting"~y[t]))
points(yhat, col="red")
lines(pi.y.lower, lty=2, col="blue")
lines(pi.y.upper, lty=2, col="blue")


### SARIMA
par(mfrow = c(2, 1), mar = c(5, 5, 1, 1), cex = 0.8)
z2 <- diff(salesTS, lag = 12)
plot(z2)
monthplot(diff(salesTS, lag=12), ylab = "monthly salse")
acf2(z2, max.lag = 48)

# sarima(salesTS, 1, 0, 0, 0, 1, 1, S=12)
# sarima(salesTS, 1, 0, 1, 0, 1, 1, S=12)
# sarima(salesTS,  5, 1, 0, 0, 1, 1, 12)
sarima(salesTS, 5, 0, 0, 0, 1, 1, 12)
# subset model
arima(salesTS, order = c(5, 0, 0), seasonal = list(order=c(0, 1, 1), period=12), 
      xreg = 1:length(salesTS), fixed = c(NA, 0, NA, NA, 0, NA, NA))

### forecasting
sarima.for(salesTS, p=5, d=0, q=0, P=0, D=1, Q=1, S=12, n.ahead=36, main = "Forecasting out to Dec 2020")

