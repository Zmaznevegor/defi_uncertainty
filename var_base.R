# Libraries and data load ----
library(readr)
library(dplyr)
library(tsibble)
library(fpp3)
library(fable)
library(forecast)
library(ggplot2)
library(vars)

theme_set(theme_minimal())
z <- read_csv("data/weekly_epu_base.csv")

maker <- read_csv("data/defi/maker.csv") %>% rename(date = time) %>% group_by(date) %>% slice(1) %>% filter(date %in% z$date)

# Combine data sets
z <- left_join(z, maker, by = "date")

y <- ts(z[,3:5], frequency=52,
        start= c(2017,52),
        end = c(2020,45))

autoplot(y)

# Plot EPU Index
autoplot(y[,1])+
  ggtitle("Weekly EPU for DeFi services") +
  ylab("Index") + xlab("Week")

# Check differencing and log
diff(y[,3]) %>% checkresiduals()

diffed <- diff(y)

autoplot(diffed[,1])+
  ggtitle("Differenced weekly EPU") +
  ylab("Index") + xlab("Week")

autoplot(diff(log(y[,3])))+
  ggtitle("Differenced weekly Maker price") +
  ylab("Price (ETH)") + xlab("Week")

diff(log(y[,3])) %>% checkresiduals()

# Select best VAR ----
# With diffs
VARselect(cbind((y[,1]), log(y[,3])),
          lag.max=8,
          type="const")[["selection"]]

var <- VAR(cbind(epu = y[,1], 
                 diff_tvl = log(y[,3])), p=2, type="const")

irf <- irf(var, impulse = "epu", response = "diff_tvl", 
           n.ahead = 15, boot = TRUE)

#-----------------------------------------------
var_usd <- VAR(cbind(epu = y[,1][2:150], 
                  diff_tvl = diff(log(y[,3]))), p=1, type="const")
var <- VAR(cbind(epu = y[,1][2:150], 
                 diff_tvl = diff(log(y[,3]))), p=2, type="const")
var <- VAR(cbind(epu = y[,1][2:150], 
                 diff_tvl = diff(log(y[,3]))), p=3, type="const")

serial.test(var, lags.pt=10, type="PT.asymptotic")

summary(var)

# Impulse response functions
irf <- irf(var, impulse = "epu", response = "diff_tvl", 
               n.ahead = 15, boot = TRUE)

irf_usd <- irf(var_usd, impulse = "epu", response = "diff_tvl", 
           n.ahead = 15, boot = TRUE)

plot(irf, ylab = "ouput", main = "Shock from uncertainty")


# ARIMA with tsibble ----
z$date <- gsub('(.+?)-(.+?)', '\\1-W\\2', z$date)

z <- z %>% 
  mutate(date = yearweek(date)) %>% 
  as_tsibble(index = date)

train <- z %>% filter(date <= z$date[120])
test <- z %>% filter(date > z$date[120])

train %>%
  features(epu, unitroot_kpss) # stat is bigger than 1% critical value, so data is not stationary

train %>%
  mutate(diff_epu = difference(epu)) %>% 
  features(diff_epu, unitroot_kpss)

# Check with ndiffs
train %>%
  features(epu, unitroot_ndiffs) # coherent with kpss test

fit<- train %>% 
  model(ARIMA(log(tvleth)~epu)) %>% report()

train <- window(y, start = c(2017, 52),
                end = c(2020, 15),
                frequency = 52)  

test <- window(y, start = c(2020, 16),
               frequency = 52)

fit <- auto.arima(log(train[,3]), 
                  xreg = train[,1], 
                  lambda = BoxCox.lambda(train[,3]))

summary(fit)

fcs <- forecast(fit, xreg= test[,1], h = 30)

autoplot(fcs, series = "Forecast with EPU")+
  autolayer(log(y[,3]), series = "Real value")

fit2 <- auto.arima(log(train[,3]), 
                  lambda = BoxCox.lambda(train[,3]))

fcs2 <- forecast(fit2, h = 30)

autoplot(fcs2, series = "Forecast with EPU")+
  autolayer(log(y[,3]), series = "Real value")


fit3 <- auto.arima(train[,3], 
                   lambda = BoxCox.lambda(train[,3]))

fcs3 <- forecast(fit3, h = 30)

autoplot(fcs3, series = "Forecast with EPU")+
  autolayer(y[,3], series = "Real value")


fit4 <- auto.arima(train[,3],
                   xreg = train[,1])

fcs4 <- forecast(fit4, xreg = test[,1], h = 30)

autoplot(fcs4, series = "Forecast with EPU")+
  autolayer(y[,3], series = "Real value")

fit5 <- auto.arima(diff(train[,3]),
                   xreg = diff(train[,1]))

fcs5 <- forecast(fit5, xreg = test[,1], h = 30)

autoplot(fcs5, series = "Forecast with EPU")+
  autolayer(diff(y[,3]), series = "Real value")

accuracy(fcs, x = log(test[,3]))
accuracy(fcs2, x = log(test[,3]))
accuracy(fcs3, x = test[,3])
accuracy(fcs4, x = test[,3])
accuracy(fcs5, x = diff(test[,3]))
