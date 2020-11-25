# Libraries and data load
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
maker <- read_csv("data/maker.csv") %>% rename(date = time) %>% group_by(date) %>% slice(1) %>% filter(date %in% z$date)

# Combine data sets
z <- left_join(z, maker, by = "date")

y <- ts(z[,3:5], frequency=52,
        start= c(2017,52),
        end = c(2020,45))

autoplot(y)

# Convert to tsibble
z$date <- gsub('(.+?)-(.+?)', '\\1-W\\2', z$date)

z <- z %>% 
  mutate(date = yearweek(date)) %>% 
  as_tsibble(index = date)

# Plot EPU Index
z %>% 
  autoplot(epu)
  ggtitle("Weekly EPU for DeFi services") +
  ylab("Index") + xlab("Week")

# Split into training and test
train <- window(y, start = c(2017, 52),
                end = c(2020, 15),
                frequency = 52)  
  
test <- window(y, start = c(2020, 16),
               frequency = 52)


train[,1] %>% ur.kpss() %>% summary()
train[,1] %>% ndiffs()

diff(train[,c(1,3)]) %>% checkresiduals()

dtrain <- diff(train)

# Select best VAR ----
# With diffs
VARselect(dtrain[,c(1,3)],lag.max=8,
          type="const")[["selection"]]


var1 <- VAR(dtrain[,c(1,3)], p=1, type="const")
serial.test(var1, lags.pt=10, type="PT.asymptotic")

var2 <- VAR(dtrain[,c(1,3)], p=2, type="const")
serial.test(var2, lags.pt=10, type="PT.asymptotic")

summary(var2)

forecast(var2, h = 30) %>% 
  autoplot()+
  autolayer(diff(test[,c(1,3)]), series = "Actual")

# Impulse response functions
irf <- irf(var5, impulse = "epu", response = "tvlusd", 
               n.ahead = 15, boot = TRUE)

plot(irf, ylab = "ouput", main = "Shock from uncertainty")

#tsibble version ----
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