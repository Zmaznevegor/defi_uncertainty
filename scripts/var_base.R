# Libraries and data load ----
library(dplyr)
library(tsibble)
library(fpp3)
library(fable)
library(forecast)
library(ggplot2)
library(vars)

theme_set(theme_minimal())

z <- readr::read_csv("data/weekly_epu_base.csv")

z <- z %>% 
  mutate(week = yearweek(date)) %>% 
  select(-date) %>% 
  as_tsibble(key = epu, index = week)

?yearweek
# Combine data sets
z <- right_join(z, maker, by = "date")

y <- ts(z[,3:5], frequency=53,
        start= c(2017,52),
        end = c(2020,44))

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

diff(log(y[,2])) %>% checkresiduals()

# Select best VAR ----
# For Maker
VARselect(cbind(epu = y[,1][2:length(y[,2])], 
                diff_tvl = diff(log(y[,2]))),
          type="both")[["selection"]]

var <- VAR(cbind(epu = y[,1][2:length(y[,2])], 
                 diff_tvl = diff(log(y[,2]))),
           p=1, type="const")

# Impulse response functions
irf <- irf(var, impulse = "epu", response = "diff_tvl", 
               n.ahead = 10, boot = TRUE)

plot(irf, ylab = "output", main = "Shock from uncertainty")

