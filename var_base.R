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
maker <- read_csv("data/maker.csv") %>% rename(date = time)


# Combine data sets %>% 
left_join(z, maker, by = "date")

# Convert to ts
z$date <- gsub('(.+?)-(.+?)', '\\1-W\\2', z$date)

z <- z %>% 
  mutate(date = yearweek(date)) %>% 
  as_tsibble(index = date)

# Plot EPU Index
z %>% 
  autoplot(epu) +
  ggtitle("Weekly EPU for DeFi services") +
  ylab("Index") + xlab("Week")

# Split into training and test
training <- z %>% filter(date <= z$date[120])
test <- z %>% filter(date > z$date[120])

training %>%
  features(epu, unitroot_kpss) # stat is bigger than 1% critical value, so data is not stationary

training %>%
  mutate(diff_epu = difference(epu)) %>% 
  features(diff_epu, unitroot_kpss)

# Check with ndiffs
training %>%
  features(epu, unitroot_ndiffs) # coherent with kpss test

training %>%
  mutate(diff_epu = difference(epu)) %>% 
  features(diff_epu, unitroot_kpss)

exmp <- fpp2::uschange

# Selcet best VAR
VARselect(uschange[,1:2], lag.max=8,
          type="const")[["selection"]]