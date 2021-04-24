# Libraries and data load ----
# Data wrangling and visualisation
library(zoo)
library(dplyr)
library(ggplot2)

# Time-series forecasting
library(fpp3)
library(fable)
library(forecast)
library(vars)

# Custom functions for IRF plots
source(file = "scripts/uncertainty_sampling.R")

# Common theme for the plots
theme_set(theme_minimal())

# Seed for reproducability
set.seed(123)

# Saved EPU load
epu <- read.csv("data/epu_results.csv") %>% 
  mutate(yw = yearweek(yw)) %>% 
  rename(naive = Naive,
         mod = AL.Mod)

# Data pre-processing ----
## Import the TVL data ----
tvl <- read.csv("data/defi/tvl_all.csv") %>% 
  mutate(yw = yearweek(time)) %>% 
  dplyr::select(yw, tvlusd, tvleth) %>% 
  filter(yw < yearweek("2021 W12")) %>% 
  group_by(yw) %>% 
  summarise_at(vars(tvleth, tvlusd), list(avg = mean)) %>% 
  rename(tvleth = tvleth_avg,
         tvlusd = tvlusd_avg) %>% ungroup()


tvl_daily <- read.csv("data/defi/tvl_all.csv") %>% 
  mutate(yw = yearweek(time)) %>% 
  dplyr::select(yw, tvlusd, tvleth) %>% 
  filter(yw < yearweek("2021 W12"))

tvl_cat <- read.csv("data/defi/tvl_categories.csv") %>% 
  mutate(yw = yearweek(time)) %>% 
  dplyr::select(yw, category, tvlusd, tvleth) %>% 
  filter(yw < yearweek("2021 W12")) %>% 
  group_by(yw, category) %>% 
  summarise_at(vars(tvleth, tvlusd), list(avg = mean)) %>% 
  rename(tvleth = tvleth_avg,
         tvlusd = tvlusd_avg) %>% ungroup()
  
tvl_cat_daily <- read.csv("data/defi/tvl_categories.csv") %>% 
  mutate(yw = yearweek(time)) %>% 
  dplyr::select(yw, category, tvlusd, tvleth) %>% 
  filter(yw < yearweek("2021 W12"))

# Plot the data
ggplot(tvl, aes(x = as.Date(yw), y = tvleth)) +
  geom_line(aes(colour = token))+
  labs(x = "", y = "TVL (ETH)", colour = "DeFi")

ggplot(tvl_cat, aes(x = as.Date(yw), y = tvleth))+
  geom_line(aes(colour = category))+
  labs(x = "", y = "TVL (ETH)", colour = "DeFi")

## Import gas stats ----
# Gas price
gas <- read.csv("data/defi/export-AvgGasPrice.csv") %>% 
  mutate(yw = yearweek(as.Date(Date.UTC., format = "%m/%d/%Y")),
         Value = Value..Wei.) %>% 
  dplyr::select(yw, Value) %>% 
  group_by(yw) %>% 
  summarise_at(vars(Value), list(avg.gas = mean)) %>% ungroup()

gas_daily <- read.csv("data/defi/export-AvgGasPrice.csv") %>% 
  mutate(date = as.Date(Date.UTC., format = "%m/%d/%Y"),
         Value = Value..Wei.) %>% 
  dplyr::select(date, Value) %>% 
  group_by(date) %>% 
  summarise_at(vars(Value), list(avg.gas = mean)) %>% ungroup()

ggplot(gas, aes(x = as.Date(yw), y = avg.gas)) +
  geom_line()+
  labs(x = "", y = "Average Gas Price")

# Import verified contracts ----
contracts <- read.csv("data/defi/export-verified-contracts.csv") %>% 
  mutate(yw = yearweek(as.Date(Date.UTC.)),
         Value = No..of.Verified.Contracts) %>% 
  dplyr::select(yw, Value) %>% 
  group_by(yw) %>% 
  summarise_at(vars(Value), list(sum.verf = sum)) %>% ungroup()

contracts_daily <- read.csv("data/defi/export-verified-contracts.csv") %>% 
  mutate(date = as.Date(Date.UTC.),
         Value = No..of.Verified.Contracts) %>% 
  dplyr::select(date, Value) %>% 
  group_by(date) %>% 
  summarise_at(vars(Value), list(sum.verf = sum)) %>% ungroup()

ggplot(contracts, aes(x = as.Date(yw), y = sum.verf)) +
  geom_line()+
  labs(x = "", y = "Average Contracts Verified")

# Import borrowing and lending rates
comp_rates <- read.csv("data/defi/lending/compound_rates.csv") %>% 
  mutate(yw = yearweek(time)) %>% 
  relocate(yw) %>% 
  dplyr::select(-time) %>% 
  drop_na() %>% 
  group_by(yw) %>% 
  summarise_at(vars(borrow_rate, lend_rate), list(avg = mean)) %>% 
  rename(br_rate = borrow_rate_avg,
         ln_rate = lend_rate_avg)

ggplot(comp_rates, aes(x = as.Date(yw))) +
  geom_line(aes(y = br_rate), colour = "red")+
  geom_line(aes(y = ln_rate), colour = "blue")+
  labs(x = "", y = "Rate")

maker_rates <- read.csv("data/defi/lending/maker_rates.csv") %>% 
  mutate(yw = yearweek(time)) %>% 
  relocate(yw) %>% 
  dplyr::select(-time) %>% 
  drop_na()%>% 
  group_by(yw) %>% 
  summarise_at(vars(borrow_rate, lend_rate), list(avg = mean)) %>% 
  rename(br_rate = borrow_rate_avg,
         ln_rate = lend_rate_avg)

ggplot(maker_rates, aes(x = as.Date(yw))) +
  geom_line(aes(y = br_rate), colour = "red")+
  geom_line(aes(y = ln_rate), colour = "blue")+
  labs(x = "", y = "Rate")

# Daily data
comp_rates_daily <- read.csv("data/defi/lending/compound_rates.csv") %>% 
  rename(date = time) %>% 
  drop_na()

maker_rates_daily <- read.csv("data/defi/lending/maker_rates.csv") %>% 
  rename(date = time) %>% 
  drop_na()

ggplot(comp_rates_daily, aes(x = as.Date(date))) +
  geom_line(aes(y = borrow_rate), colour = "red")+
  geom_line(aes(y = lend_rate), colour = "blue")+
  labs(x = "", y = "Rate")

## Combine the results ----
inp <- epu %>% 
  mutate(yw = yearweek(yw)) %>% 
  left_join(tvl, by = "yw") %>% 
  left_join(gas, by = "yw") %>% 
  left_join(contracts, by ="yw") %>% 
  dplyr::select(yw, naive, mod, sum.verf, avg.gas, tvleth, tvlusd) %>% 
  as_tsibble(index = yw) %>% 
  drop_na()

inp_daily <- daily_mod %>% 
  mutate(date = as.Date(date)) %>% 
  left_join(tvl_daily %>% filter(token == "maker"), by = "date") %>% 
  left_join(gas_daily, by = "date") %>% 
  left_join(contracts_daily, by ="date") %>% 
  dplyr::select(date, norm, sum.verf, avg.gas, tvleth, tvlusd) %>% 
  rename(mod = norm) %>% 
  as_tsibble(index = date) %>% 
  drop_na()

# Save the results for the reproducibility
write.csv(inp, file = "data/inp.csv", row.names = F)
write.csv(inp_daily, file = "data/inp_daily.csv", row.names = F)

# Transform to the readable for the VAR format
inp_ts <- ts(inp[,2:7], frequency=52,
        start= c(year(inp$yw[1]),week(inp$yw[1])),
        end = c(year(inp$yw[length(inp$yw)]),week(inp$yw[length(inp$yw)])))

# Plot EPU Index
autoplot(inp_ts[,1])+
  ggtitle("Weekly EPU for DeFi services") +
  ylab("Modifed Economic Policy Uncertainty") +
  xlab("Date")

autoplot(log(inp_ts[,5]))+
  ggtitle("TVL for the DeFi") +
  ylab("Total Value Locked") +
  xlab("Date")

# Select best VAR ----
## With weekly EPU ----
### For all DeFi TVL ----
mtr <- cbind(epu = inp_ts[,"mod"],
             contracts = log(inp_ts[,"sum.verf"]),
             gas = log(inp_ts[,"avg.gas"]),
             tvleth = log(inp_ts[,"tvleth"]),
             tvlusd = log(inp_ts[,"tvlusd"]))

VARselect(mtr, type="both", lag.max = 12)[["selection"]]

var <- VAR(mtr, p = 7, type = "both", season = NULL)

# A-matrix
a.mat <- diag(5)
diag(a.mat) <- NA
a.mat[3:5, 1:3] <- NA
a.mat[2, 1] <- NA
a.mat[5, 4] <- NA

# B-matrix
b.mat <- diag(5)
diag(b.mat) <- NA

svar <- SVAR(var, Amat = a.mat, Bmat = b.mat, max.iter = 10000, hessian = TRUE)
svar

# For TVL USD
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvlusd", ortho = T, n.ahead = 12, boot = TRUE)
irf_usd(irf)

# For TVL ETH
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvleth", ortho = T, n.ahead = 12, boot = TRUE)
irf_eth(irf)


### For the categories in DeFi ----
# Lending
inp <- epu %>% 
  mutate(yw = yearweek(yw)) %>% 
  left_join(tvl_cat %>% filter(category == "lending"), by = "yw") %>% 
  left_join(gas, by = "yw") %>% 
  left_join(contracts, by ="yw") %>% 
  dplyr::select(yw, naive, mod, sum.verf, avg.gas, tvleth, tvlusd) %>% 
  as_tsibble(index = yw) %>% 
  drop_na()

# Transform to the readable for the VAR format
inp[1:3, 6:7] <- 1
inp_ts <- ts(inp[,2:7], frequency=52,
             start= c(year(inp$yw[1]),week(inp$yw[1])),
             end = c(year(inp$yw[length(inp$yw)]),week(inp$yw[length(inp$yw)])))

mtr <- cbind(epu = inp_ts[,"mod"],
             contracts = log(inp_ts[,"sum.verf"]),
             gas = log(inp_ts[,"avg.gas"]),
             tvleth = log(inp_ts[,"tvleth"]),
             tvlusd = log(inp_ts[,"tvlusd"]))

VARselect(mtr, type="both", lag.max = 12)[["selection"]]

var <- VAR(mtr, p = 2, type = "both", season = NULL)

# A-matrix
a.mat <- diag(5)
diag(a.mat) <- NA
a.mat[3:5, 1:3] <- NA
a.mat[2, 1] <- NA
a.mat[5, 4] <- NA

# B-matrix
b.mat <- diag(5)
diag(b.mat) <- NA

svar <- SVAR(var, Amat = a.mat, Bmat = b.mat, max.iter = 10000, hessian = TRUE)
svar

# For TVL USD
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvlusd", ortho = T, n.ahead = 12, boot = TRUE)
irf_usd(irf)

# For TVL ETH
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvleth", ortho = T, n.ahead = 12, boot = TRUE)
irf_eth(irf)


## With daily EPU ----
a <- ts(inp_daily[,2:6], frequency=365,
        start= c(year(inp_daily$date[1]), yday(inp_daily$date[1])),
        end = c(year(inp_daily$date[length(inp_daily$date)]), yday(inp_daily$date[length(inp_daily$date)])))

# Plot EPU Index
autoplot(a[,1])+
  ggtitle("Daily EPU for DeFi services") +
  ylab("Index") + xlab("Day")

autoplot(log(a[,5]))+
  ggtitle("TVL for the DeFi") +
  ylab("TVL") + xlab("Week")

a[,1] %>% checkresiduals()
diff(a[,2]) %>% checkresiduals()
diff(a[,3]) %>% checkresiduals()
diff(log(a[,4])) %>% checkresiduals()
diff(log(a[,5])) %>% checkresiduals()
diff(a[,6]) %>% checkresiduals()

mtr <- cbind(epu = a[,"mod"],
             contracts = log(a[,"sum.verf"]),
             gas = log(a[,"avg.gas"]),
             tvleth = log(a[,"tvleth"])
             #tvlusd = log(a[,"tvlusd"])
)

VARselect(mtr, type="both", lag.max = 30)[["selection"]]

var <- VAR(mtr, p = 14, type = "both", season = NULL)

# A-matrix
a.mat <- diag(4)
diag(a.mat) <- NA
a.mat[3:4, 1:2] <- NA
a.mat[2, 1] <- NA
a.mat[4, 3] <- NA

# B-matrix
b.mat <- diag(4)
diag(b.mat) <- NA

svar <- SVAR(var, Amat = a.mat, Bmat = b.mat, max.iter = 10000, hessian = TRUE)
svar

# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvleth", ortho = T, n.ahead = 30, boot = TRUE)

plot(irf, ylab = "output", main = "Shock from uncertainty")


irf[["irf"]][["epu"]] %>% as.data.frame() %>% 
  ggplot(aes(y = tvleth, x = c(1:31)))+
  geom_line(linetype = 2, colour = "black", alpha = 1, size = 0.75)+
  geom_line(data = irf[["Lower"]][["epu"]] %>% as.data.frame(), aes(y = tvleth, x =c(1:31)), linetype = 2, colour = "red", alpha = 0.8, size = 0.4)+
  geom_line(data = irf[["Upper"]][["epu"]] %>% as.data.frame(), aes(y = tvleth, x =c(1:31)), linetype = 2, colour = "red", alpha = 0.8, size = 0.4)+
  geom_hline(yintercept = 0, linetype = "dotted", colour = "black", alpha = 0.5)+
  coord_cartesian(ylim = c(min(irf[["Lower"]][["epu"]])-0.05, max(irf[["Upper"]][["epu"]])+0.05), xlim = c(1,31))+
  scale_y_continuous(breaks = seq(round(min(irf[["Lower"]][["epu"]])-0.05, 1), 
                                  round(max(irf[["Upper"]][["epu"]])+0.05, 1),
                                  by = 0.1))+
  scale_x_continuous(breaks = seq(1, 30, by = 2))+
  labs(subtitle = "Modified DeFi EPU index", title = "Effect of uncertainty impulse on TVL (ETH)",
       x = "Lag", y = "") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5))


# TODO: check for the different categories