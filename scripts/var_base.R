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

# Seed for reproducibility
set.seed(123)

# Saved EPU load
epu <- read.csv("data/epu_results.csv") %>% 
  mutate(yw = yearweek(yw)) %>% 
  rename(naive = Naive,
         mod = AL.Mod)

epu_daily <- read.csv("data/epu_results_daily.csv") %>% 
  mutate(date = as.Date(date)) %>% 
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
  mutate(date = as.Date(time)) %>% 
  dplyr::select(date, tvlusd, tvleth) %>% 
  filter(date < as.Date(yearweek("2021 W12")))

tvl_cat <- read.csv("data/defi/tvl_categories.csv") %>% 
  mutate(yw = yearweek(time)) %>% 
  dplyr::select(yw, category, tvlusd, tvleth) %>% 
  filter(yw < yearweek("2021 W12")) %>% 
  group_by(yw, category) %>% 
  summarise_at(vars(tvleth, tvlusd), list(avg = mean)) %>% 
  rename(tvleth = tvleth_avg,
         tvlusd = tvlusd_avg) %>% ungroup()
  
tvl_cat_daily <- read.csv("data/defi/tvl_categories.csv") %>% 
  mutate(date = as.Date(time)) %>% 
  dplyr::select(date, category, tvlusd, tvleth) %>% 
  filter(date < as.Date(yearweek("2021 W12")))

# Plot the data
ggplot(tvl, aes(x = as.Date(yw), y = tvleth)) +
  geom_line()+
  labs(x = "", y = "TVL (ETH)", colour = "DeFi")

ggplot(tvl_cat, aes(x = as.Date(yw), y = tvleth))+
  geom_line(aes(colour = category))+
  scale_y_continuous(breaks = seq(0, 12000000, by = 2000000),
                     labels = seq(0, 12000, by = 2000))+
  labs(x = "", y = "Total Value Locked (in 1000 ETH)", colour = "DeFi Category")

## Import gas stats ----
# Gas price
gas <- read.csv("data/defi/export-AvgGasPrice.csv") %>% 
  mutate(yw = yearweek(as.Date(Date.UTC., format = "%m/%d/%Y")),
         Value = Value..Wei.) %>% 
  dplyr::select(yw, Value) %>% 
  filter(yw < yearweek("2021 W12")) %>% 
  group_by(yw) %>% 
  summarise_at(vars(Value), list(avg.gas = mean)) %>% ungroup()

gas_daily <- read.csv("data/defi/export-AvgGasPrice.csv") %>% 
  mutate(date = as.Date(Date.UTC., format = "%m/%d/%Y"),
         Value = Value..Wei.) %>% 
  dplyr::select(date, Value) %>% 
  filter(date < as.Date(yearweek("2021 W12"))) %>% 
  group_by(date) %>% 
  summarise_at(vars(Value), list(avg.gas = mean)) %>% ungroup()

ggplot(gas, aes(x = as.Date(yw), y = avg.gas)) +
  geom_line()+
  labs(x = "", y = "Average Gas Price")

ggplot(gas_daily, aes(x = as.Date(date), y = avg.gas)) +
  geom_line()+
  labs(x = "", y = "Average Gas Price")

## Import verified contracts ----
contracts <- read.csv("data/defi/export-verified-contracts.csv") %>% 
  mutate(yw = yearweek(as.Date(Date.UTC.)),
         Value = No..of.Verified.Contracts) %>% 
  dplyr::select(yw, Value) %>% 
  filter(yw < yearweek("2021 W12")) %>% 
  group_by(yw) %>% 
  summarise_at(vars(Value), list(sum.verf = sum)) %>% ungroup()

contracts_daily <- read.csv("data/defi/export-verified-contracts.csv") %>% 
  mutate(date = as.Date(Date.UTC.),
         Value = No..of.Verified.Contracts) %>% 
  dplyr::select(date, Value) %>% 
  filter(date < as.Date(yearweek("2021 W12"))) %>% 
  group_by(date) %>% 
  summarise_at(vars(Value), list(sum.verf = sum)) %>% ungroup()

ggplot(contracts, aes(x = as.Date(yw), y = sum.verf)) +
  geom_line()+
  labs(x = "", y = "Average Contracts Verified")

# Combine the results ----
inp <- epu %>% 
  mutate(yw = yearweek(yw)) %>% 
  left_join(tvl, by = "yw") %>% 
  left_join(gas, by = "yw") %>% 
  left_join(contracts, by ="yw") %>% 
  dplyr::select(yw, naive, mod, sum.verf, avg.gas, tvleth, tvlusd) %>% 
  as_tsibble(index = yw) %>% 
  drop_na()

inp_daily <- epu_daily %>% 
  left_join(tvl_daily, by = "date") %>% 
  left_join(gas_daily, by = "date") %>% 
  left_join(contracts_daily, by ="date") %>% 
  dplyr::select(date, naive, mod, sum.verf, avg.gas, tvleth, tvlusd) %>% 
  as_tsibble(index = date) %>% 
  drop_na()

# Save the results for the reproducibility
write.csv(inp, file = "data/inp.csv", row.names = F)
write.csv(inp_daily, file = "data/inp_daily.csv", row.names = F)

# Pre-process for VAR ----
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
all_usd <- irf_usd(irf, custom_title = "TVL (USD) response across all protocols", custom_subtitle = "Weekly frequency")
all_usd

# For TVL ETH
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvleth", ortho = T, n.ahead = 12, boot = TRUE)
all_eth <- irf_eth(irf, custom_title = "TVL (ETH) response across all protocols", custom_subtitle = "Weekly frequency")
all_eth

ggpubr::ggarrange(all_usd, all_eth)

# For gas
irf <- irf(svar, impulse = "epu", response = "gas", ortho = T, n.ahead = 12, boot = TRUE)
h <- irf[["irf"]][["epu"]] %>% as.data.frame() %>% nrow()

irf[["irf"]][["epu"]] %>% as.data.frame() %>% 
  ggplot(aes(y = gas, x = c(1:h)))+
  geom_line(linetype = 2, colour = "black", alpha = 1, size = 0.75)+
  geom_line(data = irf[["Lower"]][["epu"]] %>% as.data.frame(), aes(y = gas, x =c(1:h)), linetype = 2, colour = "red", alpha = 0.8, size = 0.4)+
  geom_line(data = irf[["Upper"]][["epu"]] %>% as.data.frame(), aes(y = gas, x =c(1:h)), linetype = 2, colour = "red", alpha = 0.8, size = 0.4)+
  geom_hline(yintercept = 0, linetype = "dotted", colour = "black", alpha = 0.5)+
  coord_cartesian(ylim = c(min(irf[["Lower"]][["epu"]])-0.05,
                           max(irf[["Upper"]][["epu"]])+0.05),
                  xlim = c(1,h))+
  scale_y_continuous(breaks = seq(round(min(irf[["Lower"]][["epu"]])-0.05, 1), 
                                  round(max(irf[["Upper"]][["epu"]])+0.05, 1),
                                  by = 0.05))+
  scale_x_continuous(breaks = seq(1, h-1, by = 2))+
  labs(subtitle = "Based on modified DeFi uncertainty index", title = "Gas prices response to the uncertainty index in DeFi",
       x = "Lag", y = "") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5))

### For the categories in DeFi ----
unique(tvl_cat$category)
#### DEXes ----
mtr <- mtr_cat(unique(tvl_cat$category)[1])
VARselect(mtr, type="both", lag.max = 12)[["selection"]]
var <- VAR(mtr, p = 1, type = "both", season = NULL)

svar <- SVAR(var, Amat = a.mat, Bmat = b.mat, max.iter = 10000, hessian = TRUE)
svar

# For TVL USD
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvlusd", ortho = T, n.ahead = 12, boot = TRUE)
dex_usd <- irf_usd(irf, custom_title = "TVL (USD) response of DEXes", custom_subtitle = "Weekly frequency")

# For TVL ETH
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvleth", ortho = T, n.ahead = 12, boot = TRUE)
dex_eth <- irf_eth(irf, custom_title = "TVL (ETH) response of DEXes", custom_subtitle = "Weekly frequency")

ggpubr::ggarrange(dex_usd, dex_eth)

#### Lending ----
mtr <- mtr_cat(unique(tvl_cat$category)[2])
VARselect(mtr, type="both", lag.max = 12)[["selection"]]
var <- VAR(mtr, p = 2, type = "both", season = NULL)

svar <- SVAR(var, Amat = a.mat, Bmat = b.mat, max.iter = 10000, hessian = TRUE)
svar

# For TVL USD
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvlusd", ortho = T, n.ahead = 12, boot = TRUE)
lend_usd <- irf_usd(irf, custom_title = "TVL (USD) response of lending protocols", custom_subtitle = "Weekly frequency")

# For TVL ETH
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvleth", ortho = T, n.ahead = 12, boot = TRUE)
lend_eth <- irf_eth(irf, custom_title = "TVL (ETH) response of lending protocols", custom_subtitle = "Weekly frequency")

ggpubr::ggarrange(lend_usd, lend_eth)

#### Payments ----
mtr <- mtr_cat(unique(tvl_cat$category)[3])
VARselect(mtr, type="both", lag.max = 12)[["selection"]]
var <- VAR(mtr, p = 2, type = "both", season = NULL)

svar <- SVAR(var, Amat = a.mat, Bmat = b.mat, max.iter = 10000, hessian = TRUE)
svar

# For TVL USD
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvlusd", ortho = T, n.ahead = 12, boot = TRUE)
pay_usd <- irf_usd(irf, custom_title = "TVL (USD) response of payment protocols", custom_subtitle = "Weekly frequency")

# For TVL ETH
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvleth", ortho = T, n.ahead = 12, boot = TRUE)
pay_eth <- irf_eth(irf, custom_title = "TVL (ETH) response of payment protocols", custom_subtitle = "Weekly frequency")

ggpubr::ggarrange(pay_usd, pay_eth)

#### Assets ----
mtr <- mtr_cat(unique(tvl_cat$category)[4])
mtr[1:3, 4:5] <- 0
VARselect(mtr, type="both", lag.max = 12)[["selection"]]
var <- VAR(mtr, p = 2, type = "both", season = NULL)

svar <- SVAR(var, Amat = a.mat, Bmat = b.mat, max.iter = 10000, hessian = TRUE)
svar

# For TVL USD
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvlusd", ortho = T, n.ahead = 12, boot = TRUE)
ast_usd <- irf_usd(irf, custom_title = "TVL (USD) response of assets protocols", custom_subtitle = "Weekly frequency")

# For TVL ETH
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvleth", ortho = T, n.ahead = 12, boot = TRUE)
ast_eth <- irf_eth(irf, custom_title = "TVL (ETH) response of assets protocols", custom_subtitle = "Weekly frequency")

ggpubr::ggarrange(ast_usd, ast_eth)

#### Derivatives ----
mtr <- mtr_cat(unique(tvl_cat$category)[5])
mtr[1, 4:5] <- 0
VARselect(mtr, type="both", lag.max = 12)[["selection"]]
var <- VAR(mtr, p = 5, type = "both", season = NULL)

svar <- SVAR(var, Amat = a.mat, Bmat = b.mat, max.iter = 10000, hessian = TRUE)
svar

# For TVL USD
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvlusd", ortho = T, n.ahead = 12, boot = TRUE)
drv_usd <- irf_usd(irf, custom_title = "TVL (USD) response of derivatives protocols", custom_subtitle = "Weekly frequency")

# For TVL ETH
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvleth", ortho = T, n.ahead = 12, boot = TRUE)
drv_eth <- irf_eth(irf, custom_title = "TVL (ETH) response of derivatives protocols", custom_subtitle = "Weekly frequency")

ggpubr::ggarrange(drv_usd, drv_eth)

## With daily EPU ----
### For all DeFi TVL ----
inp_ts_daily <- ts(inp_daily[,2:7], frequency=365,
        start= c(year(inp_daily$date[1]), yday(inp_daily$date[1])),
        end = c(year(inp_daily$date[length(inp_daily$date)]), yday(inp_daily$date[length(inp_daily$date)])))

# Plot EPU Index
autoplot(inp_ts_daily[,2])+
  ggtitle("Daily EPU for DeFi services") +
  ylab("Index") + xlab("Day")

autoplot(log(inp_ts_daily[,5]))+
  ggtitle("TVL for the DeFi") +
  ylab("TVL") + xlab("Week")

mtr <- cbind(epu = inp_ts_daily[,"mod"],
             contracts = log(inp_ts_daily[,"sum.verf"]),
             gas = log(inp_ts_daily[,"avg.gas"]),
             tvleth = log(inp_ts_daily[,"tvleth"]),
             tvlusd = log(inp_ts_daily[,"tvlusd"])
)

mtr[1, 4:5] <- 0
VARselect(mtr, type="both", lag.max = 30)[["selection"]]

var <- VAR(mtr, p = 21, type = "both", season = NULL)
svar <- SVAR(var, Amat = a.mat, Bmat = b.mat, max.iter = 10000, hessian = TRUE)
svar

# For TVL USD
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvlusd", ortho = T, n.ahead = 30, boot = TRUE)
all_usd_daily <- irf_usd(irf, custom_title = "TVL (USD) response across all protocols", custom_subtitle = "Daily frequency")
all_usd_daily

# For TVL ETH
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvleth", ortho = T, n.ahead = 30, boot = TRUE)
all_eth_daily <- irf_eth(irf, custom_title = "TVL (ETH) response across all protocols", custom_subtitle = "Daily frequency")
all_eth_daily

ggpubr::ggarrange(all_usd_daily, all_eth_daily)

### For the categories in DeFi ----
unique(tvl_cat$category)
#### DEXes ----
mtr <- mtr_cat_daily(unique(tvl_cat$category)[1])
mtr[1, 4:5] <- 0
VARselect(mtr, type="both", lag.max = 30)[["selection"]]

var <- VAR(mtr, p = 21, type = "both", season = NULL)
svar <- SVAR(var, Amat = a.mat, Bmat = b.mat, max.iter = 10000, hessian = TRUE)
svar

# For TVL USD
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvlusd", ortho = T, n.ahead = 30, boot = TRUE)
dex_usd_daily <- irf_usd(irf, custom_title = "TVL (USD) response of DEXes", custom_subtitle = "Daily frequency")

# For TVL ETH
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvleth", ortho = T, n.ahead = 30, boot = TRUE)
dex_eth_daily <- irf_eth(irf, custom_title = "TVL (ETH) response of DEXes", custom_subtitle = "Daily frequency")

ggpubr::ggarrange(dex_usd_daily, dex_eth_daily)

#### Lending ----
mtr <- mtr_cat_daily(unique(tvl_cat$category)[2])
mtr[1, 4:5] <- 0
VARselect(mtr, type="both", lag.max = 30)[["selection"]]
var <- VAR(mtr, p = 8, type = "both", season = NULL)
svar <- SVAR(var, Amat = a.mat, Bmat = b.mat, max.iter = 10000, hessian = TRUE)
svar

# For TVL USD
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvlusd", ortho = T, n.ahead = 30, boot = TRUE)
lend_usd_daily <- irf_usd(irf, custom_title = "TVL (USD) response of lending protocols", custom_subtitle = "Daily frequency")

# For TVL ETH
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvleth", ortho = T, n.ahead = 30, boot = TRUE)
lend_eth_daily <- irf_eth(irf, custom_title = "TVL (ETH) response of lending protocols", custom_subtitle = "Daily frequency")

ggpubr::ggarrange(lend_usd_daily, lend_eth_daily)

#### Payments ----
mtr <- mtr_cat_daily(unique(tvl_cat$category)[3])
VARselect(mtr, type="both", lag.max = 30)[["selection"]]
var <- VAR(mtr, p = 15, type = "both", season = NULL)
svar <- SVAR(var, Amat = a.mat, Bmat = b.mat, max.iter = 10000, hessian = TRUE)
svar

# For TVL USD
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvlusd", ortho = T, n.ahead = 30, boot = TRUE)
pay_usd_daily <- irf_usd(irf, custom_title = "TVL (USD) response of payment protocols", custom_subtitle = "Daily frequency")

# For TVL ETH
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvleth", ortho = T, n.ahead = 30, boot = TRUE)
pay_eth_daily <- irf_eth(irf, custom_title = "TVL (ETH) response of payment protocols", custom_subtitle = "Daily frequency")

ggpubr::ggarrange(pay_usd_daily, pay_eth_daily)

#### Assets ----
mtr <- mtr_cat_daily(unique(tvl_cat$category)[4])
mtr[1:24, 4:5] <- 0
VARselect(mtr, type="both", lag.max = 30)[["selection"]]
var <- VAR(mtr, p = 14, type = "both", season = NULL)
svar <- SVAR(var, Amat = a.mat, Bmat = b.mat, max.iter = 10000, hessian = TRUE)
svar

# For TVL USD
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvlusd", ortho = T, n.ahead = 30, boot = TRUE)
ast_usd_daily <- irf_usd(irf, custom_title = "TVL (USD) response of assets protocols", custom_subtitle = "Daily frequency")

# For TVL ETH
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvleth", ortho = T, n.ahead = 30, boot = TRUE)
ast_eth_daily <- irf_eth(irf, custom_title = "TVL (ETH) response of assets protocols", custom_subtitle = "Daily frequency")

ggpubr::ggarrange(ast_usd_daily, ast_eth_daily)

#### Derivatives ----
mtr <- mtr_cat_daily(unique(tvl_cat$category)[5])
mtr[1:3, 4:5] <- 0
VARselect(mtr, type="both", lag.max = 30)[["selection"]]
var <- VAR(mtr, p = 7, type = "both", season = NULL)

svar <- SVAR(var, Amat = a.mat, Bmat = b.mat, max.iter = 10000, hessian = TRUE)
svar

# For TVL USD
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvlusd", ortho = T, n.ahead = 30, boot = TRUE)
drv_usd_daily <- irf_usd(irf, custom_title = "TVL (USD) response of derivatives protocols", custom_subtitle = "Daily frequency")

# For TVL ETH
# Impulse response functions
irf <- irf(svar, impulse = "epu", response = "tvleth", ortho = T, n.ahead = 30, boot = TRUE)
drv_eth_daily <- irf_eth(irf, custom_title = "TVL (ETH) response of derivatives protocols", custom_subtitle = "Daily frequency")

ggpubr::ggarrange(drv_usd_daily, drv_eth_daily)
