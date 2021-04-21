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
tvl <- read.csv("data/defi/tvl_data.csv") %>% 
  mutate(yw = yearweek(time)) %>% 
  dplyr::select(yw, tvlusd, tvleth, token) %>% 
  filter(yw > yearweek("2017 W51"),
         yw < yearweek("2021 W12")) %>% 
  group_by(yw, token) %>% 
  summarise_at(vars(tvleth, tvlusd), list(avg = mean)) %>% 
  rename(tvleth = tvleth_avg,
         tvlusd = tvlusd_avg)

tvl_daily <- read.csv("data/defi/tvl_data.csv") %>% 
  mutate(date = as.Date(time)) %>% 
  dplyr::select(date, tvlusd, tvleth, token) %>% 
  filter(date > as.Date(yearweek("2017 W51")),
         date < as.Date(yearweek("2021 W12"))) %>% 
  group_by(date, token) %>% 
  summarise_at(vars(tvleth, tvlusd), list(avg = mean)) %>% 
  rename(tvleth = tvleth_avg,
         tvlusd = tvlusd_avg)

# Plot the data
ggplot(tvl, aes(x = as.Date(yw), y = tvleth)) +
  geom_line(aes(colour = token))+
  labs(x = "", y = "TVL (ETH)", colour = "DeFi")

tvl %>% 
  filter(token == "maker") %>% 
  ggplot(aes(x = as.Date(yw)))+
  geom_line(aes(y = log(tvlusd)), colour = "red")+
  geom_line(aes(y = log(tvleth)), colour = "black")

## Import gas stats ----
# Gas price
gas <- read.csv("data/defi/export-AvgGasPrice.csv") %>% 
  mutate(yw = yearweek(as.Date(Date.UTC., format = "%m/%d/%Y")),
         Value = Value..Wei.) %>% 
  dplyr::select(yw, Value) %>% 
  group_by(yw) %>% 
  summarise_at(vars(Value), list(avg.gas = mean)) %>% 
  filter(yw > yearweek("2017 W51"), yw < yearweek("2021 W12")) %>% ungroup()

gas_daily <- read.csv("data/defi/export-AvgGasPrice.csv") %>% 
  mutate(date = as.Date(Date.UTC., format = "%m/%d/%Y"),
         Value = Value..Wei.) %>% 
  dplyr::select(date, Value) %>% 
  group_by(date) %>% 
  summarise_at(vars(Value), list(avg.gas = mean)) %>% 
  filter(date > as.Date(yearweek("2017 W51")), date < as.Date(yearweek("2021 W12"))) %>% ungroup()

ggplot(gas, aes(x = as.Date(yw), y = avg.gas)) +
  geom_line()+
  labs(x = "", y = "Average Gas Price")

# Verified contracts
contracts <- read.csv("data/defi/export-verified-contracts.csv") %>% 
  mutate(yw = yearweek(as.Date(Date.UTC.)),
         Value = No..of.Verified.Contracts) %>% 
  dplyr::select(yw, Value) %>% 
  group_by(yw) %>% 
  summarise_at(vars(Value), list(sum.verf = sum)) %>% 
  filter(yw > yearweek("2017 W51"), yw < yearweek("2021 W12")) %>% ungroup()

contracts_daily <- read.csv("data/defi/export-verified-contracts.csv") %>% 
  mutate(date = as.Date(Date.UTC.),
         Value = No..of.Verified.Contracts) %>% 
  dplyr::select(date, Value) %>% 
  group_by(date) %>% 
  summarise_at(vars(Value), list(sum.verf = sum)) %>% 
  filter(date > as.Date(yearweek("2017 W51")), date < as.Date(yearweek("2021 W12"))) %>% ungroup()

ggplot(contracts, aes(x = as.Date(yw), y = sum.verf)) +
  geom_line()+
  labs(x = "", y = "Average Contracts Verified")

## Combine the results ----
inp <- epu %>% 
  mutate(yw = yearweek(yw)) %>% 
  left_join(tvl %>% filter(token == "maker"), by = "yw") %>% 
  left_join(gas, by = "yw") %>% 
  left_join(contracts, by ="yw") %>% 
  dplyr::select(yw, mod, sum.verf, avg.gas, tvleth, tvlusd) %>% 
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

# Save the results for the reproducability
write.csv(inp, file = "data/inp.csv", row.names = F)
write.csv(inp_daily, file = "data/inp_daily.csv", row.names = F)

a <- ts(inp[,2:6], frequency=52,
        start= c(year(inp$yw[1]),week(inp$yw[1])),
        end = c(year(inp$yw[length(inp$yw)]),week(inp$yw[length(inp$yw)])))

# Plot EPU Index
autoplot(a[,1])+
  ggtitle("Weekly EPU for DeFi services") +
  ylab("Index") + xlab("Week")

autoplot(log(a[,4]))+
  ggtitle("TVL for the DeFi") +
  ylab("TVL") + xlab("Week")


a[,1] %>% checkresiduals()
diff(a[,2]) %>% checkresiduals()
diff(a[,3]) %>% checkresiduals()
diff(log(a[,4])) %>% checkresiduals()
diff(log(a[,5])) %>% checkresiduals()
diff(log(a[,6])) %>% checkresiduals()

# Select best VAR ----
## With weekly EPU ----
### For Maker ----
mtr <- cbind(epu = a[,"mod"],
             contracts = log(a[,"sum.verf"]),
             gas = log(a[,"avg.gas"]),
             tvleth = a[,"tvleth"]
             #tvlusd = log(a[,"tvlusd"])
             )

VARselect(mtr, type="both", lag.max = 12)[["selection"]]

var <- VAR(mtr, p = 3, type = "both", season = NULL)

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
irf <- irf(svar, impulse = "epu", response = "tvleth", ortho = T, n.ahead = 12, boot = TRUE)
plot(irf, ylab = "output", main = "Shock from uncertainty")


irf[["irf"]][["epu"]] %>% as.data.frame() %>% 
  ggplot(aes(y = tvleth, x = c(1:13)))+
  geom_line(linetype = 2, colour = "black", alpha = 1, size = 0.75)+
  geom_line(data = irf[["Lower"]][["epu"]] %>% as.data.frame(), aes(y = tvleth, x =c(1:13)), linetype = 2, colour = "red", alpha = 0.8, size = 0.4)+
  geom_line(data = irf[["Upper"]][["epu"]] %>% as.data.frame(), aes(y = tvleth, x =c(1:13)), linetype = 2, colour = "red", alpha = 0.8, size = 0.4)+
  geom_hline(yintercept = 0, linetype = "dotted", colour = "black", alpha = 0.5)+
  coord_cartesian(ylim = c(min(irf[["Lower"]][["epu"]])-0.1, max(irf[["Upper"]][["epu"]])+0.1), xlim = c(1,13))+
  scale_y_continuous(breaks = seq(round(min(irf[["Lower"]][["epu"]])-0.15, 1), 
                                  round(max(irf[["Upper"]][["epu"]])+0.15, 1),
                                  by = 0.1))+
  scale_x_continuous(breaks = seq(1, 12, by = 2))+
  labs(subtitle = "Modified DeFi EPU index", title = "Effect of uncertainty impulse on TVL (ETH)",
       x = "Lag", y = "") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5))

VARselect(cbind(epu = diff(a[,1]), 
                tvl = diff(log(a[,5]))),
          type="both")[["selection"]]

var <- VAR(cbind(epu = a[,1], 
                 tvl = a[,2]), 
           p=4, 
           type="both")

# Impulse response functions
irf <- irf(var, impulse = "epu", response = "tvl", 
           n.ahead = 12, boot = TRUE, runs = 1500)

plot(irf, ylab = "output", main = "Shock from uncertainty")

irf[["irf"]][["epu"]] %>% as.data.frame() %>% 
  ggplot(aes(y = tvl, x = c(1:13)))+
  geom_line(linetype = 2, colour = "black", alpha = 1, size = 0.75)+
  geom_line(data = irf[["Lower"]][["epu"]] %>% as.data.frame(), aes(y = tvl, x =c(1:13)), linetype = 2, colour = "red", alpha = 0.8, size = 0.4)+
  geom_line(data = irf[["Upper"]][["epu"]] %>% as.data.frame(), aes(y = tvl, x =c(1:13)), linetype = 2, colour = "red", alpha = 0.8, size = 0.4)+
  geom_hline(yintercept = 0, linetype = "dotted", colour = "black", alpha = 0.5)+
  coord_cartesian(ylim = c(-5, 6))+
  scale_y_continuous(breaks = seq(-6, 6, by = 1))+
  scale_x_continuous(breaks = seq(0, 12, by = 2))+
  labs(subtitle = "Modified DeFi EPU index", title = "Effect of uncertainty impulse on verified contracts",
       x = "Lag", y = "") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
                                  plot.subtitle = element_text(hjust = 0.5, vjust = 0.5))


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
             tvleth = a[,"tvleth"]
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
  coord_cartesian(ylim = c(min(irf[["Lower"]][["epu"]])-0.1, max(irf[["Upper"]][["epu"]])+0.1), xlim = c(1,31))+
  scale_y_continuous(breaks = seq(round(min(irf[["Lower"]][["epu"]])-0.15, 1), 
                                  round(max(irf[["Upper"]][["epu"]])+0.15, 1),
                                  by = 0.1))+
  scale_x_continuous(breaks = seq(1, 30, by = 2))+
  labs(subtitle = "Modified DeFi EPU index", title = "Effect of uncertainty impulse on TVL (ETH)",
       x = "Lag", y = "") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5))
