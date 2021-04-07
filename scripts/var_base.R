# Libraries and data load ----
library(dplyr)
library(tsibble)
library(fpp3)
library(fable)
library(forecast)
library(ggplot2)
library(vars)

theme_set(theme_minimal())

a <- ts(inp[,2:7], frequency=53,
        start= c(2017,52),
        end = c(2021,11))

autoplot(a)

# Plot EPU Index
autoplot(a[,1])+
  ggtitle("Weekly EPU for DeFi services") +
  ylab("Index") + xlab("Week")

# Check differencing and log
diff(a[,3]) %>% checkresiduals()

diffed <- diff(a)

autoplot(diffed[,1])+
  ggtitle("Differenced weekly EPU") +
  ylab("Index") + xlab("Week")

autoplot(diff(log(y[,3])))+
  ggtitle("Differenced weekly Maker price") +
  ylab("Price (ETH)") + xlab("Week")

diff(log(a[,2])) %>% checkresiduals()

# Select best VAR ----
# For Maker
VARselect(cbind(epu = a[,1], 
                tvl = log(a[,5])),
          type="both")[["selection"]]

var <- VAR(cbind(epu =a[,1], 
                 tvl = log(a[,5])), 
           p=3, 
           type="both")

# Impulse response functions
irf <- irf(var, impulse = "epu", response = "tvl", 
               n.ahead = 12, boot = TRUE, runs = 100)

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

# TODO: try with summed verified transactions

VARselect(cbind(epu = a[,1], 
                tvl = a[,2]),
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
