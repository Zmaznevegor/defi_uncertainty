# Load libraries----
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(fpp3)
library(tsibble)

theme_set(theme_minimal())

# Load data ----
data <- read.csv("data/all_articles.csv") %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

tvl <- fread("data/defi/tvl_data.csv")

# Cleaning text data ----
## News Bitcoin ----
# Articles that do not have the end line: sponsored and ads that should be dropped
misc <- data[which(data$source=="newsbitcoin"),][grep("comments.+?below", data[which(data$source =="newsbitcoin"),"text"]),] %>% 
  anti_join(data[which(data$source=="newsbitcoin"),], ., by = "text")

# Fix footer for news bitcoin (recommendation articles)
# For the newsdata, gsub text to drop the text that goes after the end line
data[which(data$source=="newsbitcoin"),][grep("comments.+?below", data[which(data$source =="newsbitcoin"),"text"]),]$text <- gsub("(.+?)comments.+?below(.+)", "\\1", data[which(data$source=="newsbitcoin"),][grep("comments.+?below", data[which(data$source =="newsbitcoin"),"text"]),]$text)

'%!in%' <- function(x,y)!('%in%'(x,y))

data  <-  data %>% 
  filter(data$text %!in% misc$text)

## Cryptonews ----
# Defining the irrelevant to the article content
footer <- data[which(data$source=="cryptonews")][grep("Learn more:|\n_____\n",  data[which(data$source=="cryptonews")]$text)]

# gsub unecessary content
data[which(data$source=="cryptonews")][grep("Learn more:|\\n_____\\n",  data[which(data$source=="cryptonews")]$text)]$text <- gsub("(.+?)(Learn more:|\n_____\n)(.+)", "\\1", data[which(data$source=="cryptonews")][grep("Learn more:|\n_____\n",  data[which(data$source=="cryptonews")]$text)]$text)

    
## Cointelegraph ----
footer <- data[which(data$source=="cointelegraph")][grep("Subscribe to the Finance Redefined newsletter",  data[which(data$source=="cointelegraph")]$text)]
data[which(data$source=="cointelegraph")][grep("Subscribe to the Finance Redefined newsletter",  data[which(data$source=="cointelegraph")]$text)]$text <- gsub("(.+?)Subscribe to the Finance Redefined newsletter.+", "\\1", data[which(data$source=="cointelegraph")][grep("Subscribe to the Finance Redefined newsletter",  data[which(data$source=="cointelegraph")]$text)]$text)

# TODO: Clean updated footer
# TODO: Check whether cleaning is necessary for other data sources

# Export clean data ----
write.csv(data, file = "data/all_articles.csv", row.names = F)

# Check posts per month by media----
posts_per_m <- data %>% 
  mutate(month = as.yearmon(data$date)) %>% 
  group_by(month)%>%
  arrange(month) %>% 
  count(source)

# Line chart per month for all
ggplot(posts_per_m, aes(x = month, y=n, group = source)) + 
  geom_line(aes(color=source))+
  scale_y_continuous(limits = c(0, 1100))+
  labs(x = "Date",
       y = "Number of articles per month",
       color = "Media")


# Naive base method
defi_words = " defi | decentrali(z|s)ed finance"
defi_service_words = " decentralized borrow| decentralized lend| stablecoin| decentralised exchange| decentralized exchange| DEX | yield farm| DEXes| decentralized oracle"
defi = grep(defi_words, data$text, ignore.case = T)
defi_service = grep(defi_service_words, data$text, ignore.case = T)
all_defi <- union(defi,defi_service)

defi <- data[all_defi,]

defi_reg <-  grep(" regulat| \bregulation\b|\blegislation\b\regulatory\b|\bdeficit\b|regulatory|White House|Federal Reserve|\bCongress\b| supreme court| government| European Commission|legislat| European Central Bank|\bESMA\b|\bECB\b|\bFCA\b|\bEBA\b|\bjurisdiction\b|\bSEC\b|\bcompliance\b", defi$text, ignore.case = T)
defi_reg <- defi[defi_reg,]

defi_unc <-  grep(" uncertain", defi_reg$text, ignore.case = T)
epu_base <- defi_reg[defi_unc,]

econ <- grep(" econom", epu_base$text, ignore.case = T)
a <- epu_base[econ,]

a1 <- a %>% 
  filter(source != "decrypt")

# Try unnest_tokens approach (!)

# Convert to weeks
epu_wbase <- a1 %>%
  mutate(yw = yearweek(date)) %>% 
  group_by(yw, source) %>% 
  summarise(n_articles = n())

ggplot(epu_wbase, aes(x = yw, y = n_articles))+geom_line()

wdata <- data %>% 
  mutate(yw = yearweek(date)) %>% 
  group_by(yw, source) %>% 
  summarise(n_articles = n())

ggplot(wdata, aes(x = yw, y = n_articles))+geom_line()

scale <- left_join(wdata, epu_wbase, by = c("yw", "source")) %>% 
  rename(n_articles = n_articles.x,
         epu_articles = n_articles.y) %>% 
  replace_na(list(epu_articles = 0)) %>% 
  filter(yw > yearweek("2017 W51"),
         yw <yearweek("2020 W45"))

a <- defi %>% mutate(yw = yearweek(date)) %>% 
  group_by(yw) %>% 
  summarise(n_art = n())

ggplot(scale, aes(x = yw, y = epu_articles, colour = source))+ geom_line()

wk <- scale %>% 
  group_by(yw) %>% 
  summarise(articles = sum(n_articles),
            epu_a = sum(epu_articles))

ggplot(wk, aes(x=yw, y = epu_a))+geom_line()
ggplot(wk, aes(x=yw, y = articles))+geom_line()

t1 <- wk$yw[1:round(0.8*length(wk$yw))]
t2 <- wk$yw[(length(t1)+1):length(wk$yw)]

a <- scale %>% 
  mutate(scaled = epu_articles/n_articles)
sd1 <- with(a, sd(scaled[scale$yw %in% t1]))

m <- a %>% mutate(stnd = scaled/sd1) %>% 
  group_by(yw) %>% 
  summarise(m = mean(stnd)) %>% 
  filter(yw %in% t2)

epu <- a %>% 
  mutate(stnd = scaled/sd1) %>% 
  group_by(yw) %>% 
  summarise(stnd1 = mean(stnd)) %>% 
  mutate(norm = stnd1/mean(m$m)*100)

# Annotations
label <- data.frame(
  date = c(55, 80), 
  eruptions = c(2, 4.3), 
  label = c("peak one", "peak two")
)

ggplot(epu, aes(x = yw))+
  geom_line(data = epu, aes(y = norm))+
  coord_cartesian(xlim = c("2018-09-22", "2019-01-01"))+
  # geom_line(data = epu, aes(y = stnd1*100, colour = "Standar`dised"))+
  labs(title = "Weekly DeFi Uncertainty Index",
       y = "Policy Uncertainty Index",
       x = "Date")
