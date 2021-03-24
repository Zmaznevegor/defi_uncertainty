# Load libraries and data ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(zoo)
library(fpp3)
library(tsibble)
library(stargazer)

theme_set(theme_minimal())

data <- read.csv("data/all_articles.csv") %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# TODO: update TVL data with Ether scan https://etherscan.io/apis#tokens 
# tvl <- fread("data/defi/tvl_data.csv")

# Cleaning text data ----
'%!in%' <- function(x,y)!('%in%'(x,y))

## News Bitcoin ----
# Articles that do not have the end line: sponsored and ads that should be dropped
misc <- data[which(data$source=="newsbitcoin"),][grep("comments.+?below", data[which(data$source =="newsbitcoin"),"text"]),] %>% 
  anti_join(data[which(data$source=="newsbitcoin"),], ., by = "text")

# Fix footer for news bitcoin (recommendation articles)
# For the newsdata, gsub text to drop the text that goes after the end line
# TODO: paraphrase the following line
data[which(data$source=="newsbitcoin"),][grep("comments.+?below", data[which(data$source =="newsbitcoin"),"text"]),]$text <- gsub("(.+?)comments.+?below(.+)", "\\1", data[which(data$source=="newsbitcoin"),][grep("comments.+?below", data[which(data$source =="newsbitcoin"),"text"]),]$text)

data  <-  data %>% 
  filter(data$text %!in% misc$text)

## Cryptonews ----
# gsub unecessary content
data[which(data$source=="cryptonews"),] <- data %>% 
  filter(source == "cryptonews") %>% 
  mutate(text = ifelse(grepl("Learn more:|\n_____\n", .$text), gsub("(.+?)(Learn more:|\n_____\n)(.+)", "\\1", .$text), no =.$text))

# Advertised materials
misc <- data[which(data$source=="cryptonews"),][grep("The text below is an advertorial article",  data[which(data$source=="cryptonews"), "text"]),]
data  <-  data %>% 
  filter(data$text %!in% misc$text)
    
## Cointelegraph ----
data[which(data$source=="cointelegraph"),][grep("Subscribe to the Finance Redefined newsletter",  data[which(data$source=="cointelegraph"), "text"]),]$text <- gsub("(.+?)Subscribe to the Finance Redefined newsletter.+", "\\1", data[which(data$source=="cointelegraph"),][grep("Subscribe to the Finance Redefined newsletter",  data[which(data$source=="cointelegraph"), "text"]),]$text)
data[which(data$source=="cointelegraph"),][grep("DELIVERED EVERY MONDAY",  data[which(data$source=="cointelegraph"), "text"]),]$text <- gsub("(.+?)DELIVERED.EVERY.MONDAY.+", "\\1", data[which(data$source=="cointelegraph"),][grep("DELIVERED EVERY MONDAY",  data[which(data$source=="cointelegraph"), "text"]),]$text)

misc <- data[which(data$source=="cointelegraph"),][grep("The views and opinions expressed here are solely those of the author and do not necessarily reflect the views of Cointelegraph", data[which(data$source=="cointelegraph"),"text"]),]
data  <-  data %>% 
  filter(data$text %!in% misc$text)

## Coindesk ----
footer <- data[which(data$source=="coindesk"),][grep("Also read:|See also:|Read more:",  data[which(data$source=="coindesk"), "text"]),]
data[which(data$source=="coindesk"),][grep("Also read:|See also:|Read more:",  data[which(data$source=="coindesk"), "text"]),]$text <- gsub("(.+?)[[:space:]][[:space:]][[:space:]](Also read:|See also:|Read more)[[:space:]][[:space:]][[:space:]](.+)", "\\1 \\3", data[which(data$source=="coindesk"),][grep("Also read:|See also:|Read more:",  data[which(data$source=="coindesk"), "text"]),]$text)
data[which(data$source=="coindesk"),][grep("Also read:|See also:|Read more:",  data[which(data$source=="coindesk"), "text"]),]$text <- gsub("[[:space:]](Also read:|See also:|Read more).+?[[:space:]][[:space:]][[:space:]]", "", data[which(data$source=="coindesk"),][grep("Also read:|See also:|Read more:",  data[which(data$source=="coindesk"), "text"]),]$text)

## The Block ----
data[which(data$source=="block"),"text"] <- gsub("<.+?>","", data[which(data$source=="block"),"text"])

## NewsBTC ----
data[which(data$source=="newsbtc"),][grep("Related Reading",  data[which(data$source=="newsbtc"), "text"]),]$text <- gsub("[[:space:]](Related Reading).+?[[:space:]][[:space:]][[:space:]]", "", data[which(data$source=="newsbtc"),][grep("Related Reading",  data[which(data$source=="newsbtc"), "text"]),]$text)

# Export clean data ----
write.csv(data, file = "data/all_articles.csv", row.names = F)

# EDA Plotting ----
# Check posts per month by media
posts_per_m <- data %>% 
  mutate(month = as.yearmon(data$date)) %>% 
  group_by(month)%>%
  arrange(month) %>% 
  count(source)

# Line chart per month for all
media_names <- c(`block` = "The Block",
                 `blockonomi` = "Blockonomi",
                 `cnf` = "Crypto News Flash",
                 `coindesk` = "Coindesk",
                 `cointelegraph` = "Cointelegraph",
                 `cryptonews` = "Crypto News",
                 `newsbitcoin` = "News Bitcoin",
                 `newsbtc` = "NewsBTC",
                 `slate` = "CryptoSlate")

ggplot(posts_per_m, aes(x = as.Date(month), y=n, group = source)) +
  geom_col(aes(fill=source), colour = "black", size = 0.2) +
  geom_hline(aes(yintercept = mean(n), colour = "red"), size = 0.4)+
  facet_wrap(source~., labeller = as_labeller(media_names)) +
  labs(#title= "Monthly articles per media",
       #subtitle = "Based on the cleaned and preprocessed data",
       x = "", y = "Articles per month") +
  theme_bw() +
  theme(legend.position = "none", 
        strip.background = element_rect(fill = "#FFFFFF"),
        strip.text.x = element_text(size = 10, face = "bold"))

data %>% 
  mutate(month = as.yearmon(data$date)) %>% 
  group_by(source) %>% 
  summarise(n = n()) %>% 
  left_join(posts_per_m %>% group_by(source) %>% summarise(avg_art_m = round(mean(n)),
                                                           min = min(n),
                                                           max = max(n)) %>% ungroup(), by = "source") %>% 
  stargazer(type ="text", summary = F)
  

# EPU Construction ----
## Naive base method ----
defi_words = " defi | decentrali(z|s)ed finance"
defi_service_words = " decentralized borrow| decentralized lend| stablecoin| decentrali(s|z)ed exchange| DEX | yield farm| DEXes | decentralized oracle"
defi = grep(defi_words, data$text, ignore.case = T)
defi_service = grep(defi_service_words, data$text, ignore.case = T)
all_defi <- union(defi,defi_service)

defi <- data[all_defi,]

defi_reg <-  grep(" regulat| \bregulation\b|\blegislation\b\regulatory\b|\bdeficit\b|regulatory|White House|Federal Reserve|\bCongress\b| supreme court| government| European Commission|legislat| European Central Bank|\bESMA\b|\bECB\b|\bFCA\b|\bEBA\b|\bjurisdiction\b|\bSEC\b|\bcompliance\b", defi$text, ignore.case = T)
defi_reg <- defi[defi_reg,]

defi_unc <-  grep(" uncertain", defi_reg$text, ignore.case = T)
pu_base <- defi_reg[defi_unc,]
econ <- grep(" econom", pu_base$text, ignore.case = T)
epu_base <- pu_base[econ,]

### Normalization and standartisation ----
# Convert to weeks
epu_base_yw <- epu_base %>%
  mutate(yw = yearweek(date)) %>% 
  group_by(yw, source) %>% 
  summarise(n_articles = n())

ggplot(epu_base_yw, aes(x = yw, y = n_articles))+geom_line()

data_yw <- data %>% 
  mutate(yw = yearweek(date)) %>% 
  group_by(yw, source) %>% 
  summarise(n_articles = n())

ggplot(data_yw, aes(x = yw, y = n_articles))+geom_line()

scale <- left_join(data_yw, epu_base_yw, by = c("yw", "source")) %>% 
  rename(n_articles = n_articles.x,
         epu_articles = n_articles.y) %>% 
  replace_na(list(epu_articles = 0)) %>% 
  filter(yw > yearweek("2017 W51"))

ggplot(scale, aes(x = yw, y = epu_articles, colour = source))+ geom_line()

wk <- scale %>% 
  group_by(yw) %>% 
  summarise(articles = sum(n_articles),
            epu_a = sum(epu_articles))

t1 <- wk$yw[1:round(0.8*length(wk$yw))]
t2 <- wk$yw[(length(t1)+1):length(wk$yw)]

base <- scale %>% 
  mutate(scaled = epu_articles/n_articles)
sd1 <- with(base, sd(scaled[scale$yw %in% t1]))

m <- base %>% mutate(stnd = scaled/sd1) %>% 
  group_by(yw) %>% 
  summarise(m = mean(stnd)) %>% 
  filter(yw %in% t2)

epu <- base %>% 
  mutate(stnd = scaled/sd1) %>% 
  group_by(yw) %>% 
  summarise(stnd1 = mean(stnd)) %>% 
  mutate(norm = stnd1/mean(m$m)*100)

### Plotting naive index ----
# Annotations
label <- data.frame(
  date = c(55, 80), 
  eruptions = c(2, 4.3), 
  label = c("peak one", "peak two")
)

ggplot(epu, aes(x = as.Date(yw)))+
  geom_line(data = epu, aes(y = norm))+
  #coord_cartesian(xlim = c("2018-09-22", "2019-01-01"))+
  # geom_line(data = epu, aes(y = stnd1*100, colour = "Standar`dised"))+
  labs(title = "Weekly DeFi Uncertainty Index",
       y = " Economic Policy Uncertainty",
       x = "")

epu %>% 
  filter(norm != 0) %>% 
  summarise(mean = mean(norm))

data %>% 
  group_by(source) %>% 
  summarise(n = n())
