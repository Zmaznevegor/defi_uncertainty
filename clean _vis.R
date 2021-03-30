# Load libraries and data ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(zoo)
library(fpp3)
library(tsibble)
library(stargazer)
library(caret)

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

## Decrypt ----


# Export clean data ----
write.csv(data, file = "data/all_articles.csv", row.names = F)

# EDA Plotting ----
# Check posts per month by media
posts_per_m <- data %>% 
  mutate(month = as.yearmon(data$date)) %>% 
  group_by(month)%>%
  arrange(month) %>% 
  count(source)

# Bar chart per month for all
media_names <- c(`block` = "The Block",
                 `decrypt` = "Decrypt",
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

# TAble with summary data
data %>% 
  mutate(month = as.yearmon(data$date)) %>% 
  group_by(source) %>% 
  summarise(n = n()) %>% 
  left_join(posts_per_m %>% group_by(source) %>% summarise(avg_art_m = round(mean(n)),
                                                           min = min(n),
                                                           max = max(n)) %>% ungroup(), by = "source") %>% 
  arrange(desc(n))%>% 
  rename("Number of articles" = n,
         "Avg per month" = avg_art_m,
         "Min" = min,
         "Max" = max) %>% 
  stargazer(type ="text", summary = F)
