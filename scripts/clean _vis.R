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

# Cleaning text data ----
'%!in%' <- function(x,y)!('%in%'(x,y))

## News Bitcoin ----
# Articles that do not have the end line: sponsored and ads that should be dropped
misc <- data %>% 
  filter(source == "newsbitcoin") %>% 
  filter(grepl("comments.+?below|in.the.comments.section", .$text)) %>% 
  anti_join(data[which(data$source=="newsbitcoin"),], ., by = "text")

# Fix footer for news bitcoin (recommendation articles)
data[which(data$source=="newsbitcoin"),] <- data %>% 
  filter(source == "newsbitcoin") %>% 
  mutate(text = ifelse(grepl("comments.+?below|in.the.comments.section", .$text), gsub("(.+?)(comments.+?below|in.the.comments.section)(.+)", "\\1", .$text), no =.$text))

data  <-  data %>% 
  filter(data$text %!in% misc$text)

## Cryptonews ----
# Fix footer
data[which(data$source=="cryptonews"),] <- data %>% 
  filter(source == "cryptonews") %>% 
  mutate(text = ifelse(grepl("Learn more:|\n_____\n", .$text), gsub("(.+?)(Learn more:|\n_____\n)(.+)", "\\1", .$text), no =.$text))

# Drop press releases and ads
misc <- data %>% 
  filter(source == "cryptonews") %>% 
  filter(grepl("Disclaimer: The text below is a press release that was not written by Cryptonews.com", .$text)|
           grepl("The text below is an advertorial article", .$text))

data  <-  data %>% 
  filter(data$text %!in% misc$text)

## Cointelegraph ----
# Fixing footer
data[which(data$source=="cointelegraph"),] <- data %>% 
  filter(source == "cointelegraph") %>% 
  mutate(text = ifelse(grepl("Subscribe to the Finance Redefined newsletter", .$text), gsub("(.+?)Subscribe to the Finance Redefined newsletter.+", "\\1", .$text), no =.$text)) %>% 
  mutate(text = ifelse(grepl("DELIVERED EVERY (MONDAY|FRIDAY)", .$text), gsub("(.+?)DELIVERED.EVERY.(MONDAY|FRIDAY).+", "\\1", .$text), no =.$text))

# Dropping consulting reports, guest articles, press releases, paid article
misc <- data %>% 
  filter(source == "cointelegraph") %>% 
  filter(grepl("Cointelegraph Consulting Report", .$text) |
           grepl("The views and opinions expressed here ", .$text) |
           grepl("Cointelegraph does not endorse", .$text) |
           grepl("article does not contain investment advice", .$text) |
           grepl("this article is for general information purposes and is not intended to be and should not be taken as legal advice", .$text))

# Removing recommendations from the articles
data[which(data$source=="cointelegraph"),"text"] <- gsub("(Also read:|See also:|Read more:|Related:).+?\n", "", data[which(data$source=="cointelegraph"),"text"])

data  <-  data %>% 
  filter(data$text %!in% misc$text)

## Coindesk ----
data[which(data$source=="coindesk"),] <- data %>% 
  filter(source == "coindesk") %>% 
  mutate(text = ifelse(grepl("Also read:|See also:|Read more:", .$text), gsub("[[:space:]](Also read:|See also:|Read more).+?[[:space:]][[:space:]]", "", .$text), no =.$text))

## The Block ----
# Cleaning formatting and disclaimer
data[which(data$source=="block"),] <- data %>% 
  filter(source == "block") %>% 
  mutate(text = ifelse(grepl("<.+?>", .$text), gsub("<.+?>", "", .$text), no =.$text)) %>% 
  mutate(text = ifelse(grepl("This article is provided for informational purposes only. It is not offered or intended to be used as legal, tax, investment, financial, or other advice", .$text), gsub("This article is provided for informational purposes only. It is not offered or intended to be used as legal, tax, investment, financial, or other advice", "", .$text), no =.$text))


## NewsBTC ----
# Drop recommended articles
data[which(data$source=="newsbtc"),] <- data %>% 
  filter(source == "newsbtc") %>% 
  mutate(text = ifelse(grepl("Related Reading", .$text), gsub("<.+?>", "[[:space:]](Related Reading).+?[[:space:]][[:space:]]", .$text), no =.$text))

## Slate ----
misc <- data %>% 
  filter(source == "slate") %>% 
  filter(grepl("sign up for ", .$text) |
           grepl("advertising partner for CryptoSlate", .$text) |
           grepl(" Disclaimer ", .$text) |
           grepl("this is a sponsored post", .$text))

data  <-  data %>% 
  filter(data$text %!in% misc$text)

## Decrypt ----
# Delete guest articles
misc <- data %>% 
  filter(source == "decrypt") %>% 
  filter(grepl("for informational purposes only and do not constitute financial, investment, or other advice", .$text) |
           grepl("The views and opinions expressed by the author are for informational purposes", .$text))

data  <-  data %>% 
  filter(data$text %!in% misc$text)

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

# Table with summary data
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
