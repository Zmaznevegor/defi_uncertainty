# Load libraries and data ----
library(dplyr)
library(ggplot2)
library(data.table)
library(zoo)

theme_set(theme_minimal())
data <- fread("data/all_articles.csv")

# Fixes for the news bitcoin data
# Articles that do not have the end line: sponsored and ads that should be dropped
misc <- data[which(data$source=="newsbitcoin")][!grep("comments.+?below",  data[which(data$source=="newsbitcoin")]$text)]

# Fix footer for news bitcoin (recommendation articles)
# Check for the final line before the end of the article
footer <- data[which(data$source=="newsbitcoin")][grep("comments.+?below",  data[which(data$source=="newsbitcoin")]$text)]

# For the newsdata, gsub text to drop the text that goes after the end line
data[which(data$source=="newsbitcoin")][grep("comments.+?below",  data[which(data$source=="newsbitcoin")]$text)]$text <- gsub("(.+?)comments.+?below(.+)", "\\1", data[which(data$source=="newsbitcoin")][grep("comments.+?below",  data[which(data$source=="newsbitcoin")]$text)]$text)

'%!in%' <- function(x,y)!('%in%'(x,y))

data  <-  data %>% 
  filter(data$text %!in% misc$text)

# Check cryptonews articles for junk
# Defining the irrelevant to the article content
footer <- data[which(data$source=="cryptonews")][grep("Learn more:|\n_____\n",  data[which(data$source=="cryptonews")]$text)]

# gsub unecessary content
data[which(data$source=="cryptonews")][grep("Learn more:|\\n_____\\n",  data[which(data$source=="cryptonews")]$text)]$text <- gsub("(.+?)(Learn more:|\n_____\n)(.+)", "\\1", data[which(data$source=="cryptonews")][grep("Learn more:|\n_____\n",  data[which(data$source=="cryptonews")]$text)]$text)


# Сheck ambcrypto articles for junk
footer <- data[which(data$source=="ambcrpyto")][grep("Did you like the article.\\nYes\\nNo",  data[which(data$source=="ambcrpyto")]$text)]
data[which(data$source=="ambcrpyto")][grep("Did you like the article.\\nYes\\nNo",  data[which(data$source=="ambcrpyto")]$text)]$text<- gsub("(.+?)Did you like the article.+", "\\1", data[which(data$source=="ambcrpyto")][grep("Did you like the article.\\nYes\\nNo",  data[which(data$source=="ambcrpyto")]$text)]$text)

misc <- data[which(data$source=="ambcrpyto")][grep("This is a paid post and should not be considered as news",  data[which(data$source=="ambcrpyto")]$text, ignore.case = T)]
data  <-  data %>% 
  filter(data$text %!in% misc$text)
    
# Check cointelegraph articles for junk
footer <- data[which(data$source=="cointelegraph")][grep("Subscribe to the Finance Redefined newsletter",  data[which(data$source=="cointelegraph")]$text)]
data[which(data$source=="cointelegraph")][grep("Subscribe to the Finance Redefined newsletter",  data[which(data$source=="cointelegraph")]$text)]$text <- gsub("(.+?)Subscribe to the Finance Redefined newsletter.+", "\\1", data[which(data$source=="cointelegraph")][grep("Subscribe to the Finance Redefined newsletter",  data[which(data$source=="cointelegraph")]$text)]$text)

# Check posts per month by media----
posts_per_m <- data %>% 
  mutate(month = as.yearmon(data$date)) %>% 
  group_by(month)%>%
  arrange(month) %>% 
  count(source)

# Line chart per month for all
ggplot(posts_per_m, aes(x = month, y=n, group = source)) + 
  geom_line(aes(color=source))+
  scale_y_continuous(limits = c(0, 1100))

# Per AMB
ggplot(posts_per_m[which(posts_per_m$source=="ambcrpyto"), ], aes(x = month, y=n, group = source)) + 
  geom_line(aes(color=source))+
  scale_y_continuous(limits = c(0, 1100))


defi_words = " defi | decentralized finance| decentralised finance"
defi_service_words = " decentralized borrow| decentralized lend| stablecoin| decentralised exchange| decentralized exchange| DEX | yield farm| DEXes| decentralized oracle| oracle"

defi = grep(defi_words, data$text, ignore.case = T)
defi_service = grep(defi_service_words, data$text, ignore.case = T)
all_defi <- union(defi,defi_service)

# First mention 
max(defi)

data$text[94051]

# All DeFi in media
defi_per_m <- data[all_defi,] %>% 
  mutate(month = as.yearmon(.$date)) %>% 
  group_by(month)%>%
  arrange(month) %>% 
  count(source)

ggplot(defi_per_m, aes(x = month, y=n, group = source)) + 
  geom_line(aes(color=source))+
  scale_y_continuous(limits = c(0, 1100))

# Just defi direct mentions
defi_m <- data[defi,] %>% 
  mutate(month = as.yearmon(.$date)) %>% 
  group_by(month)%>%
  arrange(month) %>% 
  count(source)

ggplot(defi_m, aes(x = month, y=n, group = source)) + 
  geom_point(defi,aes(color=source, size = n))+
  scale_y_continuous(limits = c(0, 1100))

write.csv(data, file = "data/all_articles.csv")