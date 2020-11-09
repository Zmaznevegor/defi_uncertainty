# Load libraries and data ----
library(dplyr)
library(ggplot2)
library(data.table)
library(zoo)

theme_set(theme_minimal())
data <- fread("data/all_articles.csv")

# Fixes for the news bitcoin data
# Fix footer for news bitcoin (recommendation articles)
# Check for the final line before the end of the article
footer <- data[which(data$source=="newsbitcoin")][grep("comments.+?below",  data[which(data$source=="newsbitcoin")]$text)]

# For the newsdata, gsub text to drop the text that goes after the end line
data[which(data$source=="newsbitcoin")][grep("comments.+?below",  data[which(data$source=="newsbitcoin")]$text)] <- gsub("(.+?)comments.+?below(.+)", "\\1", data[which(data$source=="newsbitcoin")][grep("comments.+?below",  data[which(data$source=="newsbitcoin")]$text)]$text)
    
# Articles that do not have the end line: sponsored and ads that should be dropped
misc <- data[which(data$source=="newsbitcoin")][!grep("comments.+?below",  data[which(data$source=="newsbitcoin")]$text)]

'%!in%' <- function(x,y)!('%in%'(x,y))

data  <-  data %>% 
  filter(data$text %!in% misc$text)


# TODO: check cryptonews articles for junk
# TODO: check ambcrypto articles for junk
# TODO: check cointelegraph articles for junk

# Check posts per month by media----
posts_per_m <- data %>% 
  mutate(month = as.yearmon(data$date)) %>% 
  group_by(month)%>%
  arrange(month) %>% 
  count(source)

# Line chart per month
ggplot(posts_per_m, aes(x = month, y=n, group = source)) + 
  geom_line(aes(color=source))+
  scale_y_continuous(limits = c(0, 700))