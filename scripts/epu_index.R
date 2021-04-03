# Load libraries and data ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(stargazer)
library(caret)
library(tidytext)
library(tm)

'%!in%' <- function(x,y)!('%in%'(x,y))

theme_set(theme_minimal())

data <- read.csv("data/all_articles.csv")

# EPU Construction ----
## Naive base method ----
defi_words = " defi | decentrali(z|s)ed finance"
defi_service_words = " decentralized borrow| decentralized lend| stablecoin| decentrali(s|z)ed exchange| DEX | yield farm| DEXes | decentralized oracle"
defi <-  grep(defi_words, data$text, ignore.case = T)
defi_service <-  grep(defi_service_words, data$text, ignore.case = T)
all_defi <- union(defi,defi_service)

defi <- data[all_defi,]

defi_reg <-  grep(" regulat| \bregulation\b|\blegislation\b\regulatory\b|\bdeficit\b|regulatory|White House|Federal Reserve|\bCongress\b| supreme court| government| European Commission|legislat| European Central Bank|\bESMA\b|\bECB\b|\bFCA\b|\bEBA\b|\bjurisdiction\b|\bSEC\b|\bcompliance\b", defi$text, ignore.case = T)
defi_reg <- defi[defi_reg,]

defi_unc <-  grep("uncertain", defi_reg$text, ignore.case = T)
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
  filter(yw > yearweek("2017 W51"),
         yw < yearweek("2021 W12"))

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
  yw = c(as.Date("2020-03-22"), as.Date("2019-07-18"), as.Date("2018-01-22"),
         as.Date("2020-11-06"), as.Date("2020-05-05"), as.Date("2019-09-10")), 
  norm = c(610, 435, 420,
           360, 280, 270), 
  label = c(paste("COVID-19", sep = "\n"),
            paste("Facebook testifies", "about Libra", sep = "\n"),
            paste("Overregulation", "from South Korea", sep = "\n"),
            paste("US elections", sep = "\n"),
            paste("ECON study on", "cryptoassets", sep = "\n"),
            paste("PBoC announces", "plan to launch", "digital currency", sep = "\n")))

ggplot(epu, aes(x = as.Date(yw), y = norm))+
  geom_line(data = epu)+
  #coord_cartesian(xlim = c("2018-09-22", "2019-01-01"))+
  # geom_line(data = epu, aes(y = stnd1*100, colour = "Standar`dised"))+
  geom_text(data = label, aes(label = label), size = 4)+
  labs(#title = "Weekly DeFi Uncertainty Index",
    y = " Economic Policy Uncertainty",
    x = "")


a <- epu_base %>% 
  mutate(yw = yearweek(date)) %>% 
  filter(yw == yearweek("2020 W16"))


## Active learner approach ----
# Import functions
source(file = "scripts/uncertainty_sampling.R")

# Add response to data
data$y <- NA
defi$y <- NA

data("stop_words")
stop_words <- stop_words %>% 
  rbind(c("nbsp", "custom"))

# Filter articles on DeFi
defi_econ <- defi[grep(" econom", defi$text, ignore.case = T),] %>% 
  mutate(id = row_number()) %>% 
  relocate(id)

defi_lab <- lab(defi_lab, 100)

write.csv(defi_lab, file = "data/labbed.csv", row.names = F)


# TESTING ----
# Defining training sample
sample <- defi[grep("econom", defi$text, ignore.case = T),] %>% 
  mutate(id = row_number()) %>% 
  relocate(id)

sample$y <- sample(x = c("yes", "no"), size = 3302, replace = TRUE)
sample$y <- factor(sample$y)

# DTM construction
dtm <- DocumentTermMatrix(Corpus(VectorSource(sample$text)),
                   control = list(removePunctuation = T,
                                  stopwords = T,
                                  tolower = T,
                                  removeNumbers = T,
                                  wordLengths=c(4, 20),
                                  bounds = list(global = c(15,1500)))) %>% 
  as.matrix()


dtm <- sample %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(word %!in% grep("[\\p{Han}]", .$word, value = T, perl = T)) %>% 
  count(id, word, sort = TRUE) %>% 
  cast_dtm(id, word, n) %>% 
  as.matrix()


  
# Split into training and test
idx <- createDataPartition(sample$y, p = 0.8, list = FALSE)
train_x <- dtm[idx,]
train_y <- sample$y[idx]

test_x <- dtm[-idx,]
test_y <- sample$y[-idx]

# Train model (without CV)
fit <- train(x= train_x,
             y = train_y,
             method = "glmnet")


# Retreive and organise top terms by coefficients
coefs <- coef(fit$finalModel, fit$bestTune$lambda)%>% 
  as.matrix() %>% as.data.frame() %>% 
  tibble::rownames_to_column("word") %>% 
  rename(value = `1`) %>% 
  arrange(desc(abs(value))) %>% 
  filter(value !=0)


# Assess the predictive performance
prd <- predict(fit, newdata = test_x)

# Confusion matrix
confusionMatrix(prd, test_y)





# TODO: response to non-defi





# Testing uncertainty sampling ----
## Testing function with the sample data ----
data <- subset(iris, subset = (Species != "virginica"))
data$Species <- droplevels(data$Species)

set.seed(123)
idx <- createDataPartition(data$Species, p = 0.66, list = FALSE)
train <- data[idx, ]
test <- data[-idx, ]

fit_control <- trainControl(method = "cv", number = 5, classProbs = TRUE)
x <- train[, 1:4]
y <- train[, 5]
y[20:23] <- NA


l <- uncertainty_sampling(x, y,
                          uncertainty = "least_confidence", num_query = 3,
                          classifier = "svmLinear", trControl = fit_control)

# Check the least certain data points
which(is.na(y))[l$query]






