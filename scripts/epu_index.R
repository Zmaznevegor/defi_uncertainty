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
### Preparation ----
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

defi_lab <- lab(defi_lab, 80)

write.csv(defi_lab, file = "data/labbed.csv", row.names = F)

defi_lab <- read.csv("data/labbed.csv")

# Defining training sample
defi_lab$y <- factor(defi_lab$y, levels = c(1,2), labels = c("Yes", "No"))

# DTM construction
dtm <- DocumentTermMatrix(Corpus(VectorSource(defi_lab$text)),
                   control = list(removePunctuation = T,
                                  stopwords = T,
                                  tolower = T,
                                  removeNumbers = T,
                                  wordLengths=c(4, 20),
                                  bounds = list(global = c(10,1500)))) %>% 
  as.matrix()
  
### Split into training and test ----
### For uncertainty sampling
idx <- defi_lab %>% 
  filter(!is.na(y)) %>% 
  sample_n(100)

test_x <- dtm[idx$id,]
test_y <- idx$y

### Train model on the non-NA
idx_1 <-  defi_lab %>% 
  filter(!is.na(y)) %>% 
  filter(id %!in% idx$id)

train_x1 <- dtm[idx_1$id,]
train_y1 <- idx_1$y
  
## Modeling and predictions
fit_control <- trainControl(method = "cv", number = 10, classProbs = TRUE)

fit1 <- train(x= train_x1,
             y = train_y1,
             method = "svmLinear",
             trControl = fit_control)

prd1 <- predict(fit1, newdata = test_x, type = "prob")

## Evaluating results: confusion matrix and AUC
test_set <- data.frame(obs = test_y,
                       Yes = prd1$Yes,
                       No = prd1$No)
test_set$pred <- factor(ifelse(test_set$Yes >= .5, 1, 2), labels = c("Yes", "No"))

confusionMatrix(test_set$pred, test_y)
prSummary(test_y, lev = levels(test_set$obs))

auc_0<- prSummary(test_set, lev = levels(test_set$obs))[1]

## Uncertainty sampling
### Retrieve and label top 100 most uncertain
unc <- uncertainty_sampling(dtm, defi_lab$y,
                            uncertainty = "least_confidence", num_query = 100,
                            classifier = "svmLinear", trControl = fit_control)

defi_lab$y <- factor(defi_lab$y, levels = c("Yes","No"), labels = c(1, 2))
defi_lab <- lab_unc(defi_lab, unc$query)

defi_lab %>% 
  group_by(y) %>% 
  summarise(n = n())

write.csv(defi_lab, file = "data/labbed.csv", row.names = F)

### Reshuffle and repeat for the first batch ----
### For uncertainty sampling
defi_lab$y <- factor(defi_lab$y, levels = c(1,2), labels = c("Yes", "No"))
idx <- defi_lab %>% 
  filter(!is.na(y)) %>% 
  sample_n(100)

test_x <- dtm[idx$id,]
test_y <- idx$y

### Train model on the non-NA
idx_2 <-  defi_lab %>% 
  filter(!is.na(y)) %>% 
  filter(id %!in% idx$id)

train_x2 <- dtm[idx_2$id,]
train_y2 <- idx_2$y

## Modeling and predictions
fit2 <- train(x= train_x2,
             y = train_y2,
             method = "svmLinear",
             trControl = fit_control)

prd2 <- predict(fit, newdata = test_x, type = "prob")

## Evaluating results: confusion matrix and AUC
test_set <- data.frame(obs = test_y,
                       Yes = prd2$Yes,
                       No = prd2$No)
test_set$pred <- factor(ifelse(test_set$Yes >= .5, 1, 2), labels = c("Yes", "No"))

confusionMatrix(test_set$pred, test_y)
prSummary(test_set, lev = levels(test_set$obs))

auc_1<- prSummary(test_set, lev = levels(test_set$obs))[1]

## Uncertainty sampling, second iteration
### Retrieve and label top 100 most uncertain
unc <- uncertainty_sampling(dtm, defi_lab$y,
                            uncertainty = "least_confidence", num_query = 10,
                            classifier = "svmLinear", trControl = fit_control)

defi_lab$y <- factor(defi_lab$y, levels = c("Yes","No"), labels = c(1, 2))
defi_lab <- lab_unc(defi_lab, unc$query)
defi_lab$y <- factor(defi_lab$y, levels = c(1,2), labels = c("Yes", "No"))

defi_lab %>% 
  group_by(y) %>% 
  summarise(n = n())

write.csv(defi_lab, file = "data/labbed.csv", row.names = F)

# 3rd Iteration
idx <- defi_lab %>% 
  filter(!is.na(y)) %>% 
  sample_n(100)

test_x <- dtm[idx$id,]
test_y <- idx$y

### Train model on the non-NA
idx_2 <-  defi_lab %>% 
  filter(!is.na(y)) %>% 
  filter(id %!in% idx$id)

train_x2 <- dtm[idx_2$id,]
train_y2 <- idx_2$y

## Modeling and predictions
fit2 <- train(x= train_x2,
              y = train_y2,
              method = "svmLinear",
              trControl = fit_control)

prd2 <- predict(fit, newdata = test_x, type = "prob")

## Evaluating results: confusion matrix and AUC
test_set <- data.frame(obs = test_y,
                       Yes = prd2$Yes,
                       No = prd2$No)
test_set$pred <- factor(ifelse(test_set$Yes >= .5, 1, 2), labels = c("Yes", "No"))

confusionMatrix(test_set$pred, test_y)
prSummary(test_set, lev = levels(test_set$obs))

auc_3<- prSummary(test_set, lev = levels(test_set$obs))[1]


# Retreive and organise top terms by coefficients
coefs <- coef(fit$finalModel, fit$bestTune$lambda)%>% 
  as.matrix() %>% as.data.frame() %>% 
  tibble::rownames_to_column("word") %>% 
  rename(value = `1`) %>% 
  arrange(desc(abs(value))) %>% 
  filter(value !=0)




# TODO: response to non-defi
