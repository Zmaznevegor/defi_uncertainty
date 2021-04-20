# Load libraries and data ----
# Data wrangling and cleaning
library(dplyr)
library(tidyr)
library(reshape2)

# Data representation and visuals
library(ggplot2)
library(ggrepel)
library(stargazer)

# Work with text
library(tidytext)
library(tm)

# Work with time series and ML
library(caret)
library(fpp3)

# Custom functions for AL
source(file = "scripts/uncertainty_sampling.R")

# Theming for graphs
theme_set(theme_minimal())

# Cleaned data load
data <- read.csv("data/all_articles.csv")

# Seed for reproducability
set.seed(123)

# EPU Construction ----
## Naive base method ----
# Choosing relevant terms for DeFi
defi_words = " defi | decentrali(z|s)ed finance"
defi_service_words = " decentralized borrow| decentralized lend| stablecoin| decentrali(s|z)ed exchange| DEX | yield farm| DEXes | decentralized oracle"
defi <-  grep(defi_words, data$text, ignore.case = T)
defi_service <-  grep(defi_service_words, data$text, ignore.case = T)
all_defi <- union(defi,defi_service)
defi <- data[all_defi,]

# Terms related to regulations (P)
defi_reg <-  grep(" regulat|\bCongress\b| supreme court| government| European Commission|legislat|\bESMA\b|\bECB\b|\bFCA\b|\bEBA\b|\bjurisdiction\b|\bSEC\b|\bcompliance\b|\bFATF\b|\bFSB\b| central bank|Financial Stability Board|Financial Action Task Force|Financial conduct authority", defi$text, ignore.case = T)
defi_reg <- defi[defi_reg,]

# Basic terms related to uncertainty (U)
defi_unc <-  grep("uncertain", defi_reg$text, ignore.case = T)
pu_base <- defi_reg[defi_unc,]

# Terms related to economics (E)
econ <- grep(" econom", pu_base$text, ignore.case = T)
epu_base <- pu_base[econ,]

### Normalization and standartisation ----
# Convert to weeks
epu_base_yw <- epu_base %>%
  mutate(yw = yearweek(date)) %>% 
  group_by(yw, source) %>% 
  summarise(n_articles = n())

ggplot(epu_base_yw, aes(x = yw, y = n_articles))+geom_line()

epu_naive <- normalize(epu_base_yw)

### Plotting naive index ----
# Annotations
label <- data.frame(
  yw = c(as.Date("2020-03-28"), as.Date("2019-07-18"), as.Date("2020-11-07"), 
         as.Date("2018-02-10"), as.Date("2021-03-10"), as.Date("2019-08-31"), 
         as.Date("2020-06-05"), as.Date("2020-01-10")), 
  norm = c(615, 465, 345, 435, 205, 285, 275, 405), 
  label = c(paste("COVID-19", "Phase I", sep = "\n"),
            paste("Libra hearing", "in Senate", sep = "\n"),
            paste("US Presidential", "Election", sep = "\n"),
            paste("Crypto regulation", "in South Korea", sep = "\n"),
            paste("US Stimulus", "Package", sep = "\n"),
            paste("PBoC", "CBDC", sep = "\n"),
            paste("US", "Protests", sep = "\n"),
            paste("Digital", "Dollar","Initiative", sep = "\n")))

ggplot(epu_naive, aes(x = as.Date(yw), y = norm))+
  ggplot2::annotate(geom = "rect", xmin=as.Date("2020-03-01"), xmax=as.Date("2020-05-05"), ymin=0, ymax=Inf, fill = "lightgreen", alpha = 0.3)+
  geom_line(colour = "darkblue", alpha = 0.9)+
  geom_text(data = label, aes(label = label), size = 3.5)+
  labs(#title = "Index of Economic Policy Uncertainty for DeFi",
    y = " Economic Policy Uncertainty",
    x = "") 
#+ theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 15, face = "bold"), plot.subtitle = element_text(hjust = 0.5, vjust = 0.5))

# Use the following to check the articles within a certain range
a <- epu_base %>% 
  mutate(yw = yearweek(date)) %>% 
  filter(yw == yearweek("2020 W53"))

a <- defi_lab %>% 
  mutate(yw = yearweek(date)) %>% 
  filter(yw == yearweek("2020 W01"),
         y_pred == "Yes")

## Active learner approach ----
### Pre-processing ----
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

# For reprocutoin
# defi_lab <- read.csv("data/labbed.csv")

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
# For uncertainty sampling
idx <- defi_lab %>% 
  filter(!is.na(y)) 

idx <- idx[createDataPartition(idx$y, p = 0.0823, list = F),]

train_x <- dtm[-idx$id, ]
train_y <- defi_lab %>%
  filter(id %!in% idx$id) %>% 
  .$y

test_x <- dtm[idx$id,]
test_y <- idx$y

### Train predictive model on the non-NA ----
idx_1 <-  defi_lab %>% 
  filter(!is.na(y)) %>% 
  filter(id %!in% idx$id)

#idx_1 <- idx_1[createDataPartition(idx_1$y, p = 0.718, list = F),]

train_x1 <- dtm[idx_1$id,]
train_y1 <- idx_1$y
  
### Modeling and predictions ----
fit_control <- trainControl(method = "cv", number = 10, classProbs = TRUE)

fit1 <- train(x= train_x1,
             y = train_y1,
             method = "svmLinear",
             trControl = fit_control)

prd1 <- predict(fit1, newdata = test_x, type = "prob")

### Evaluating results: confusion matrix and AUC ----
test_set <- data.frame(obs = test_y,
                       Yes = prd1$Yes,
                       No = prd1$No)
test_set$pred <- factor(ifelse(test_set$Yes >= .5, 1, 2), labels = c("Yes", "No"))

confusionMatrix(test_set$pred, test_y)
prSummary(test_set, lev = levels(test_set$obs))

# Change number to the corresponding iteration of uncertainty sampling
auc_7 <- prSummary(test_set, lev = levels(test_set$obs))[1]
# auc <- c(auc_0, auc_1, auc_2, auc_3, auc_4, auc_5, auc_6, auc_7)

### Uncertainty sampling, second iteration ----
# Retrieve and label top 100 most uncertain
unc <- uncertainty_sampling(train_x, train_y,
                            uncertainty = "least_confidence", num_query = 100,
                            classifier = "svmLinear", trControl = fit_control)

# Finding corresponding IDs
raw <- defi_lab %>% 
  anti_join(idx)

# Fixing factor for uncertainty labeling
defi_lab$y <- factor(defi_lab$y, levels = c("Yes","No"), labels = c(1, 2))
defi_lab <- lab_unc(defi_lab, raw$id[unc$query])

# Fixing factoring back
defi_lab$y <- factor(defi_lab$y, levels = c(1,2), labels = c("Yes", "No"))

# Checking data
defi_lab %>% 
  group_by(y) %>% 
  summarise(n = n())

auc_plot <- ggplot(as.data.frame(auc), aes(y = auc, x = c(0:7)))+
  geom_line(linetype = 1, colour = "black", alpha = 1, size = 0.75)+
  #geom_hline(yintercept = 0, linetype = "dotted", colour = "black", alpha = 0.5)+
  coord_cartesian(ylim = c(0.4, 1))+
  scale_y_continuous(breaks = seq(0.4, 1, by = 0.2))+
  scale_x_continuous(breaks = seq(0, 7, by = 2))+
  labs(#title = "Evolution of AUC throughout AL process", subtitle = "AUC on the test set were collected after each iteration",
       x = "Iteration", y = "AUC") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 10, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0.5))

# Saving preliminary results
write.csv(defi_lab, file = "data/labbed.csv", row.names = F)

# ITERATING: after the top 100 most uncertain rows are found
# rerun the code lines with spliiting, modeling and predicting to check the updated model
# save AUC results for the further plotting
# repeat sampling and modeling till AUC doe not improve

# Check top importance terms
top_terms <- varImp(fit1)[["importance"]]

top <- top_terms %>% 
  rename(Importance = "Yes") %>% 
  select(Importance) %>% 
  tibble::rownames_to_column("Word") %>% 
  arrange(desc(Importance)) %>% 
  head(20) 
top <- cbind(top[1:10,], top[11:20,])
colnames(top) <- c("Word", "Importance", "Word ", "Importance ")
stargazer(top, summary = F, style = "qje", header = F, report = "vc*", type = "html",
            single.row = F, no.space = T, digits = 2)


# By weights
m <- fit1$finalModel@xmatrix[[1]]
n <- fit1$finalModel@coef[[1]]

n%*%m %>% t() %>% 
  as.data.frame() %>% 
  rename(weight = V1) %>% 
  arrange((weight)) %>% 
  head(20)

### Construct index based on the fit model ----
# All articles that are not used for training
test_x1 <- dtm[-idx_1$id,]

defi_lab$y_pred <- predict(fit1, newdata = dtm)

epu_mod_yw <- defi_lab %>% 
  filter(y_pred == "Yes") %>% 
  select(date, source, y_pred) %>% 
  mutate(yw = yearweek(date)) %>% 
  group_by(yw, source) %>% 
  summarise(n_articles = n())

ggplot(epu_mod_yw, aes(x = yw, y = n_articles)) +
  geom_line()

epu_mod <- normalize(epu_mod_yw)

### Plotting naive index ----
# Annotations
label <- data.frame(
  yw = c(as.Date(yearweek("2018 W12")),#A
         as.Date(yearweek("2018 W37")),#B
         as.Date("2019-06-29"),#C
         as.Date("2019-07-18"),#D
         as.Date(yearweek("2019 W38")),#E
         as.Date("2019-10-16"),#F
         as.Date(yearweek("2019 W45")),#G
         as.Date(yearweek("2019 W51")),#H
         as.Date(yearweek("2020 W04")),#I
         as.Date("2020-02-21"),#J
         as.Date(yearweek("2020 W16")),#K
         as.Date(yearweek("2020 W22")),#L
         as.Date(yearweek("2020 W38")),#M
         as.Date(yearweek("2020 W42")),#N
         as.Date(yearweek("2021 W08"))), 
  norm = c(435, #A
           200, #B
           305, #C
           340, #D
           295, #E
           485, #F
           290, #G
           320, #H
           350, #I
           425, #J
           790, #K
           210, #L
           235, #M
           295,#N
           325), #O
  label = c(paste("A", sep = "\n"),
            paste("B", sep = "\n"),
            paste("C", sep = "\n"),
            paste("D", sep = "\n"),
            paste("E", sep = "\n"),
            paste("F", sep = "\n"),
            paste("G", sep = "\n"),
            paste("H", sep = "\n"),
            paste("I", sep = "\n"),
            paste("J", sep = "\n"),
            paste("K", sep = "\n"),
            paste("L", sep = "\n"),
            paste("M", sep = "\n"),
            paste("N", sep = "\n"),
            paste("O", sep = "\n")))

ggplot(epu_mod, aes(x = as.Date(yw), y = norm))+
  ggplot2::annotate(geom = "rect", xmin=as.Date("2020-03-01"), xmax=as.Date("2020-05-05"), ymin=0, ymax=Inf, fill = "lightgreen", alpha = 0.3)+
  geom_line(colour = "darkred", alpha = 0.9)+
  geom_text(data = label, aes(label = label), size = 4)+
  labs(#title = "Index of Economic Policy Uncertainty for DeFi",
    y = " Economic Policy Uncertainty",
    x = "") 
#+ theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 15, face = "bold"), plot.subtitle = element_text(hjust = 0.5, vjust = 0.5))

# Use the following to check the articles within a certain range
a <- defi_lab %>% 
  mutate(yw = yearweek(date)) %>% 
  filter(yw == yearweek("2021 W08"),
         y_pred == "Yes")

## Combine and save the results ----
left_join(epu_naive, epu_mod, by = "yw") %>% 
  rename("Naive" = norm.x,
         "AL Mod" = norm.y) %>% 
  melt(id = "yw") %>% 
  ggplot(aes(x = as.Date(yw)))+
  geom_line(aes(y = value, colour = variable), alpha=0.7)+
  labs(x = "", y = "EPU", colour = "Method")+
  theme(legend.position = "top")+ 
  scale_color_manual(values=c("darkblue", "darkred"))

idx$y_pred <- prd1

write.csv(left_join(epu_naive, epu_mod, by = "yw") %>% 
            rename("Naive" = norm.x,
                   "AL Mod" = norm.y), file = "data/epu_results.csv", row.names = F)

# EPU index with the daily frequency ----
daily_naive <- epu_base %>% 
  group_by(date, source) %>% 
  summarise(n_articles = n()) %>% 
  ungroup() %>% 
  normalize_daily()

daily_mod <- defi_lab %>% 
  filter(y_pred == "Yes") %>% 
  select(date, source, y_pred) %>% 
  group_by(date, source) %>% 
  summarise(n_articles = n()) %>% 
  ungroup() %>% normalize_daily()

ggplot(daily_mod, aes(x = as.Date(date), y = norm))+
  geom_line(colour = "darkblue", alpha = 0.9)+
  labs(#title = "Index of Economic Policy Uncertainty for DeFi",
    y = " Economic Policy Uncertainty",
    x = "") 
