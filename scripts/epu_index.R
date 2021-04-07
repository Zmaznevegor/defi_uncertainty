# Load libraries and data ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(stargazer)
library(caret)
library(tidytext)
library(tm)
library(fpp3)
library(reshape2)

'%!in%' <- function(x,y)!('%in%'(x,y))

theme_set(theme_minimal())

data <- read.csv("data/all_articles.csv")

set.seed(123)

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

epu_naive <- normalize(epu_base_yw)

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
  filter(!is.na(y)) 

idx <- idx[createDataPartition(idx$y, p = 0.0823, list = F),]

train_x <- dtm[-idx$id, ]
train_y <- defi_lab %>%
  filter(id %!in% idx$id) %>% 
  .$y

test_x <- dtm[idx$id,]
test_y <- idx$y

### Train predictive model on the non-NA
idx_1 <-  defi_lab %>% 
  filter(!is.na(y)) %>% 
  filter(id %!in% idx$id)

#idx_1 <- idx_1[createDataPartition(idx_1$y, p = 0.718, list = F),]

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
prSummary(test_set, lev = levels(test_set$obs))

# Change number to the corresponding iteration
auc_7 <- prSummary(test_set, lev = levels(test_set$obs))[1]

# auc <- c(auc_0, auc_1, auc_2, auc_3, auc_4, auc_5, auc_6, auc_7)

## Uncertainty sampling, second iteration
### Retrieve and label top 100 most uncertain
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

# Saving preliminary results
write.csv(defi_lab, file = "data/labbed.csv", row.names = F)

# ITERATING: after the top 100 most uncertain rows are found
# rerun the code lines with spliiting, modeling and predicting to check the updated model
# save AUC results for the further plotting
# repeat sampling and modeling till AUC doe not improve

# Check top importance terms
top_terms <- plot(varImp(fit1), top = 20)

# Construct index based on the fit model
# All articles that are not used for training
test_x1 <- dtm[-idx_1$id,]

defi_lab$y_pred <- predict(fit1, newdata = dtm)

epu_mod_yw <- defi_lab %>% 
  filter(y_pred == "Yes") %>% 
  select(date, source, y_pred) %>% 
  mutate(yw = yearweek(date)) %>% 
  group_by(yw, source) %>% 
  summarise(n_articles = n())

ggplot(epu_mod, aes(x = yw, y = n_articles))+geom_line()

epu_mod <- normalize(epu_mod_yw)

left_join(epu_naive, epu_mod, by = "yw") %>% 
  rename("Naive" = norm.x,
         "AL Mod" = norm.y) %>% 
  melt(id = "yw") %>% 
  ggplot(aes(x = as.Date(yw)))+
  geom_line(aes(y = value, colour = variable))+
  geom_line(data = ff, aes(y=norm), colour = "black")+
  labs(x = "", y = "EPU", colour = "Method")+
  theme(legend.position = "bottom")

a <- defi_lab %>% 
  filter(y_pred == "Yes") %>% 
  filter(date < as.Date(yearweek("2020 W13")),
         date > as.Date(yearweek("2020 W11")))

idx$y_pred <- prd1
f <- idx %>% 
  mutate(y = factor(ifelse(y_pred$Yes >= .5, 1, 2), labels = c("Yes", "No"))) %>%
  select(id, date, text, source, y) %>% 
  bind_rows(idx_1) %>% 
  sample_n(800) %>% 
  filter(y == "Yes") %>% 
  select(date, source, y) %>% 
  mutate(yw = yearweek(date)) %>% 
  group_by(yw, source) %>% 
  summarise(n_articles = n())


ff <- normalize(f)

# VAR modeling ----
# Check correlation
cor.test(epu_mod$norm,epu_naive$norm)

## Import the data and plot ----
tvl <- read.csv("data/defi/tvl_data.csv") %>% 
  mutate(yw = yearweek(time)) %>% 
  select(yw, tvlusd, tvleth, token) %>% 
  filter(yw > yearweek("2017 W51"),
         yw < yearweek("2021 W12")) %>% 
  group_by(yw, token) %>% 
  slice(1)

ggplot(tvl, aes(x = as.Date(yw), y = tvleth)) +
  geom_line(aes(colour = token))+
  labs(x = "", y = "TVL (ETH)", colour = "DeFi")

## Import gas and difficulty stats ----
### Block difficulty
bldif <- read.csv("data/defi/export-BlockDifficulty.csv") %>% 
  mutate(yw = yearweek(as.Date(Date.UTC., format = "%m/%d/%Y"))) %>% 
  select(yw, Value) %>% 
  group_by(yw) %>% 
  summarise_at(vars(Value), list(avg.diff = mean)) %>% 
  filter(yw > yearweek("2017 W51"), yw < yearweek("2021 W12")) %>% ungroup()

ggplot(bldif, aes(x = as.Date(yw), y = avg.diff)) +
  geom_line()+
  labs(x = "", y = "Average Block Difficultry")

cor.test(tvl %>% filter(token == "maker") %>% .$tvlusd,bldif$avg.diff)

### Gas price
gas <- read.csv("data/defi/export-AvgGasPrice.csv") %>% 
  mutate(yw = yearweek(as.Date(Date.UTC., format = "%m/%d/%Y")),
         Value = Value..Wei.) %>% 
  select(yw, Value) %>% 
  group_by(yw) %>% 
  summarise_at(vars(Value), list(avg.gas = mean)) %>% 
  filter(yw > yearweek("2017 W51"), yw < yearweek("2021 W12")) %>% ungroup()

ggplot(gas, aes(x = as.Date(yw), y = avg.gas)) +
  geom_line()+
  labs(x = "", y = "Average Gas Price")

cor.test(tvl %>% filter(token == "maker") %>% .$tvlusd, gas$avg.gas)

### Verified contracts
contracts <- read.csv("data/defi/export-verified-contracts.csv") %>% 
  mutate(yw = yearweek(as.Date(Date.UTC.)),
         Value = No..of.Verified.Contracts) %>% 
  select(yw, Value) %>% 
  group_by(yw) %>% 
  summarise_at(vars(Value), list(avg.verf = mean)) %>% 
  filter(yw > yearweek("2017 W51"), yw < yearweek("2021 W12")) %>% ungroup()

ggplot(contracts, aes(x = as.Date(yw), y = avg.verf)) +
  geom_line()+
  labs(x = "", y = "Average Contracts Verified")

cor.test(tvl %>% filter(token == "maker") %>% .$tvlusd, contracts$avg.verf)


A = as.matrix(data.frame(c(3,4,3),c(4,8,6),c(3,6,9)))
chol(A)


# TODO: check with tvlusd

inp <- epu_mod %>% 
  left_join(tvl %>% filter(token == "maker"), by = "yw") %>% 
  left_join(bldif, by = "yw") %>% 
  left_join(gas, by = "yw") %>% 
  left_join(contracts, by ="yw") %>% 
  select(yw, norm, avg.verf, avg.diff, avg.gas, tvleth, tvlusd) %>% 
  as_tsibble(index = yw)

fit <- inp %>% 
  model(org = ARIMA(log(tvleth)),
        unc = ARIMA(log(tvleth)~norm))


        
