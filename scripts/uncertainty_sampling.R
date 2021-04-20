## Uncertainty sampling ----
uncertainty_sampling <- function(x, y, uncertainty="entropy", classifier,
                                 num_query=1, ...) {
  
  # Validates the classifier string.
  validate_classifier <- function(classifier, posterior_prob = FALSE) {
    # Tests that the specified classifier is given in 'caret', is actually a
    # classifier, and provides posterior probabilities of class membership.
    if (missing(classifier) || is.null(classifier) || is.na(classifier)) {
      stop("A classifier must be specified")
    }
    caret_lookup <- try(modelLookup(classifier), silent = TRUE)
    if (inherits(caret_lookup, "try-error")) {
      stop("Cannot find, '", classifier, "' in the 'caret' package")
    } else if (!any(caret_lookup$forClass)) {
      stop("The method, '", classifier, "' must be a classifier")
    }
    
    if (posterior_prob && !any(caret_lookup$probModel)) {
      stop("The method, '", classifier, "' must return posterior probabilities")
    }
    
    invisible(TRUE)
  }
  
  # Execute function
  validate_classifier(classifier, posterior_prob=TRUE)
  print("Classifier is valid")
  
  x <- as.matrix(x)
  y <- factor(y)
  
  # Splits a matrix and its class labels into labeled and unlabeled pairs.
  which_unlabeled <- function(y) {
    which(is.na(y))
  }
  
  split_labeled <- function(x, y) {
    x <- as.matrix(x)
    y <- factor(y)
    
    unlabeled_i <- which_unlabeled(y)
    list(x_labeled=x[-unlabeled_i, ],
         y_labeled=y[-unlabeled_i],
         x_unlabeled=x[unlabeled_i, ],
         y_unlabeled=y[unlabeled_i])
  }
  
  # Execute function
  split_out <- split_labeled(x, y)
  
  train_out <- train(x=split_out$x_labeled, y=split_out$y_labeled,
                     method=classifier, ...)
  
  # Extracts the class posterior probabilities for the unlabeled observations.
  posterior <- predict(train_out, newdata=split_out$x_unlabeled, type = "prob")
  posterior <- as.matrix(posterior)
  
  # Computes the specified uncertainty for each of the unlabeled observations
  # based on the posterior probabilities of class membership.
  
  # Adding entropy plugin
  entropy.plugin = function(freqs, unit=c("log", "log2", "log10"))
  {
    unit = match.arg(unit)
    
    freqs = freqs/sum(freqs) # just to make sure ...
    
    H = -sum( ifelse(freqs > 0, freqs*log(freqs), 0) )
    
    if (unit == "log2")  H = H/log(2)  # change from log to log2 scale
    if (unit == "log10") H = H/log(10) # change from log to log10 scale
    
    return(H)
  }
  
  # Possible options
  entropy_uncertainty <- function(posterior) {
    apply(posterior, 1, entropy.plugin)
  }
  
  least_confidence <- function(posterior) {
    apply(posterior, 1, max)
  }
  
  margin_uncertainty <- function(posterior) {
    apply(posterior, 1, function(post_i) {
      post_i[order(post_i, decreasing=T)[1:2]] %*% c(1, -1)
    })
  }
  
  # Execution
  obs_uncertainty <- switch(uncertainty,
                            entropy=entropy_uncertainty(posterior),
                            least_confidence=least_confidence(posterior),
                            margin=margin_uncertainty(posterior)
  )
  
  # Determines the order of the unlabeled observations by uncertainty measure.
  query <- order(obs_uncertainty, decreasing=T)[seq_len(num_query)]
  
  list(query=query, posterior=posterior, uncertainty=obs_uncertainty)
}

query_oracle <- function(i, y_truth) {
  as.vector(y_truth[i])
}

lab <- function(x, n){
  # Start the count
  x_na <- x %>% 
    filter(is.na(y)) %>%
    .$id
  idx <- head(sample(x_na), n)
  p <- 1
  
  # Start loop with response variable updates
  for (i in idx) {
    print(paste(p, "/", n))
    message((paste("Article from", x$source[i])))
    cat(paste(x$text[i], sep ="\n"), "\n", "\n")
    x$y[i] <- menu(c("Yes", # equals 1, otherwise 0
                     "No"), title="Is it related to DeFi EPU?")
    p <- p+1
  }
  return(x)
}

'%!in%' <- function(x,y)!('%in%'(x,y))

lab_unc <- function(x, n){
  p <- 1
  # Start loop with response variable updates
  for (i in n) {
    print(paste(p, "/", length(n)))
    message((paste("Article from", x$source[i])))
    cat(paste(x$text[i], sep ="\n"), "\n", "\n")
    x$y[i] <- menu(c("Yes", # equals 1, otherwise 0
                     "No"), title="Is it related to DeFi EPU?")
    p <- p+1
  }
  return(x)
}

normalize <- function(x){
  data_yw <- data %>% 
    mutate(yw = yearweek(date)) %>% 
    group_by(yw, source) %>% 
    summarise(n_articles = n())
  
  scale <- left_join(data_yw, x, by = c("yw", "source")) %>% 
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
  
  return(epu %>% select(yw, norm))
}

normalize_daily <- function(x){
  data_daily <- data %>% 
    group_by(date, source) %>% 
    summarise(n_articles = n())
  
  scale <- left_join(data_daily, x, by = c("date", "source")) %>% 
    rename(n_articles = n_articles.x,
           epu_articles = n_articles.y) %>% 
    replace_na(list(epu_articles = 0)) %>% 
    filter(date > as.Date(yearweek("2017 W51")),
           date < as.Date(yearweek("2021 W12")))
  
  wk <- scale %>% 
    group_by(date) %>% 
    summarise(articles = sum(n_articles),
              epu_a = sum(epu_articles))
  
  t1 <- wk$date[1:round(0.8*length(wk$date))]
  t2 <- wk$date[(length(t1)+1):length(wk$date)]
  
  base <- scale %>% 
    mutate(scaled = epu_articles/n_articles)
  sd1 <- with(base, sd(scaled[scale$date %in% t1]))
  
  m <- base %>% mutate(stnd = scaled/sd1) %>% 
    group_by(date) %>% 
    summarise(m = mean(stnd)) %>% 
    filter(date %in% t2)
  
  epu <- base %>% 
    mutate(stnd = scaled/sd1) %>% 
    group_by(date) %>% 
    summarise(stnd1 = mean(stnd)) %>% 
    mutate(norm = stnd1/mean(m$m)*100)
  
  return(epu %>% select(date, norm))
}
