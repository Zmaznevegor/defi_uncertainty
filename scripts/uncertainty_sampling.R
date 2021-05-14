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


# EPU Normalisation ----
normalize <- function(x){
  data_yw <- data %>% 
    mutate(yw = yearweek(date)) %>% 
    group_by(yw, source) %>% 
    summarise(n_articles = n()) %>% 
    as_tsibble(index = yw, key = source)
  
  scale <- left_join(data_yw, x, by = c("yw", "source")) %>% 
    rename(n_articles = n_articles.x,
           epu_articles = n_articles.y) %>% 
    replace_na(list(epu_articles = 0)) %>% 
    filter(yw > yearweek("2017 W30"),
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
  
  return(epu %>% dplyr::select(yw, norm))
}

normalize_daily <- function(x){
  data_daily <- data %>% 
    group_by(date, source) %>% 
    summarise(n_articles = n())
  
  scale <- left_join(data_daily, x, by = c("date", "source")) %>% 
    rename(n_articles = n_articles.x,
           epu_articles = n_articles.y) %>% 
    replace_na(list(epu_articles = 0)) %>% 
    filter(date > as.Date(yearweek("2017 W30")),
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
  
  return(epu %>% dplyr::select(date, norm))
}

normalize_monthly <- function(x){
  data_ym <- data %>% 
    mutate(ym = yearmonth(date)) %>% 
    group_by(ym, source) %>% 
    summarise(n_articles = n()) %>% 
    as_tsibble(index = ym, key = source)
  
  scale <- left_join(data_ym, x, by = c("ym", "source")) %>% 
    rename(n_articles = n_articles.x,
           epu_articles = n_articles.y) %>% 
    replace_na(list(epu_articles = 0)) %>% 
    filter(ym >= yearmonth(yearweek("2017 W30")),
           ym <= yearmonth(yearweek("2021 W12")))
  
  wk <- scale %>% 
    group_by(ym) %>% 
    summarise(articles = sum(n_articles),
              epu_a = sum(epu_articles))
  
  t1 <- wk$ym[1:round(0.8*length(wk$ym))]
  t2 <- wk$ym[(length(t1)+1):length(wk$ym)]
  
  base <- scale %>% 
    mutate(scaled = epu_articles/n_articles)
  sd1 <- with(base, sd(scaled[scale$ym %in% t1]))
  
  m <- base %>% mutate(stnd = scaled/sd1) %>% 
    group_by(ym) %>% 
    summarise(m = mean(stnd)) %>% 
    filter(ym %in% t2)
  
  epu <- base %>% 
    mutate(stnd = scaled/sd1) %>% 
    group_by(ym) %>% 
    summarise(stnd1 = mean(stnd)) %>% 
    mutate(norm = stnd1/mean(m$m)*100)
  
  return(epu %>% dplyr::select(ym, norm))
}

# VAR pre-processing
mtr_cat <- function(x){
  inp <- epu %>% 
    mutate(yw = yearweek(yw)) %>% 
    left_join(tvl_cat %>% filter(category == x), by = "yw") %>% 
    left_join(gas, by = "yw") %>% 
    left_join(contracts, by ="yw") %>% 
    dplyr::select(yw, naive, mod, sum.verf, avg.gas, tvleth, tvlusd) %>% 
    as_tsibble(index = yw) %>% 
    drop_na()
  
  # Transform to the readable for the VAR format
  inp_ts <- ts(inp[,2:7], frequency=52,
               start= c(year(inp$yw[1]),week(inp$yw[1])),
               end = c(year(inp$yw[length(inp$yw)]),week(inp$yw[length(inp$yw)])))
  
  mtr <- cbind(epu = inp_ts[,"mod"],
               contracts = log(inp_ts[,"sum.verf"]),
               gas = log(inp_ts[,"avg.gas"]),
               tvleth = log(inp_ts[,"tvleth"]),
               tvlusd = log(inp_ts[,"tvlusd"]))
  return(mtr)
}

mtr_cat_daily <- function(x){
  inp_daily <- epu_daily %>% 
    left_join(tvl_cat_daily %>% filter(category == x), by = "date") %>% 
    left_join(gas_daily, by = "date") %>% 
    left_join(contracts_daily, by ="date") %>% 
    dplyr::select(date, naive, mod, sum.verf, avg.gas, tvleth, tvlusd) %>% 
    as_tsibble(index = date) %>% 
    drop_na()
  
  inp_ts_daily <- ts(inp_daily[,2:7], frequency=365,
                     start= c(year(inp_daily$date[1]), yday(inp_daily$date[1])),
                     end = c(year(inp_daily$date[length(inp_daily$date)]), yday(inp_daily$date[length(inp_daily$date)])))
  
  mtr <- cbind(epu = inp_ts_daily[,"mod"],
               contracts = log(inp_ts_daily[,"sum.verf"]),
               gas = log(inp_ts_daily[,"avg.gas"]),
               tvleth = log(inp_ts_daily[,"tvleth"]),
               tvlusd = log(inp_ts_daily[,"tvlusd"]))
  
  return(mtr)
}

# IRF Plotting ----
irf_usd <- function(irf, custom_title = "Effect of uncertainty impulse on TVL (USD)", custom_subtitle = "Modified DeFi EPU index"){
  h <- irf[["irf"]][["epu"]] %>% as.data.frame() %>% nrow()
  
  irf[["irf"]][["epu"]] %>% as.data.frame() %>% 
    ggplot(aes(y = tvlusd, x = c(1:h)))+
    geom_line(linetype = 2, colour = "black", alpha = 1, size = 0.75)+
    geom_line(data = irf[["Lower"]][["epu"]] %>% as.data.frame(), aes(y = tvlusd, x =c(1:h)), linetype = 2, colour = "red", alpha = 0.8, size = 0.4)+
    geom_line(data = irf[["Upper"]][["epu"]] %>% as.data.frame(), aes(y = tvlusd, x =c(1:h)), linetype = 2, colour = "red", alpha = 0.8, size = 0.4)+
    geom_hline(yintercept = 0, linetype = "dotted", colour = "black", alpha = 0.5)+
    coord_cartesian(ylim = c(min(irf[["Lower"]][["epu"]])-0.05,
                             max(irf[["Upper"]][["epu"]])+0.05),
                    xlim = c(1,h))+
    scale_y_continuous(breaks = seq(round(min(irf[["Lower"]][["epu"]])-0.05, 1), 
                                    round(max(irf[["Upper"]][["epu"]])+0.05, 1),
                                    by = 0.05))+
    scale_x_continuous(breaks = seq(1, h-1, by = 2))+
    labs(subtitle = custom_subtitle, title = custom_title,
         x = "Lag", y = "") +
    theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, vjust = 0.5))
}

irf_eth <- function(irf, custom_title = "Effect of uncertainty impulse on TVL (ETH)", custom_subtitle = "Modified DeFi EPU index"){
  h <- irf[["irf"]][["epu"]] %>% as.data.frame() %>% nrow()
  
  irf[["irf"]][["epu"]] %>% as.data.frame() %>% 
    ggplot(aes(y = tvleth, x = c(1:h)))+
    geom_line(linetype = 2, colour = "black", alpha = 1, size = 0.75)+
    geom_line(data = irf[["Lower"]][["epu"]] %>% as.data.frame(), aes(y = tvleth, x =c(1:h)), linetype = 2, colour = "red", alpha = 0.8, size = 0.4)+
    geom_line(data = irf[["Upper"]][["epu"]] %>% as.data.frame(), aes(y = tvleth, x =c(1:h)), linetype = 2, colour = "red", alpha = 0.8, size = 0.4)+
    geom_hline(yintercept = 0, linetype = "dotted", colour = "black", alpha = 0.5)+
    coord_cartesian(ylim = c(min(irf[["Lower"]][["epu"]])-0.05,
                             max(irf[["Upper"]][["epu"]])+0.05),
                    xlim = c(1,h))+
    scale_y_continuous(breaks = seq(round(min(irf[["Lower"]][["epu"]])-0.05, 1), 
                                    round(max(irf[["Upper"]][["epu"]])+0.05, 1),
                                    by = 0.05))+
    scale_x_continuous(breaks = seq(1, h-1, by = 2))+
    labs(subtitle = custom_subtitle, title = custom_title,
         x = "Lag", y = "") +
    theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, vjust = 0.5))
}
