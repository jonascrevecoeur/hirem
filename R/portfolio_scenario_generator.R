#' Simulate portfolios along various scenarios using the simulation engine
#'
#' Simulate portfolios along different scenarios using a simulation engine. Four scenarios are currently implemented: a baseline, change in claim mix, extreme event or change in settlement scenario.
#'
#' @param seed Integer. Set the seed for the random number generators.
#' @param n Integer. The number of claims (occurrence dates) to generate.
#' @param prob.Type Optional. Numeric vector of size 3 representing the probability distribution of the claim type.
#' @param prob.Hidden Optional. Numeric vector of length 3 representing the probability distribution of the unobserved covariate `hidden`.
#' @param max_rep_delay Optional. The assumed maximal reporting delay in years.
#' @param max_set_delay Optional. The assumed maximal settlement delay in years.
#' @param inflation Optional. Numeric vector of size 3 representing the yearly change in the probability distribution of the claim types in the scenario describing the change in claim mix.
#' @param period_extra Optional character. Year in which the extreme event takes place. The extreme event lasts for one month and occurs somewhere randomly within the chosen year.
#' @param settlement_change Optional character. Year from when a change in settlement delay takes place.
#'
#' @return A portfolio generated along the chosen scenario.
#'
#' @importFrom lubridate days year month
#'
#' @export

simulate_scenario_baseline <- function(seed, n = 125000, prob.Type = c(0.60,0.25,0.15), prob.Hidden = c(0.35,0.45,0.20),
                                       max_rep_delay = 2, max_set_delay = 20){

  # Set seed
  set.seed(seed)

  ## Occurence date simulation -> Every possible date in between 2010-2020
  date_occ_seq <- seq(as.Date("2010/1/1"), as.Date("2020/12/31"), "day")
  date_occ     <- sample(date_occ_seq, size = n, replace = TRUE)

  # Add to dataframe
  base_year <- year(date_occ_seq[1]) + max_rep_delay
  df <- data.frame('occ.date' = as.character(date_occ), 'occ.year' = year(date_occ),
                   'occ.month' = factor(month(date_occ), levels = 1:12))
  df <- df %>% mutate('occ.year.fact' = factor(occ.year, levels = sort(unique(occ.year))),
                      .after = 'occ.year')

  ## 1 discrete + 1 hidden covariate
  # Simulation of the (un)known covariates
  x1 <- sample(c("T1","T2","T3"), size = n, replace = TRUE,  prob = prob.Type)
  xh <- sample(c("L","M","H"), size = n, replace = TRUE, prob = prob.Hidden)
  x1 <- factor(x1, levels = c("T1",'T2',"T3"))
  xh <- factor(xh, levels = c("L","M","H"))
  df <- data.frame('claim.nr' = 1:n, 'type' = x1, 'hidden' = xh, df)
  row.names(df) <- paste('claim',1:n)

  ## Reporting delay simulation - in days (max two years)
  ndays        <- 365.25*max_rep_delay
  delay_report <- floor(ndays * rbeta(n, c(1,2,3)[as.numeric(x1)], 10))
  df <- df %>% dplyr::mutate('rep.delay' = as.numeric(delay_report)/365.25)

  ## Reporting date
  date_report <- date_occ + delay_report
  df <- df %>% dplyr::mutate('rep.date' = as.character(date_report)) %>%
    dplyr::mutate("rep.year" = year(date_report) - base_year + 1,
                  'rep.year.fact' = factor(rep.year, levels = sort(unique(rep.year))),
                  "rep.month" = factor(month(date_report), levels = 1:12))

  ## Payment delay
  n.payments <- 30
  lijst <- lapply(1:n, function(x)
    as.numeric(date_report[x]) +
      floor(cumsum(c(rexp(1, rate = c(6,5,4)[as.numeric(x1[x])]),
                     rexp(n.payments - 1, rate = c(2,1.5,1)[as.numeric(x1[x])])))*365.25))
  df_pay <- as.data.frame(data.table::transpose(lijst))
  colnames(df_pay) <- paste0("Pay",1:n.payments)

  ## Add Reporting date to payment delay matrix
  df_pay_copy <- df_pay
  df_pay_copy$Report <- as.numeric(date_report)
  for(j in 1:30){
    df_pay_copy[,j] <- (df_pay_copy[,j] - df_pay_copy$Report)/365.25
  }
  df_pay_copy <- as.data.frame(t(df_pay_copy %>% dplyr::select(-c(Report))))
  df_pay      <- as.data.frame(t(df_pay))

  # Payment sizes
  delay.rep <- rep((date_report - as.Date('2010-01-01'))/365.25, each = n.payments)
  payments <- c(as.matrix(df_pay_copy))
  meanlog <- function(x1, x2){
    base <- log(c(100,200,400)[as.numeric(x1)])
    ext  <- 0.1*(payments)^(c(1.50,1.25,1.40)[as.numeric(x2)])
    rep(base + ext, times = n.payments)
  }

  df_size <- matrix(rlnorm(length(payments),
                           meanlog = meanlog(x1 = x1, x2 = xh),
                           sdlog =  1), nrow = n, ncol = 30, byrow = T)
  df_size <- as.data.frame(t(df_size))
  rownames(df_size) <- paste0("Size", 1:n.payments)

  ## Settlement delay
  ndays <- 365.25*max_set_delay
  settlement_delay <- floor(rbeta(n, shape1 = 1, shape2 = 8*c(1,0.75,0.5)[as.numeric(x1)])*ndays)

  date_settlement <- date_report + settlement_delay
  df <- df %>% dplyr::mutate("settlement.date" = as.character(date_settlement)) %>%
    dplyr::mutate("settlement.year" = year(date_settlement) - base_year + 1)

  # Remove claims that occur after settlement date in payment dates and payment sizes
  max_delays <- as.numeric(date_settlement)
  df_pay  <- t(df_pay)
  df_size <- t(df_size)
  ind <- which(df_pay > max_delays, arr.ind = TRUE)
  df_pay[ind] = df_size[ind] <- NA
  df_pay  <- t(df_pay)
  df_size <- t(df_size)

  # Break dates to calculate payments in each observation year
  breaks <- as.numeric(as.Date(paste0(2009:2040,"-12-31")))
  names(breaks) <- 2009:2040

  # Calculate grouping of observation years - payments after observation window are put at NaN and excluded
  calc_obs_year <- function(k){
    pay_dates <- na.omit(as.numeric(df_pay[,k]))
    rep.year  <- df$rep.year[k] + base_year - 1
    breaks.k  <- breaks[as.character((rep.year - 1):(rep.year + 8))]
    vec0      <- (diff(rank(c(breaks.k+0.0001, pay_dates)))-1)[1:(length(breaks.k)-1)]
    vec1      <- as.numeric(rep(names(vec0), times = vec0)) - rep.year + 1
    vec       <- c(vec1, rep(NA, length(pay_dates) - length(vec1)))
    vec
  }
  obs_years <- lapply(1:n, function(k) calc_obs_year(k))

  # Aggregrate claim sizes belonging to same observation year
  agg_fu <- function(k){
    obsy  <- obs_years[[k]]
    vec   <- if(length(obsy) > 0) sapply(na.omit(df_size[,k]) %>% split(obsy), FUN = sum) else NULL
    vec.add <- rep(0, 9 - length(vec))
    names(vec.add) <- as.character(1:9)[! as.character(1:9) %in% names(vec)]
    vec <- c(vec,vec.add)
    vec[order(names(vec))]
  }

  df.size <- t(sapply(1:n, function(k) agg_fu(k)))
  colnames(df.size) <- paste0('size_obs', 1:9)

  # Is open indicator
  create_open <- function(ones){
    ones.vec <- rep(0, 9)
    ones.vec[1:(min(ones+1,9))] <- 1
    ones.vec
  }
  df.open <- t(sapply(df$settlement.year - df$rep.year, create_open))
  colnames(df.open) <- paste0("open_obs", 1:9)

  # Settlement indicator (close)
  create_settlement <- function(zeros){
    zeros.vec <- rep(1,9)
    zeros.vec[0:min(zeros,9)] <- 0
    zeros.vec
  }
  df.settlement <- t(sapply(df$settlement.year - df$rep.year, create_settlement))
  colnames(df.settlement) <- paste0("settlement_obs", 1:9)

  # Add to dataset
  df <- df %>% bind_cols(as_tibble(df.open)) %>% bind_cols(as_tibble(df.settlement)) %>%
    bind_cols(as_tibble(df.size))

  # Remove claims with reporting year later than 2028
  df <- df %>% filter(rep.year <= 9, rep.year >= 1)
  df <- df %>% mutate(rep.year.fact = factor(rep.year, levels = 1:9))

  # Put in long format
  df.long <- df %>% pivot_longer(cols = starts_with(c('open_obs', 'settlement_obs', 'size_obs')),
                                 names_to = c(".value", "obs"),
                                 names_sep = "_")

  # Add observation year variable
  df.long <- df.long %>%
    dplyr::mutate('dev.year.fact' =  recode_factor(obs, obs1 = 1, obs2 = 2, obs3 = 3, obs4 = 4, obs5 = 5, obs6 = 6,
                                                   obs7 = 7, obs8 = 8, obs9 = 9), .before = 'open') %>%
    dplyr::mutate('dev.year' = as.numeric(as.character(dev.year.fact)), .before = 'dev.year.fact') %>%
    dplyr::select(-c(obs))

  # Add calendar year and payment variable
  df.long <- df.long %>% dplyr::mutate('calendar.year' = rep.year + dev.year - 1, .after = 'dev.year.fact') %>%
    dplyr::mutate('payment' = (size > 0)*1, .before = 'size')

  df.long

}

#' @rdname simulate_scenario_baseline
#' @export
simulate_scenario_claim_mix <- function(seed, n = 125000, prob.Type = c(0.60,0.25,0.15), prob.Hidden = c(0.35,0.45,0.20),
                                        max_rep_delay = 2, max_set_delay = 20, inflation = c(-0.02,0.005,0.015)){

  # Set seed
  set.seed(seed)

  ## Occurence date simulation -> Every possible date in between 2010-2020
  date_occ_seq <- seq(as.Date("2010/1/1"), as.Date("2020/12/31"), "day")
  date_occ     <- sample(date_occ_seq, size = n, replace = TRUE)

  # Add to dataframe
  base_year <- year(date_occ_seq[1]) + max_rep_delay
  df <- data.frame('occ.date' = as.character(date_occ), 'occ.year' = year(date_occ),
                   'occ.month' = factor(month(date_occ), levels = 1:12))
  df <- df %>% mutate('occ.year.fact' = factor(occ.year, levels = sort(unique(occ.year))),
                      .after = 'occ.year')

  ## 1 discrete + 1 hidden covariate
  ## 1 discrete + 1 hidden covariate -> include inflation
  # Accident year dependent probabilities in case of any inflation
  prob.T1 <- prob.Type[1] + inflation[1]*(df$occ.year - year(date_occ_seq[1]))
  prob.T2 <- prob.Type[2] + inflation[2]*(df$occ.year - year(date_occ_seq[1]))
  prob.T3 <- prob.Type[3] + inflation[3]*(df$occ.year - year(date_occ_seq[1]))

  # Simulation of the (un)known covariates
  x1 <- sapply(1:n, function(p) sample(c("T1","T2","T3"), size = 1, replace = TRUE,
                                       prob = c(prob.T1[p], prob.T2[p], prob.T3[p])))
  xh <- sample(c("L","M","H"), size = n, replace = TRUE, prob = prob.Hidden)
  x1 <- factor(x1, levels = c("T1",'T2',"T3"))
  xh <- factor(xh, levels = c("L","M","H"))
  df <- data.frame('claim.nr' = 1:n, 'type' = x1, 'hidden' = xh, df)
  row.names(df) <- paste('claim',1:n)

  ## Reporting delay simulation - in days (max two years)
  ndays        <- 365.25*max_rep_delay
  delay_report <- floor(ndays * rbeta(n, c(1,2,3)[as.numeric(x1)], 10))
  df <- df %>% dplyr::mutate('rep.delay' = as.numeric(delay_report)/365.25)

  ## Reporting date
  date_report <- date_occ + delay_report
  df <- df %>% dplyr::mutate('rep.date' = as.character(date_report)) %>%
    dplyr::mutate("rep.year" = year(date_report) - base_year + 1,
                  'rep.year.fact' = factor(rep.year, levels = sort(unique(rep.year))),
                  'rep.month' = factor(month(date_report), levels = 1:12))

  ## Payment delay
  n.payments <- 30
  lijst <- lapply(1:n, function(x)
    as.numeric(date_report[x]) +
      floor(cumsum(c(rexp(1, rate = c(6,5,4)[as.numeric(x1[x])]),
                     rexp(n.payments - 1, rate = c(2,1.5,1)[as.numeric(x1[x])])))*365.25))
  df_pay <- as.data.frame(data.table::transpose(lijst))
  colnames(df_pay) <- paste0("Pay",1:n.payments)

  ## Add Reporting date to payment delay matrix
  df_pay_copy <- df_pay
  df_pay_copy$Report <- as.numeric(date_report)
  for(j in 1:30){
    df_pay_copy[,j] <- (df_pay_copy[,j] - df_pay_copy$Report)/365.25
  }
  df_pay_copy <- as.data.frame(t(df_pay_copy %>% dplyr::select(-c(Report))))
  df_pay      <- as.data.frame(t(df_pay))

  # Payment sizes
  delay.rep <- rep((date_report - as.Date('2010-01-01'))/365.25, each = n.payments)
  payments <- c(as.matrix(df_pay_copy))
  meanlog <- function(x1, x2){
    base <- log(c(100,200,400)[as.numeric(x1)])
    ext  <- 0.1*(payments)^(c(1.50,1.25,1.40)[as.numeric(x2)])
    rep(base + ext, times = n.payments)
  }

  df_size <- matrix(rlnorm(length(payments),
                           meanlog = meanlog(x1 = x1, x2 = xh),
                           sdlog =  1), nrow = n, ncol = 30, byrow = T)
  df_size <- as.data.frame(t(df_size))
  rownames(df_size) <- paste0("Size", 1:n.payments)

  ## Settlement delay
  ndays <- 365.25*max_set_delay
  settlement_delay <- floor(rbeta(n, shape1 = 1, shape2 = 8*c(1,0.75,0.5)[as.numeric(x1)])*ndays)

  date_settlement <- date_report + settlement_delay
  df <- df %>% dplyr::mutate("settlement.date" = as.character(date_settlement)) %>%
    dplyr::mutate("settlement.year" = year(date_settlement) - base_year + 1)

  # Remove claims that occur after settlement date in payment dates and payment sizes
  max_delays <- as.numeric(date_settlement)
  df_pay  <- t(df_pay)
  df_size <- t(df_size)
  ind <- which(df_pay > max_delays, arr.ind = TRUE)
  df_pay[ind] = df_size[ind] <- NA
  df_pay  <- t(df_pay)
  df_size <- t(df_size)

  # Break dates to calculate payments in each observation year
  breaks <- as.numeric(as.Date(paste0(2009:2040,"-12-31")))
  names(breaks) <- 2009:2040

  # Calculate grouping of observation years - payments after observation window are put at NaN and excluded
  calc_obs_year <- function(k){
    pay_dates <- na.omit(as.numeric(df_pay[,k]))
    rep.year  <- df$rep.year[k] + base_year - 1
    breaks.k  <- breaks[as.character((rep.year - 1):(rep.year + 8))]
    vec0      <- (diff(rank(c(breaks.k+0.0001, pay_dates)))-1)[1:(length(breaks.k)-1)]
    vec1      <- as.numeric(rep(names(vec0), times = vec0)) - rep.year + 1
    vec       <- c(vec1, rep(NA, length(pay_dates) - length(vec1)))
    vec
  }
  obs_years <- lapply(1:n, function(k) calc_obs_year(k))

  # Aggregrate claim sizes belonging to same observation year
  agg_fu <- function(k){
    obsy  <- obs_years[[k]]
    vec   <- if(length(obsy) > 0) sapply(na.omit(df_size[,k]) %>% split(obsy), FUN = sum) else NULL
    vec.add <- rep(0, 9 - length(vec))
    names(vec.add) <- as.character(1:9)[! as.character(1:9) %in% names(vec)]
    vec <- c(vec,vec.add)
    vec[order(names(vec))]
  }

  df.size <- t(sapply(1:n, function(k) agg_fu(k)))
  colnames(df.size) <- paste0('size_obs', 1:9)

  # Is open indicator
  create_open <- function(ones){
    ones.vec <- rep(0, 9)
    ones.vec[1:(min(ones+1,9))] <- 1
    ones.vec
  }
  df.open <- t(sapply(df$settlement.year - df$rep.year, create_open))
  colnames(df.open) <- paste0("open_obs", 1:9)

  # Settlement indicator (close)
  create_settlement <- function(zeros){
    zeros.vec <- rep(1,9)
    zeros.vec[0:min(zeros,9)] <- 0
    zeros.vec
  }
  df.settlement <- t(sapply(df$settlement.year - df$rep.year, create_settlement))
  colnames(df.settlement) <- paste0("settlement_obs", 1:9)

  # Add to dataset
  df <- df %>% bind_cols(as_tibble(df.open)) %>% bind_cols(as_tibble(df.settlement)) %>%
    bind_cols(as_tibble(df.size))

  # Remove claims with reporting year later than 2028
  df <- df %>% filter(rep.year <= 9, rep.year >= 1)
  df <- df %>% mutate(rep.year.fact = factor(rep.year, levels = 1:9))

  # Put in long format
  df.long <- df %>% pivot_longer(cols = starts_with(c('open_obs', 'settlement_obs', 'size_obs')),
                                 names_to = c(".value", "obs"),
                                 names_sep = "_")

  # Add observation year variable
  df.long <- df.long %>%
    dplyr::mutate('dev.year.fact' =  recode_factor(obs, obs1 = 1, obs2 = 2, obs3 = 3, obs4 = 4, obs5 = 5, obs6 = 6,
                                                   obs7 = 7, obs8 = 8, obs9 = 9), .before = 'open') %>%
    dplyr::mutate('dev.year' = as.numeric(as.character(dev.year.fact)), .before = 'dev.year.fact') %>%
    dplyr::select(-c(obs))

  # Add calendar year and payment variable
  df.long <- df.long %>% dplyr::mutate('calendar.year' = rep.year + dev.year - 1, .after = 'dev.year.fact') %>%
    dplyr::mutate('payment' = (size > 0)*1, .before = 'size')

  df.long

}

#' @rdname simulate_scenario_baseline
#' @export
simulate_scenario_extreme_event <- function(seed, n = 125000, prob.Type = c(0.60,0.25,0.15), prob.Hidden = c(0.35,0.45,0.20),
                                            max_rep_delay = 2, max_set_delay = 20, period_extra = '2019'){

  # Set seed
  set.seed(seed)

  ## Occurence date simulation -> Every possible date in between 2010-2020
  date_occ_seq <- seq(as.Date("2010/1/1"), as.Date("2020/12/31"), "day")
  date_occ     <- if(! is.null(period_extra)){
    start_date <- sample(seq(as.Date(paste0(period_extra,'/1/1')),
                             as.Date(paste0(period_extra,'/12/31')), "day"), size = 1)
    period     <- seq(start_date, start_date + days(30), "day")
    len <- length(period)
    total.len <- length(date_occ_seq) + 4*len
    prob <- rep(1/total.len, length(date_occ_seq))
    names(prob) <- as.character(date_occ_seq)
    prob[as.character(period)] <- 5/total.len
    sample(date_occ_seq, size = n, replace = TRUE, prob = prob)} else
      sample(date_occ_seq, size = n, replace = TRUE)

  # Add to dataframe
  base_year <- year(date_occ_seq[1]) + max_rep_delay
  df <- data.frame('occ.date' = as.character(date_occ), 'occ.year' = year(date_occ),
                   'occ.month' = factor(month(date_occ), levels = 1:12))
  df <- df %>% mutate('occ.year.fact' = factor(occ.year, levels = sort(unique(occ.year))),
                      .after = 'occ.year')

  ## 1 discrete + 1 hidden covariate
  # Simulation of the (un)known covariates
  x1 <- sample(c("T1","T2","T3"), size = n, replace = TRUE,  prob = prob.Type)
  xh <- sample(c("L","M","H"), size = n, replace = TRUE, prob = prob.Hidden)
  x1 <- factor(x1, levels = c("T1",'T2',"T3"))
  xh <- factor(xh, levels = c("L","M","H"))
  df <- data.frame('claim.nr' = 1:n, 'type' = x1, 'hidden' = xh, df)
  row.names(df) <- paste('claim',1:n)

  ## Reporting delay simulation - in days (max two years)
  ndays        <- 365.25*max_rep_delay
  delay_report <- floor(ndays * rbeta(n, c(1,2,3)[as.numeric(x1)], 10))
  df <- df %>% dplyr::mutate('rep.delay' = as.numeric(delay_report)/365.25)

  ## Reporting date
  date_report <- date_occ + delay_report
  df <- df %>% dplyr::mutate('rep.date' = as.character(date_report)) %>%
    dplyr::mutate("rep.year" = year(date_report) - base_year + 1,
                  'rep.year.fact' = factor(rep.year, levels = sort(unique(rep.year))),
                  'rep.month' = factor(month(date_report), levels = 1:12))

  ## Payment delay
  n.payments <- 30
  lijst <- lapply(1:n, function(x)
    as.numeric(date_report[x]) +
      floor(cumsum(c(rexp(1, rate = c(6,5,4)[as.numeric(x1[x])]),
                     rexp(n.payments - 1, rate = c(2,1.5,1)[as.numeric(x1[x])])))*365.25))
  df_pay <- as.data.frame(data.table::transpose(lijst))
  colnames(df_pay) <- paste0("Pay",1:n.payments)

  ## Add Reporting date to payment delay matrix
  df_pay_copy <- df_pay
  df_pay_copy$Report <- as.numeric(date_report)
  for(j in 1:30){
    df_pay_copy[,j] <- (df_pay_copy[,j] - df_pay_copy$Report)/365.25
  }
  df_pay_copy <- as.data.frame(t(df_pay_copy %>% dplyr::select(-c(Report))))
  df_pay      <- as.data.frame(t(df_pay))

  # Payment sizes
  delay.rep <- rep((date_report - as.Date('2010-01-01'))/365.25, each = n.payments)
  payments <- c(as.matrix(df_pay_copy))
  meanlog <- function(x1, x2){
    base <- log(c(100,200,400)[as.numeric(x1)])
    ext  <- 0.1*(payments)^(c(1.50,1.25,1.40)[as.numeric(x2)])
    rep(base + ext, times = n.payments)
  }

  df_size <- matrix(rlnorm(length(payments),
                           meanlog = meanlog(x1 = x1, x2 = xh),
                           sdlog =  1), nrow = n, ncol = 30, byrow = T)
  df_size <- as.data.frame(t(df_size))
  rownames(df_size) <- paste0("Size", 1:n.payments)

  ## Settlement delay
  ndays <- 365.25*max_set_delay
  settlement_delay <- floor(rbeta(n, shape1 = 1, shape2 = 8*c(1,0.75,0.5)[as.numeric(x1)])*ndays)

  date_settlement <- date_report + settlement_delay
  df <- df %>% dplyr::mutate("settlement.date" = as.character(date_settlement)) %>%
    dplyr::mutate("settlement.year" = year(date_settlement) - base_year + 1)

  # Remove claims that occur after settlement date in payment dates and payment sizes
  max_delays <- as.numeric(date_settlement)
  df_pay  <- t(df_pay)
  df_size <- t(df_size)
  ind <- which(df_pay > max_delays, arr.ind = TRUE)
  df_pay[ind] = df_size[ind] <- NA
  df_pay  <- t(df_pay)
  df_size <- t(df_size)

  # Break dates to calculate payments in each observation year
  breaks <- as.numeric(as.Date(paste0(2009:2040,"-12-31")))
  names(breaks) <- 2009:2040

  # Calculate grouping of observation years - payments after observation window are put at NaN and excluded
  calc_obs_year <- function(k){
    pay_dates <- na.omit(as.numeric(df_pay[,k]))
    rep.year  <- df$rep.year[k] + base_year - 1
    breaks.k  <- breaks[as.character((rep.year - 1):(rep.year + 8))]
    vec0      <- (diff(rank(c(breaks.k+0.0001, pay_dates)))-1)[1:(length(breaks.k)-1)]
    vec1      <- as.numeric(rep(names(vec0), times = vec0)) - rep.year + 1
    vec       <- c(vec1, rep(NA, length(pay_dates) - length(vec1)))
    vec
  }
  obs_years <- lapply(1:n, function(k) calc_obs_year(k))

  # Aggregrate claim sizes belonging to same observation year
  agg_fu <- function(k){
    obsy  <- obs_years[[k]]
    vec   <- if(length(obsy) > 0) sapply(na.omit(df_size[,k]) %>% split(obsy), FUN = sum) else NULL
    vec.add <- rep(0, 9 - length(vec))
    names(vec.add) <- as.character(1:9)[! as.character(1:9) %in% names(vec)]
    vec <- c(vec,vec.add)
    vec[order(names(vec))]
  }

  df.size <- t(sapply(1:n, function(k) agg_fu(k)))
  colnames(df.size) <- paste0('size_obs', 1:9)

  # Is open indicator
  create_open <- function(ones){
    ones.vec <- rep(0, 9)
    ones.vec[1:(min(ones+1,9))] <- 1
    ones.vec
  }
  df.open <- t(sapply(df$settlement.year - df$rep.year, create_open))
  colnames(df.open) <- paste0("open_obs", 1:9)

  # Settlement indicator (close)
  create_settlement <- function(zeros){
    zeros.vec <- rep(1,9)
    zeros.vec[0:min(zeros,9)] <- 0
    zeros.vec
  }
  df.settlement <- t(sapply(df$settlement.year - df$rep.year, create_settlement))
  colnames(df.settlement) <- paste0("settlement_obs", 1:9)

  # Add to dataset
  df <- df %>% bind_cols(as_tibble(df.open)) %>% bind_cols(as_tibble(df.settlement)) %>%
    bind_cols(as_tibble(df.size))

  # Remove claims with reporting year later than 2028
  df <- df %>% filter(rep.year <= 9, rep.year >= 1)
  df <- df %>% mutate(rep.year.fact = factor(rep.year, levels = 1:9))

  # Put in long format
  df.long <- df %>% pivot_longer(cols = starts_with(c('open_obs', 'settlement_obs', 'size_obs')),
                                 names_to = c(".value", "obs"),
                                 names_sep = "_")

  # Add observation year variable
  df.long <- df.long %>%
    dplyr::mutate('dev.year.fact' =  recode_factor(obs, obs1 = 1, obs2 = 2, obs3 = 3, obs4 = 4, obs5 = 5, obs6 = 6,
                                                   obs7 = 7, obs8 = 8, obs9 = 9), .before = 'open') %>%
    dplyr::mutate('dev.year' = as.numeric(as.character(dev.year.fact)), .before = 'dev.year.fact') %>%
    dplyr::select(-c(obs))

  # Add calendar year and payment variable
  df.long <- df.long %>% dplyr::mutate('calendar.year' = rep.year + dev.year - 1, .after = 'dev.year.fact') %>%
    dplyr::mutate('payment' = (size > 0)*1, .before = 'size')

  df.long

}

#' @rdname simulate_scenario_baseline
#' @export
simulate_scenario_change_in_settlement <- function(seed, n = 125000, prob.Type = c(0.60,0.25,0.15), prob.Hidden = c(0.35,0.45,0.20),
                                                   max_rep_delay = 2, max_set_delay = 20, settlement_change = '2017'){

  # Set seed
  set.seed(seed)

  ## Occurence date simulation -> Every possible date in between 2010-2020
  date_occ_seq <- seq(as.Date("2010/1/1"), as.Date("2020/12/31"), "day")
  date_occ     <- sample(date_occ_seq, size = n, replace = TRUE)

  # Add to dataframe
  base_year <- year(date_occ_seq[1]) + max_rep_delay
  df <- data.frame('occ.date' = as.character(date_occ), 'occ.year' = year(date_occ),
                   'occ.month' = factor(month(date_occ), levels = 1:12))
  df <- df %>% mutate('occ.year.fact' = factor(occ.year, levels = sort(unique(occ.year))),
                      .after = 'occ.year')

  ## 1 discrete + 1 hidden covariate
  # Simulation of the (un)known covariates
  x1 <- sample(c("T1","T2","T3"), size = n, replace = TRUE,  prob = prob.Type)
  xh <- sample(c("L","M","H"), size = n, replace = TRUE, prob = prob.Hidden)
  x1 <- factor(x1, levels = c("T1",'T2',"T3"))
  xh <- factor(xh, levels = c("L","M","H"))
  df <- data.frame('claim.nr' = 1:n, 'type' = x1, 'hidden' = xh, df)
  row.names(df) <- paste('claim',1:n)

  ## Reporting delay simulation - in days (max two years)
  ndays        <- 365.25*max_rep_delay
  delay_report <- floor(ndays * rbeta(n, c(1,2,3)[as.numeric(x1)], 10))
  df <- df %>% dplyr::mutate('rep.delay' = as.numeric(delay_report)/365.25)

  ## Reporting date
  date_report <- date_occ + delay_report
  df <- df %>% dplyr::mutate('rep.date' = as.character(date_report)) %>%
    dplyr::mutate("rep.year" = year(date_report) - base_year + 1,
                  'rep.year.fact' = factor(rep.year, levels = sort(unique(rep.year))),
                  'rep.month' = factor(month(date_report), levels = 1:12))

  ## Payment delay
  n.payments <- 30
  lijst <- lapply(1:n, function(x)
    as.numeric(date_report[x]) +
      floor(cumsum(c(rexp(1, rate = c(6,5,4)[as.numeric(x1[x])]),
                     rexp(n.payments - 1, rate = c(2,1.5,1)[as.numeric(x1[x])])))*365.25))
  df_pay <- as.data.frame(data.table::transpose(lijst))
  colnames(df_pay) <- paste0("Pay",1:n.payments)

  ## Add Reporting date to payment delay matrix
  df_pay_copy <- df_pay
  df_pay_copy$Report <- as.numeric(date_report)
  for(j in 1:30){
    df_pay_copy[,j] <- (df_pay_copy[,j] - df_pay_copy$Report)/365.25
  }
  df_pay_copy <- as.data.frame(t(df_pay_copy %>% dplyr::select(-c(Report))))
  df_pay      <- as.data.frame(t(df_pay))

  # Payment sizes
  delay.rep <- rep((date_report - as.Date('2010-01-01'))/365.25, each = n.payments)
  payments <- c(as.matrix(df_pay_copy))
  meanlog <- function(x1, x2){
    base <- log(c(100,200,400)[as.numeric(x1)])
    ext  <- 0.1*(payments)^(c(1.50,1.25,1.40)[as.numeric(x2)])
    rep(base + ext, times = n.payments)
  }

  df_size <- matrix(rlnorm(length(payments),
                           meanlog = meanlog(x1 = x1, x2 = xh),
                           sdlog =  1), nrow = n, ncol = 30, byrow = T)
  df_size <- as.data.frame(t(df_size))
  rownames(df_size) <- paste0("Size", 1:n.payments)

  ## Settlement delay
  ndays <- 365.25*max_set_delay
  settlement_delay <- floor(rbeta(n, shape1 = 1, shape2 = 8*c(1,0.75,0.5)[as.numeric(x1)])*ndays)

  date_settlement <- date_report + settlement_delay
  df <- df %>% dplyr::mutate("settlement.date" = as.character(date_settlement)) %>%
    dplyr::mutate("settlement.year" = year(date_settlement) - base_year + 1)

  # Remove claims that occur after settlement date in payment dates and payment sizes
  max_delays <- as.numeric(date_settlement)
  df_pay  <- t(df_pay)
  df_size <- t(df_size)
  ind <- which(df_pay > max_delays, arr.ind = TRUE)
  df_pay[ind] = df_size[ind] <- NA
  df_pay  <- t(df_pay)
  df_size <- t(df_size)

  # Settlement change
  if(! is.null(settlement_change)){
    ind <- which(as.numeric(date_occ) >= as.numeric(as.Date(paste0(settlement_change,'-01-01'))))
    df_pay[,ind] <- matrix(rep(as.numeric(date_report[ind]), each = 30), nrow = 30, ncol = length(ind)) +
      floor((df_pay[,ind] -  matrix(rep(as.numeric(date_report[ind]), each = 30), nrow = 30, ncol = length(ind)))*0.85)
    date_settlement[ind]    <- date_report[ind] + floor(settlement_delay[ind]*0.85)
    df$settlement.date[ind] <- as.character(date_settlement[ind])
    df$settlement.year[ind] <- year(date_settlement[ind]) - base_year + 1
  }

  # Break dates to calculate payments in each observation year
  breaks <- as.numeric(as.Date(paste0(2009:2040,"-12-31")))
  names(breaks) <- 2009:2040

  # Calculate grouping of observation years - payments after observation window are put at NaN and excluded
  calc_obs_year <- function(k){
    pay_dates <- na.omit(as.numeric(df_pay[,k]))
    rep.year  <- df$rep.year[k] + base_year - 1
    breaks.k  <- breaks[as.character((rep.year - 1):(rep.year + 8))]
    vec0      <- (diff(rank(c(breaks.k+0.0001, pay_dates)))-1)[1:(length(breaks.k)-1)]
    vec1      <- as.numeric(rep(names(vec0), times = vec0)) - rep.year + 1
    vec       <- c(vec1, rep(NA, length(pay_dates) - length(vec1)))
    vec
  }
  obs_years <- lapply(1:n, function(k) calc_obs_year(k))

  # Aggregrate claim sizes belonging to same observation year
  agg_fu <- function(k){
    obsy  <- obs_years[[k]]
    vec   <- if(length(obsy) > 0) sapply(na.omit(df_size[,k]) %>% split(obsy), FUN = sum) else NULL
    vec.add <- rep(0, 9 - length(vec))
    names(vec.add) <- as.character(1:9)[! as.character(1:9) %in% names(vec)]
    vec <- c(vec,vec.add)
    vec[order(names(vec))]
  }

  df.size <- t(sapply(1:n, function(k) agg_fu(k)))
  colnames(df.size) <- paste0('size_obs', 1:9)

  # Is open indicator
  create_open <- function(ones){
    ones.vec <- rep(0, 9)
    ones.vec[1:(min(ones+1,9))] <- 1
    ones.vec
  }
  df.open <- t(sapply(df$settlement.year - df$rep.year, create_open))
  colnames(df.open) <- paste0("open_obs", 1:9)

  # Settlement indicator (close)
  create_settlement <- function(zeros){
    zeros.vec <- rep(1,9)
    zeros.vec[0:min(zeros,9)] <- 0
    zeros.vec
  }
  df.settlement <- t(sapply(df$settlement.year - df$rep.year, create_settlement))
  colnames(df.settlement) <- paste0("settlement_obs", 1:9)

  # Add to dataset
  df <- df %>% bind_cols(as_tibble(df.open)) %>% bind_cols(as_tibble(df.settlement)) %>%
    bind_cols(as_tibble(df.size))

  # Remove claims with reporting year later than 2028
  df <- df %>% filter(rep.year <= 9, rep.year >= 1)
  df <- df %>% mutate(rep.year.fact = factor(rep.year, levels = 1:9))

  # Put in long format
  df.long <- df %>% pivot_longer(cols = starts_with(c('open_obs', 'settlement_obs', 'size_obs')),
                                 names_to = c(".value", "obs"),
                                 names_sep = "_")

  # Add observation year variable
  df.long <- df.long %>%
    dplyr::mutate('dev.year.fact' =  recode_factor(obs, obs1 = 1, obs2 = 2, obs3 = 3, obs4 = 4, obs5 = 5, obs6 = 6,
                                                   obs7 = 7, obs8 = 8, obs9 = 9), .before = 'open') %>%
    dplyr::mutate('dev.year' = as.numeric(as.character(dev.year.fact)), .before = 'dev.year.fact') %>%
    dplyr::select(-c(obs))

  # Add calendar year and payment variable
  df.long <- df.long %>% dplyr::mutate('calendar.year' = rep.year + dev.year - 1, .after = 'dev.year.fact') %>%
    dplyr::mutate('payment' = (size > 0)*1, .before = 'size')

  df.long

}


