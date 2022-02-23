
#' Convert dataframe from one currency to another
#'
#' @param df dataframe
#' @param curncy0 initial currency
#' @param curncy1 target currency
#'
#' @return converted dataframe
#' @export
#'
#' @examples
calc.convert <- function(df, curncy0, curncy1) {

  if (curncy0 == curncy1 | is.null(df)) return (df)

  direction <- paste0(curncy0, '_to_', curncy1)
  if (direction == 'USD_to_RUB') {
    rates <- db.take_px('USD')
  } else if (direction == 'RUB_to_USD') {
    rates <- db.take_px('USD')
    rates$px <- 1 / rates$px
  } else if (direction == 'EUR_to_RUB') {
    rates <- db.take_px('EUR')
  } else if (direction == 'RUB_to_EUR') {
    rates <- db.take_px('EUR')
    rates$px <- 1 / rates$px
  } else if (direction == 'EUR_to_USD') {
    rates <- db.take_ratio('EUR', 'USD')
  } else if (direction == 'USD_to_EUR') {
    rates <- db.take_ratio('USD', 'EUR')
  }
  else {
    stop('Cannot convert ', curncy0, ' to ', curncy1)
  }
  names(rates) <- c('date', 'rate')

  col_name <- names(df)[2]

  df <- merge(df, rates)
  df[[col_name]] <- df[[col_name]] * df$rate
  df <- df[-3]

  return (df)

}


#' Calculate converted prices
#'
#' @param ticker ticker
#' @param curncy target currency
#'
#' @return converted dataframe
#' @export
#'
#' @examples
calc.px <- function(ticker, curncy) {
  df <- db.take_px(ticker)
  df <- calc.convert(df, db.curncy(ticker), curncy)
  names(df) <- c('date', ticker)
  return (df)
}


#' Calculate converted dividends
#'
#' @param ticker ticker
#' @param curncy target currency
#'
#' @return converted dataframe
#' @export
#'
#' @examples
calc.dvd <- function(ticker, curncy) {
  df <- db.take_dvd(ticker)
  if (!is.null(df)) {
    df <- calc.convert(df, db.curncy(ticker), curncy)
    names(df) <- c('date', paste0(ticker, '_dvd'))
  }
  return (df)
}


#' Calculate converted prices and dividends in one dataframe
#'
#' @param ticker ticker
#' @param curncy target currency
#'
#' @return dataframe of converted prices and dividends
#' @export
#'
#' @examples
calc.px_dvd <- function(ticker, curncy) {
  df_px <- calc.px(ticker, curncy)
  df_dvd <- calc.dvd(ticker, curncy)
  if (is.null(df_dvd)) {
    df_dvd <- data.frame(date=df_px$date, dvd=rep(0, nrow(df_px)))
  }
  df <- merge(df_px, df_dvd, all.x=TRUE)
  i <- which(is.na(df[ , 3]))
  df[i, 3] <- 0
  names(df) <- c('date', ticker, paste0(ticker, '_dvd'))
  return (df)
}


#' Calculate returns for a given ticker for one day periods only!
#'
#' @param ticker ticket
#' @param curncy target currency
#'
#' @return dataframe of returns
#' @export
#'
#' @examples
calc.returns <- function(ticker, curncy) {
  df <- calc.px_dvd(ticker, curncy)
  r <- round(append(NA, log(tail(df[ , 2] + df[ , 3], -1) / head(df[ , 2], -1)) * 100), 4)
  i <- which(diff(as.Date(df[ , 1])) == 1) + 1
  df2 <- data.frame(df[i, 1], r[i])
  names(df2) <- c('date', ticker)
  return (df2)
}


#' Calculates returns for all tickers for joint one day periods!
#'
#' @param curncy target currency
#'
#' @return dataframe of returns for all tickers
#' @export
#'
#' @examples
calc.all_returns <- function(curncy, include_date=FALSE) {
  tickers <- db.take_all_tickers()$ticker
  df <- calc.returns(tickers[1], curncy)
  for (t in tail(tickers, -1)) df <- merge(df, calc.returns(t, curncy))
  if (!include_date) df <- df[ , -1]
  return (df)
}


#' Calculate covariance matrix of returns of all tickers
#'
#' @param curncy target currency
#' @param cov_win number of last dates in calculation
#'
#' @return covariance matrix
#' @export
#'
#' @examples
calc.cov <- function(curncy, cov_win = K$cov_win) {
  df <- tail(calc.all_returns(curncy), cov_win)
  covar <- cov(df)
  i <- which(db.take_all_tickers()$type==K$crypto)
  covar[i, i] <- covar[i, i] * K$year_ratio
  return(covar)
}


#' Return expected simple and cc returns, simple and cc covariance matrices
#'
#' @param curncy currency
#' @param rfr risk free rates from bbg.rfr
#' @param hor horizon in years
#' @param cov_win number of last dates in calculation
#'
#' @return list of expected simple and cc returns, simple and cc covariance matrices
#' @export
#'
#' @examples
calc.model <- function(curncy, rfr, hor = 1, cov_win = K$cov_win) {

  R00 <- (1 + db.last_row(K$sp_expect)$r / 100) ^ hor - 1
  F00 <- (1 + rfr$simp['USD'] / 100) ^ hor - 1

  g__0 <- calc.cov('USD', cov_win) * K$year * hor / 10000
  d_0 <- diag(g__0)
  g0_0 <- g__0['IVV', ]
  d00 <- g0_0['IVV']
  g__c <-  calc.cov(curncy, cov_win) * K$year * hor / 10000
  d_c <- diag(g__c)

  D00 <- (exp(d00) - 1) * (1 + R00) ^ 2
  denom <- D00 - (1 + R00) * (R00 - F00) * (exp(g0_0) - 1)
  if (any(denom <= 0)) stop('calc.model MODEL FAILURE!')
  R_0 <- ( D00 * F00 + (1 + R00) * (R00 - F00) * (exp(g0_0) - 1) ) / denom

  r_0 <- log(1 + R_0) - d_0 / 2
  r_c <- r_0 - r_0[curncy] + rfr$cc[curncy] * hor / 100
  R_c <- exp(r_c + d_c / 2) - 1
  G__c <- (exp(g__c) - 1) * ((1 + R_c) %*% t(1 + R_c))

  return (list(ercc=r_c * 100,
               er=R_c * 100,
               covcc=g__c * 10000,
               cov=G__c * 10000))

}


#' Return sample of simple returns for all assets for given currency
#'
#' @param curncy currency
#' @param rfr risk free rates from bbg.rfr
#' @param len sample length
#' @param cov_win number of last dates in calculation
#'
#' @return
#' @export
#'
#' @examples
calc.sample <- function(curncy, rfr, len=K$sample_len, cov_win = K$cov_win) {

  mod <- calc.model(curncy, rfr, cov_win)

  g <- mod$covcc[rownames(mod$covcc)!=curncy, colnames(mod$covcc)!=curncy]
  er <- mod$ercc[names(mod$ercc)!=curncy]

  X <- matrix(rnorm(nrow(g) * len), nrow=nrow(g))
  r <- t(chol(g)) %*% X + er

  tickers <- names(mod$ercc)
  r <- rbind(r, XXX=rep(unname(mod$ercc[curncy]), len))
  rownames(r)[rownames(r)=='XXX'] <- curncy
  r <- r[order(match(rownames(r), tickers)), ]

  R <- (exp(r / 100) - 1) * 100

  return (R)

}

