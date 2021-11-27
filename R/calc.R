
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


#' Take converted prices
#'
#' @param ticker ticker
#' @param curncy target currency
#'
#' @return converted dataframe
#' @export
#'
#' @examples
calc.take_px <- function(ticker, curncy) {
  df <- db.take_px(ticker)
  df <- calc.convert(df, db.curncy(ticker), curncy)
  return (df)
}


#' Take converted dividends
#'
#' @param ticker ticker
#' @param curncy target currency
#'
#' @return converted dataframe
#' @export
#'
#' @examples
calc.take_dvd <- function(ticker, curncy) {
  df <- db.take_dvd(ticker)
  df <- calc.convert(df, db.curncy(ticker), curncy)
  return (df)
}


#' Calculate returns along with base dates for each return
#'
#' @param ticker ticker
#' @param curncy target currency
#'
#' @return dataframe of returns
#' @export
#'
#' @examples
calc.returns <- function(ticker, curncy=db.curncy(ticker)) {
  px <- calc.take_px(ticker, curncy)
  if (is.null(px)) return (NULL)
  dvd <- calc.take_dvd(ticker, curncy)
  if (is.null(dvd)) {
    df <- px
    r <- append(NA, log(tail(df$px, -1) / head(df$px, -1)) * 100)
    bd <- append(NA, head(df$date, -1))
  } else {
    dvd <- dvd[dvd$date <= tail(px$date, 1), ]
    df <- merge(px, dvd, all=TRUE)
    df$dvd[is.na(df$dvd)] <- 0
    r <- append(NA, log(tail(df$px + df$dvd, -1) / head(df$px, -1)) * 100)
    bd <- append(NA, head(df$date, -1))
  }
  r <- round(r, K$return_round)
  df <- data.frame(df$date, r, bd)
  names(df) <- c('date', ticker, paste0('bd_', ticker))
  df <- df[-1, ]
  return (df)
}


#' Return which rows are rows of equal values
#'
#' @param df dataframe
#'
#' @return numbers of rows
#' @export
#'
#' @examples
calc.flat_rows <- function(df) {
  n <- names(df)
  w <- c()
  for (i in 1:nrow(df)) {
    if (all(df[i, n] == df[i, n[1]])) {
      w <- append(w, i)
    }
  }
  return (w)
}


#' Calculate returns for all tickers
#'
#' @param curncy target currency
#' @param include_date return with date or without
#'
#' @return dataframe of returns
#' @export
#'
#' @examples
calc.all_returns <- function(curncy, include_date=FALSE) {

  tickers <- db.take_all_tickers()$ticker
  df <- calc.returns(tickers[1], curncy)
  for (t in tail(tickers, -1)) df <- merge(df, calc.returns(t, curncy))

  bd_tickers <- paste0('bd_', tickers)
  bd_tickers_less_curncy <- paste0('bd_', tickers[tickers != curncy])
  w <- calc.flat_rows(df[ , bd_tickers_less_curncy])
  df <- df[w, ]

  n <- names(df)
  n <- n[!n %in% bd_tickers]
  df <- df[ , n]

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
  return(cov(df))
}


#' Calculate correlation matrix of returns of all tickers
#'
#' @param curncy target currency
#' @param cov_win number of last dates in calculation
#'
#' @return correlation matrix
#' @export
#'
#' @examples
calc.cor <- function(curncy, cov_win = K$cov_win) {
  df <- tail(calc.all_returns(curncy), cov_win)
  df <- df[names(df) != curncy]
  return(cor(df))
}



#' Return expected simple and cc returns, simple and cc covariance matrices
#'
#' @param curncy currency
#' @param rfr risk free rates from bbg.rfr
#' @param cov_win number of last dates in calculation
#'
#' @return list of expected simple and cc returns, simple and cc covariance matrices
#' @export
#'
#' @examples
calc.model <- function(curncy, rfr, cov_win = K$cov_win) {

  R00 <- db.last_row(K$sp_expect)$r / 100
  F00 <- rfr$simp['USD'] / 100

  g__0 <- calc.cov('USD', cov_win) * K$year / 10000
  d_0 <- diag(g__0)
  g0_0 <- g__0['IVV', ]
  d00 <- g0_0['IVV']
  g__c <-  calc.cov(curncy, cov_win) * K$year / 10000
  d_c <- diag(g__c)

  D00 <- (exp(d00) - 1) * (1 + R00) ^ 2
  denom <- D00 - (1 + R00) * (R00 - F00) * (exp(g0_0) - 1)
  if (any(denom <= 0)) stop('MODEL FAILURE!')
  R_0 <- ( D00 * F00 + (1 + R00) * (R00 - F00) * (exp(g0_0) - 1) ) / denom

  r_0 <- log(1 + R_0) - d_0 / 2
  r_c <- r_0 -  r_0[curncy] + rfr$cc[curncy] / 100
  R_c <- exp(r_c + d_c / 2) - 1
  G__c <- (exp(g__c) - 1) * ((1 + R_c) %*% t(1 + R_c))

  return (list(ercc=r_c * 100,
               er=R_c * 100,
               covcc=g__c * 10000,
               cov=G__c * 10000))

}


# calc.sample <- function(curncy, cov_win = K$cov_win) {
#
#   mod <- calc.mode(curncy, cov_win)
#
#
# }
