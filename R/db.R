
#' Read price dataframe from Finam file
#'
#' @param file path to file
#' @param split split parameter that divides all prices
#' @param split_date first trade date after split
#'
#' @return dataframe of prices
#' @export
#'
#' @examples
db.read_finam <- function(file, split=NA, split_date=NA) {
  df <- read.csv(file, sep=',')
  df <- df[which(df$X.TIME. == K$time_finam), ]
  date <- as.Date(as.character(df$X.DATE.), format='%Y%m%d')
  px <- df$X.CLOSE.
  df <- data.frame(date, px)
  df <- df[order(df$date), ]
  if (!is.na(split) & !is.na(split_date))
    df[df$date < split_date, ]$px <- df[df$date < split_date, ]$px / split
  return (df)
}


#' Add ticker to tickers table
#'
#' @param ticker ticker
#' @param bbg_ticker BBG ticker
#' @param curncy currency
#'
#' @return nothing
#' @export
#'
#' @examples
db.add_ticker <- function(ticker, bbg_ticker, curncy, asset_type) {
  db <- DBI::dbConnect(RSQLite::SQLite(), K$db_name)
  df <- data.frame(ticker=ticker, bbg_ticker=bbg_ticker, curncy=curncy, asset_type=asset_type)
  DBI::dbWriteTable(db, 'tickers', df, append=TRUE)
  DBI::dbDisconnect(db)
}


#' Return BBG ticker
#'
#' @param ticker ticker
#'
#' @return BBG ticker
#' @export
#'
#' @examples
db.bbg <- function(ticker) {
  db <- DBI::dbConnect(RSQLite::SQLite(), K$db_name)
  query <- paste0("SELECT bbg_ticker FROM tickers WHERE ticker=\'",
                  ticker, "\'")
  df <- DBI::dbGetQuery(db, query)
  DBI::dbDisconnect(db)
  return(df$bbg_ticker[1])
}


#' Return currency of ticker
#'
#' @param ticker ticker
#'
#' @return currency
#' @export
#'
#' @examples
db.curncy <- function(ticker) {
  db <- DBI::dbConnect(RSQLite::SQLite(), K$db_name)
  query <- paste0("SELECT curncy FROM tickers WHERE ticker=\'",
                  ticker, "\'")
  df <- DBI::dbGetQuery(db, query)
  DBI::dbDisconnect(db)
  return(df$curncy[1])
}


#' Take all tickers
#'
#' @return dataframe of tickers
#' @export
#'
#' @examples
db.take_all_tickers <- function() {
  db <- DBI::dbConnect(RSQLite::SQLite(), K$db_name)
  query <- paste0("SELECT * FROM tickers")
  df <- DBI::dbGetQuery(db, query)
  DBI::dbDisconnect(db)
  return(df)
}


#' Does element from x contain in y
#'
#' @param x vector
#' @param y vector
#'
#' @return logical vector of question answers
#' @export
#'
#' @examples
db.any <- function(x, y) {
  z <- (x == y[1])
  for (e in tail(y, -1)) z <- z | (x == e)
  return (z)
}


#' Return last date of table, if no table return init_date
#'
#' @param table_name table name
#'
#' @return last date
#' @export
#'
#' @examples
db.last_date <- function(table_name) {
  d <- K$init_date
  db <- DBI::dbConnect(RSQLite::SQLite(), K$db_name)
  query <- paste0("SELECT max(date) FROM ", table_name)
  try(
    d <- as.Date(DBI::dbGetQuery(db, query)[1,1]),
    silent=TRUE
  )
  DBI::dbDisconnect(db)
  return(d)
}


#' Return row of most recent date
#'
#' @param table_name table name
#'
#' @return last row
#' @export
#'
#' @examples
db.last_row <- function(table_name) {
  lr <- NULL
  db <- DBI::dbConnect(RSQLite::SQLite(), K$db_name)
  query <- paste0("SELECT * FROM ", table_name,
                  " WHERE date = (SELECT max(date) FROM ", table_name, ")")
  try(
    lr <- DBI::dbGetQuery(db, query),
    silent=TRUE
  )
  DBI::dbDisconnect(db)
  return(lr)
}


#' Does table exist in data base
#'
#' @param table_name table name
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
db.exists <- function(table_name) {
  r <- NULL
  db <- DBI::dbConnect(RSQLite::SQLite(), K$db_name)
  query <- paste0("SELECT * FROM ", table_name, " LIMIT 1")
  try(
    r <- DBI::dbGetQuery(db, query),
    silent=TRUE
  )
  DBI::dbDisconnect(db)
  return(!is.null(r))
}


#' Add to dataframe that contains date field
#'
#' @param table_name table name
#' @param df dataframe to add
#'
#' @return nothing
#' @export
#'
#' @examples
db.add_df <- function(table_name, df) {
  db <- DBI::dbConnect(RSQLite::SQLite(), K$db_name)
  df$date <- as.character(df$date)
  DBI::dbWriteTable(db, table_name, df, append=TRUE)
  DBI::dbDisconnect(db)
}

#' Add price dataframe to price table
#'
#' @param ticker ticker
#' @param df dataframe
#'
#' @return nothing
#' @export
#'
#' @examples
db.add_px <- function(ticker, df) {
  db.add_df(paste0(ticker, '_px'), df)
}


#' Add dividend dataframe to dividend table
#'
#' @param ticker ticker
#' @param df dataframe
#'
#' @return nothing
#' @export
#'
#' @examples
db.add_dvd <- function(ticker, df) {
  db.add_df(paste0(ticker, '_dvd'), df)
}


#' Take prices
#'
#' @param ticker ticker
#'
#' @return price dataframe
#' @export
#'
#' @examples
db.take_px <- function(ticker) {
  df <- NULL
  db <- DBI::dbConnect(RSQLite::SQLite(), K$db_name)
  query <- paste0("SELECT * FROM ", ticker, "_px")
  try(
    df <- DBI::dbGetQuery(db, query),
    silent=TRUE
  )
  DBI::dbDisconnect(db)
  return (df)
}


#' Take ratio of prices
#'
#' @param ticker_num numerator ticker
#' @param ticker_denom denominator ticker
#'
#' @return ratio dataframe
#' @export
#'
#' @examples
db.take_ratio <- function(ticker_num, ticker_denom) {
  num <- db.take_px(ticker_num)
  denom <- db.take_px(ticker_denom)
  names(denom) <- c('date', 'px2')
  df <- merge(num, denom)
  df$px <- df$px / df$px2
  return (df[-3])
}


#' Take dividends
#'
#' @param ticker ticker
#'
#' @return dividend dataframe
#' @export
#'
#' @examples
db.take_dvd <- function(ticker) {
  df <- NULL
  db <- DBI::dbConnect(RSQLite::SQLite(), K$db_name)
  query <- paste0("SELECT * FROM ", ticker, "_dvd")
  try(
    df <- DBI::dbGetQuery(db, query),
    silent=TRUE
  )
  DBI::dbDisconnect(db)
  return (df)
}


#' Refresh price table
#'
#' @param ticker ticker
#'
#' @return nothing
#' @export
#'
#' @examples
db.refresh_px <- function(ticker) {
  if (ticker != 'RUB' & db.exists(paste0(ticker, '_px'))) {
    con <- Rblpapi::blpConnect()
    d <- db.last_date(paste0(ticker, "_px")) + 1
    if (d <= Sys.Date()) {
      df <- bbg.read_px(ticker, d, con)
      if (nrow(df) > 0) db.add_px(ticker, df)
    }
    Rblpapi::blpDisconnect(con)
  }
}


#' Refresh dividend table
#'
#' @param ticker ticker
#'
#' @return nothing
#' @export
#'
#' @examples
db.refresh_dvd <- function(ticker) {
  if (ticker != 'RUB' & db.exists(paste0(ticker, '_dvd'))) {
    con <- Rblpapi::blpConnect()
    df <- bbg.read_dvd(ticker, con, db.last_date(paste0(ticker, "_dvd")) + 1)
    if (nrow(df) > 0) db.add_dvd(ticker, df)
    Rblpapi::blpDisconnect(con)
  }
}


#' Refresh all price and dividend tables
#'
#' @return nothing
#' @export
#'
#' @examples
db.refresh_all_px_dvd <- function() {
  cat('DOING DB REFRESH PX DVD...\n', sep='')
  tickers <- db.take_all_tickers()$ticker
  for (t in tickers) {
    cat('doing ', t, '...\n', sep='')
    db.refresh_px(t)
    db.refresh_dvd(t)
    cat('done!\n')
  }
  cat('DONE PX DVD!\n')
}


#' Refresh SP_EXPECT table
#'
#' @return nothing
#' @export
#'
#' @examples
db.refresh_sp <- function() {

  cat('DOING DB REFRESH SP_EXPECT...\n', sep='')

  year <- function(shift) {
    as.character(lubridate::year(Sys.Date()) - 2000 + shift)
  }

  d <- db.last_date(K$sp_expect) + 1

  if (d <= Sys.Date()) {

    con <- Rblpapi::blpConnect()
    dvd <- Rblpapi::bdh('SPX Index', 'BEST_DIV_YLD', d,
                        overrides = c(BEST_FPERIOD_OVERRIDE = '1TY'))
    inf <- Rblpapi::bdh('USGGBE10 Index', 'PX_LAST', d)
    gdp1 <- Rblpapi::bdh(paste0('ECGDUS ', year(1), ' Index'), 'PX_LAST', d)
    gdp2 <- Rblpapi::bdh(paste0('ECGDUS ', year(2), ' Index'), 'PX_LAST', d)
    Rblpapi::blpDisconnect(con)

    names(dvd) <- c('date', 'dvd')
    names(inf) <- c('date', 'inf')
    names(gdp1) <- c('date', 'gdp1')
    names(gdp2) <- c('date', 'gdp2')

    df <- merge(gdp1, gdp2, all=TRUE)

    if (nrow(df) > 0) {
      for (i in 1:nrow(df))
        if (is.na(df$gdp2[i])) df$gdp2[i] <- df$gdp1[i]
        gdp <- data.frame(date=df$date, gdp=df$gdp2)

        df <- merge(dvd, inf)
        df <- merge(df, gdp)

        r <- df$dvd + df$inf + df$gdp
        r_cc <- round(log(1 + r / 100) * 100, K$return_round)
        df <- data.frame(df, r, r_cc)

        db.add_df('SP_EXPECT', df)
    }

  }

  cat('DONE SP_EXPECT!\n')

}


#' Refresh everything in db
#'
#' @return nothing
#' @export
#'
#' @examples
db.refresh <- function() {
  db.refresh_all_px_dvd()
  db.refresh_sp()
}
