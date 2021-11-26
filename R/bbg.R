#' Return BBG connection token
#'
#' @return BBG connection token
#' @export
#'
#' @examples
bbg.con <- function() {
  return (Rblpapi::blpConnect())
}


#' BBG disconnect
#'
#' @param con BBG connection token
#'
#' @return
#' @export
#'
#' @examples
bbg.discon <- function(con) {
  Rblpapi::blpDisconnect(con)
}


#' Read price dataframe from BBG
#'
#' @param ticker ticker
#' @param startDate start date
#' @param con BBG connection
#'
#' @return dataframe of prices
#' @export
#'
#' @examples
bbg.read_px <- function(ticker, startDate, con) {
  df <- Rblpapi::getBars(db.bbg(ticker),
                         tz="Europe/Moscow",
                         barInterval=30,
                         startTime=as.POSIXct(paste(startDate,'00:00:00 MSK')),
                         endTime=Sys.time(),
                         con=con)
  df <- df[which(format(df$times, format="%H:%M:%S") == K$time_bbg), ]
  date <- as.Date(df$times)
  px <- df$open
  df <- data.frame(date, px)
  df <- df[order(df$date), ]
  return (df)
}


#' Read dividend data from BBG
#'
#' @param ticker ticker
#' @param con BBG connection
#' @param startDate start date
#'
#' @return dividend dataframe
#' @export
#'
#' @examples
bbg.read_dvd <- function(ticker, con, startDate=K$init_date) {
  df <- Rblpapi::bds(db.bbg(ticker), 'DVD_HIST_ALL', con=con)
  df <- df[!is.na(df$'Ex-Date'), ]
  if (!is.null(df)) {
    df <- df[df$`Ex-Date` >= startDate, ]
    regular <- db.any(df$`Dividend Type`, K$reg_div)
    if (any(!regular)) {
      warning(length(which(!regular)),
              ' dividend(s) of ', ticker, ' is(are) not regular cash\n'
      )
    }
    df <- df[regular, ]
    df <- df[order(df$`Ex-Date`), ]
    df <- data.frame(date=df$`Ex-Date`, dvd=df$`Dividend Amount`)
    if (nrow(df) > 0) {
      df <- aggregate(df$dvd, by=list(df$date), sum)
      names(df) <- c('date', 'dvd')
    } else {
      df <- NULL
    }
  }
  return (df)
}


#' Return PX_LAST of BBG ticker
#'
#' @param bbg_ticker BBG ticker
#' @param con BBG connection
#'
#' @return last price
#' @export
#'
#' @examples
bbg.read_last_price <- function(bbg_ticker, con) {
  Rblpapi::bdp(bbg_ticker, 'PX_LAST', con=con)[1,1]
}


#' Return last RUB RFR 1Y
#'
#' @param con BBG connection
#'
#' @return last RUB RFR 1Y
#' @export
#'
#' @examples
bbg.rub_rfr <- function(con) {
  log(1 + bbg.read_last_price('MICXRU1Y Index', con) / 100) * 100
}


#' Return last USD RFR 1Y
#'
#' @param con BBG connection
#'
#' @return last USD RFR 1Y
#' @export
#'
#' @examples
bbg.usd_rfr <- function(con) {
  log(1 + bbg.read_last_price('USOSFR1 Curncy', con) / 100) * 100
}


#' Return last EUR RFR 1Y
#'
#' @param con BBG connection
#'
#' @return last EUR RFR 1Y
#' @export
#'
#' @examples
bbg.eur_rfr <- function(con) {
  log(1 + bbg.read_last_price('EESWE1 Curncy', con) / 100) * 100
}


#' Return simple and cc last RFR 1Y for USD, RUB, EUR
#'
#' @param con BBG connection
#'
#' @return list of simple and cc last RFR 1Y for USD, RUB, EUR
#' @export
#'
#' @examples
bbg.rfr <- function(con) {
  simp <- c(
    bbg.read_last_price('USOSFR1 Curncy', con),
    bbg.read_last_price('MICXRU1Y Index', con),
    bbg.read_last_price('EESWE1 Curncy', con)
    )
  names(simp) <- c('USD', 'RUB', 'EUR')
  cc <- log(1 + simp / 100) * 100
  return (list(simp=simp, cc=cc))
}

