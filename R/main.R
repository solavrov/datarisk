#' Put new ticker to database
#'
#' @param ticker ticker
#' @param bbg_ticker BBG ticker
#' @param finam_file finam file
#' @param split split multiplier
#' @param split_date split date
#'
#' @return nothing
#' @export
#'
#' @examples
main.put_new_ticker <- function(ticker,
                                curncy,
                                bbg_ticker=NA,
                                finam_file=paste0(K$finam_dir, ticker, '.csv'),
                                split=NA,
                                split_date=NA) {
  cat('doing ', ticker, '...\n', sep='')
  if (!is.na(bbg_ticker)) db.add_ticker(ticker, bbg_ticker, curncy)
  con <- Rblpapi::blpConnect()
  df <- db.read_finam(finam_file, split, split_date)
  db.add_px(ticker, df)
  df2 <- bbg.read_px(ticker, max(df$date) + 1, con)
  db.add_px(ticker, df2)
  df3 <- bbg.read_dvd(ticker, con)
  if (!is.null(df3)) db.add_dvd(ticker, df3)
  Rblpapi::blpDisconnect(con)
  cat('done!\n')
}


#' Refresh db and fb
#'
#' @param pwd fb password
#'
#' @return nothing
#' @export
#'
#' @examples
main.refresh <- function(pwd) {
  db.refresh()
  fb <- fb.auth(pwd)
  fb.refresh(fb)
}

