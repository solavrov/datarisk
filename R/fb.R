
#' Authorize in Firebase
#'
#' @param pwd password
#'
#' @return database token
#' @export
#'
#' @examples
fb.auth <- function(pwd) {
  fb <- fireData::auth(projectAPI=K$api_key, email=K$email, password=pwd)
  return (fb)
}


#' Put value to firebase
#'
#' @param value value
#' @param directory directory
#' @param fb firebase token
#'
#' @return nothing
#' @export
#'
#' @examples
fb.put_value <- function(value, directory, fb) {
  try(
    fireData::put(value,
        projectURL=K$fb_url,
        directory=directory,
        token=fb$idToken),
    silent=TRUE
  )
}


#' Put dataframe to firebase
#'
#' @param df dataframe
#' @param directory directory
#' @param fb firebase token
#'
#' @return nothing
#' @export
#'
#' @examples
fb.put_df <- function(df, directory, fb) {
  fireData::put(df,
      projectURL=K$fb_url,
      directory=directory,
      token=fb$idToken)
}


#' Take value from firebase
#'
#' @param directory directory
#'
#' @return value
#' @export
#'
#' @examples
fb.take_value <- function(directory) {
  suppressWarnings(
    v <- fireData::download(projectURL=K$fb_url, fileName=directory)
  )
  return (v)
}


#' Take dataframe from firebase
#'
#' @param directory directory
#'
#' @return dataframe
#' @export
#'
#' @examples
fb.take_df <- function(directory) {
  df <- fireData::download(projectURL=K$fb_url, directory)
  return (df)
}


#' Delete directory or value
#'
#' @param directory directory
#' @param fb firebase token
#'
#' @return nothing
#' @export
#'
#' @examples
fb.del_dir <- function(directory, fb) {
  fireData::delete('', K$fb_url, directory, fb$idToken)
}


#' Refresh fb
#'
#' @param fb firebase token
#'
#' @return nothing
#' @export
#'
#' @examples
fb.refresh <- function(fb) {
  cat('START FB REFRESH...\n', sep='')
  fb.put_df(db.take_all_tickers()$ticker, K$dir_tickers, fb)
  fb.put_df(db.take_all_tickers()$type, K$dir_types, fb)
  fb.put_df(db.take_all_tickers()$name, K$dir_names, fb)

  rfr <- bbg.rfr(1)

  cat('DOING FB REFRESH RUB 1Y...\n', sep='')
  mod <- calc.model('RUB', rfr, 1)
  fb.put_df(mod$cov, K$dir_cov_rub, fb)
  fb.put_df(mod$er, K$dir_er_rub, fb)
  fb.put_df(mod$covcc, K$dir_covcc_rub, fb)
  fb.put_df(mod$ercc, K$dir_ercc_rub, fb)
  cat('DONE!\n', sep='')

  cat('DOING FB REFRESH USD 1Y...\n', sep='')
  mod <- calc.model('USD', rfr, 1)
  fb.put_df(mod$cov, K$dir_cov_usd, fb)
  fb.put_df(mod$er, K$dir_er_usd, fb)
  fb.put_df(mod$covcc, K$dir_covcc_usd, fb)
  fb.put_df(mod$ercc, K$dir_ercc_usd, fb)
  cat('DONE!\n', sep='')

  cat('DOING FB REFRESH EUR 1Y...\n', sep='')
  mod <- calc.model('EUR', rfr, 1)
  fb.put_df(mod$cov, K$dir_cov_eur, fb)
  fb.put_df(mod$er, K$dir_er_eur, fb)
  fb.put_df(mod$covcc, K$dir_covcc_eur, fb)
  fb.put_df(mod$ercc, K$dir_ercc_eur, fb)
  cat('DONE!\n', sep='')

  rfr <- bbg.rfr(10)

  cat('DOING FB REFRESH RUB 10Y...\n', sep='')
  mod <- calc.model('RUB', rfr, 10)
  fb.put_df(mod$cov, K$dir_cov_rub_10, fb)
  fb.put_df(mod$er, K$dir_er_rub_10, fb)
  fb.put_df(mod$covcc, K$dir_covcc_rub_10, fb)
  fb.put_df(mod$ercc, K$dir_ercc_rub_10, fb)
  cat('DONE!\n', sep='')

  cat('DOING FB REFRESH USD 10Y...\n', sep='')
  mod <- calc.model('USD', rfr, 10)
  fb.put_df(mod$cov, K$dir_cov_usd_10, fb)
  fb.put_df(mod$er, K$dir_er_usd_10, fb)
  fb.put_df(mod$covcc, K$dir_covcc_usd_10, fb)
  fb.put_df(mod$ercc, K$dir_ercc_usd_10, fb)
  cat('DONE!\n', sep='')

  cat('DOING FB REFRESH EUR 10Y...\n', sep='')
  mod <- calc.model('EUR', rfr, 10)
  fb.put_df(mod$cov, K$dir_cov_eur_10, fb)
  fb.put_df(mod$er, K$dir_er_eur_10, fb)
  fb.put_df(mod$covcc, K$dir_covcc_eur_10, fb)
  fb.put_df(mod$ercc, K$dir_ercc_eur_10, fb)
  cat('DONE!\n', sep='')

  fb.put_value(format(Sys.time(), '%Y-%m%-%d %H:%M:%S %Z'),
               K$dir_refresh_time, fb)
  cat('END FB REFRESH...\n', sep='')
}

