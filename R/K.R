#' List of constants
#'
#' @export
K <- list(
  finam_dir = 'finam/',

  api_key = "AIzaSyDA2rvK4pmbmstno1_ixUsM954Z9xSIa0E",
  fb_url =  "https://risk-ffdb5-default-rtdb.firebaseio.com/",
  email = "2slavrov@gmail.com",

  time_finam = 180000, #end of bar time
  time_bbg = '18:00:00', #start of bar time
  init_date = as.Date('2015-01-01'),

  db_name = 'datarisk.sqlite',

  cov_win = 600,
  year = 250,
  sample_len = 1000,

  return_round = 4,

  crypto = 'crypto',
  year_ratio = 360/250,

  dir_refresh_time = 'data/refresh_time',
  dir_tickers = 'data/tickers',
  dir_types = 'data/types',
  dir_names = 'data/names',

  dir_cov_rub = 'data/cov_rub',
  dir_cor_rub = 'data/cor_rub',
  dir_cov_usd = 'data/cov_usd',
  dir_cor_usd = 'data/cor_usd',
  dir_cov_eur = 'data/cov_eur',
  dir_cor_eur = 'data/cor_eur',
  dir_er_usd = 'data/er_usd',
  dir_er_rub = 'data/er_rub',
  dir_er_eur = 'data/er_eur',

  dir_covcc_rub = 'data/covcc_rub',
  dir_corcc_rub = 'data/corcc_rub',
  dir_covcc_usd = 'data/covcc_usd',
  dir_corcc_usd = 'data/corcc_usd',
  dir_covcc_eur = 'data/covcc_eur',
  dir_corcc_eur = 'data/corcc_eur',
  dir_ercc_usd = 'data/ercc_usd',
  dir_ercc_rub = 'data/ercc_rub',
  dir_ercc_eur = 'data/ercc_eur',


  reg_div = c('Regular Cash',
              'Interim',
              'Final',
              'Income',
              'Special Cash',
              'Capital Gains'),

  sp_expect = 'SP_EXPECT'

)
