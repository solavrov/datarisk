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
  dir_cov_usd = 'data/cov_usd',
  dir_cov_eur = 'data/cov_eur',
  dir_er_usd = 'data/er_usd',
  dir_er_rub = 'data/er_rub',
  dir_er_eur = 'data/er_eur',

  dir_covcc_rub = 'data/covcc_rub',
  dir_covcc_usd = 'data/covcc_usd',
  dir_covcc_eur = 'data/covcc_eur',
  dir_ercc_usd = 'data/ercc_usd',
  dir_ercc_rub = 'data/ercc_rub',
  dir_ercc_eur = 'data/ercc_eur',

  dir_cov_rub_10 = 'data/cov_rub_10',
  dir_cov_usd_10 = 'data/cov_usd_10',
  dir_cov_eur_10 = 'data/cov_eur_10',
  dir_er_usd_10 = 'data/er_usd_10',
  dir_er_rub_10 = 'data/er_rub_10',
  dir_er_eur_10 = 'data/er_eur_10',

  dir_covcc_rub_10 = 'data/covcc_rub_10',
  dir_covcc_usd_10 = 'data/covcc_usd_10',
  dir_covcc_eur_10 = 'data/covcc_eur_10',
  dir_ercc_usd_10 = 'data/ercc_usd_10',
  dir_ercc_rub_10 = 'data/ercc_rub_10',
  dir_ercc_eur_10 = 'data/ercc_eur_10',


  reg_div = c('Regular Cash',
              'Interim',
              'Final',
              'Income',
              'Special Cash',
              'Capital Gains'),

  sp_expect = 'SP_EXPECT'

)
