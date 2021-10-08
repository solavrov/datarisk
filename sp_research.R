con <- Rblpapi::blpConnect()

start_date <- as.Date('2009-02-12')

inf <- Rblpapi::bdh('USGGBE10 Index', 'PX_LAST', start_date)

dvd <- Rblpapi::bdh('SPX Index', 'BEST_DIV_YLD', start_date,
                     overrides = c(BEST_FPERIOD_OVERRIDE = '1TY'))

names(inf) <- c('date', 'inf')
names(dvd) <- c('date', 'dvd')

df <- merge(dvd, inf)

add_zero <- function(n) {
  if (n < 10) n <- paste0('0', n)
  else n <- as.character(n)
  return (n)
}

to_na <- function(x) {
  if (length(x) == 0) return (NA)
  else return (x)
}

gdp_data <- list()
for (y in 8:23) {
  t <- paste0('ECGDUS ', add_zero(y), ' Index')
  gdp_data[[y]] <- Rblpapi::bdh(t, 'PX_LAST', as.Date('2000-01-01'))
}

Rblpapi::blpDisconnect(con)

gdp <- c()
for (i in 1:nrow(df)) {
  d <- df[i, 'date']
  y <- lubridate::year(d) - 2000 + 2
  g <- gdp_data[[y]]
  x <- to_na(g[g$date == d, ]$PX_LAST)
  if (is.na(x) & i>1) x <- gdp[i-1]
  gdp[i] <- x
}

df <- data.frame(df, gdp)
df <- df[!is.na(df$gdp), ]
r <- df$dvd + df$gdp + df$inf
r_cc <- round(log(1 + r/100) * 100, 4)
df <- data.frame(df, r, r_cc)

plot(df$date, df$r_cc, type='l', lwd=2, xlab='time', ylab='expected sp500 return')

plot(df$date, log(1 + df$dvd/100 + df$gdp/100) * 100, type='l', lwd=2,
     xlab='time', ylab='real expected sp500 return')

plot(df$date, df$dvd, type='l', col='green', ylim=c(0.5, 3.5), lwd=3,
     xlab='time', ylab='rate')
lines(df$date, df$gdp, type='l', col='blue', lwd=3)
lines(df$date, df$inf, type='l', col='red', lwd=3)
legend(15250, 1.5,
       legend=c('dvd', 'gdp', 'inf'),
       col=c('green', 'blue', 'red'), lty=c(1,1,1), lwd=2)


# db <- DBI::dbConnect(RSQLite::SQLite(), datarisk::K$db_name)
# df$date <- as.character(df$date)
# DBI::dbWriteTable(db, 'SP_EXPECT', df, append=FALSE)
# DBI::dbDisconnect(db)

