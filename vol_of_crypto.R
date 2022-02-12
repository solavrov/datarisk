r <- calc.returns('BTC')
r_we <- r[as.POSIXlt(r$date)$wday == 6 | as.POSIXlt(r$date)$wday == 0,]
r_wd <- r[as.POSIXlt(r$date)$wday != 6 & as.POSIXlt(r$date)$wday != 0,]
sd(r_wd$BTC)
sd(r_we$BTC)

r <- calc.returns('ETH')
r_we <- r[as.POSIXlt(r$date)$wday == 6 | as.POSIXlt(r$date)$wday == 0,]
r_wd <- r[as.POSIXlt(r$date)$wday != 6 & as.POSIXlt(r$date)$wday != 0,]
sd(r_wd$ETH)
sd(r_we$ETH)
