rfr <- bbg.rfr()

# USD
mod1 <- calc.model('USD', rfr)

max(abs(mod1$er['USD'] + mod1$cov['IVV',] / mod1$cov['IVV', 'IVV'] * (mod1$er['IVV'] - mod1$er['USD']) - mod1$er))

g1 <- mod1$covcc[rownames(mod1$covcc)!='USD',colnames(mod1$covcc)!='USD']
G1 <- mod1$cov[rownames(mod1$cov)!='USD',colnames(mod1$cov)!='USD']
er1 <- mod1$ercc[names(mod1$ercc)!='USD']
ER1 <- mod1$er[names(mod1$er)!='USD']

n <- 10^6
m <- nrow(g1)

X <- matrix(rnorm(m*n), nrow=m)
r1 <- t(chol(g1)) %*% X + er1

R1 <- (exp(r1/100) - 1) * 100

max(abs(er1 / rowMeans(r1) - 1))
max(abs(ER1 / rowMeans(R1) - 1))

max(abs(cov(t(r1)) / g1 - 1))
max(abs(cov(t(R1)) / G1 - 1))

# RUB
mod2 <- calc.model('RUB', rfr)

max(abs(mod2$ercc - mod1$ercc + mod1$ercc['RUB'] - mod2$ercc['RUB']))

g2 <- mod2$covcc[rownames(mod2$covcc)!='RUB',colnames(mod2$covcc)!='RUB']
G2 <- mod2$cov[rownames(mod2$cov)!='RUB',colnames(mod2$cov)!='RUB']
er2 <- mod2$ercc[names(mod2$ercc)!='RUB']
ER2 <- mod2$er[names(mod2$er)!='RUB']

n <- 10^6
m <- nrow(g2)

X <- matrix(rnorm(m*n), nrow=m)
r2 <- t(chol(g2)) %*% X + er2

R2 <- (exp(r2/100) - 1) * 100

max(abs(er2 / rowMeans(r2) - 1))
max(abs(ER2 / rowMeans(R2) - 1))

max(abs(cov(t(r2)) / g2 - 1))
max(abs(cov(t(R2)) / G2 - 1))

# distributions
n <- 10^3
m <- nrow(g1)

X <- matrix(rnorm(m*n), nrow=m)
r1 <- t(chol(g1)) %*% X + er1

R1 <- (exp(r1/100) - 1) * 100
hist(R1['SBER', ], breaks=20)


# test var for 1000 sample

rfr <- bbg.rfr()
ss <- calc.sample('USD', rfr, 1000)
sb <- calc.sample('USD', rfr, 10^7)

t1 <- c('SBER', 'JPM', 'BTC', 'TSLA', 'IVV') # high vol
t2 <- c('EUR', 'RUB', 'XAU', 'EFA', 'IVV') # low vol
t3 <- c('EUR', 'RUB', 'XAU') # low vol

l <- list(t1, t2, t3)

for (t0 in l) {
  ps <- colSums(ss[t0, ]) / length(t0)
  pb <- colSums(sb[t0, ]) / length(t0)

  cat(t0, '\n')

  cat('1000 5%:', quantile(ps, 0.05), '\n')
  cat('big 5%:', quantile(pb, 0.05), '\n\n')

  cat('1000 1%:', quantile(ps, 0.01), '\n')
  cat('big 1%:', quantile(pb, 0.01), '\n\n')
}


# test var lognormal approximation

rfr <- bbg.rfr()
mod <- calc.model('USD', rfr)
s <- calc.sample('USD', rfr, 10^7)

t1 <- c('SBER', 'JPM', 'BTC', 'TSLA', 'IVV') # high vol
t2 <- c('EUR', 'RUB', 'XAU', 'EFA', 'IVV') # low vol
t3 <- c('EUR', 'RUB', 'XAU') # low vol

l <- list(t1, t2, t3)

for (t0 in l) {
  R <- mean(mod$er[t0]) / 100
  G <- mod$cov[t0, t0] / 10000
  w <- rep(1, length(t0)) / length(t0)
  D <- t(w) %*% G %*% w

  d <- log(1 + D/(1 + R)^2)
  r <- log(1 + R) - d / 2

  cat(t0, '\n')

  cat('VaR lognorm:', (qnorm(0.05) * sqrt(d) + r) * 100, '\n')

  ps <- colSums(s[t0, ]) / length(t0)
  cat('VaR monte carlo', quantile(ps, 0.05), '\n\n')
}




