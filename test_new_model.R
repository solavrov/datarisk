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

t1 <- c('SBER', 'JPM', 'BTC', 'TSLA', 'IVV')

ps <- colSums(ss[t1, ]) / 5
pb <- colSums(sb[t1, ]) / 5

quantile(ps, 0.05)
quantile(pb, 0.05)

quantile(ps, 0.01)
quantile(pb, 0.01)




