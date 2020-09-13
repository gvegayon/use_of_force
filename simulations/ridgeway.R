# Trying out Ridgeway (2016) equations for conditional logit

# This function computes the conditional likelihood for an arbitrary number of
# police officers. If sum(y) in {0, n} then the loglikelihood equals 0.
ll <- function(y, x, beta) {
  
  n1 <- sum(y)
  n <- length(y)
  
  if (n1 == n | n1 == 0)
    return(0)
  
  ans <- sum(x[which(y == 1), ] %*% beta)
  
  sets <- combn(seq_len(n), n1, simplify = FALSE)
  
  tmp <- 0
  for (s in sets)
    tmp <- tmp + exp(sum(x[s,] %*% beta))
  
  ans - log(tmp)
  
}

Pa <- -.2
Pb <- 0
Pc <- .2
Pd <- .5
Pe <- -.5

Z  <- 1

P <- 1/(1 + exp(-c(Pa, Pb, Pc, Pd, Pe) - Z))
nsim <- 2e6

experiments <- function(n = nsim) {
  
  matrix(as.integer(runif(5*n) < P), ncol = 5, byrow = TRUE)
  
}

set.seed(12381)
dat <- experiments()

# Ridgeway P(a=1, b=1, c=0 | a+b+c=2)
dat_i <- dat[, 1:3]
dat_i <- dat_i[rowSums(dat_i) == 2, ]

mean(dat_i[,1] == 1 & dat_i[,2] == 1 & dat_i[,3] == 0)
exp(Pa + Pb)/(
  exp(Pa + Pb) + exp(Pa + Pc) + exp(Pb + Pc)
)
exp(ll(c(1,1,0), cbind(c(Pa,Pb,Pc)), cbind(1)))

# Ridgeway P(a=1, b=0, c=0 | a+b+c=1)
dat_i <- dat[, 1:3]
dat_i <- dat_i[rowSums(dat_i) == 1, ]
mean(dat_i[,1] == 1 & dat_i[,2] == 0 & dat_i[,3] == 0)
exp(Pa)/(
  exp(Pa) + exp(Pb) + exp(Pc)
)

# Ridgeway P(a=0, b=1, c=0 | a+b+c=1)
dat_i <- dat[, 1:3]
dat_i <- dat_i[rowSums(dat_i) == 1, ]

mean(dat_i[,1] == 0 & dat_i[,2] == 1 & dat_i[,3] == 0)
exp(Pb)/(
  exp(Pa) + exp(Pb) + exp(Pc)
)

dat_i <- dat[, 1:3]
dat_i <- dat_i[rowSums(dat_i) == 1, ]
mean(dat_i[,1] == 0 & dat_i[,2] == 0 & dat_i[,3] == 1)
exp(Pc)/(
  exp(Pa) + exp(Pb) + exp(Pc)
)

# Looking at four ---------------------------------------------------
dat_i <- dat[, 1:4]
dat_i <- dat_i[rowSums(dat_i) == 2, ]

mean(dat_i[,1] == 1 & dat_i[,2] == 1 & dat_i[,3] == 0 & dat_i[,4] == 0)
exp(ll(c(1,1,0,0), cbind(c(Pa,Pb,Pc,Pd)), cbind(1)))

dat_i <- dat[, 1:4]
dat_i <- dat_i[rowSums(dat_i) == 3, ]

mean(dat_i[,1] == 1 & dat_i[,2] == 1 & dat_i[,3] == 0 & dat_i[,4] == 1)
exp(ll(c(1,1,0,1), cbind(c(Pa,Pb,Pc,Pd)), cbind(1)))

# Looking at five ---------------------------------------------------
dat_i <- dat[, 1:5]
dat_i <- dat_i[rowSums(dat_i) == 3, ]
mean(dat_i[,1] == 1 & dat_i[,2] == 1 & dat_i[,3] == 0 & dat_i[,4] == 0 & dat_i[,5]==1)
exp(ll(c(1,1,0,0,1), cbind(c(Pa,Pb,Pc,Pd,Pe)), cbind(1)))

