# Trying out Ridgeway (2016) equations for conditional logit

Pa <- -.2
Pb <- 0
Pc <- .2

Z  <- 1

P <- 1/(1 + exp(-c(Pa, Pb, Pc) - Z))
nsim <- 2e6

experiments <- function(n = nsim) {
  
  matrix(as.integer(runif(3*n) < P), ncol = 3, byrow = TRUE)
  
}

set.seed(12381)
dat <- experiments()

# Ridgeway P(a=1, b=1, c=0 | a+b+c=2)
dat_i <- dat[rowSums(dat) == 2, ]
mean(dat_i[,1] == 1 & dat_i[,2] == 1 & dat_i[,3] == 0)
exp(Pa + Pb)/(
  exp(Pa + Pb) + exp(Pa + Pc) + exp(Pb + Pc)
)

# Ridgeway P(a=1, b=0, c=0 | a+b+c=1)
dat_i <- dat[rowSums(dat) == 1, ]
mean(dat_i[,1] == 1 & dat_i[,2] == 0 & dat_i[,3] == 0)
exp(Pa)/(
  exp(Pa) + exp(Pb) + exp(Pc)
)

# Ridgeway P(a=0, b=1, c=0 | a+b+c=1)
dat_i <- dat[rowSums(dat) == 1, ]
mean(dat_i[,1] == 0 & dat_i[,2] == 1 & dat_i[,3] == 0)
exp(Pb)/(
  exp(Pa) + exp(Pb) + exp(Pc)
)
