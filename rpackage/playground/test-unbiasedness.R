nexperiments <- 20
nobs         <- 500
nperm        <- 1000

datagen <- function(nobs) {
  x0 <- sample(rep(c(0, 1), nobs/2))
  x1 <- rnorm(nobs)
  y <- as.integer(
    runif(nobs) < plogis(-.5 + x0*.3 - x1 * .3)
  )
  
  data.frame(cbind(y, x0, x1, x2 = rnorm(nobs)))
}

set.seed(123)

# Is it giving the expected results
d <- datagen(1e3)
summary(glm(y ~ x0 + x1 + x2, family = binomial("logit"), data=d))

# Are the candidates drawn in an uniform fashion? To test for this, we compare this to a
# Binomial random variable, Y ~ Binom(nperm, 1/nmatches)
library(njforce)
p <- find_candidates(as.matrix(d[,2,drop=FALSE]), 0, 0, TRUE)
set.seed(131)
perm <- replicate(4e3, permute(p), simplify = FALSE)

perm <- do.call(rbind, perm)
tab  <- apply(perm, 2, table)
# tab  <- lapply(tab, prop.table)
tab <- lapply(tab, quantile, probs = c(.025, .5, .975))

boxplot(do.call(rbind, tab))
colMeans(do.call(rbind, tab))
qbinom(c(.025, .5, 1-.025), 4e3, 1/(500 - 1))
