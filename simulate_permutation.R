#' This script analyzes what are the power levels of our analysis under
#' different circumstances of data generating process.

nexperiments <- 1000
nobs         <- 700
nperm        <- 5000

datagen <- function(
  nobs
  ) {
  
  x0 <- sample(rep(c(0, 1), nobs/2))
  x1 <- rnorm(nobs)
  y <- as.integer(
    runif(nobs) < plogis(-.5 + x0*.5 - x1 * .5)
  )
  
  data.frame(cbind(y, x0, x1, x2 = rnorm(nobs)))
}

set.seed(123)

# Is it giving the expected results
d <- datagen(1e4)
summary(glm(y ~ x0 + x1 + x2, family = binomial("logit"), data=d))

#' Permutation test
#' @param nobs Number of observations to simulate
#' @param nperm Number of permutations to perform
#' @param condition Logical. When `TRUE` uses a conditional permutation test
#' otherwise it is unconditional.
permutation_test <- function(nobs, nperm, conditional = TRUE) {
  # Generating random data and indentifying the candidates
  d <- datagen(nobs)
  candidates <- find_candidates(as.matrix(d)[,2,drop=FALSE], upper = 0, lower=0, as_abs = TRUE)
  
  # Permutations
  permutations <- if (conditional) {
    replicate(nperm, permute(candidates), simplify = FALSE)
  } else {
    replicate(nperm, sample.int(nrow(d)) - 1, simplify = FALSE)
  }
  
  # Running the analysis
  res <- lapply(permutations, function(p) {
    dtmp <- d
    dtmp[,1] <- dtmp[p + 1, 1]
    coef(glm(
      y ~ x0 + x1 + x2, data = dtmp, family = binomial("logit")
    ))
  })
  res <- do.call(rbind, res)
  
  return(
    list(
      perm     = res,
      baseline = coef(glm(y ~ x0 + x1 + x2, data = d, family = binomial("logit")))
    )
  )
  
}

# Setting up the cluster
library(slurmR)
cl <- slurmR::makeSlurmCluster(100, job_name = "permutation-george")
# cl <- parallel::makePSOCKcluster(4)

invisible(parallel::clusterEvalQ(cl, {
  library(njforce)
}))
parallel::clusterExport(cl, c("datagen", "nobs", "nperm", "permutation_test"))
parallel::clusterSetRNGStream(cl, 12331)

ans_conditional <- parallel::parLapply(cl, 1:nexperiments, function(i) {
  permutation_test(nobs, nperm, conditional = TRUE)
})

ans_unconditional <- parallel::parLapply(cl, 1:nexperiments, function(i) {
  permutation_test(nobs, nperm, conditional = FALSE)
})

parallel::stopCluster(cl)

# # Visualizing
pvals_conditional <- lapply(ans_conditional, function(a) {
  pvals <- rowMeans(t(a$perm) < a$baseline)
  ifelse(pvals > .5, 1 - pvals, pvals) * 2
})

pvals_unconditional <- lapply(ans_unconditional, function(a) {
  pvals <- rowMeans(t(a$perm) < a$baseline)
  ifelse(pvals > .5, 1 - pvals, pvals) * 2
})

# Times significant
colMeans(do.call(rbind, pvals_conditional) < .05)
colMeans(do.call(rbind, pvals_unconditional) < .05)

ans <- list(conditional = ans_conditional, unconditional = ans_unconditional)
saveRDS(ans, "~/permutation.rds", compress = FALSE)

# > # Times significant
#   > colMeans(do.call(rbind, pvals) < .05)
# # When using unconditional permutation
# (Intercept)          x0          x1          x2 
#        0.35        0.35        0.90        0.00 
