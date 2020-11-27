library(texreg)
library(njforce)
models <- readRDS("models/clogit_permutation.rds")
screenreg(
  l = models$models,
  custom.coef.map = models$labels,
  stars = c(.01, .05, .1),
  caption = paste(
    "Conditional Logit Estimates with Permutation. Standard errors may be invalid due to",
    "possible contagion (or anti-contagion) effects.",
    "The last column corresponds to a logistic regression."
  ), odds = TRUE)


texreg::texreg(list(
  ans_a1_perm, ans_a2_perm, ans_a3_perm,
  ans_b1_perm, ans_b2_perm, ans_b3_perm
), 
  stars = c(.01, .05, .1),
  caption = paste(
    "Conditional Logit Estimates with permutation-based Confidence intervals."
  ),
custom.coef.map = varnames, ci.force=TRUE, single.row=TRUE, ci.test = NA)
