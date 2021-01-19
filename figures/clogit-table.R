library(texreg)
library(njforce)

models <- readRDS("models/clogit.rds")


# screenreg(
#   l = models$models[c("ans_a2_perm" ,"ans_b2_perm" ,"ans_a2_total_perm" ,"ans_b2_total_perm")],
#   custom.coef.map = models$labels,
#   stars = c(.01, .05, .1),
paste(
  "Conditional Logit Estimates with Permutation. Standard errors may be invalid due to",
  "possible contagion (or anti-contagion) effects.",
  "The last column corresponds to a logistic regression."
) #, odds = TRUE, ci.force = TRUE, single.row = TRUE)

print(models$models$ans_direct, labels = models$labels, out="tex")
print(models$models$ans_indirect, labels = models$labels, out="tex")

print(models$models$ans_direct_count, labels = models$labels, out="tex")
print(models$models$ans_indirect_count, labels = models$labels, out="tex")
