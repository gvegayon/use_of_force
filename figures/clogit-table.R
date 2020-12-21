library(texreg)
library(njforce)

models <- readRDS("models/clogit.rds")

screenreg(
  l = models$models[models$ok_cont],
  custom.coef.map = models$labels,
  stars = c(.01, .05, .1),
  caption = paste(
    "Conditional Logit Estimates with Permutation. Standard errors may be invalid due to",
    "possible contagion (or anti-contagion) effects.",
    "The last column corresponds to a logistic regression."
  ), odds = TRUE, ci.force = TRUE, single.row = TRUE)


pretty_table <- function(x, odds = TRUE) {
  
  cis <- t(confint(x))
  
  dat <- cbind(Coef = coef(x), cis, pval = x$pvals)
  if (odds)
    dat[,1:3] <- exp(dat[,1:3])
  
  rownames(dat) <- models$labels[rownames(dat)]
  
  dat <- sprintf(
    "%s & $%.2f^{%s}$ & $[%.2f, %.2f]$ & %s",
    rownames(dat),
    dat[,"Coef"],
    ifelse(dat[,4] <= .01, "***",
           ifelse(dat[,4] <= .05, "**\\hphantom{*}", 
                  ifelse(dat[,4] <= .1, "*\\hphantom{**}", "\\hphantom{***}"))),
    dat[,2],
    dat[,3],
    ifelse(dat[,4] <= .01, "$< 0.01$", sprintf("$\\hphantom{< }%.2f$",dat[,4]))
    )
  
  paste(dat, collapse = "\\\\\n")
  
}

cat(pretty_table(models$models$ans_a2_perm))
cat(pretty_table(models$models$ans_b2_perm))

cat(pretty_table(models$models$ans_a2_total_perm))
cat(pretty_table(models$models$ans_b2_total_perm))
