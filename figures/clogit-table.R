library(texreg)
library(mc3logit)

models <- readRDS("models/clogit2.rds")


# screenreg(
#   l = models$models[c("ans_a2_perm" ,"ans_b2_perm" ,"ans_a2_total_perm" ,"ans_b2_total_perm")],
#   custom.coef.map = models$labels,
#   stars = c(.01, .05, .1),
paste(
  "Conditional Logit Estimates with Permutation. Standard errors may be invalid due to",
  "possible contagion (or anti-contagion) effects.",
  "The last column corresponds to a logistic regression."
) #, odds = TRUE, ci.force = TRUE, single.row = TRUE)

print(models$models$ans_model_direct_cum, labels = models$labels, out="ascii")
print(models$models$ans_model_indirect_cum, labels = models$labels, out="ascii")
# print(models$models$ans_joint, labels = models$labels, out="tex")

# model_data[
#   setdiff(1:.N, models$models$ans_direct$fit$na.action),
#   table(exposure_d)
#   ]
# 
# model_data[, table(exposure_i > 0, exposure_d > 0)]
# 
# models$models

graphics.off()

svg("figures/clogit-plot-direct.svg")
plot(models$models$ans_model_direct_cum, labels = models$labels)
title("Direct Exposure")
dev.off()

svg("figures/clogit-plot-indirect.svg")
plot(models$models$ans_model_indirect_cum, labels = models$labels)
title("Indirect Exposure")
dev.off()

svg("figures/clogit-plot-direct-odds.svg")
plot(models$models$ans_model_direct_cum, labels = models$labels, odds = TRUE)
title("Direct Exposure")
dev.off()

svg("figures/clogit-plot-indirect-odds.svg")
plot(models$models$ans_model_indirect_cum, labels = models$labels, odds = TRUE)
title("Indirect Exposure")
dev.off()

# Null distribution confint ----------

svg("figures/clogit-plot-direct-nullconfint.svg")
plot(models$models$ans_model_direct_cum, labels = models$labels, which.="null")
title("Direct Exposure")
dev.off()

svg("figures/clogit-plot-indirect-nullconfint.svg")
plot(models$models$ans_model_indirect_cum, labels = models$labels, which.="null")
title("Indirect Exposure")
dev.off()

svg("figures/clogit-plot-direct-odds-nullconfint.svg")
plot(models$models$ans_model_direct_cum, labels = models$labels, which.="null", odds = TRUE)
title("Direct Exposure")
dev.off()

svg("figures/clogit-plot-indirect-odds-nullconfint.svg")
plot(models$models$ans_model_indirect_cum, labels = models$labels, which.="null", odds = TRUE)
title("Indirect Exposure")
dev.off()

graphics.off()
