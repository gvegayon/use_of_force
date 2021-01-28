library(data.table)
library(survival)

model_data <- fread("data/model_data.csv")

model_data[, lapply(.SD, function(i) sum(is.na(i)))]

# Summary stats
summarizer <- function(x, prop = FALSE, f = mean, digits = 2) {
  if (prop)
    sprintf(
      sprintf("%%d/%%d (%%.%df%%%%)", digits),
      sum(x, na.rm = TRUE), sum(!is.na(x)), f(x, na.rm = TRUE)*100
      )
  else
    sprintf(
      sprintf("%%.%df (%%.2f)", digits),
      f(x, na.rm = TRUE), sd(x, na.rm = TRUE)
      )
}
sstats <- model_data[,
                     .(
                       "Sex = Male"                      = summarizer(officer_male, TRUE),
                       "Years of Exp."                   = summarizer(officer_nyears, FALSE),
                       "Exposure (direct)"               = summarizer(exposure_d > 0, TRUE),
                       "Exposure (direct) cumulative"    = summarizer(exposure_d_cum, FALSE),
                       "Exposure (indirect)"             = summarizer(exposure_i > 0, TRUE),
                       "Exposure (indirect) cumulative"  = summarizer(exposure_i_cum, FALSE),
                       "Race = White"                    = summarizer(officer_race == "white", TRUE),
                       "Police Officer = Yes"            = summarizer(officer_po, TRUE),
                       "N Officers"                      = summarizer(nofficers, FALSE),
                       "N Events"                        = summarizer(nevents, FALSE),
                       "N observations"                  = .N
                     ), by = firearm_pointed
]

sstats <- t(sstats)
colnames(sstats) <- c("No", "Yes")
# rownames(sstats)[1] <- c("Firearm pointed")
# sstats[1,] <- c("No", "Yes")


library(xtable)
print(xtable(sstats[-1,]), booktabs = TRUE, file = "figures/summary-stats.tex")

# saveRDS(sstats, file = "models/clogit_descriptive_stats.rds")

# Comparing raw differences (prop tests)
model_data[, prop.test(table(firearm_pointed, officer_male == 1))]
model_data[, wilcox.test(officer_nyears ~ firearm_pointed)] # Not normal, so need to assume wilcox
model_data[, prop.test(table(firearm_pointed, exposure_d > 0))]
model_data[, wilcox.test(exposure_d_cum ~ firearm_pointed)]
model_data[, prop.test(table(firearm_pointed, exposure_i > 0))]
model_data[, wilcox.test(exposure_i_cum ~ firearm_pointed)]

model_data[, prop.test(table(firearm_pointed, officer_race == "white"))]
model_data[, prop.test(table(firearm_pointed, officer_po))]
model_data[, wilcox.test(nofficers ~ firearm_pointed, alternative = "two.sided")] # Not normal, so need to assume wilcox
model_data[, wilcox.test(nevents ~ firearm_pointed, alternative = "two.sided")] # Not normal, so need to assume wilcox

# 
# 2-sample test for equality of proportions with continuity correction
# 
# data:  table(firearm_pointed, officer_male == 1)
# X-squared = 4.0764e-30, df = 1, p-value = 1
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   -0.02866569  0.02483592
# sample estimates:
#   prop 1     prop 2 
# 0.01798561 0.01990050 
# 
# Warning message:
#   In prop.test(table(firearm_pointed, officer_male == 1)) :
#   Chi-squared approximation may be incorrect
# > model_data[, wilcox.test(officer_nyears ~ firearm_pointed)] # Not normal, so need to assume wilcox
# 
# Wilcoxon rank sum test with continuity correction
# 
# data:  officer_nyears by firearm_pointed
# W = 27166, p-value = 0.6043
# alternative hypothesis: true location shift is not equal to 0
# 
# > model_data[, prop.test(table(firearm_pointed, exposure_d > 0))]
# 
# 2-sample test for equality of proportions with continuity correction
# 
# data:  table(firearm_pointed, exposure_d > 0)
# X-squared = 0.14756, df = 1, p-value = 0.7009
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   -0.04050493  0.06853027
# sample estimates:
#   prop 1    prop 2 
# 0.9244604 0.9104478 
# 
# > model_data[, wilcox.test(exposure_d_cum ~ firearm_pointed)]
# 
# Wilcoxon rank sum test with continuity correction
# 
# data:  exposure_d_cum by firearm_pointed
# W = 26252, p-value = 0.1525
# alternative hypothesis: true location shift is not equal to 0
# 
# > model_data[, prop.test(table(firearm_pointed, exposure_i > 0))]
# 
# 2-sample test for equality of proportions with continuity correction
# 
# data:  table(firearm_pointed, exposure_i > 0)
# X-squared = 3.6366, df = 1, p-value = 0.05652
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   -0.003133466  0.162981707
# sample estimates:
#   prop 1    prop 2 
# 0.7913669 0.7114428 
# 
# > model_data[, wilcox.test(exposure_d_cum ~ firearm_pointed)]
# 
# Wilcoxon rank sum test with continuity correction
# 
# data:  exposure_d_cum by firearm_pointed
# W = 26252, p-value = 0.1525
# alternative hypothesis: true location shift is not equal to 0

# Visualizations
library(ggplot2)
library(magrittr)
tmp <- rbind(
  # model_data[, .(firearm_pointed, x = officer_male, var = "officer_male")],
  # model_data[, .(firearm_pointed, x = exposure_i, var = "exposure_i")],
  # model_data[, .(firearm_pointed, x = exposure_d, var = "exposure_d")],
  model_data[, .(firearm_pointed, x = exposure_i_cum, var = "exposure_i_cum")],
  model_data[, .(firearm_pointed, x = exposure_d_cum, var = "exposure_d_cum")],
  model_data[, .(firearm_pointed, x = officer_nyears, var = "officer_nyears")],
  model_data[, .(firearm_pointed, x = nevents, var = "nevents")]
)
ggplot(tmp, aes(x, group = firearm_pointed)) + 
  geom_histogram(
    aes(y=0.5*..density..), #,fill = as.factor(firearm_pointed)),
    position="identity",
    alpha = .5,
    binwidth = 1
  ) + facet_wrap(vars(var))
  

ggplot(model_data, aes(nevents)) + 
  geom_histogram(
    aes(y=0.5*..density..,fill = as.factor(firearm_pointed)),
    position="identity",
    alpha = .5,
    binwidth = 1
    )
   # +
  facet_wrap(vars(as.factor(firearm_pointed)))
