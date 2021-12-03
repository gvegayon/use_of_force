library(data.table)
library(survival)

model_data <- fread("data/model_data.csv")
all_data <- fread("data-raw/njdatageorge200918.csv")

# Comparing full sample vs final sample ----------------------------------------

n_final <- NULL
n_origi <- NULL

# Totals
n_final <- c(n_final, "N observations" = nrow(model_data))
n_origi <- c(n_origi, "N observations" = nrow(all_data))

# N events
n_final <- c(n_final, "N Events" = model_data[, length(unique(caseid))])
n_origi <- c(n_origi, "N Events" = all_data[, length(unique(caseid))])

# Unique officers
n_final <- c(n_final, "N Officers" = model_data[, length(unique(officerid_num))])
n_origi <- c(n_origi, "N Officers" = all_data[, length(unique(officerid_num))])

# departments
model_data[, county_town := paste0(county, "-", town)]
all_data[, county_town := paste0(county, "-", town)]
n_final <- c(n_final, "N Departments" = model_data[, length(unique(county_town))])
n_origi <- c(n_origi, "N Departments" = all_data[, length(unique(county_town))])

# Counties
n_final <- c(n_final, "N Counties" = model_data[, length(unique(county))])
n_origi <- c(n_origi, "N Counties" = all_data[, length(unique(county))])

print(xtable::xtable(
  rbind("Comple sample" = n_origi, "Used sample" = n_final),
  label   = "tab:sample-size",
  caption = paste(
    "Complete vs used sample. For the analysis, we only included events in which",
    "there were two or more officers. Furthermore, ",
    "events in which none or all officers pointed their firearm",
    "were excluded."
  )
), booktabs=TRUE,
)

# Final sample -----------------------------------------------------------------


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
                       "Sex = Male"           = summarizer(officer_male, TRUE),
                       "Race = White"         = summarizer(officer_race == "white", TRUE),
                       "Police Officer = Yes" = summarizer(officer_po, TRUE),
                       "Years of Exp."        = summarizer(officer_nyears, FALSE),
                       "Decades of Exp.^2"    = summarizer(officer_nyears2, FALSE),
                       # "Exposure (direct)"               = summarizer(exposure_d > 0, TRUE),
                       "Exposure (direct)"    = summarizer(exposure_d_cum, FALSE),
                       # "Exposure (indirect)"             = summarizer(exposure_i > 0, TRUE),
                       "Exposure (indirect)"  = summarizer(exposure_i_cum, FALSE),
                       "N Officers"           = summarizer(nofficers, FALSE),
                       "N of peers"           = summarizer(alters_cum, FALSE),
                       "N Events"             = summarizer(nevents, FALSE),
                       "N observations"       = .N
                     ), by = firearm_used
]

sstats <- t(sstats)
colnames(sstats) <- c("No", "Yes")
# rownames(sstats)[1] <- c("Firearm pointed")
# sstats[1,] <- c("No", "Yes")


library(xtable)
print(xtable(sstats[-1,]), booktabs = TRUE, file = "figures/summary-stats2.tex")

# saveRDS(sstats, file = "models/clogit_descriptive_stats.rds")

# Comparing raw differences (prop tests)
model_data[, prop.test(table(firearm_used, officer_male == 1))]
model_data[, wilcox.test(officer_nyears ~ firearm_used)] # Not normal, so need to assume wilcox
model_data[, wilcox.test(officer_nyears2 ~ firearm_used)] # Not normal, so need to assume wilcox
# model_data[, prop.test(table(firearm_used, exposure_d > 0))]
model_data[, wilcox.test(exposure_d_cum ~ firearm_used)]
# model_data[, prop.test(table(firearm_used, exposure_i > 0))]
model_data[, wilcox.test(exposure_i_cum ~ firearm_used)]

model_data[, prop.test(table(firearm_used, officer_race == "white"))]
model_data[, prop.test(table(firearm_used, officer_po))]
model_data[, wilcox.test(nofficers ~ firearm_used, alternative = "two.sided")] # Not normal, so need to assume wilcox
model_data[, wilcox.test(nevents ~ firearm_used, alternative = "two.sided")] # Not normal, so need to assume wilcox
model_data[, wilcox.test(alters_cum ~ firearm_used, alternative = "two.sided")] # Not normal, so need to assume wilcox

# 
# 2-sample test for equality of proportions with continuity correction
# 
# data:  table(firearm_used, officer_male == 1)
# X-squared = 4.0764e-30, df = 1, p-value = 1
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   -0.02866569  0.02483592
# sample estimates:
#   prop 1     prop 2 
# 0.01798561 0.01990050 
# 
# Warning message:
#   In prop.test(table(firearm_used, officer_male == 1)) :
#   Chi-squared approximation may be incorrect
# > model_data[, wilcox.test(officer_nyears ~ firearm_used)] # Not normal, so need to assume wilcox
# 
# Wilcoxon rank sum test with continuity correction
# 
# data:  officer_nyears by firearm_used
# W = 27166, p-value = 0.6043
# alternative hypothesis: true location shift is not equal to 0
# 
# > model_data[, prop.test(table(firearm_used, exposure_d > 0))]
# 
# 2-sample test for equality of proportions with continuity correction
# 
# data:  table(firearm_used, exposure_d > 0)
# X-squared = 0.14756, df = 1, p-value = 0.7009
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   -0.04050493  0.06853027
# sample estimates:
#   prop 1    prop 2 
# 0.9244604 0.9104478 
# 
# > model_data[, wilcox.test(exposure_d_cum ~ firearm_used)]
# 
# Wilcoxon rank sum test with continuity correction
# 
# data:  exposure_d_cum by firearm_used
# W = 26252, p-value = 0.1525
# alternative hypothesis: true location shift is not equal to 0
# 
# > model_data[, prop.test(table(firearm_used, exposure_i > 0))]
# 
# 2-sample test for equality of proportions with continuity correction
# 
# data:  table(firearm_used, exposure_i > 0)
# X-squared = 3.6366, df = 1, p-value = 0.05652
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   -0.003133466  0.162981707
# sample estimates:
#   prop 1    prop 2 
# 0.7913669 0.7114428 
# 
# > model_data[, wilcox.test(exposure_d_cum ~ firearm_used)]
# 
# Wilcoxon rank sum test with continuity correction
# 
# data:  exposure_d_cum by firearm_used
# W = 26252, p-value = 0.1525
# alternative hypothesis: true location shift is not equal to 0

# Visualizations
library(ggplot2)
library(magrittr)
tmp <- rbind(
  # model_data[, .(firearm_used, x = officer_male, var = "officer_male")],
  # model_data[, .(firearm_used, x = exposure_i, var = "exposure_i")],
  # model_data[, .(firearm_used, x = exposure_d, var = "exposure_d")],
  model_data[, .(firearm_used, x = exposure_i_cum, var = "exposure_i_cum")],
  model_data[, .(firearm_used, x = exposure_d_cum, var = "exposure_d_cum")],
  model_data[, .(firearm_used, x = officer_nyears, var = "officer_nyears")],
  model_data[, .(firearm_used, x = nevents, var = "nevents")]
)
ggplot(tmp, aes(x, group = firearm_used)) + 
  geom_histogram(
    aes(y=0.5*..density..), #,fill = as.factor(firearm_used)),
    position="identity",
    alpha = .5,
    binwidth = 1
  ) + facet_wrap(vars(var))
  

ggplot(model_data, aes(nevents)) + 
  geom_histogram(
    aes(y=0.5*..density..,fill = as.factor(firearm_used)),
    position="identity",
    alpha = .5,
    binwidth = 1
    )
   # +
  facet_wrap(vars(as.factor(firearm_used)))

knitr::kable(data.table(prop.table(table(model_data$nevents))), digits=2)
