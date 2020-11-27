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
                       "Sex = Male"           = summarizer(officer_male, TRUE),
                       "Years of Exp."        = summarizer(officer_nyears, FALSE),
                       "Exposure (direct)"    = summarizer(exposure_i > 0, TRUE),
                       "Exposure (indirect)"  = summarizer(exposure_d > 0, TRUE),
                       "Race = White"         = summarizer(officer_race == "white", TRUE),
                       "Police Officer = Yes"       = summarizer(officer_po, TRUE),
                       # "Special Law Enforce." = summarizer(officer_sleo, FALSE, sum),
                       "N Officers"           = summarizer(nofficers, FALSE),
                       "N Events"             = summarizer(nevents, FALSE),
                       "N observations"       = .N
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
model_data[, prop.test(table(firearm_pointed, exposure_i > 0))]
model_data[, prop.test(table(firearm_pointed, exposure_d > 0))]
model_data[, prop.test(table(firearm_pointed, officer_race == "white"))]
model_data[, prop.test(table(firearm_pointed, officer_po))]
model_data[, wilcox.test(nofficers ~ firearm_pointed, alternative = "two.sided")] # Not normal, so need to assume wilcox
model_data[, wilcox.test(nevents ~ firearm_pointed, alternative = "two.sided")] # Not normal, so need to assume wilcox


