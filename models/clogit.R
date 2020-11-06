library(data.table)
library(survival)

model_data <- fread("data/model_data.csv")

model_data[, lapply(.SD, function(i) sum(is.na(i)))]

# Summary stats
summarizer <- function(x, prop = FALSE, f = mean) {
  if (prop)
    sprintf("%.2f%% (%.2f)", f(x, na.rm = TRUE)*100, sd(x, na.rm = TRUE))
  else
    sprintf("%.2f (%.2f)", f(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}
sstats <- model_data[,
  .(
    "Sex = Male"           = summarizer(officer_male, TRUE),
    "Years of Exp."        = summarizer(officer_nyears, FALSE),
    "Exposure (direct)"    = summarizer(exposure_i > 0, TRUE),
    "Exposure (indirect)"  = summarizer(exposure_d > 0, TRUE),
    "Race = White"         = summarizer(officer_race == "white"),
    "Police Officer"       = summarizer(officer_po, FALSE, sum),
    # "Special Law Enforce." = summarizer(officer_sleo, FALSE, sum),
    "N Officers"           = summarizer(nofficers, FALSE),
    "N Events"             = summarizer(nevents, FALSE),
    "N observations"       = .N
  ), by = firearm_pointed
  ]

sstats <- t(sstats)
rownames(sstats)[1] <- c("Firearm pointed")
sstats[1,] <- c("Yes", "No")

saveRDS(sstats, file = "models/clogit_descriptive_stats.rds")

# Comparing raw differences (prop tests)
model_data[, prop.test(table(firearm_pointed, exposure_i > 0))]
model_data[, prop.test(table(firearm_pointed, exposure_d > 0))]
model_data[, prop.test(table(firearm_pointed, officer_race == "white"))]
model_data[, t.test(nevents ~ firearm_pointed)]

# Models
model_a1 <- firearm_pointed ~ I(as.integer(exposure_i > 0)) + officer_male +
  nevents + officer_nyears + officer_po +
  I(officer_race == "white") + strata(caseid)

model_a2 <- firearm_pointed ~ I(as.integer(exposure_i > 0)) + officer_male +
  nevents + officer_nyears + I(officer_nyears^2) + officer_po +
  I(officer_race == "white") + strata(caseid)

model_a3 <- firearm_pointed ~ I(as.integer(exposure_i > 0)) + 
  nevents + officer_nyears + I(officer_nyears^2) + strata(caseid)

model_b1 <- firearm_pointed ~ I(as.integer(exposure_d > 0)) + officer_male +
  nevents + officer_nyears + officer_po +
  I(officer_race == "white") + strata(caseid)

model_b2 <- firearm_pointed ~ I(as.integer(exposure_d > 0)) + officer_male +
  nevents + officer_nyears + I(officer_nyears^2) + officer_po +
  I(officer_race == "white") + strata(caseid)

model_b3 <- firearm_pointed ~ I(as.integer(exposure_d > 0)) + 
  nevents + officer_nyears + I(officer_nyears^2) + strata(caseid)


ans_a1 <- clogit(model_a1, dat = model_data)
ans_a2 <- clogit(model_a2, dat = model_data)
ans_a3 <- clogit(model_a3, dat = model_data)
ans_b1 <- clogit(model_b1, dat = model_data)
ans_b2 <- clogit(model_b2, dat = model_data)
ans_b3 <- clogit(model_b3, dat = model_data)

ans_b3_logit <-  glm(
  update.formula(model_b3, .~.-strata(caseid)), dat = model_data,
  family = binomial("logit"))

varnames <- list(
  'firearm_pointed',
  'I(as.integer(exposure_i > 0))' = "Exposure (direct)",
  'I(as.integer(exposure_d > 0))' = "Exposure (indirect)",
  'officer_male' = "Sex = Male",
  'nevents' = "N of Events",
  'officer_nyears' = "Years of Exp.",
  'I(officer_nyears^2)' = "Years of Exp.^2",
  'officer_po' = "Police Officer",
  'I(officer_race == "white")TRUE' = "Race = White",
  'strata(caseid)'
)

texreg::screenreg(list(
  ans_a1, ans_a2, ans_a3,
  ans_b1, ans_b2, ans_b3,
  ans_b3_logit
), stars = c(0.01, 0.05, 0.1),
custom.coef.map = varnames)

saveRDS(
  list(models = list(
    ans_a1, ans_a2, ans_a3,
    ans_b1, ans_b2, ans_b3,
    ans_b3_logit
  ), labels = varnames), file = "models/clogit.rds"
)

# Permutation test -------------------------------------------------------------

library(njforce)
set.seed(1231)

model_data[, relative_exp := officer_nyears - mean(officer_nyears), by = caseid]
model_data[, relative_exp := which.max(officer_nyears) == 1:.N, by = caseid]

ans0_perm <- clogit_perm(
  2000,
  firearm_pointed ~ I(as.integer(exposure_any > 0)) + officer_male + 
    nevents + officer_nyears + officer_po + I(officer_race == "white") +
    strata(caseid) ,
  dat = model_data, ncpus = 4
  )

ans_a1_perm <- clogit_perm(2000, model_a1, dat = model_data, ncpus = 4)
ans_a2_perm <- clogit_perm(2000, model_a2, dat = model_data, ncpus = 4)
ans_a3_perm <- clogit_perm(2000, model_a3, dat = model_data, ncpus = 4)
ans_b1_perm <- clogit_perm(2000, model_b1, dat = model_data, ncpus = 4)
ans_b2_perm <- clogit_perm(2000, model_b2, dat = model_data, ncpus = 4)
ans_b3_perm <- clogit_perm(2000, model_b3, dat = model_data, ncpus = 4)


texreg::screenreg(
  l = list(
    ans_a1_perm, ans_a2_perm, ans_a3_perm,
    ans_b1_perm, ans_b2_perm, ans_b3_perm
  ), custom.coef.map = varnames
)

saveRDS(
  list(models = list(
    ans_a1_perm, ans_a2_perm, ans_a3_perm,
    ans_b1_perm, ans_b2_perm, ans_b3_perm
  ), labels = varnames), file = "models/clogit_permutation.rds"
)
