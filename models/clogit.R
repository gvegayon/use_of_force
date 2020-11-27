library(data.table)
library(survival)
library(njforce)

model_data <- fread("data/model_data.csv")

# Models
model_a1 <- firearm_pointed ~
  I(as.integer(exposure_i > 0)) +
  officer_male +
  nevents +
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

model_a2 <- firearm_pointed ~
  I(as.integer(exposure_i > 0)) +
  officer_male +
  nevents +
  officer_nyears + 
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

model_a3 <- firearm_pointed ~
  I(as.integer(exposure_i > 0)) +
  officer_male +
  nevents +
  I(officer_nyears^2) + 
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

model_a4 <- firearm_pointed ~
  I(as.integer(exposure_i > 0)) +
  officer_male +
  nevents +
  I(officer_nyears^2) + 
  officer_po +
  I(officer_race == "white") +
  relative_exp + 
  strata(caseid)

model_b1 <- firearm_pointed ~
  I(as.integer(exposure_d > 0)) +
  officer_male +
  nevents +
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

model_b2 <- firearm_pointed ~
  I(as.integer(exposure_d > 0)) +
  officer_male +
  nevents +
  officer_nyears + 
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

model_b3 <- firearm_pointed ~
  I(as.integer(exposure_d > 0)) +
  officer_male +
  nevents +
  I(officer_nyears^2) + 
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

model_b4 <- firearm_pointed ~
  I(as.integer(exposure_d > 0)) +
  officer_male +
  nevents +
  I(officer_nyears^2) + 
  officer_po +
  I(officer_race == "white") +
  relative_exp + 
  strata(caseid)


set.seed(77173)

ans_a1_perm <- clogit_perm(2000, model_a1, dat = model_data, ncpus = 4)
ans_a2_perm <- clogit_perm(2000, model_a2, dat = model_data, ncpus = 4)
ans_a3_perm <- clogit_perm(2000, model_a3, dat = model_data, ncpus = 4)
ans_a4_perm <- clogit_perm(2000, model_a4, dat = model_data, ncpus = 4)
ans_b1_perm <- clogit_perm(2000, model_b1, dat = model_data, ncpus = 4)
ans_b2_perm <- clogit_perm(2000, model_b2, dat = model_data, ncpus = 4)
ans_b3_perm <- clogit_perm(2000, model_b3, dat = model_data, ncpus = 4)
ans_b4_perm <- clogit_perm(2000, model_b4, dat = model_data, ncpus = 4)

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
  'strata(caseid)',
  "relative_exp" = "Relative Exp."
)

texreg::screenreg(list(
  ans_a1_perm, ans_a2_perm, ans_a3_perm, ans_a4_perm,
  ans_b1_perm, ans_b2_perm, ans_b3_perm, ans_b4_perm
), stars = c(0.01, 0.05, 0.1),
custom.coef.map = varnames)


saveRDS(
  list(models = list(
    ans_a1_perm, ans_a2_perm, ans_a3_perm,
    ans_b1_perm, ans_b2_perm, ans_b3_perm
  ), labels = varnames), file = "models/clogit_permutation.rds"
)
