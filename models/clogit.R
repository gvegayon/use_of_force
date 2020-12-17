library(data.table)
library(survival)
library(njforce)

model_data <- fread("data/model_data.csv")
ncpus      <- 3L

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
  strata(caseid);

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

model_a5 <- firearm_pointed ~
  I(as.integer(exposure_i > 0)) +
  officer_male +
  officer_nyears +
  I(officer_nyears^2) + 
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

model_a6 <- firearm_pointed ~
  I(as.integer(exposure_i > 0)) +
  officer_male +
  nevents +
  I(officer_race == "white") +
  strata(caseid)

model_a7 <- firearm_pointed ~
  I(as.integer(exposure_i > 0)) +
  I(as.integer(exposure_i > 0) * nevents) +
  officer_male +
  nevents +
  I(officer_race == "white") +
  strata(caseid)

model_a8 <- firearm_pointed ~
  I(as.integer(exposure_i > 0)) +
  officer_male +
  nevents +
  I(nevents^2) +
  I(officer_race == "white") +
  strata(caseid)

# Exposure indirect

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

model_b5 <- firearm_pointed ~
  I(as.integer(exposure_d > 0)) +
  officer_male +
  officer_nyears +
  I(officer_nyears^2) + 
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

model_b6 <- firearm_pointed ~
  I(as.integer(exposure_d > 0)) +
  officer_male +
  nevents +
  I(officer_race == "white") +
  strata(caseid)

model_b7 <- firearm_pointed ~
  I(as.integer(exposure_d > 0)) +
  I(as.integer(exposure_d > 0) * nevents) +
  officer_male +
  nevents +
  I(officer_race == "white") +
  strata(caseid)

model_b8 <- firearm_pointed ~
  I(as.integer(exposure_d > 0)) +
  officer_male +
  nevents +
  I(nevents^2) +
  I(officer_race == "white") +
  strata(caseid)

set.seed(77173)

ans_a1_perm <- clogit_perm(2000, model_a1, dat = model_data, ncpus = ncpus)
ans_a2_perm <- clogit_perm(2000, model_a2, dat = model_data, ncpus = ncpus)
ans_a3_perm <- clogit_perm(2000, model_a3, dat = model_data, ncpus = ncpus)
ans_a4_perm <- clogit_perm(2000, model_a4, dat = model_data, ncpus = ncpus)
ans_a5_perm <- clogit_perm(2000, model_a5, dat = model_data, ncpus = ncpus)
ans_a6_perm <- clogit_perm(2000, model_a6, dat = model_data, ncpus = ncpus)
ans_a7_perm <- clogit_perm(2000, model_a7, dat = model_data, ncpus = ncpus)
ans_a8_perm <- clogit_perm(2000, model_a8, dat = model_data, ncpus = ncpus)

ans_b1_perm <- clogit_perm(2000, model_b1, dat = model_data, ncpus = ncpus)
ans_b2_perm <- clogit_perm(2000, model_b2, dat = model_data, ncpus = ncpus)
ans_b3_perm <- clogit_perm(2000, model_b3, dat = model_data, ncpus = ncpus)
ans_b4_perm <- clogit_perm(2000, model_b4, dat = model_data, ncpus = ncpus)
ans_b5_perm <- clogit_perm(2000, model_b5, dat = model_data, ncpus = ncpus)
ans_b6_perm <- clogit_perm(2000, model_b6, dat = model_data, ncpus = ncpus)
ans_b7_perm <- clogit_perm(2000, model_b7, dat = model_data, ncpus = ncpus)
ans_b8_perm <- clogit_perm(2000, model_b8, dat = model_data, ncpus = ncpus)

varnames <- list(
  'firearm_pointed',
  'I(as.integer(exposure_i > 0) * nevents)' = "Exposure (direct) x nevents",
  'I(as.integer(exposure_d > 0) * nevents)' = "Exposure (indirect) x nevents",
  'I(as.integer(exposure_i > 0))' = "Exposure (direct)",
  'exposure_i' = "Exposure (direct) total",
  'I(as.integer(exposure_d > 0))' = "Exposure (indirect)",
  'exposure_d' = "Exposure (indirect) total",
  'officer_male' = "Sex = Male",
  'nevents' = "N of Events",
  'I(nevents^2)' = "N of Events ^ 2",
  'officer_nyears' = "Years of Exp.",
  'I(officer_nyears^2)' = "Years of Exp.^2",
  'officer_po' = "Police Officer",
  'I(officer_race == "white")TRUE' = "Race = White",
  'strata(caseid)',
  "relative_exp" = "Relative Exp."
)

texreg::screenreg(
  mget(ls(pattern = "^ans_.+_perm$")),
  stars = c(0.01, 0.05, 0.1),
custom.coef.map = varnames)

# The following are not significant in any of the specfications:
# - officer_po
# - relative_exp
# - x nevents

models <- mget(ls(pattern = "^ans_.+perm$"))
tokeep <- sapply(models, function(m) {
  grepl("officer_po|relative_exp|\\* nevents", paste(m$formula, collapse = " "))
})

tokeep <- which(!tokeep)
texreg::screenreg(
  models[tokeep],
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = varnames)

model_a6_total <- firearm_pointed ~
  exposure_i +
  officer_male +
  nevents +
  I(officer_race == "white") +
  strata(caseid)

model_b6_total <- firearm_pointed ~
  exposure_d +
  officer_male +
  nevents +
  I(officer_race == "white") +
  strata(caseid)

model_a8_total <- firearm_pointed ~
  exposure_i +
  officer_male +
  nevents +
  I(nevents^2) +
  I(officer_race == "white") +
  strata(caseid)

model_b8_total <- firearm_pointed ~
  exposure_d +
  officer_male +
  nevents +
  I(nevents^2) +
  I(officer_race == "white") +
  strata(caseid)

ans_a6_total_perm <- clogit_perm(2000, model_a6_total, dat = model_data, ncpus = ncpus)
ans_b6_total_perm <- clogit_perm(2000, model_b6_total, dat = model_data, ncpus = ncpus)
ans_a8_total_perm <- clogit_perm(2000, model_a8_total, dat = model_data, ncpus = ncpus)
ans_b8_total_perm <- clogit_perm(2000, model_b8_total, dat = model_data, ncpus = ncpus)

texreg::screenreg(
  mget(ls(pattern = "^ans_.+_total_perm$")),
  stars = c(0.01, 0.05, 0.1),
  custom.coef.map = varnames)

saveRDS(
  list(
    models   = mget(ls(pattern = "^ans_.+_perm$")),
    labels   = varnames,
    ok_dummy = c("ans_a6_perm", "ans_a8_perm", "ans_b6_perm", "ans_b8_perm"),
    ok_cont  = c("ans_a6_total_perm", "ans_a8_total_perm", "ans_b6_total_perm", "ans_b8_total_perm")
    ), file = "models/clogit.rds"
)
