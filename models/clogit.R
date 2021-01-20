library(data.table)
library(survival)
library(njforce)

model_data <- fread("data/model_data.csv", na.strings = "")
model_data[, officer_nyears2 := (officer_nyears/10) ^ 2]
model_data[, officer_nyears_sd := officer_nyears/sd(officer_nyears, TRUE)]
ncpus      <- 3L
nperms     <- 4000L

# Dummy exposure ---------------------------------------------------------------

# Exposure direct
model_direct <- firearm_pointed ~
  I(as.integer(exposure_i > 0)) +
  # I(as.integer(exposure_i > 0) * officer_nyears_sd) +
  officer_male +
  # nevents +
  officer_nyears +
  officer_nyears2 + 
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

# Exposure indirect
model_indirect <- firearm_pointed ~
  I(as.integer(exposure_d > 0)) +
  # I(as.integer(exposure_d > 0) * officer_nyears_sd) +
  # nevents +
  officer_male +
  officer_nyears +
  officer_nyears2 + 
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

model_joint <- firearm_pointed ~
  # I(as.integer(exposure_d > 0)) +
  I(as.integer(exposure_i > 0)) +
  # I(as.integer(exposure_d > 0) * officer_nyears_sd) +
  # nevents +
  # officer_male +
  officer_nyears +
  officer_nyears2 + 
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

set.seed(77173)
ans_direct <- clogit_perm(nperms, model_direct, dat = model_data, ncpus = ncpus)

set.seed(77173)
ans_indirect <- clogit_perm(nperms, model_indirect, dat = model_data, ncpus = ncpus)

set.seed(77173)
ans_joint <- clogit_perm(nperms, model_joint, dat = model_data, ncpus = ncpus)


# Count exposure ---------------------------------------------------------------

# Exposure direct
model_direct_nevents <- firearm_pointed ~
  I(exposure_i > 0) +
  # I(exposure_i * officer_nyears_sd) +
  officer_male +
  nevents +
  # officer_nyears +
  # officer_nyears2 + 
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

# Exposure indirect
model_indirect_nevents <- firearm_pointed ~
  I(exposure_d > 0) +
  nevents +
  # officer_nyears +
  # officer_nyears2 + 
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

set.seed(77173)
# ans_direct_nevents <- clogit_perm(nperms, model_direct_nevents, dat = model_data, ncpus = ncpus)

set.seed(77173)
# ans_indirect_nevents <- clogit_perm(nperms, model_indirect_nevents, dat = model_data, ncpus = ncpus)

varnames <- list(
  'firearm_pointed',
  'I(as.integer(exposure_i > 0) * officer_nyears_sd)' = "Exposure (direct) x Years of Exp.",
  'I(as.integer(exposure_d > 0) * officer_nyears_sd)' = "Exposure (indirect) x Years of Exp.",
  'I(exposure_i * officer_nyears_sd)' = "Exposure (direct) x Years of Exp.",
  'I(exposure_d * officer_nyears_sd)' = "Exposure (indirect) x Years of Exp.",
  'I(as.integer(exposure_i > 0))' = "Exposure (direct)",
  'exposure_i' = "Exposure (direct) total",
  'I(as.integer(exposure_d > 0))' = "Exposure (indirect)",
  'exposure_d' = "Exposure (indirect) total",
  'officer_male' = "Sex = Male",
  'nevents' = "N of Events",
  'I(nevents^2)' = "N of Events ^ 2",
  'officer_nyears' = "Years of Exp.",
  'officer_nyears2' = "Years of Exp.^2",
  'officer_po' = "Police Officer",
  'I(officer_race == "white")TRUE' = "Race = White",
  'strata(caseid)',
  "relative_exp" = "Relative Exp."
)

saveRDS(
  list(
    models   = mget(ls(pattern = "^ans_.+$")),
    labels   = varnames
    ), file = "models/clogit.rds"
)

mget(ls(pattern = "^ans_.+$"))

model_data[
  setdiff(1:nrow(model_data), ans_joint$fit$na.action)][
    , chisq.test(table(exposure_i > 0, firearm_pointed))
    ]

model_data[
  setdiff(1:nrow(model_data), ans_direct$fit$na.action)][
    , #chisq.test(
      table(exposure_i > 0, officer_po)
      #)
  ]
