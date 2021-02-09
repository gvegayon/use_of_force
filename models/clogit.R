library(data.table)
library(survival)
library(njforce)

model_data <- fread("data/model_data.csv", na.strings = "")
model_data[, officer_nyears_sd := officer_nyears/sd(officer_nyears, TRUE)]
ncpus      <- 3L
nperms     <- 2000L

# Dummy exposure ---------------------------------------------------------------

# Exposure direct
model_direct <- firearm_pointed ~
  I(as.integer(exposure_d > 0)) +
  nevents +
  officer_male +
  officer_nyears +
  officer_nyears2 + 
  alters_cum +
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

# Exposure indirect
model_indirect <- firearm_pointed ~
  I(as.integer(exposure_i > 0)) +
  nevents + 
  officer_male +
  officer_nyears +
  officer_nyears2 + 
  alters_cum +
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

# Cumuilative exposure ---------------------------------------------------------

# Previous use
model_direct_cum <- firearm_pointed ~
  exposure_d_cum +
  nevents +
  officer_male +
  officer_nyears +
  officer_nyears2 + 
  alters_cum +
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

model_indirect_cum <- firearm_pointed ~
  exposure_i_cum +
  nevents +
  officer_male +
  officer_nyears +
  officer_nyears2 + 
  alters_cum +
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

# Dummy exposure (without nevents)----------------------------------------------

# Exposure direct
model_direct_no_nevents <- firearm_pointed ~
  I(as.integer(exposure_d > 0)) +
  # nevents +
  officer_male +
  officer_nyears +
  officer_nyears2 + 
  alters_cum +
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

# Exposure indirect
model_indirect_no_nevents <- firearm_pointed ~
  I(as.integer(exposure_i > 0)) +
  # nevents + 
  officer_male +
  officer_nyears +
  officer_nyears2 + 
  alters_cum +
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

# Cumuilative exposure (without nevents)----------------------------------------

# Previous use
model_direct_cum_no_nevents <- firearm_pointed ~
  exposure_d_cum +
  # nevents +
  officer_male +
  officer_nyears +
  officer_nyears2 + 
  alters_cum +
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

model_indirect_cum_no_nevents <- firearm_pointed ~
  exposure_i_cum +
  # nevents +
  officer_male +
  officer_nyears +
  officer_nyears2 + 
  alters_cum +
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

# Model with interaction on the number of days since
# Previous use
model_direct_interact <- firearm_pointed ~
  I(as.integer(exposure_d > 0)) +
  I(as.integer(exposure_d > 0) * days_since_exp_d) +
  nevents +
  officer_male +
  officer_nyears +
  officer_nyears2 + 
  alters_cum +
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

model_indirect_interact <- firearm_pointed ~
  I(as.integer(exposure_i > 0)) +
  I(as.integer(exposure_i > 0) * days_since_exp_i) +
  nevents +
  officer_male +
  officer_nyears +
  officer_nyears2 + 
  alters_cum +
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

# Final model ------------------------------------------------------------------

model_indirect_final <- firearm_pointed ~
  I(as.integer(exposure_i > 0)) +
  exposure_i_cum +
  nevents +
  officer_male +
  officer_nyears +
  officer_nyears2 + 
  alters_cum +
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

fitter <- function(nperms, model, dat, ncpus, seed = 11231) {
  
  # Retrieving the model name
  mname <- match.call()$model
  
  # Checking if it existst
  fn_model <- file.path("models", paste0(mname,".rds"))
  if (!file.exists(fn_model)) {
    
    message("Fitting the model ", mname, "...", appendLF = FALSE)
    set.seed(seed)
    ans <- clogit_perm(nperms, model, dat = dat, ncpus = ncpus)
    message("Done!")
    saveRDS(ans, fn_model)
    
  } else {
    message("Model already fitted, loading...")
    ans <- readRDS(fn_model)
  }
  
  assign(paste0("ans_", mname), value = ans, envir = .GlobalEnv)
  invisible()
  
}

fitter(nperms, model_direct, dat = model_data, ncpus = ncpus)
fitter(nperms, model_indirect, dat = model_data, ncpus = ncpus)
fitter(nperms, model_direct_cum, dat = model_data, ncpus = ncpus)
fitter(nperms, model_indirect_cum, dat = model_data, ncpus = ncpus)
fitter(nperms, model_direct_no_nevents, dat = model_data, ncpus = ncpus)
fitter(nperms, model_indirect_no_nevents, dat = model_data, ncpus = ncpus)
fitter(nperms, model_direct_cum_no_nevents, dat = model_data, ncpus = ncpus)
fitter(nperms, model_indirect_cum_no_nevents, dat = model_data, ncpus = ncpus)
fitter(nperms, model_direct_interact, dat = model_data, ncpus = ncpus)
fitter(nperms, model_indirect_interact, dat = model_data, ncpus = ncpus)
fitter(nperms, model_indirect_final, dat = model_data, ncpus = ncpus)

varnames <- list(
  'firearm_pointed',
  firearm_pointed_prev = "Firearm pointed (t-1)",
  'I(as.integer(exposure_d > 0) * officer_nyears_sd)' = "Exposure (direct) x Years of Exp.",
  'I(as.integer(exposure_i > 0) * officer_nyears_sd)' = "Exposure (indirect) x Years of Exp.",
  'I(exposure_d * officer_nyears_sd)' = "Exposure (direct) x Years of Exp.",
  'I(exposure_i * officer_nyears_sd)' = "Exposure (indirect) x Years of Exp.",
  'I(as.integer(exposure_d > 0))' = "Exposure (direct)",
  'exposure_d_cum' = "Exposure (direct) cumulative",
  'I(as.integer(exposure_i > 0))' = "Exposure (indirect)",
  'exposure_i_cum' = "Exposure (indirect) cumulative",
  'officer_male' = "Sex = Male",
  'nevents' = "N of Events",
  'officer_nyears' = "Years of Exp.",
  'officer_nyears2' = "Decades of Exp.^2",
  'officer_po' = "Police Officer",
  'I(officer_race == "white")TRUE' = "Race = White",
  'strata(caseid)',
  "relative_exp" = "Relative Exp.",
  alters_cum  = "Total number of alters",
  "I(as.integer(exposure_d > 0) * days_since_exp_d)" = "Exposure (direct) x days since",
  "I(as.integer(exposure_i > 0) * days_since_exp_i)" = "Exposure (indirect) x days since"
)

saveRDS(
  list(
    models   = mget(ls(pattern = "^ans_.+$")),
    labels   = varnames
    ), file = "models/clogit.rds"
)

models <- readRDS("models/clogit.rds")
invisible(lapply(models$models, print, labels = models$labels))

# model_data[
#   setdiff(1:nrow(model_data), ans_joint$fit$na.action)][
#     , chisq.test(table(exposure_i > 0, firearm_pointed))
#     ]
# 
# model_data[
#   setdiff(1:nrow(model_data), ans_direct$fit$na.action)][
#     , #chisq.test(
#       table(exposure_i > 0, officer_po)
#       #)
#   ]

op <- par(mai = par("mai") * c(.5, .5, .5, .5), mfrow = c(2,2))
with(models, plot(ans_model_direct, labels = varnames))
with(models, plot(ans_model_direct_cum, labels = varnames))
with(models, plot(ans_model_indirect, labels = varnames))
with(models, plot(ans_model_indirect_cum, labels = varnames))
par(op)

op <- par(mai = par("mai") * c(.5, .5, .5, .5), mfrow = c(2,2))
with(models, plot(models$ans_direct_no_nevents, labels = varnames))
with(models, plot(models$ans_direct_cum_no_nevents, labels = varnames))
with(models, plot(models$ans_indirect_no_nevents, labels = varnames))
with(models, plot(models$ans_indirect_cum_no_nevents, labels = varnames))
par(op)

op <- par(mai = par("mai") * c(.75, .5, .5, .5), mfrow = c(2,2))
plot(ans_model_indirect, labels = varnames)
plot(ans_model_indirect_cum, labels = varnames)
plot(ans_model_indirect_final, labels = varnames)
par(op)

print(ans_indirect_final, labels = varnames)
