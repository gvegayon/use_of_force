library(data.table)
library(survival)
library(mc3logit)

model_data <- fread("data/model_data_dyn.csv", na.strings = "")
model_data[, officer_nyears_sd := officer_nyears/sd(officer_nyears, TRUE)]
model_data <- model_data[clog_used == TRUE,]
ncpus      <- 3L
nperms     <- 2000L

table(
  model_data$exposure_d_cum -
  model_data$exposure_d_cum2
)

table(
  model_data$exposure_i_cum -
  model_data$exposure_i_cum2
)


# Cumuilative exposure ---------------------------------------------------------

# Previous use
model_direct_cum <- firearm_used ~
  exposure_d_cum2 +
  nevents +
  officer_male +
  officer_nyears +
  officer_nyears2 + 
  alters_cum +
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

model_indirect_cum <- firearm_used ~
  exposure_i_cum2 +
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
  # fn_model <- file.path("models", paste0(mname,".rds"))
  # if (!file.exists(fn_model)) {
    
    message("Fitting the model ", mname, "...", appendLF = FALSE)
    set.seed(seed)
    ans <- clogit_perm(nperms, model, dat = dat, ncpus = ncpus)
    message("Done!")
    # saveRDS(ans, fn_model)
    
  # } else {
  #   message("Model already fitted, loading...")
  #   ans <- readRDS(fn_model)
  # }
  
  assign(paste0("ans_", mname), value = ans, envir = .GlobalEnv)
  invisible()
  
}

fitter(nperms, model_direct_cum, dat = model_data, ncpus = ncpus)
fitter(nperms, model_indirect_cum, dat = model_data, ncpus = ncpus)

varnames <- list(
  'firearm_used',
  firearm_used_prev = "Firearm pointed (t-1)",
  'I(as.integer(exposure_d > 0) * officer_nyears_sd)' = "Exposure (direct) x Years of Exp.",
  'I(as.integer(exposure_i > 0) * officer_nyears_sd)' = "Exposure (indirect) x Years of Exp.",
  'I(exposure_d * officer_nyears_sd)' = "Exposure (direct) x Years of Exp.",
  'I(exposure_i * officer_nyears_sd)' = "Exposure (indirect) x Years of Exp.",
  'I(as.integer(exposure_d > 0))' = "Exposure (direct)",
  'exposure_d_cum' = "Exposure (direct) cumulative",
  'exposure_d_cum2' = "Exposure (direct) cumulative (v2)",
  'I(as.integer(exposure_i > 0))' = "Exposure (indirect)",
  'exposure_i_cum' = "Exposure (indirect) cumulative",
  'exposure_i_cum2' = "Exposure (indirect) cumulative (v2)",
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
    ), file = "models/clogit2_dyn.rds"
)

models <- readRDS("models/clogit2_dyn.rds")
invisible(lapply(models$models, print, labels = models$labels))
lapply(models$models, confint, labels = models$labels)

op <- par(mai = par("mai") * c(.5, .5, .5, .5), mfrow = c(1,2))
with(models, plot(ans_model_direct_cum, labels = varnames))
with(models, plot(ans_model_indirect_cum, labels = varnames))
par(op)
