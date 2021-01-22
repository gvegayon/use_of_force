library(data.table)
library(survival)
library(njforce)

model_data <- fread("data/model_data.csv", na.strings = "")
model_data[, officer_nyears2 := (officer_nyears/10) ^ 2]
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
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

# Previous use
model_direct_cum <- firearm_pointed ~
  exposure_d_cum +
  nevents +
  officer_male +
  officer_nyears +
  officer_nyears2 + 
  officer_po +
  I(officer_race == "white") +
  strata(caseid)

model_indirect_cum <- firearm_pointed ~
  exposure_i_cum +
  nevents +
  officer_male +
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
ans_direct_cum <- clogit_perm(nperms, model_direct_cum, dat = model_data, ncpus = ncpus)

set.seed(77173)
ans_indirect_cum <- clogit_perm(nperms, model_indirect_cum, dat = model_data, ncpus = ncpus)

varnames <- list(
  'firearm_pointed',
  firearm_pointed_prev = "Firearm pointed (t-1)",
  'I(as.integer(exposure_d > 0) * officer_nyears_sd)' = "Exposure (direct) x Years of Exp.",
  'I(as.integer(exposure_i > 0) * officer_nyears_sd)' = "Exposure (indirect) x Years of Exp.",
  'I(exposure_d * officer_nyears_sd)' = "Exposure (direct) x Years of Exp.",
  'I(exposure_i * officer_nyears_sd)' = "Exposure (indirect) x Years of Exp.",
  'I(as.integer(exposure_d > 0))' = "Exposure (direct)",
  'exposure_d' = "Exposure (direct) total",
  'I(as.integer(exposure_i > 0))' = "Exposure (indirect)",
  'exposure_i' = "Exposure (indirect) total",
  'officer_male' = "Sex = Male",
  'nevents' = "N of Events",
  'officer_nyears' = "Years of Exp.",
  'officer_nyears2' = "Decades of Exp.^2",
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

plot.clogit_perm <- function(
  x,
  y             = NULL,
  level         = .95,
  col           = 1:length(stats::coef(x)),
  args_points   = list(pch = 19, cex = 1.5),
  args_arrows   = list(lwd = 2, code=3, angle=90, length = .1),
  labels        = NULL,
  ...
  ) {
  
  ci     <- stats::confint(x, level = level)
  ranges <- range(ci)
  ranges_extended <- ranges + c(- diff(ranges)*.75, 0)
  betas  <- stats::coef(x)
  
  # Reversing order
  betas <- rev(betas)
  ci    <- ci[,ncol(ci):1, drop=FALSE]
  
  ylims <- as.factor(colnames(ci))
  
  graphics::plot.new()
  graphics::plot.window(
    xlim = ranges_extended,
    ylim = c(1, length(ylims) + .5)
    )
  graphics::abline(v = 0, lty=2, lwd=1)
  do.call(
    graphics::arrows,
    c(
      args_arrows,
      list(
        x0 = ci[1,],
        x1 = ci[2,],
        y0 = 1:length(ylims),
        # y1 = as.integer(ylims),
        col = col
        )
      )
    )
  
  do.call(
    graphics::points,
    c(
      args_points,
      list(x = betas, y = 1:length(ylims), col = col)
      )
    )
  

  labs <- if (!is.null(labels))
    labels[as.character(ylims)]
  else
    as.character(ylims)

  graphics::text(
    x      = ranges_extended[1],
    y      = 1:length(ylims) + .25,
    labels = labs,
    pos    = 4,
    offset = -.5
  )
  
  loc <- pretty(ranges)
  graphics::axis(1, labels = loc, at=loc)
  
}

op <- par(mai = par("mai") * c(.5, .5, .5, .5), mfrow = c(2,2))
with(models, plot(models$ans_direct, labels = varnames))
with(models, plot(models$ans_direct_cum, labels = varnames))
with(models, plot(models$ans_indirect, labels = varnames))
with(models, plot(models$ans_indirect_cum, labels = varnames))
par(op)
