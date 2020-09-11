library(njforce)
library(data.table)

nevents   <- 700
nofficers <- 400


# Reading the data and identifying individuals and their first shooting event
njforce <- data.table::fread("data-raw/njforce_200820_clean.csv")
reports <- subset(
  njforce,
  select = c(
    date, officerid_num, firearm_discharged, firearm_pointed, incidentid,
    officer_male, officer_nyears, officer_race, officer_rank, town,
    officer_po, officer_sleo, nsubjects, Incident_type
  ))
reports[, date := as.Date(date, format = "%m/%d/%Y")]
colnames(reports)[2] <- "officerid"

ans <- replicate(100, sim_events2(
  incidentid = reports$incidentid,
  officerid = reports$officerid, female = !reports$officer_male,
  years = reports$officer_nyears, nsims = 1, context_par = 0
  ), simplify = FALSE)
ans <- lapply(ans, "[[", "pointed000001")
ans <- do.call(cbind, ans)

library(Matrix)
mat <- as(ans, "dgCMatrix")

Matrix::image(mat, useRaster = TRUE, aspect = "fill")

permtest <- function(model, vars, dat) {
  
  # Get the permutations
  candidates <- njforce::find_candidates(
    features = dat[,vars], 
    upper = rep(0, length(vars)),
    lower = rep(0, length(vars)),
    as_abs = rep(FALSE, length(vars))
  )
    
}

simulation <- function(rho) {
  dat <- sim_events(
    nevents   = nevents,
    nofficers = nofficers,
    rho_par   = 1,
    exposure_par = rho,
    fe_par = 1
    )
  # GLM
  ans0 <- glm(pointed000001 ~ female + years + exposed + violence_level, data = dat, family = binomial())
  ans1 <- glm(pointed000001 ~ female + years + exposed, data = dat, family = binomial())
  c(
    summary(ans0)$coefficients["exposed", "Pr(>|z|)"],
    summary(ans1)$coefficients["exposed", "Pr(>|z|)"]
  )
    
}

# Type I error
set.seed(1231)
ans_typeI <- replicate(1e3, {
  simulation(0)
})

rowMeans(ans_typeI < .05)

# Power
set.seed(1231)
ans_power <- replicate(1e3, {
  simulation(.5)
})

rowMeans(ans_power < .05)

