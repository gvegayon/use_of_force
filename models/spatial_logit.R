library(njforce)
library(data.table)
library(McSpatial)

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


# Filtering complete cases only
reports2 <- reports[complete.cases(officer_male, officer_nyears)]

# Simulating some fake data
set.seed(12351)
dat <- sim_events2(
  event_id       = reports2$incidentid,
  officer_id     = reports2$officerid,
  officer_female = !reports2$officer_male,
  officer_years  = reports2$officer_nyears,
  par_event_violence = 1,
  par_exposure_event = .5,
  par_exposure_prev  = 0,
  par_officer_fe     = 0,
  par_officer_female = -.5,
  par_officer_years  = -.5,
  nsims              = 1
)

set.seed(12351)
dat <- sim_events(
  nevents = 2000,
  nofficers = 500,
  par_event_violence = 1,
  par_exposure_event = .5,
  par_exposure_prev  = 0,
  par_officer_fe     = 0,
  par_officer_female = -.5,
  par_officer_years  = -.5,
  nsims              = 1
)

table(dat$pointed000001)

# Generating wmat
wmat <- matrix(0, nrow = nrow(dat), ncol = nrow(dat))
el <- lapply(dat$incidentid, function(u) which(dat$incidentid == u))

library(spdep)
# nb2listw(el)



library(Matrix)
wmat[cbind(rep(1:nrow(wmat), sapply(el, length)), unlist(el))] <- 1
diag(wmat) <- 0
wmat <- as(wmat, "dgCMatrix")
wmat <- wmat/(rowSums(wmat) + 1e-20)

summary(
  splogit( 
    pointed000001 ~ -1+female + years, wmat = as.matrix(wmat),
    data=dat
  )
)

summary(
  glm( 
    pointed000001 ~ -1+female + years,
    family = binomial(),
    data=dat
  )
)

