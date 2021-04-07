library(mc3logit)

library(data.table)

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

# Simulation -------------------------------------------------------------------
complete <- complete.cases(reports[, c("incidentid", "officerid", "officer_male", "officer_nyears")])
reports <- reports[which(complete),]#[1:500,]


ans <- sim_events2(
  event_id       = as.integer(reports$incidentid),
  officer_id     = as.integer(reports$officerid), 
  officer_female = as.logical(1 - reports$officer_male),
  officer_years  = as.integer(floor(reports$officer_nyears)),
  par_officer_female = -.5,
  par_officer_years  = -.5,
  par_officer_exposure = .5,
  seed         = 123
)

# Are we getting the same data back?
ans <- data.table(ans)
ans <- ans[order(officerid, incidentid)]
reports <- reports[order(officerid, incidentid)]

library(magrittr)
(ans[,-5] == reports[,list(officerid, 1 - officer_male, floor(officer_nyears), incidentid)]) %>%
  apply(2, sum)

hist(reports[, list(diff(range(officer_nyears))), by = officerid][,2][[1]])

  range(reports$date, na.rm = TRUE)
