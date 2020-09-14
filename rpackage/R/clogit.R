# This function computes the conditional likelihood for an arbitrary number of
# police officers. If sum(y) in {0, n} then the loglikelihood equals 0.
ll <- function(y, x, beta) {

  n1 <- sum(y)
  n <- length(y)

  if (n1 == n | n1 == 0)
    return(0)

  ans <- sum(x[which(y == 1), ] %*% beta)

  sets <- combn(seq_len(n), n1, simplify = FALSE)

  tmp <- 0
  for (s in sets)
    tmp <- tmp + exp(sum(x[s,] %*% beta))

  ans - log(tmp)


}

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

reports <- reports[order(officerid, incidentid),]
reports[, exposure_i := as.integer((sum(firearm_pointed) - firearm_pointed) > 0), by = "incidentid"]
reports[, exposure_i := shift(exposure_i, type="lag"), by = "officerid"]
reports[, firearm_pointed_prev := shift(firearm_pointed, type = "lag", fill = FALSE), by = officerid]
reports2 <- reports[complete.cases(
  exposure_i, firearm_pointed, officer_male, officer_nyears,
  officer_po, officer_sleo,
  officer_rank
  )]
reports2[, officer_nyears := officer_nyears/sd(officer_nyears, na.rm = TRUE)]

fun <- function(b, verb = TRUE) {
  ans <- reports2[,
          .(ll = ll(
            y = firearm_pointed,
            x = cbind(
              exposure_i,
              # officer_male,
              officer_nyears,
              officer_race != "white",
              officer_po,
              # officer_sleo,
              officer_rank
              ),
            beta = cbind(b)
            )),
          by = incidentid]

  # message(sum(ans < (0 - 1e-10)), " counted.")
  sum(ans$ll, na.rm = TRUE)
}

ans <- optim(par = rep(0, 5), fun, control = list(fnscale = -1), hessian = TRUE)
ans
hess <- solve(-ans$hessian)
fun(ans$par)
ans$par

# Computing pvalues
pval <- 2*pnorm(-abs(ans$par/diag(hess)))

knitr::kable(
  data.frame(
    coef = ans$par,
    pval = pval,
    sd   = diag(hess)
  )
)

reports2[, noff := .N, by = incidentid]
reports2[, incl := sum(firearm_pointed) != 0 & sum(firearm_pointed) != .N, by = incidentid]

ans2<-clogit(firearm_pointed ~ exposure_i + officer_nyears +
       I(officer_race != "white") +
       officer_po +
       # officer_sleo +
       officer_rank +
         strata(incidentid), data = reports2 #[incl == TRUE]
       )
summary(ans2)


# These two numbers must match
logLik(ans2)
print(ans$value)
