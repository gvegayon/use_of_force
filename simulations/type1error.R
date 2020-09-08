library(njforce)

nevents   <- 700
nofficers <- 400

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

