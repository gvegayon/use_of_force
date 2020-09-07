#' Simulate Police Force Events
#'
#' This function generates data similar to that featured in the paper. Events
#' are drawn at random, as the number of officers per event. The outcome variable,
#' whether the officer points his gun or not, is drawn sequentially as a poisson
#' process.
#'
#' @param nevents,nofficers Integers. Number of events and officers to simulate.
#' @param min_per_event,max_per_event Integers. Lower and upper bounds for the
#' number of officers in the event.
#' @param min_year,max_years Integers. Lower and upper bounds for the number
#' of years of experience of the officers.
#' @param min_rate,max_rate Doubles. Lower and upper bounds for the reaction
#' rates (see details).
#' @param female_par,years_par,rho_par,exposure_par Doubles. Parameters (coefficients) for
#' the logistic probabilities.
#' @param seed Integer. Seed for the pseudo-number generation.
#'
#' @details
#' The simulation process goes as follow:
#' 1. The officers are simulated. Female ~ Bernoulli(0.5),
#'    Action rate ~ Unif(min_rate, max_rate),
#'    Years of experience ~ Discrete Unif[min_years, max_year]
#' 2. Events are simulated, each event has a nofficers ~ Discrete Unif[min_per_event, max_per_event]
#'    Once the event is done, a sequence of reaction is given by each officers'
#'    action rate (Poisson process). Whether an officer points or not is set by
#'    a logistic model
#'
#'    point ~ female + years of experience + has any pointed? + previous exposure
#'
#'    The corresponding parameters are as specified by the user. Events are simulated
#'    one at a time.
#' @returns
#' A data frame with the following columns
#' - Officer id
#' - Whether the officer is female
#' - Years of experience
#' - Incident id
#' - Whether the officer pointed a gun
#'
#' Each row represents one report per officer involved in the event.
#' @export
#' @examples
#' x <- simulate_njforce(1000, 400)
#'
#' x <- sim_events(
#'   20000,200,
#'   female_par = -.5, years_par = -.5, rho_par = -.5,
#'   context_par = 1, exposure_par = .25, fe_par = 1,
#'   seed = 445
#' )
#'
#' # Full model knowing latent variables
#' ans <- glm(
#'   pointed000001 ~ -1+female + years + exposed + I(-first) + fixed_effect +
#'   violence_level, data = x, family = binomial()
#' )
#'
#' summary(ans)
sim_events <- function(
  nevents,
  nofficers,
  min_per_event = 1,
  max_per_event = 5,
  min_year      = 0,
  max_year      = 10,
  min_rate      = 5,
  max_rate      = 5,
  female_par    = -.5,
  years_par     = -.5,
  rho_par       = 0,
  exposure_par  = .5,
  context_par   = 1,
  fe_par        = 1,
  nsims         = 1,
  seed          = sample.int(.Machine$integer.max, 1)
) {

  ans <- sim_events_cpp(
    nevents,
    nofficers,
    min_per_event,
    max_per_event,
    min_year,
    max_year,
    min_rate,
    max_rate,
    female_par,
    years_par,
    rho_par,
    exposure_par,
    context_par,
    fe_par,
    nsims,
    seed
  )

  ans <- do.call(cbind, ans)
  colnames(ans) <- c(
    "officerid",
    "female",
    "years",
    "fixed_effect",
    "incidentid",
    "violence_level",
    "response_time",
    "first",
    "exposed",
    sprintf("pointed%06i", 1:nsims)
  )

  as.data.frame(ans)
}


#' @export
#' @param incidentid,officerid Integer vectors. Values for the incident and
#' officer id.
#' @param female,years Logical and integer vectors, respectively. Features
#' of the officers.
#' @rdname sim_events
#' @details
#' In the case of `sim_events2`, the user can pass predefined events and
#' officers and use those to simulate each officers' reactions.
#' @importFrom Rcpp sourceCpp
#' @useDynLib njforce, .registration = TRUE
sim_events2 <- function(
  incidentid,
  officerid,
  female,
  years,
  fixed_effect = rep(0, length(incidentid)),
  rate         = rep(1, length(incidentid)),
  female_par   = -.5,
  years_par    = .-5,
  rho_par      = .5,
  exposure_par = .5,
  context_par  = 1,
  fe_par       = 1,
  nsims        = 1,
  seed         = sample.int(.Machine$integer.max, 1)
) {

  ans <- sim_events_cpp2(
    incidentid,
    officerid,
    female,
    rate,
    fixed_effect,
    years,
    female_par,
    years_par,
    rho_par,
    exposure_par,
    context_par,
    fixed_effect_par,
    nsims,
    seesd
  )

  ans <- do.call(cbind, ans)
  colnames(ans) <- c(
    "officerid",
    "female",
    "years",
    "fixed_effect",
    "incidentid",
    "violence_level",
    "response_time",
    "first",
    "exposed",
    sprintf("pointed%06i", 1:nsims)
  )

  as.data.frame(ans)
}
