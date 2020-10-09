#' Conditional logit with permutation
#' @param nperm Integer. Number of permutations
#' @param formula,data,... Parameters passed to [survival::clogit()]
#' @param ncpus Integer. Number or cores.
#' @param parallel_args Parameters passed to [parallel::makeCluster()].
#' @export
#' @importFrom survival clogit
#' @importFrom stats terms
clogit_perm <- function(
  nperm = 1000,
  formula,
  data,
  ...,
  ncpus = parallel::detectCores(),
  parallel_args = list()
  ) {

  # Capturing strata variable
  fterms <- stats::terms(formula)

  strata_term <- attr(fterms, "term.labels")
  strata_term <- strata_term[which(grepl("^strata[(]", strata_term))]

  if (length(strata_term) != 1)
    stop("There should be one -strata()- term in the formula.", call. = FALSE)

  strata_term <- gsub("^strata[(]|[)]$", "", strata_term)

  if (!(strata_term %in% colnames(data)))
    stop("The term \"", strata_term, "\" is not present in -dat-.")

  # Finding the dependent variable
  if (!attr(fterms, "response"))
    stop("No dependent variable in this model.", call. = FALSE)

  depvar <- rownames(attr(fterms, "factors"))[1L]

  # Finding the permutations
  groups <- as.integer(as.factor(data[, strata_term]))
  if (any(is.na(groups)))
    stop("There are missings in the strata() term.")

    # Finding candidates
  candidates <- find_candidates(
    features = cbind(groups),
    upper    = 0,
    lower    = 0,
    as_abs   = TRUE
  )

  # Generating the permutations
  ORD <- replicate(nperm, permute(candidates) + 1L, simplify = FALSE)

  # Baseline model
  model0 <- survival::clogit(formula = formula, data = data, ...)

  # Preparing the permutation
  coefs <- if (ncpus > 1L) {
    cl <- do.call(
      parallel::makeCluster,
      c(list(spec = ncpus), parallel_args)
    )
    parallel::clusterEvalQ(cl, library(survival))
    on.exit(parallel::stopCluster(cl))
    parallel::parLapply(cl, ORD, function(ord, formula., data., ..., depvar.) {

      # Permuting the dependent variable
      data.[, depvar.] <- data.[, depvar.][ord]

      # Fitting the survival model
      stats::coef(survival::clogit(formula = formula., data = data., ...))

    }, formula. = formula, data. = data, ..., depvar. = depvar)
  } else {

    lapply(ORD, function(ord, formula., data., ..., depvar.) {

      # Permuting the dependent variable
      data.[, depvar.] <- data.[, depvar.][ord]

      # Fitting the survival model
      stats::coef(survival::clogit(formula = formula., data = data., ...))

    }, formula. = formula, data. = data, ..., depvar. = depvar)

  }

  # Building a list
  coefs <- do.call(rbind, coefs)

  # Calculating confidence intervals and pvals
  pvals   <- rowMeans(t(coefs) < stats::coef(model0))
  pvals[] <- ifelse(pvals < .5, pvals, 1 - .5)*2

  ci      <- apply(coefs, 2, stats::quantile, probs = c(.025, .975))

  structure(
    list(
      pvals      = pvals,
      ci         = ci,
      model      = model0,
      coefs      = coefs,
      candidates = candidates
      ),
    class = "clogit_perm"
  )

}
