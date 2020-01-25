# Powersets of models
powersets <- function(x, include_empty = FALSE) {
  
  k <- ifelse(include_empty, 0, 1)
  ans <- NULL
  for (i in k:length(x)) {
    ans <- c(ans, combn(x, i, simplify = FALSE))
  }
  
  ans
  
}

# This creates all combinations of models based on a set of terms
# and returns a vector of formulas that can be used directly
all_models <- function(nets., terms.) {
  
  if (length(nets.) > 1) {
    return(unlist(lapply(nets., all_models, terms.)))
  }
  
  ans <- paste(
    nets.,
    "~",
    sapply(powersets(terms.), paste, collapse = " + ")
  )
  ans <- lapply(ans, as.formula)
  ans <- lapply(ans, `environment<-`, .GlobalEnv)
  
  ans
  
}

# A wrapper that makes fitting easier
ergm_lite <- function(...) {
  
  ans <- tryCatch(
    ergm(...), error = function(e) e
  )
  
  if (inherits(ans, "error"))
    return(ans)
  
  model. <- paste(deparse(formula(ans)), collapse = " ")
  
  list(
    model.   = model.,
    network. = gsub("\\s*[~].+", "", model.),
    fit.     = ans
  )
  
}
