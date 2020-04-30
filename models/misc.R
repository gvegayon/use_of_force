# Powersets of models
powersets <- function(x, include_empty = FALSE, max_size = 5) {
  
  k <- ifelse(include_empty, 0, 1)
  max_size <- min(max_size, length(x))
  ans <- NULL
  for (i in k:max_size) {
    ans <- c(ans, combn(x, i, simplify = FALSE))
  }
  
  ans
  
}

# This creates all combinations of models based on a set of terms
# and returns a vector of formulas that can be used directly
all_models <- function(nets., terms., max_size=4) {
  
  if (length(nets.) > 1) {
    return(unlist(lapply(nets., all_models, terms.)))
  }
  
  ans <- paste(
    nets.,
    "~ edges +",
    sapply(powersets(terms., max_size = max_size), paste, collapse = " + ")
  )
  ans <- lapply(ans, as.formula)
  ans <- lapply(ans, `environment<-`, .GlobalEnv)
  
  ans
  
}

# A wrapper that makes fitting easier
ergm_lite <- function(...) {
  
  if (exists(".counter.", .GlobalEnv)) {
    .counter. <<- .counter. + 1L
  } else {
    assign(".counter.", 1L, envir = .GlobalEnv)
  }
  
  if (!(.counter. %% 10))
    message(
      paste(rep("/", 80), collapse=""),
      "\nModel ", .counter., "\n",
      paste(rep("/", 80), collapse="")
      )
  
  ans <- tryCatch(
    ergm(...), error = function(e) e
  )
  
  if (inherits(ans, "error"))
    return(ans)
  
  model. <- paste(deparse(formula(ans)), collapse = " ")
  
  list(
    model.   = model.,
    network. = gsub("\\s*[~].+", "", model.),
    fit.     = tryCatch(texreg::extract(ans), error = function(e) e)
  )
  
}
