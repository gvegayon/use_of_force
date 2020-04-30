library(ergm)
library(data.table)
library(texreg)

ergms <- readRDS("models/ergms_copar.rds")

# Subsetting models in which ergm did not fail to mix
ergms <- ergms[which(!sapply(ergms, inherits, what = "error"))]

# Extracting AIC and BIC, ranking according those
net  <- sapply(ergms, "[[", "network.")

ans <- data.table(
  aic      = sapply(ergms, function(i) AIC(i$fit)),
  bic      = sapply(ergms, function(i) AIC(i$fit)),
  location = net,
  model    = sapply(ergms, "[[", "model."),
  fit      = lapply(ergms, "[[", "fit.")
  )

# Too big
rm(ergms)
gc()

ans[, location := gsub(".+[$]", "", location)]

# They should have the edges term
ans <- ans[grepl("edges", model),]
  
ans <- ans[order(bic, aic, decreasing = FALSE)]
ans[, rank := 1L:.N, by = location]
# ans <- ans[rankd, by = location]
ans <- ans[rank <= 20]

# Indentifying the top 10 models per network
cities <- sort(unique(ans$location))
summaries <- structure(vector("list", length(cities)), names = cities)

unlink("models/ergms_copar-analysis.txt")
for (city in cities) {

  # Creating the tables  
  summaries[[city]] <- screenreg(
    ans[location == city]$fit
    )
  
  # Saving the tables
  cat(
    "\n\n",
    paste(rep("-", 80), collapse = ""),
    sprintf("Top 10 (or so) models for city %s", city),
    summaries[[city]],
    file   = "models/ergms_copar-analysis.txt",
    append = TRUE, sep = "\n"
    )
  
}


