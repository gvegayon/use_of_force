library(ergm)
library(intergraph)
library(texreg)

networks <- readRDS("data/networks.rds")
networks <- lapply(networks, asNetwork)

# Model 1
model1 <- lapply(networks, function(net) 
  tryCatch(ergm(
  net ~ edges + nodematch("officer_race") + nodecov("officer_race")
  ), error = function(e) e)
  )

# Model 1
model2 <- lapply(networks, function(net) 
  tryCatch(ergm(net ~ edges + balance), error = function(e) e)
  )

screenreg(model1)
screenreg(model2)
