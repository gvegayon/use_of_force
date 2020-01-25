#' The permutations should be done considering the following
#' 
#' - The degree of the individual
#' - The age of the individual
#' - The gender of the individual
#' 
#' As the very minimum. A function follows

library(igraph)
networks <- readRDS("data/networks.rds")

get_features <- function(net, attr_names = NULL) {
  
  ans <- data.frame(deg = degree(net))
  for (a in attr_names)
    ans[[a]] <- igraph::vertex_attr(net, a)

  ans  
  
}
ans <- get_features(networks[[1]], c("officer_race"))

# Finding neareast neighbour
d <- dist(ans)
