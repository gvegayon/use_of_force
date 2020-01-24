library(igraph)

dat <- read.csv(
  "data-raw/njforce_191230.csv",
  stringsAsFactors = FALSE
  )

# Networks per town-id
towns <- sort(unique(dat$town))
races <- sort(unique(dat$officer_race))
races_col <- c("black", "tomato", "steelblue", "gray", "green")
races_col <- adjustcolor(races_col, .5)

networks <- vector("list", length(towns))
names(networks) <- towns
for (town. in towns) {
  
  # Geting the officers from a given town
  dat_town <- subset(
    dat, town == town.,
    select = c(date, officerid, incidentid, firearm_discharged)
    )
  
  # Identifying the ties
  networks[[town.]] <- subset(dat_town, select = c(-date, -firearm_discharged))
  cnames <- colnames(networks[[town.]])
  
  # Creating the edgelist
  colnames(networks[[town.]])[cnames == "officerid"] <- "ego"
  networks[[town.]] <- merge(
    networks[[town.]], dat_town, by = c("incidentid"))
  
  networks[[town.]] <- subset(networks[[town.]], ego != officerid)
  
  # Removing duplicates
  to_swap <- with(networks[[town.]], ego > officerid)
  networks[[town.]][to_swap, c("ego", "officerid")] <- 
    networks[[town.]][to_swap, c("ego", "officerid")][, 2:1]

  networks[[town.]] <- unique(networks[[town.]])
  
  # Preparing for igraph
  networks[[town.]] <- networks[[town.]][
    , c(
      "ego", "officerid",
      setdiff(colnames(networks[[town.]]), c("ego", "officerid"))
      )
    ]
  
  vertex_town <- subset(
    dat, town == town.,
    select = c(officerid, officer_race))
  
  vertex_town <- unique(vertex_town)
  
  # Building the graph obeject
  networks[[town.]] <- graph_from_data_frame(
    networks[[town.]],
    vertices = vertex_town,
    directed = FALSE
    ) 
  
  
  # Setting up for visualization
  V(networks[[town.]])$size        <- 5 # sqrt(degree(networks[[town.]]))
  V(networks[[town.]])$color       <- races_col[
    V(networks[[town.]])$officer_race + 1
    ]
  V(networks[[town.]])$frame.color <- adjustcolor("gray", .5)
  V(networks[[town.]])$label.cex   <- sqrt(degree(networks[[town.]])/40) + .2
  V(networks[[town.]])$label <- NA
  
  E(networks[[town.]])$arrow.size <- 1 + 
    E(networks[[town.]])$firearm_discharged*4
  E(networks[[town.]])$color <- c(
    adjustcolor("steelblue", .5),
    "tomato")[
    E(networks[[town.]])$firearm_discharged + 1
  ]
  
}

graphics.off()
pdf("data/networks.pdf", width = 9, height = 6)
op <- par(mar = c(0,0,2,0), oma = rep(0,4), mfrow = c(2, 3))
lapply(names(networks), function(net) {
  plot(networks[[net]])
  title(net)
})
par(op)
dev.off()

saveRDS(networks, "data/networks.rds")
