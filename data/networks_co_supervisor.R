library(igraph)

dat <- read.csv(
  "data-raw/njforce_200210.csv",
  stringsAsFactors = FALSE
  )

# Networks per town-id
towns <- sort(unique(dat$town))
races <- sort(unique(dat$officer_race))
races_col <- c("black", "tomato", "steelblue", "gray", "green")
races_col <- adjustcolor(races_col, .5)

ego_covariates <- c(
  "officer_race",
  "officer_male",
  "officer_county_mode",
  "officer_meanyears",
  "officer_sup_mode"#,
  #"officer_po",
  #"officer_nforce"
)

networks <- vector("list", length(towns))
names(networks) <- towns
for (town. in towns) {
  
  # Geting the officers from a given town
  dat_town <- subset(
    dat, town == town.,
    select = c(
      "date", "officerid", "supervisorid", "incidentid", "firearm_discharged",
      ego_covariates
      )
    )
  dat_town <- unique(dat_town)
  
  # Identifying the ties
  networks[[town.]] <- dat_town
  cnames <- colnames(networks[[town.]])
  
  # Creating the edgelist
  colnames(networks[[town.]])[cnames == "officerid"] <- "ego"
  networks[[town.]] <- merge(
    networks[[town.]][, setdiff(colnames(networks[[town.]]), ego_covariates)],
    dat_town[, c("officerid", "supervisorid")],
    by = c("supervisorid")
    )
  
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
    select = unique(c("officerid", "officer_race", ego_covariates))
    )
  
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
  
  E(networks[[town.]])$arrow.size <- 1 #1 + 
    # E(networks[[town.]])$firearm_discharged*4
  # E(networks[[town.]])$color <- c(
  #   adjustcolor("steelblue", .5),
  #   "tomato")[
  #   E(networks[[town.]])$firearm_discharged + 1
  # ]
  
}

graphics.off()
pdf("data/networks_co_supervisor.pdf", width = 9, height = 6)
op <- par(mar = c(0,0,2,0), oma = rep(0,4), mfrow = c(2, 3))
lapply(names(networks), function(net) {
  plot(networks[[net]])
  title(net)
})
par(op)
dev.off()

saveRDS(networks, "data/networks_co_supervisor.rds")
