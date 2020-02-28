library(igraph)

dat <- read.csv(
  "data-raw/njforce_200210.csv",
  stringsAsFactors = FALSE
  )

# Naive approach towards missingness, for now, we just drop all observations that
# have missing in the relevant variables
ego_covariates <- c(
  "officer_race",
  "officer_male",
  "officer_county_mode",
  "officer_meanyears" #,
  # "officer_sup_mode"#,
  # These cannot be included since they are dynamic
  #"officer_po",     
  #"officer_nforce"
)

# The first four of above are OK in terms of missingness,
# less that 10% of the sample is missing. We will work with that
#
# "officer_race",
# "officer_male",
# "officer_county_mode",
# "officer_meanyears",
# > table(complete.cases( dat[, ego_covariates[1:4]]))
# 
# FALSE  TRUE 
#   222  3823 
dat <- dat[complete.cases( dat[, ego_covariates[1:4]]),]

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
    select = c(
      "date", "officerid", "incidentid", "firearm_discharged",
      ego_covariates
      )
    )
  
  # Identifying the ties
  networks[[town.]] <- subset(dat_town, select = c(-date, -firearm_discharged))
  cnames <- colnames(networks[[town.]])
  
  # Creating the edgelist
  colnames(networks[[town.]])[cnames == "officerid"] <- "ego"
  # networks[[town.]] <- merge(
  #   networks[[town.]], dat_town, by = c("incidentid"))
  networks[[town.]] <- merge(
    networks[[town.]][, setdiff(colnames(networks[[town.]]), ego_covariates)],
    dat_town[, c("officerid", "incidentid")],
    by = c("incidentid")
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
    select = unique(c("officerid", ego_covariates))
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
