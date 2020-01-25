#!/bin/sh
#SBATCH --job-name=NJforce
#SBATCH --partition=scavenge
#SBATCH --account=lc_ggv
#SBATCH --time=05:00:00
#SBATCH --mem-per-cpu=8G


library(ergm)
library(intergraph)
library(texreg)
library(slurmR)

source("models/misc.R")

networks <- readRDS("data/networks.rds")
networks <- lapply(networks, asNetwork)

models <- all_models(
  paste0("networks$`", names(networks), "`"),
  c(
    "balance", "triangle", "edges", "isolates",
    "degree1.5", 
    'nodematch("officer_race")', 'nodecov("officer_race")'
    )
  )



# Fitting the models using slurmr ----------------------------------------------
ans <- Slurm_lapply(
  X          = models,
  FUN        = ergm_lite(m),
  njobs      = 75L,
  mc.cores   = 1L,
  tmp_path   = "/staging/ggv",
  plan       = "collect",
  export     = ls(),
  job_name   = "NJforce-Slurm_lapply",
  overwrite  = TRUE,
  sbatch_opt = list(
    partition     = "scavenge",
    time          = "05:00:00",
    `mem-per-cpu` = "4G",
    account       = "lc_ggv"
  )
)

saveRDS(ans, file = "models/ergms_copar.rds")
