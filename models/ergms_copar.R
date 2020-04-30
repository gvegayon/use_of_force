#!/bin/sh
#SBATCH --job-name=NJforce
#SBATCH --partition=scavenge
#SBATCH --account=lc_ggv
#SBATCH --time=05:00:00
#SBATCH --mem-per-cpu=16G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=g.vegayon@gmail.com

library(network, lib.loc = "/auto/rcf-proj2/wjg/vegayon/R/x86_64-pc-linux-gnu-library/3.6")
library(ergm, lib.loc = "/auto/rcf-proj2/wjg/vegayon/R/x86_64-pc-linux-gnu-library/3.6")
library(intergraph, lib.loc = "/auto/rcf-proj2/wjg/vegayon/R/x86_64-pc-linux-gnu-library/3.6")
library(texreg)
library(slurmR)

source("models/misc.R")

networks <- readRDS("data/networks.rds")
networks <- lapply(networks, asNetwork)

models <- all_models(
  paste0("networks$`", names(networks), "`"),
  c(
    "balance", "triangle", "isolates",
    "cycle(4)", "gwesp",
    "degree1.5", "gwdegree", 
    'nodematch("officer_race")', 'nodefactor("officer_race")',
    'nodematch("officer_male")', 'nodefactor("officer_male")',
    'nodemix("officer_race")',
    'nodematch("officer_county_mode")', 'nodefactor("officer_county_mode")',
    'absdiff("officer_mean_years")', 'nodecov("officer_mean_years")'#,           # can't recall how to change this one to tenure diff...
    # 'nodematch("officer_sup_mode")', 'nodefactor("officer_sup_mode")'#,
    # 'nodematch("officer_po")', 'nodefactor("officer_po")',
    # 'nodematch("officer_nforce")', 'nodefactor("officer_nforce")'
  )
  )



# Fitting the models using slurmr ----------------------------------------------
ans <- Slurm_lapply(
  X          = models,
  FUN        = ergm_lite,
  njobs      = 400L,
  mc.cores   = 1L,
  tmp_path   = "/staging/ggv",
  plan       = "collect",
  export     = ls(),
  job_name   = "NJforce-Slurm_lapply",
  overwrite  = TRUE,
  sbatch_opt = list(
    partition     = "scavenge",
    time          = "05:00:00",
    `mem-per-cpu` = "2G",
    account       = "lc_ggv"
  )
)

# Filtering only those that didn't failed
ans <- ans[which(!sapply(ans, inherits, what="error"))]

saveRDS(ans, file = "models/ergms_copar.rds")
