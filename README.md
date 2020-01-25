Source code for the NJ Force Networks

The structure of the project follows:

- `data-raw` Contains the raw data of the research. Currently we are not sharing
  The actual data here. If you are interested on getting it, email ....

- `data` R scripts used to generate the processed data, including networks.
  The programs listed here use as input datasets in `data-raw`.

- `models` R script that estimate models.

# Required R packages

- ergm: For fitting ERGMs.

- intergraph: For data management.

- igraph: For data management

- slurmR: Used to fit the models in a Slurm cluster

- texreg: For tabulating results

# Funding

-

- USC's Center for High Performance Computing.

