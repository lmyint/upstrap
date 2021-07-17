library(tidyverse)
library(dagitty)
library(broom)

## Functions for generating data from known mechanisms
source("functions_gen_data.R")

## Functions for obtaining quantities of interest
source("functions_get_targets.R")

## Function for running a round of the simulation and
## setup code for setting up axes of the sensitivity analysis
source("functions_run_setup_sim.R")

## ============================================================================
# Get array id value from the environment variable passed from sbatch
slurm_arrayid <- Sys.getenv("SLURM_ARRAY_TASK_ID")
slurm_arrayid <- as.numeric(slurm_arrayid)

this_dag_index <- parameter_combos$dag_index[slurm_arrayid]
this_dag <- list_dags[[this_dag_index]]
this_noise_sd <- parameter_combos$noise_sd[slurm_arrayid]

cat("DAG used:\n")
print(this_dag)

cat("Noise SD used:\n")
print(this_noise_sd)

set.seed(207)
curve_sample_sizes <- seq(2000, 16000, 2000)
train_sample_sizes <- c(1000, 2000, 4000)
sim_results <- run_simulation(
    dag = this_dag,
    noise_sd = this_noise_sd,
    curve_sample_sizes = curve_sample_sizes,
    train_sample_sizes = train_sample_sizes,
    train_reps = 50, # originally 100
    iters_truth = 5000, # originally 1000
    iters_upstrap = 5000 # originally 1000
)

write_rds(sim_results, file = paste0("../data/sim_results", slurm_arrayid, ".rds"), compress = "gz")


## ============================================================================
## Session Info
sessionInfo()

