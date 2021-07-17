#!/bin/sh

#SBATCH --job-name=boot_simulations
#SBATCH --time=72:00:00
#SBATCH --mail-user=lmyint@macalester.edu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mem=20g
#SBATCH --cpus-per-task=1
#SBATCH --array=1-36

module load R/4.0.4
cd ~/upstrapping/code
Rscript --no-save --no-restore run_simulations.R run_simulations_out_$SLURM_ARRAY_TASK_ID
