#!/bin/bash

#SBATCH -n 4 #number of cores
#SBATCH -t 6-23:59 # max time limit on this queue is 7 days
#SBATCH -p shared #partition
#SBATCH -o /n/home07/swoodward/PM_COVID_ecoinference/RCCluster/main_error.out #specify where to save errors returned by the program
#SBATCH -e /n/home07/swoodward/PM_COVID_ecoinference/RCCluster/main_log.err #specify where to save the output log
#SBATCH --array=1 
#SBATCH --mem=8000 #memory requested
#SBATCH -J fit_main  #job name
#SBATCH --mail-type=END #notifications for job done
#SBATCH --mail-user=swoodward@college.harvard.edu # send to address

export R_LIBS_USER=/n/home07/swoodward/apps/R_3.5.1:$R_LIBS_USER  #change this accordingly
module load gcc/9.2.0-fasrc01 R/3.6.3-fasrc02
#module load R/3.6.2-fasrc01
#module load R_core/3.4.2-fasrc01
#module load R_packages/3.4.2-fasrc02

R CMD BATCH --quiet --no-restore --no-save fit_main.R /n/home07/swoodward/PM_COVID_ecoinference/RCCluster/main_${SLURM_ARRAY_TASK_ID}.Rout
