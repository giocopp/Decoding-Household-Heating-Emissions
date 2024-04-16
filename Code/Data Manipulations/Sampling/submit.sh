#!/bin/bash

#SBATCH --qos=short
#SBATCH --job-name=summary
#SBATCH --partition=largemem
#SBATCH --mem 200000
#SBATCH --account=bymarka
#SBATCH --output=jobfile-%j.out
#SBATCH --error=jobfile-%j.err

# Load required modules
module load R/4.2.0/gcc-mkl

# Execute the R script
Rscript simple_sample_D.R

