#!/bin/bash
#SBATCH --get-user-env
#SBATCH --qos=express
#SBATCH --job-name=harp_verify

# Check if R/4.3.3 is loaded already
check=$(module list | grep R/4.3.3)
if [ -z "$check" ]; then
    echo "Load in R/4.3.3"
    module load R/4.3.3
fi

# Config
config_file=${1:-"example8"}

export config_file

# Environment variables
export R_LIBS_USER=$HOME/R/x86_64-pc-linux-gnu-library/4.3
export HARP_HOME=$HOME/harp_plot

R --no-save --no-restore --slave < ${HARP_HOME}/plot_fields.R

