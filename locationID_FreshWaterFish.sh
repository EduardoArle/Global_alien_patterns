#!/bin/bash
#SBATCH -D /work/arlribei

# ----------------------------------------------------------------------
# slurm arguments
# ----------------------------------------------------------------------

#SBATCH -J regionsFreshWaterFish
#SBATCH -t 0-72:00:00
#SBATCH --mem-per-cpu=50G

# ----------------------------------------------------------------------
# setup job output/error reports
# ----------------------------------------------------------------------

#SBATCH -o /work/%u/%x-%j-%a_log.txt

# ----------------------------------------------------------------------
# load required modules
# ----------------------------------------------------------------------

module load foss/2018b R/3.5.1-2

# ----------------------------------------------------------------------
# execute task
# ----------------------------------------------------------------------

# set real index 

Rscript --vanilla /data/idiv_meyer/00_data/original/GBIF/27_june_2022/External_data/Scripts/locationID_FreshWaterFish.R