#!/bin/bash
#SBATCH -D /work/arlribei

# ----------------------------------------------------------------------
# slurm arguments
# ----------------------------------------------------------------------

#SBATCH -J gbifID_establishmentMeans
#SBATCH -t 0-24:00:00
#SBATCH --mem-per-cpu=1000G

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

Rscript --vanilla /data/idiv_meyer/00_data/original/GBIF/Scripts_to_run_jobs/gbifID_establishmentMeansID.R