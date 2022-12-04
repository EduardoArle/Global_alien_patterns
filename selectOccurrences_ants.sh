#!/bin/bash
#SBATCH -D /work/arlribei

# ----------------------------------------------------------------------
# slurm arguments
# ----------------------------------------------------------------------

#SBATCH -J selectOccurrencesAnts
#SBATCH -t 0-72:00:00
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

Rscript --vanilla /gpfs1/data/idiv_meyer/01_projects/eduardo/GlobalAlienPatterns/Ants/Scripts/selectOccurrences_ants.R