#!/bin/bash -l

#SBATCH --job-name=stat_thres
#SBATCH --time=0-20:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=50G
#SBATCH --array=1-504
#SBATCH --mail-user=ftsiboe@hotmail.com
#SBATCH --mail-type=ALL
#SBATCH --output=/dev/null

module purge
module load R/4.2.1-foss-2022a GDAL/3.5.0-foss-2022a PROJ/9.0.0-GCCcore-11.3.0
R --no-save -q < data-raw/scripts/005_statistical_threshold_analysis.R

