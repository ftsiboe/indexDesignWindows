#!/bin/bash -l

#SBATCH --job-name=pxp_trend
#SBATCH --time=0-30:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=150G
#SBATCH --array=1-57
#SBATCH --mail-user=ftsiboe@hotmail.com
#SBATCH --mail-type=ALL


module purge
module load R/4.2.1-foss-2022a GDAL/3.5.0-foss-2022a PROJ/9.0.0-GCCcore-11.3.0
R --no-save -q < data-raw/scripts/004_precipitation_trends_analysis.R

