#!/bin/bash

# FUNCTION: Remove extra grids and merge all files per model and per experiment into one file

# Take inputs
while getopts i:m:g:v:e: flag
do
	case "${flag}" in
		i) indir=${OPTARG};;
		m) model=${OPTARG};;
		g) gridname=${OPTARG};;
	 	v) var=${OPTARG};;
		e) expt=${OPTARG};;
	esac
done

# Print inputs
echo "Model: ${model}";
echo "Variable: ${var}";
echo "Experiment: ${expt}";
echo "Grid name: ${gridname}";

# Define directories
mkdir merged
mkdir temporary

tmpdir="temporary/"
outdir="merged/"

# Call the files per model and per experiment
curr_files=($(ls ${indir}*$var*$model*$expt*))
num_files=${#curr_files[@]}

# Loop through all these files
for((i=0; i<=num_files-1; i++)); do
	curr_file=${curr_files[i]}

	# Remove extra grids
	# Change directory
	tmpname=${curr_file/$indir/$tmpdir} # Change directory
	cdo -selgrid,$gridname $curr_file $tmpname

done

# Merge files into one model
merge_name="$outdir/${var}_${model}_${expt}_merged.nc"
cdo -mergetime ${tmpdir}/* $merge_name

rm $tmpdir/*