#!/bin/bash

# FUNCTION: Crop to specific boundaries

while getopts m:v:e:t:w:y:z:n:x: flag
do
	case "${flag}" in
		m) model=${OPTARG};;
	 	v) var=${OPTARG};;
		e) expt=${OPTARG};;
		t) latmin=${OPTARG};;
		w) latmax=${OPTARG};;
		y) lonmin=${OPTARG};;
		z) lonmax=${OPTARG};;
		n) yearmin=${OPTARG};;
		x) yearmax=${OPTARG};;
	esac
done

# Print boundaries
echo "Boundaries: ";
echo "Latitudes: ${latmin} to ${latmax}";
echo "Longitudes: ${lonmin} to ${lonmax}";

# Define directories

mkdir crop

cdo -sellonlatbox,$lonmin,$lonmax,$latmin,$latmax "remapped/${var}_${model}_${expt}_${yearmin}_${yearmax}_remapped.nc" "crop/${var}_${model}_${expt}_${yearmin}_${yearmax}_cropped.nc"