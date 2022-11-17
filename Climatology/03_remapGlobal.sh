#!/bin/bash

# FUNCTION: Remap to global 1x1 degree grid

while getopts i:m:v:e:n:x: flag
do
	case "${flag}" in
		i) inp=${OPTARG};; # inp file
		m) model=${OPTARG};;
	 	v) var=${OPTARG};;
		e) expt=${OPTARG};;
		n) yearmin=${OPTARG};;
		x) yearmax=${OPTARG};;
	esac
done

# Define directories

mkdir remapped

cdo -remapbil,global_1 $inp "remapped/${var}_${model}_${expt}_${yearmin}_${yearmax}_remapped.nc"