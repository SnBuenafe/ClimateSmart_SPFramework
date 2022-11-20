#!/bin/bash

# FUNCTION: Change frequency from monthly to annual

while getopts i:m:v:e:n:x: flag
do
	case "${flag}" in
		i) indir=${OPTARG};; # input directory
		m) model=${OPTARG};;
	 	v) var=${OPTARG};;
		e) expt=${OPTARG};;
		n) yearmin=${OPTARG};;
		x) yearmax=${OPTARG};;
	esac
done

# Define directories
mkdir newfreq

file=($(ls ${indir}/*$var*$model*$expt*$yearmin*$yearmax*))

cdo -yearmean $file "newfreq/${var}_${model}_${expt}_${yearmin}_${yearmax}_annual.nc"