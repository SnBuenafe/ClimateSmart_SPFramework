#!/bin/bash

# FUNCTION: Create ensemble mean

while getopts i:v:e:o:n:x:f: flag
do
	case "${flag}" in
		i) indir=${OPTARG};; # input directory
	 	v) var=${OPTARG};;
		e) expt=${OPTARG};;
		o) ensopt=${OPTARG};; #ensemble mean or median
		n) yearmin=${OPTARG};;
		x) yearmax=${OPTARG};;
		f) frequency=${OPTARG};;
	esac
done

# Define directories
mkdir ensemble

files=($(ls ${indir}/*$var*$model*$expt*$yearmin*$yearmax*$frequency*))
echo ${files[@]}

if [[ "$ensopt" == "mean" ]]
	then
	cdo -ensmean ${files[@]} "ensemble/${var}_${expt}_${yearmin}_${yearmax}_${frequency}_ensemble.nc"
else
	cdo -ensmedian ${files[@]} "ensemble/${var}_${expt}_${yearmin}_${yearmax}_${frequency}_ensemble.nc"
fi