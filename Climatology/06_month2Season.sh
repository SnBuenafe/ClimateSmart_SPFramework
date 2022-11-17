#!/bin/bash

# FUNCTION: Create seasonal maps

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

# January-March
cdo selmon,01,02,03 $file temporary/tmp.nc
cdo yearmonmean temporary/tmp.nc "newfreq/${var}_${model}_${expt}_${yearmin}_${yearmax}_jan-mar.nc"

rm temporary/*

# April-June
cdo selmon,04,05,06 $file temporary/tmp.nc
cdo yearmonmean temporary/tmp.nc "newfreq/${var}_${model}_${expt}_${yearmin}_${yearmax}_apr-jun.nc"

rm temporary/*

# July-September
cdo selmon,07,08,09 $file temporary/tmp.nc
cdo yearmonmean temporary/tmp.nc "newfreq/${var}_${model}_${expt}_${yearmin}_${yearmax}_jul-sept.nc"

rm temporary/*

# October-December
cdo selmon,10,11,12 $file temporary/tmp.nc
cdo yearmonmean temporary/tmp.nc "newfreq/${var}_${model}_${expt}_${yearmin}_${yearmax}_oct-dec.nc"

rm temporary/*
