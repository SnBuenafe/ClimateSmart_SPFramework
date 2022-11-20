#!/bin/bash

# FUNCTION: Extract required years

while getopts m:v:e:f:l: flag
do
	case "${flag}" in
		m) model=${OPTARG};;
	 	v) var=${OPTARG};;
		e) expt=${OPTARG};;
		f) firstyear=${OPTARG};;
		l) lastyear=${OPTARG};;
	esac
done

# Define directories

mkdir selectyears

curr_file=($(ls merged/*$var*$model*$expt*))

cdo -selyear,${firstyear}/${lastyear} $curr_file selectyears/tmpfile.nc


