#!/bin/bash

declare -a tmp_list=("CanESM5" "CMCC-ESM2" "GFDL-ESM4" "IPSL-CM6A-LR" "NorESM2-MM")
declare -a expt_list=("ssp126" "ssp245" "ssp585")

yearmin=2015
yearmax=2100

########################## TEMPERATURE ##########################

# var="tos"

# for t in ${tmp_list[@]}; do
# 	for e in ${expt_list[@]}; do
# 		# Merge the files into 1 file per model per scenario
# 		bash 01_mergeFiles.sh -i "${var}/" -m $t -g curvilinear -v $var -e $e

# 		# Select years we want
# 		bash 02_selectYears.sh -m $t -v $var -e $e -f $yearmin -l $yearmax

# 		# Remap to 1x1 degree grid
# 		bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $e -n $yearmin -x $yearmax
# 		rm selectyears/* # Free up space

# 		# Change frequency (seasons)
# 		bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $e -n $yearmin -x $yearmax 

# 		echo $t
# 	done
# done

# # Make ensemble for all experiments
# for e in ${expt_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $e -o "mean" -n $yearmin -x $yearmax -f annual
# done

########################## pH ##########################

# var="phos"

# for t in ${tmp_list[@]}; do
# 	for e in ${expt_list[@]}; do
# 		# Merge the files into 1 file per model per scenario
# 		if [[ "$t" == "GFDL-ESM4" ]]
# 			then
# 			bash 01_mergeFiles.sh -i "${var}/" -m $t -g lonlat -v $var -e $e
# 		else
# 			bash 01_mergeFiles.sh -i "${var}/" -m $t -g curvilinear -v $var -e $e
# 		fi

# 		# Select years we want
# 		bash 02_selectYears.sh -m $t -v $var -e $e -f $yearmin -l $yearmax

# 		# Remap to 1x1 degree grid
# 		bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $e -n $yearmin -x $yearmax
# 		rm selectyears/* # Free up space

# 		# Change frequency (seasons)
# 		bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $e -n $yearmin -x $yearmax 

# 		echo $t
# 	done
# done

# # Make ensemble for all experiments
# for e in ${expt_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $e -o "mean" -n $yearmin -x $yearmax -f annual
# done

########################## OXYGEN ##########################

var="o2os"

for t in ${tmp_list[@]}; do
	for e in ${expt_list[@]}; do
		# Merge the files into 1 file per model per scenario
		if [[ "$t" == "GFDL-ESM4" ]]
			then
			bash 01_mergeFiles.sh -i "${var}/" -m $t -g lonlat -v $var -e $e
		else
			bash 01_mergeFiles.sh -i "${var}/" -m $t -g curvilinear -v $var -e $e
		fi

		# Select years we want
		bash 02_selectYears.sh -m $t -v $var -e $e -f $yearmin -l $yearmax

		# Remap to 1x1 degree grid
		bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $e -n $yearmin -x $yearmax
		rm selectyears/* # Free up space

		# Change frequency (seasons)
		bash 05_month2Year.sh -i "remapped" -m $t -v $var -e $e -n $yearmin -x $yearmax 

		echo $t
	done
done

# Make ensemble for all experiments
for e in ${expt_list[@]}; do
	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $e -o "mean" -n $yearmin -x $yearmax -f annual
done