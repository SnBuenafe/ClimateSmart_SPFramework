#!/bin/bash

declare -a season_list=("jan-mar" "apr-jun" "jul-sept" "oct-dec")

########################## TEMPERATURE ##########################

# declare -a tmp_list=("BCC-CSM2-MR" "CMCC-CM2-SR5" "CMCC-ESM2" "FGOALS-f3-L" "FGOALS-g3" "MIROC6" "MIROC-ES2L" "MRI-ESM2-0" "NorESM2-LM")
# var="tos"
# expt="historical"
# yearmin=1956
# yearmax=1984

# for t in ${tmp_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency (seasons)
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax 

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# expt="ssp585"

# Present (2017-2026)
# yearmin=2017
# yearmax=2026
# for t in ${tmp_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax 

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# Mid-century (2046-2055)
# yearmin=2046
# yearmax=2055

# for t in ${tmp_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# Removed this step because it has already been done above.

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax 

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# End of the century (2091-2100)
# yearmin=2091
# yearmax=2100

# for t in ${tmp_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# Removed this step because it has already been done above.

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax 

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

########################## pH ##########################

# declare -a ph_list=("CMCC-ESM2" "MIROC-ES2L" "NorESM2-LM")
# var="phos"
# expt="historical"
# yearmin=1956
# yearmax=1984

# for t in ${ph_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# expt="ssp585"

# # Present (2017-2026)
# yearmin=2017
# yearmax=2026
# for t in ${ph_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# #	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# # Mid-century (2046-2055)
# yearmin=2046
# yearmax=2055
# for t in ${ph_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# Removed this step because it has already been done above.

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# # End of the century (2091-2100)
# yearmin=2091
# yearmax=2100
# for t in ${ph_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# Removed this step because it has already been done above.

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

########################## OXYGEN ##########################

# declare -a o2_list=("CMCC-ESM2" "MIROC-ES2L" "MRI-ESM2-0" "NorESM2-LM")
# var="o2os"
# expt="historical"
# yearmin=1956
# yearmax=1984

# for t in ${o2_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# expt="ssp585"

# # Present
# yearmin=2017
# yearmax=2026

# for t in ${o2_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# if [[ $t == "MRI-ESM2-0" ]]
# 	# 	then
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g lonlat -v $var -e $expt
# 	# else
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt
# 	# fi

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# # Mid-century
# yearmin=2046
# yearmax=2055

# for t in ${o2_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# Removing this because it has already been done above

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# # End of the century
# yearmin=2091
# yearmax=2100

# for t in ${o2_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# Removing this because it has already been done above

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

########################## CHLOROPHYLL ##########################

# declare -a ch_list=("CMCC-ESM2" "MIROC-ES2L" "MRI-ESM2-0" "NorESM2-LM")
# var="chlos"
# expt="historical"
# yearmin=1956
# yearmax=1984

# for t in ${ch_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# if [[ $t == "MRI-ESM2-0" ]]
# 	# 	then
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g lonlat -v $var -e $expt
# 	# else
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt
# 	# fi

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# expt="ssp585"

# # Present
# yearmin=2017
# yearmax=2026

# for t in ${ch_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# if [[ $t == "MRI-ESM2-0" ]]
# 	# 	then
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g lonlat -v $var -e $expt
# 	# else
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt
# 	# fi

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# # Mid-century
# yearmin=2046
# yearmax=2055

# for t in ${ch_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# Removing this line because it has already been done above

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# # End of the century
# yearmin=2091
# yearmax=2100

# for t in ${ch_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# Removing this line because it has already been done above

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

########################## SALINITY ##########################

# declare -a sal_list=("BCC-CSM2-MR" "CMCC-CM2-SR5" "CMCC-ESM2" "FGOALS-f3-L" "FGOALS-g3" "MIROC-ES2L" "MIROC6" "MRI-ESM2-0" "NorESM2-LM")
# var="sos"
# expt="historical"
# yearmin=1956
# yearmax=1984

# for t in ${sal_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# if [[ $t == "MRI-ESM2-0" ]]
# 	# 	then
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g lonlat -v $var -e $expt
# 	# else
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt
# 	# fi

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# expt="ssp585"

# # Present
# yearmin=2017
# yearmax=2026

# for t in ${sal_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# if [[ $t == "MRI-ESM2-0" ]]
# 	# 	then
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g lonlat -v $var -e $expt
# 	# else
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt
# 	# fi

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# # Mid-century
# yearmin=2046
# yearmax=2055

# for t in ${sal_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# Removing this line because it has already been done above

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# # End of the century
# yearmin=2091
# yearmax=2100

# for t in ${sal_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# Removing this line because it has already been done above

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

########################## MIXED LAYER ##########################

# declare -a mix_list=("ACCESS-ESM1-5" "BCC-CSM2-MR" "CMCC-CM2-SR5" "CMCC-ESM2" "FGOALS-f3-L" "FGOALS-g3" "GFDL-ESM4" "GISS-E2-1-G" "MIROC6" "MPI-ESM1-2-HR" "MPI-ESM1-2-LR" "MRI-ESM2-0" "NorESM2-LM") # Removed AWI-CM-1-1-MR (unstructured grid)
# var="mlotst"
# expt="historical"
# yearmin=1956
# yearmax=1984

# for t in ${mix_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# if [[ $t == "GISS-E2-1-G" ]]
# 	# 	then
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g lonlat -v $var -e $expt
# 	# else
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt
# 	# fi

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# expt="ssp585"

# # Present
# yearmin=2017
# yearmax=2026

# for t in ${mix_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# if [[ $t == "GISS-E2-1-G" ]]
# 	# 	then
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g lonlat -v $var -e $expt
# 	# else
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt
# 	# fi

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# # Mid-century
# yearmin=2046
# yearmax=2055

# for t in ${mix_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# This has been done above

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# # End of the century
# yearmin=2091
# yearmax=2100

# for t in ${mix_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# This has been done above

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

########################## NITRATE ##########################

# declare -a n_list=("ACCESS-ESM1-5" "CMCC-ESM2" "GFDL-ESM4" "MIROC-ES2L" "MPI-ESM1-2-HR" "MPI-ESM1-2-LR" "MRI-ESM2-0" "NorESM2-LM")
# var="no3os"
# expt="historical"
# yearmin=1956
# yearmax=1984

# for t in ${n_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# if [[ $t == "GFDL-ESM4" ]]
# 	# 	then
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g lonlat -v $var -e $expt
# 	# else
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt
# 	# fi

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# expt="ssp585"

# # Present
# yearmin=2017
# yearmax=2026

# for t in ${n_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# if [[ $t == "GFDL-ESM4" ]]
# 	# 	then
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g lonlat -v $var -e $expt
# 	# else
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt
# 	# fi

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# # Mid-century
# yearmin=2046
# yearmax=2055

# for t in ${n_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# This has been done above

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# # End of the century
# yearmin=2091
# yearmax=2100

# for t in ${n_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# This has been done above

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

########################## PHOSPHATE ##########################

# declare -a p_list=("CMCC-ESM2" "GFDL-ESM4" "MIROC-ES2L" "MRI-ESM2-0" "NorESM2-LM")
# var="po4os"
# expt="historical"
# yearmin=1956
# yearmax=1984

# for t in ${p_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# if [[ $t == "GFDL-ESM4" ]]
# 	# 	then
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g lonlat -v $var -e $expt
# 	# else
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt
# 	# fi

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# expt="ssp585"

# # Present
# yearmin=2017
# yearmax=2026

# for t in ${p_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# if [[ $t == "GFDL-ESM4" ]]
# 	# 	then
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g lonlat -v $var -e $expt
# 	# else
# 	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt
# 	# fi

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# # Mid-century
# yearmin=2046
# yearmax=2055

# for t in ${p_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# This has been done

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done

# # End of the century
# yearmin=2091
# yearmax=2100

# for t in ${p_list[@]}; do
# 	# Merge the files into 1 file per model per scenario
# 	# This has been done

# 	# Select years we want
# 	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

# 	# Remap to 1x1 degree grid
# 	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
# 	rm selectyears/* # Free up space

# 	# Change frequency
# 	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

# 	echo $t
# done

# # Make the seasonal ensembles
# for s in ${season_list[@]}; do
# 	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
# done


########################## AMMONIUM ##########################

declare -a a_list=("GFDL-ESM4")
var="nh4os"
expt="historical"
yearmin=1956
yearmax=1984

for t in ${a_list[@]}; do
	# Merge the files into 1 file per model per scenario
	# if [[ $t == "GFDL-ESM4" ]]
	# 	then
	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g lonlat -v $var -e $expt
	# else
	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt
	# fi

	# Select years we want
	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

	# Remap to 1x1 degree grid
	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
	rm selectyears/* # Free up space

	# Change frequency
	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

	echo $t
done

# Make the seasonal ensembles
for s in ${season_list[@]}; do
	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
done

expt="ssp585"

# Present
yearmin=2017
yearmax=2026

for t in ${a_list[@]}; do
	# Merge the files into 1 file per model per scenario
	# if [[ $t == "GFDL-ESM4" ]]
	# 	then
	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g lonlat -v $var -e $expt
	# else
	# 	bash 01_mergeFiles.sh -i "/Volumes/SeagateHub/04_LarvaBRT/${var}/" -m $t -g curvilinear -v $var -e $expt
	# fi

	# Select years we want
	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

	# Remap to 1x1 degree grid
	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
	rm selectyears/* # Free up space

	# Change frequency
	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

	echo $t
done

# Make the seasonal ensembles
for s in ${season_list[@]}; do
	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
done

# Mid century
yearmin=2046
yearmax=2055

for t in ${a_list[@]}; do
	# Merge the files into 1 file per model per scenario
	# This has been done

	# Select years we want
	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

	# Remap to 1x1 degree grid
	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
	rm selectyears/* # Free up space

	# Change frequency
	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

	echo $t
done

# Make the seasonal ensembles
for s in ${season_list[@]}; do
	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
done

# End of the century
yearmin=2091
yearmax=2100

for t in ${a_list[@]}; do
	# Merge the files into 1 file per model per scenario
	# This has been done

	# Select years we want
	bash 02_selectYears.sh -m $t -v $var -e $expt -f $yearmin -l $yearmax

	# Remap to 1x1 degree grid
	bash 03_remapGlobal.sh -i "selectyears/tmpfile.nc" -m $t -v $var -e $expt -n $yearmin -x $yearmax
	rm selectyears/* # Free up space

	# Change frequency
	bash 06_month2Season.sh -i "remapped" -m $t -v $var -e $expt -n $yearmin -x $yearmax

	echo $t
done

# Make the seasonal ensembles
for s in ${season_list[@]}; do
	bash 07_createEnsemble.sh -i "newfreq" -v $var -e $expt -o "mean" -n $yearmin -x $yearmax -f $s
done