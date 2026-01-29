#!/bin/bash

#WRK=$SCRATCH/investigation/cycles
WRK=$SCRATCH/investigation/deode_spp
#WRK=$SCRATCH/investigation/vterm

# Experiment list
#EXP="cy46_spp_p01 cy49_spp_p01"
#EXP="cy46_spp_p01_fix2"
#EXP="Cy46h111_5SPPEDAENSSuP"
#EXP="Cy46h111_19SPPCV043EDAENSSuP"
EXP="de_fin_5SPP_bnd_9km"
#EXP="de_fin_noSPP_bnd_9km"

# isx: an ugly solution to running p01 and p01x exps and wanting to
# rename everything to p01.
isx=

# Are the files in ec or ectmp
ec=ec

# User account
usr=fi3

# Either provide the dates in a list, or use a timerange
# with SDATE and EDATE. If DATELIST is not empty SDATE and
# EDATE are automatically skipped.
# 
#DATELIST="20250724" # 20250111"
DATELIST="20240826" # 20250111"
#DATELIST="20251017" #"20241119"
# or
SDATE=20220716
EDATE=20220721

# Forecast start hour
# Use "00 12" for multiple times
#
HH="00" #12"  

# Forecast length
# Use "0012 0024" for multiple times, or "0012" for a single fctime.
#
FCLEN="033" # 012 018" 
#FCLEN="006 012 018 024"

# Members
# Use "001 002" for multiple members
#
MEMB="000 001 002" # 001 002 003" # 001 002 003" # 004 005 006" # 001 002 003 004 005 006" 


# Check for dir
if [ ! -d $WRK ]; then
    mkdir -p $WRK
fi

# Generate datelist if DATELIST is empty
if [ -z "$DATELIST" ]; then
    date=$SDATE
    while [ $date -le $EDATE ]; do

	DATELIST="$DATELIST $date"
        date=$(date -d "$date + 1 days" +'%Y%m%d')
    done
fi

# GET FORECAST
for date in $DATELIST; do

    # Loop over HH (date won't allow this...)
    for hh in $HH; do

	# Loop over members
	for memb in $MEMB; do

	    yy=$(date -d $date +%Y)
	    mm=$(date -d $date +%m)
	    dd=$(date -d $date +%d)

	    # Loop over forecast lengths
	    for fclen in $FCLEN; do	       

		for exp in $EXP; do
		    # Don't fetch existing data
		    file=$WRK/${exp}-$date${hh}-m${memb}+${fclen}h
		    if [ ! -f $file ]; then
			echo "Fetching $file"
			#ecp ${ec}:/${usr}/harmonie/${exp}${isx}/$yy/$mm/$dd/$hh/mbr$memb/ICMSHHARM+0$fclen $file
			ecp ${ec}:/${usr}/deode/${exp}${isx}/archive/$yy/$mm/$dd/$hh/mbr$memb/ICMSHDEOD+0${fclen}h00m00s $file
		    else
			echo "Existing file found $file"
		    fi
		    
		    file2=$WRK/${exp}_pl-$date${hh}-m${memb}+${fclen}h
		    if [ ! -f $file2 ]; then
			echo "Fetching $file2"
			#ecp ${ec}:/${usr}/harmonie/${exp}${isx}/$yy/$mm/$dd/$hh/mbr$memb/PFHARMMETCOOP25B+0$fclen $file2
			#ecp ${ec}:/${usr}/deode/${exp}${isx}/archive/$yy/$mm/$dd/$hh/mbr$memb/GRIBPFDEOD+0${fclen}h00m00s $file2
		    else
			echo "Existing file found $file2"
		    fi
		done
	    done
	done
    done
done
