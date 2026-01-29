#!/bin/bash

# DEODE runs
WRK=$SCRATCH/investigation/deode_spp
EXP="de_fin_5SPP_bnd_9km"
is_deode=true

# Harmonie/HarmonieCSC runs
#WRK=$SCRATCH/investigation/cycles
#EXP="cy46_spp_p01 cy49_spp_p01" # Multiple experiments can be fetched
#is_deode=false

# Are the files in ec or ectmp
ec=ec

# User account
usr=fi3

# isx: an ugly solution to running p01 and p01x exps and wanting to
# rename everything to p01.
isx=

# Either provide the dates in a list ("DATE1 DATE2" for multiple or
# "DATE1" for single date), or use a timerange with SDATE and EDATE.
#If DATELIST is not empty SDATE and EDATE are automatically skipped.
# 
DATELIST="20240826" 
# or
SDATE=20220716
EDATE=20220721

# Forecast start hour
# Use "00 12" for multiple times, or "00" for a single time
#
HH="00"  

# Forecast length
# Use "0012 0024" for multiple times, or "0012" for a single fctime.
#
FCLEN="035"

# Members
# Use "001 002" for multiple members, or "000" for a single member
#
MEMB="000 001"


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
			# Deode and Harmonie/HarmonieCSC runs have different path and file name structure
			if $is_deode; then
			    ecp ${ec}:/${usr}/deode/${exp}${isx}/archive/$yy/$mm/$dd/$hh/mbr$memb/ICMSHDEOD+0${fclen}h00m00s $file
			else
			    ecp ${ec}:/${usr}/harmonie/${exp}${isx}/$yy/$mm/$dd/$hh/mbr$memb/ICMSHHARM+0$fclen $file
			fi
		    else
			echo "Existing file found $file"
		    fi
		    
		    if $is_deode; then
			# No pl files for Deode
			continue
		    else
			file2=$WRK/${exp}_pl-$date${hh}-m${memb}+${fclen}h
			if [ ! -f $file2 ]; then
			    echo "Fetching $file2"
			    ecp ${ec}:/${usr}/harmonie/${exp}${isx}/$yy/$mm/$dd/$hh/mbr$memb/PFHARMMETCOOP25B+0$fclen $file2	
			else
			    echo "Existing file found $file2"
			fi
		    fi
		done
	    done
	done
    done
done
