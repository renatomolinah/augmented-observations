#!/bin/bash
set -euo pipefail

################################################################################
# P-RAIN ensemble wrapper
# ------------------------------------------------------------------------------
# Select ensemble members and submit or sequence P-RAIN jobs.
################################################################################

#=============================
# 1. Configure the run
#=============================

ensnum="${AO_PRAIN_ENSNUM:-1000}" #[1-1000]
method="${AO_PRAIN_METHOD:-sequ}" #[rand, sequ]
runner="${AO_PRAIN_RUNNER:-local}" #[local, bsub]
submit_sleep="${AO_PRAIN_SUBMIT_SLEEP:-0}"
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
job_script="${AO_PRAIN_JOB_SCRIPT:-${script_dir}/prain_job.sh}"

# rand: generate random ensemble member indices.
# sequ: use the sequence 1:ensnum as member indices.

maxens=1000


echo Hello ${USER}. You have selected $ensnum members to run P-Rain.
echo Runner: ${runner}
echo Program Start: $(date)

#generate random number
if [ "$method" == "rand" ]; then

    reverse=false 
    #if ensnum is more than half of total member(maxens), we generate unwanted index instead
    #e.g., we have 1000 members in total and we want to run 600 members. 
    #      This program will generate 400 random numbers and then filter them out from a sequence [1-1000].
    #      So we will have 600 random numbers left for the ensemble runs.
    if [ "$ensnum" -gt "$(( $maxens / 2 ))" ]; then
	ensnum=$(( $maxens - $ensnum ))
	reverse=true
    fi

    #generating random numbers
    count=1
    while [ "$count" -le "$ensnum" ]; do
	number[$count]=$(( $(( $RANDOM%$maxens )) + 1 ))
	recal=true
	while ( $recal ); do
		# checking duplicate
		recal=false
		for i in $(seq 1 1 $(( $count-1 ))); do
			if [ "${number[$i]}" -eq "${number[$count]}" ]; then
				number[$count]=$(( $(( $RANDOM%$maxens )) + 1 ))
				recal=true
			fi
		done
	done
	count=$(( $count + 1 ))
    done

    # reverse the random numbers (only for big $ensnum operation)
    if ( $reverse ); then
	#sorting the random number: selection sort
	for iii in $(seq 1 $ensnum); do
	    pos=$iii
	    for jjj in $(seq $(( $iii+1 )) $ensnum); do
		if [ ${number[$jjj]} -lt ${number[$pos]} ]; then
			pos=$jjj
		fi	
	    done

	    tempnum=${number[$iii]}
	    number[$iii]=${number[$pos]}
	    number[$pos]=$tempnum
	done
	number_orig=("${number[@]}")
 
	#echo original random: ${number_orig[@]}

	#reverse
	opos=0 #original array position # after copying the array, it starts from 0 
	ipos=1 #inversed array position
	for iii in $(seq 1 $(( $maxens ))); do
	    # if number is in the original array, skip
	    if [ "${iii}" == "${number_orig[$opos]}" ]; then
		opos=$(( $opos + 1 ))
		continue  
	    fi
	    # otherwise, save it
	    number[$ipos]=$iii
	    ipos=$(( $ipos + 1 ))
	done
	#echo inversed random: ${number[@]}
	
	#reset the $ensnum and $reverse flag
	ensnum=$(( $maxens - $ensnum ))
	reverse=false
    fi

elif [ "$method" == "sequ" ]; then

	number=`seq 1 $ensnum`

else
    echo "Unsupported AO_PRAIN_METHOD: ${method}" >&2
    exit 1
fi

#echo ${number[@]}

count=1
for num in ${number[@]}; do
        if [ "$runner" == "bsub" ]; then
	        bsub -P svf -q normal -n 1 -W 0:15 -R "span[ptile=1]" -o PRAIN%J.out -e PRAIN%J.err "bash ${job_script} $count $num"
        elif [ "$runner" == "local" ]; then
                bash "$job_script" "$count" "$num"
        else
                echo "Unsupported AO_PRAIN_RUNNER: ${runner}" >&2
                exit 1
        fi

        if [ "$submit_sleep" -gt 0 ]; then
                sleep "$submit_sleep"
        fi
	count=$(( $count + 1 ))
done

echo Program End: $(date)

exit 0
