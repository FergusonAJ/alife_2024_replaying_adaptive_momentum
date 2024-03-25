#!/bin/bash

THIS_DIR=$(pwd)

IS_VERBOSE=0
while getopts "v" OPT; do
  case $OPT in
    v)
      IS_VERBOSE=1
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      return 1
      ;;
  esac
done

# Warn that we're in verbose mode
if [ ! ${IS_VERBOSE} -eq 0 ]
then
  echo "[VERBOSE] Verbose flag (-v) detected. Printing additional debug information."
fi

# Grab global variables and helper functions
# Root directory -> The root level of the repo, should be directory just above 'experiments'
REPO_ROOT_DIR=$(pwd | grep -oP ".+(?=/experiments/)")
if [ ! ${IS_VERBOSE} -eq 0 ]
then
  echo "[VERBOSE] Found repo root dir: ${REPO_ROOT_DIR}"
  echo "[VERBOSE] Loading global config and helper functions..."
fi
. ${REPO_ROOT_DIR}/config_global.sh
. ${REPO_ROOT_DIR}/global_shared_files/helper_functions.sh

# Extract info about this experiment
EXP_NAME=$( get_cur_exp_name )
EXP_REL_PATH=$( get_cur_relative_exp_path )
EXP_ROOT_DIR=${REPO_ROOT_DIR}/${EXP_REL_PATH}
SCRATCH_EXP_DIR=${SCRATCH_ROOT_DIR}/${EXP_REL_PATH}
if [ ! ${IS_VERBOSE} -eq 0 ]
then
  echo "[VERBOSE] Experiment name: ${EXP_NAME}"
  echo "[VERBOSE] Experiment path (relative): ${EXP_REL_PATH}"
  echo "[VERBOSE] Experiment root dir (not relative): ${EXP_ROOT_DIR}"
  echo "[VERBOSE] Experiment dir on scratch: ${SCRATCH_ROOT_DIR}"
  echo ""
fi


# One selected replicate
for BASE_REP_ID in 2359 5501
do
    ZERO_PADDED_MAIN_ID=$( ${REPO_ROOT_DIR}/global_shared_files/zero_pad.sh ${BASE_REP_ID} 5 )
    echo "Scraping replays for rep: ${ZERO_PADDED_MAIN_ID}"

    # Create our output file
    REP_DIR=../reps/${ZERO_PADDED_MAIN_ID}
    mkdir -p ${REP_DIR}
    FILENAME_PREFIX=combined_selected_replay_cross_data
    CROSS_INFO_FILE=${REP_DIR}/${FILENAME_PREFIX}.csv
    printf "" > ${CROSS_INFO_FILE}

    REPLAY_DIR=${SCRATCH_EXP_DIR}/reps/${ZERO_PADDED_MAIN_ID}/selected_replays
    head -n 1 ${REPLAY_DIR}/1/cross_info.csv > ${CROSS_INFO_FILE}

    # Actually scrape the data  
    for REPLAY_GEN in $(seq 1 767) 
    do
        printf "  ${REPLAY_GEN}"
        REP_CROSS_INFO_FILE=${REPLAY_DIR}/${REPLAY_GEN}/cross_info.csv
        CROSS_INFO_LINES=$( wc -l ${REP_CROSS_INFO_FILE} | grep -oP "^\d+"  )
        if [ ${CROSS_INFO_LINES} -gt 1 ]
        then
            tail -n $(( ${CROSS_INFO_LINES} - 1 )) ${REP_CROSS_INFO_FILE} >> ${CROSS_INFO_FILE}
        fi
    done
    cd ${REP_DIR}
    tar -czf ${FILENAME_PREFIX}.tar.gz ${FILENAME_PREFIX}.csv
    cd ${THIS_DIR}
    printf "  done!\n"
done
printf "  done!\n"
