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

# Create our output file
CROSS_INFO_FILE=../combined_evolution_cross_data.csv
printf "" > ${CROSS_INFO_FILE}

head -n 1 ${SCRATCH_EXP_DIR}/reps/001/cross_info.csv > ${CROSS_INFO_FILE}

# Actually scrape the data  
for REP_IDX in $( seq 1 500 )
do
    ZERO_PADDED_IDX=$( ${REPO_ROOT_DIR}/global_shared_files/zero_pad.sh ${REP_IDX} 3 )
    echo "${ZERO_PADDED_IDX}"
    REP_CROSS_INFO_FILE=${SCRATCH_EXP_DIR}/reps/${ZERO_PADDED_IDX}/cross_info.csv
    CROSS_INFO_LINES=$( wc -l ${REP_CROSS_INFO_FILE} | grep -oP "^\d+"  )
    if [ ${CROSS_INFO_LINES} -gt 1 ]
    then
        tail -n $(( ${CROSS_INFO_LINES} - 1 )) ${REP_CROSS_INFO_FILE} >> ${CROSS_INFO_FILE}
    fi
done
