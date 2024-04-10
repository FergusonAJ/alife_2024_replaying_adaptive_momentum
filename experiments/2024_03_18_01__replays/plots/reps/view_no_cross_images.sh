#!/bin/bash

# Check command line arguments
if [ ! $# -eq 2 ]
then
  echo "Error! Expected exactly one command line argument:"
  echo "  1. The number of crosses (0 for reps with no crosses, 1 for single crosses, etc)" 
  echo "  2. The search string to look for in each replicate's directory"
  exit 1
fi

# Choose the correct set of replicates based on the first command line arg
CROSS_SELECTOR=$1
if [ ${CROSS_SELECTOR} -eq 0 ]
then
  echo "Looking at no-cross replicates"
  REP_LIST="134 158 164 175 252 339 365 394 446 450"
elif [ ${CROSS_SELECTOR} -eq 1 ]
then
  echo "Looking at single-cross replicates"
  REP_LIST="011 050 075 083 105 282 343 400 408 415"
elif [ ${CROSS_SELECTOR} -eq 2 ]
then
  echo "Looking at replicates that cross twice"
  REP_LIST="093 124 138 263"
elif [ ${CROSS_SELECTOR} -eq -1 ]
then
  echo "Looking at our three selected replicates for the paper"
  REP_LIST="339 400 263"
else
  echo "Only values of 0, 1, and 2 supported for the cross selector"
  echo "Or -1 for the selected replicates for the paper"
  exit 2
fi

# Grab the search string from the second command line arg
SEARCH_STR=$2
echo "Search string: ${SEARCH_STR}"

# Construct the command to be executed
CMD_STR="eog "
for REP_ID in ${REP_LIST}
do
  CMD_STR="${CMD_STR} ${REP_ID}/${SEARCH_STR}"
done

# Print and then execute the command
echo "Running the following command:"
echo "${CMD_STR}"
$( ${CMD_STR} )
