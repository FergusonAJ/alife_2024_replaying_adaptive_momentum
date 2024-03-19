#!/bin/bash

THIS_DIR=$(pwd)

DIR_LIST=$(ls | grep -oP "\d\d\d")

for DIR in ${DIR_LIST}
do
  cd ${DIR}
  echo "Decompressing file in ${DIR}"
  tar -xzf combined_replay_cross_data.tar.gz
  cd ${THIS_DIR}
done

