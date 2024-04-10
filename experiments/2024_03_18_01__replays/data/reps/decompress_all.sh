#!/bin/bash

THIS_DIR=$(pwd)

DIR_LIST=$(ls | grep -oP "\d\d\d")

for DIR in ${DIR_LIST}
do
  cd ${DIR}
  echo "Decompressing file in ${DIR}"
  tar -xzf combined_replay_cross_data.tar.gz
  if [ -s combined_shuffled_replay_cross_data.tar.gz ] 
  then
    echo "  Found shuffled replay data, decompressing..."
    tar -xzf combined_shuffled_replay_cross_data.tar.gz
  fi
  if [ -s combined_selected_replay_cross_data.tar.gz ] 
  then
    echo "  Found selected replay data, decompressing..."
    tar -xzf combined_selected_replay_cross_data.tar.gz
  fi
  cd ${THIS_DIR}
done

