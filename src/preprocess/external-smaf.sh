#!/bin/bash

#
# sample script to fake the External Smaf protocol (use with the ERG)
#

path=$(dirname $0)
if [ "${path#./}" != "${path}" ]; then
  path="$(pwd)/${path#./}"
fi
if [ "${path#/}" = "${path}" ]; then
  if [ "${path}" = "." ]; then
    path="$(pwd)";
  else 
    path="$(pwd)/${path}"
  fi
fi

while read -d '' foo; do
  cat ${path}/erg.external-smaf
  echo ""
done
