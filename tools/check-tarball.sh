#!/bin/sh
# Fails if a built bdhs tarball contains anything that could be DHS microdata.
# Usage: sh tools/check-tarball.sh bdhs_*.tar.gz
set -e
bad=$(tar tzf "$1" | grep -iE '\.(dta|sav|zsav|sas7bdat|zip|dat|parquet)$' || true)
if [ -n "$bad" ]; then
  echo "Possible microdata in $1:"; echo "$bad"; exit 1
fi
echo "No microdata file types in $1."
