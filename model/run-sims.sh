#!/bin/bash
# ====================================================================
# run-sims.sh
# ====================================================================
# Runs Lisp simulations of an ACT-R model.
# It expects that all the files to run are named "simulations_*.lisp".
# ====================================================================

if [ -f pids.txt ]; then
    rm pids.txt
fi

for file in test-*.lisp; do
    # Starts a nice thread
    nice sbcl --load ${file} &
    # Saves the pids in case they need to be aborted
    echo "$!" >> pids.txt
done
