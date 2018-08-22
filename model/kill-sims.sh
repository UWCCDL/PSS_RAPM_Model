#!/bin/bash
# ====================================================================
# kill-sims.sh
# ====================================================================
# Kills any SBCL process
# Useful to kill malstarted simulations
# ====================================================================

for process in `ps -A | grep sbcl | cut -f1 -d' '`; do
    kill $process
done

