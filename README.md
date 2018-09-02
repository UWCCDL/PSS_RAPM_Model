# A model of the role of reinforcement learning in RAPM problems 

This is an ACT-R model that shows how reinforcement learning
parameters (and, in particular, sensititivity to negative feedback)
shapes the solving of Raven's Advanced Progressive Matrices (RAPM), a
common test of fluid intelligence.

## Experimental Data

The model was tested against behavioral and fMRI data collected by
Stocco, Prat & Graham. The raw behavioral data in experiments 1-3 is
saved in three folders, `experiment1`, `experiment2`, and
`experiment3`. Comparisong accuracy and response time data across
problem difficulty comes a different experiment from our lab, and is
saved in the `firestorm.txt` text file. 

# Model Code

The model is developed in ACT-R 7.5, with the "old-style" devices
written in Common Lisp.  All of the model code is contained in four
different Lisp files:

  1.  `rapm-model.lisp`.
  2.  `rapm-device.lisp`. This is the model's device, which encodes
  the RAPM problems and interacts with the model.
  3.  `rapm-problems.lisp`. Contains a Lisp-like definition of a RAPM
  problem, together with functions to analyze them.
  4.  `rapm-simulations`. Contains a set of functions to run large
  model simulations across parameter space 

## Loading and Running the Model

To run the model, follow these steps:

  1. Load ACT-R 7.5.x
  2. Load the `rapm-device.lisp` file. This will automatically load
  the `rapm-problems.lisp` file as well.
  3. Load the `rapm-model.lisp` file. This will load the ACT-R model
  code.
  4. Before running the model, initialze both model and device by
  calling the `(rapm-reload)` function. The function will properly
  initialize the device, reset the model, and connect the model's
  visual system to the device's interface.
  

## Running Simulations

The `rapm-simulations.lisp` file contains many handy functions for
running simulations and saving the results on a file. When saving
results, each run on an simulated experiment (by default, 16 4-feature
RAPM problems) will be saved as a single line.

### Large-Scale Simulations

Large-scale simulations across parameter space are handled by
generating multiple Lisp files that run simulations on different
portions of the parameter space. The Python script `generate-test.py`
will generate such files across a modifiable list of parameters (you
might need to change the script's specific paths to fit your own 
system). The Python script generates a number of scripts corresponding
to the different regions in which the parameter space is partitioned
(by default, 64 different scripts).  Each script is named
`test-<N>.lisp` file, with `N` being a counter from 1 to the max
number of partitions. 

A series of four shell scripts manages the various Lisp files:

  1. `run-sims.sh` will launcha new instance of SBCL (by default; modify
  the Python script to use a different Lisp interpreter) on each Lisp
  test file. Each process' PID will be saved to a `pids.txt` file.
  2. 'kill-sims.sh` will abort all the SBCL processes spawned by
  `run-sims.sh`. The script will kill, in series, all the processes
  with a PID listed in `pids.txt` (you should run this as `sudo`).
  3. `merge.sh` will merge all the generated files into a single text
  file, and the zip it.
  4. `partial.sh` will produce a file named `partial.txt`, which is
  like the file produced by `merge.sh` but carefully handles partially
  completed simulations. This is useful to inspect results before all
  the simulations are complete.


## Unit Testing

No unit testing yet. But testing functions and testing problems are
sparsed here and there.