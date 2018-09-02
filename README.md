# An ACT-R model of the role of reinforcement learning in solving RAPM problems

This is an ACT-R model that shows how reinforcement learning
parameters (and, in particular, sensititivity to negative feedback)
shapes the solving of Raven's Advanced Progressive Matrices (RAPM), a
common test of fluid intelligence.

The model was tested against behavioral and fMRI data collected by
Stocco, Prat & Graham.

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

## Running the model

To run the model, follow these steps:

  1. Load ACT-R 7.5.x
  2. Load the `rapm-device.lisp` file. This will automatically load
  the `rapm-problems.lisp` file as well.
  3. Load the `rapm-
  

## Unit Testing

No unit testing yet. But testing functions and testing problems are
sparsed here and there.