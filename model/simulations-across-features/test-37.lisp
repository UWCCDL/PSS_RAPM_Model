
(load "/projects/actr/actr7/load-act-r.lisp")
(load "/projects/actr/models/PSS_RAPM_Model/model/rapm-device.lisp")
(load "/projects/actr/models/PSS_RAPM_Model/model/rapm-model-newchoice.lisp")
(load "/projects/actr/models/PSS_RAPM_Model/model/rapm-simulations.lisp")
(general-simulations 200 :fname "simulations-devel4-newchoice-newprobs-newbold-tick-25-upper-3.0-difficulty-1.txt" :tickvals '(25) :upprbndvals '(3.0) :difficulty '(1))
(quit)