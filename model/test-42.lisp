
(load "/projects/actr/actr7/load-act-r.lisp")
(load "/projects/actr/models/PSS_RAPM_Model/model/rapm-device.lisp")
(load "/projects/actr/models/PSS_RAPM_Model/model/rapm-model-newchoice.lisp")
(load "/projects/actr/models/PSS_RAPM_Model/model/rapm-simulations.lisp")
(general-simulations 200 :fname "simulations-devel4-newchoice-newprobs-newbold-tick-30-upper-1.0-difficulty-2.txt" :tickvals '(30) :upprbndvals '(1.0) :difficulty '(2))
(quit)
