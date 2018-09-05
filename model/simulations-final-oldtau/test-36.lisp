
(load "/projects/actr/actr7/load-act-r.lisp")
(load "/projects/actr/models/PSS_RAPM_Model/model/rapm-device.lisp")
(load "/projects/actr/models/PSS_RAPM_Model/model/rapm-model-newchoice2.lisp")
(load "/projects/actr/models/PSS_RAPM_Model/model/rapm-simulations.lisp")
(general-simulations 100 :fname "simulations-september2018-tick-30-upper-1.0-difficulty-1.txt" :tickvals '(30) :upprbndvals '(1.0) :difficulty '(1))
(quit)
