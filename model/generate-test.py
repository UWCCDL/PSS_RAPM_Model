#!/usr/bin/env python

MAIN="""
(load "/projects/actr/actr7/load-act-r.lisp")
(load "/projects/actr/models/PSS_RAPM_Model/model/rapm-device.lisp")
(load "/projects/actr/models/PSS_RAPM_Model/model/rapm-model-newchoice2.lisp")
(load "/projects/actr/models/PSS_RAPM_Model/model/rapm-simulations.lisp")
(general-simulations 100 :fname "simulations-september2018-tick-%s-upper-%s-difficulty-%s.txt" :tickvals '(%d) :upprbndvals '(%0.1f) :difficulty '(%d))
(quit)
"""

count=0  # Starting point for filenames

for tickval in [24,28,32,36]:
    for upper in [1.0, 2.0, 3.0, 4.0]:
        for difficulty in [4, 3, 2, 1]:
            count += 1
            fout = open("test-%02d.lisp" % count, 'w')
            code = MAIN % (tickval, upper, difficulty, tickval, upper, difficulty)
            fout.write(code)
            fout.flush()
            fout.close()
