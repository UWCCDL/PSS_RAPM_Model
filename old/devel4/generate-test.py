#!/usr/bin/env python

MAIN="""
(load "/projects/actr/actr7/load-act-r.lisp")
(load "/projects/actr/models/PSS_RAPM_Model/devel4/rapm-device.lisp")
(load "/projects/actr/models/PSS_RAPM_Model/devel4/rapm-model-newchoice.lisp")
(load "/projects/actr/models/PSS_RAPM_Model/devel4/rapm-simulations.lisp")
(general-simulations 200 :fname "simulations-devel4-newchoice-newprobs-newbold-tick-%s-upper-%s.txt" :tickvals '(%d) :upprbndvals '(%0.1f))
(quit)
"""

count=0
for tickval in [20, 25, 30, 35]:
    for upper in [1.0, 2.0, 3.0, 4.0]:
        count += 1
        fout = open("test-%02d.lisp" % count, 'w')
        code = MAIN % (tickval, upper, tickval, upper)
        fout.write(code)
        fout.flush()
        fout.close()
