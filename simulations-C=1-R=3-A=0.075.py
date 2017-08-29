#!/usr/bin/env python
import rapm
nsims = 200
temp = 0.1
anticorrelated = True
lowbounded = True

d_vals = [x / 100.0 for x in list(range(0, 201, 20))]

F = (4, 5, 6, 7)
C = (1,)   # (1, 2)
R = (3,)   # (2, 3, 4)
A = (0.07,) # (0.1, 0.2, 0.3, 0.4) or (0.25, 0.5, 0.75, 0.1)
T = (100, 150, 200, 250)
output = open("simulations-C=1-R=3-A=0.075.txt", "w")

fstring = '%.3f,' * 11
fstring = fstring + '%.10f\n'

output.write("NFeatures,NCorrect,NRules,Alpha,MaxTime,D1,D2,MaxTime,Time,SolutionsLeft,Solved,Activity\n")

for f in F:
    for c in C:
        for r in R:
            for a in A:
                for t in T:
                    for d1 in d_vals:
                        for d2 in d_vals:
                            ps = rapm.Problem(nfeatures = f, ncorrect = c,  nrules = r, maxruns = t)
                            ps.alpha = a
                            ps.temperature = temp
                            ps.anticorrelated = anticorrelated
                            ps.lowbounded = lowbounded
                            ps.d1 = d1
                            ps.d2 = d2
                                
                            for n in range(nsims):
                                time, solutions, activity = ps.simulate()
                                s = 0
                                ns = len(solutions)
                                if ns == 0:
                                    s = 1
                                data = (f, c, r, a, t, d1, d2, t, time, ns, s, activity)
                                output.write(fstring % data)
                                output.flush()

output.close()
