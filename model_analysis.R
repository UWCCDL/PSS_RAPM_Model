# Model analysis
source("functions.R")
sims <- read.table(unzip("simulations.zip"),
                                header=T,
                                sep=","
)

maxtime <- read.table("time.txt", header=T, sep="\t")
sims$MaxTime <- maxtime$MaxTime[1:9292757]

acrossfeatures <- aggregate(sims[c("Solved", "SolutionsLeft")], 
                            list(T=sims$MaxTime, D1=sims$D1, D2=sims$D2, 
                                 R=sims$NRules, C=sims$NCorrect, A=sims$Alpha), 
                            mean)

acrosstime <- aggregate(sims[c("Solved", "SolutionsLeft")], 
                        list(F=sims$NFeatures, D1=sims$D1, D2=sims$D2, 
                             R=sims$NRules, C=sims$NCorrect, A=sims$Alpha),
                        mean)

superplot <- function() {
  layout(matrix(1:2, ncol=2))
  sub <- subset(sims, sims$Alpha==0.1 & sims$MaxTime >= 200 & sims$NFeatures <= 7)
  plot.by.2factors(sub, "Solved", "D2", "NRules", rng=c(0.5,1,0.1))
  plot.by.2factors(sub, "Solved", "D1", "NRules", rng=c(0.5,1,0.1))
  
}

layout(matrix(1:2, ncol=2))
sub <- subset(sims, sims$Alpha==0.1 & sims$MaxTime > 150 & sims$NRules > 3 & sims$NCorrect==1)
plot.by.2factors(sub, "Solved", "D2", "NFeatures", rng=c(0,1,0.1), legpos = "bottomright")
plot.by.2factors(sub, "Solved", "D1", "NFeatures", rng=c(0,1,0.1))

plot.by.1factor(sub, "Solved", "D2", rng=c(0.7,1,0.1), legpos = "bottomright")
plot.by.1factor(sub, "Solved", "D1", rng=c(0.7,1,0.1))

z <- function(x) {max(fmri.stimulus(10, onsets=1, durations = x, type="canonical"))}
Z <- Vectorize(z)
k <- aggregate(sub[c("Activity", "Solved")], list(D2=sub$D2), mean)
k$NormActivity <- (k$Activity - min(k$Activity))/(max(k$Activity)-min(k$Activity))
k$BOLD <- Z(k$NormActivity)

plot(k$D2, k$BOLD, type="l", lwd=2, ylab="Predicted BOLD", xlab=expression(lambda[D]))
points(k$D2, k$BOLD, pch=21, cex=1.5, bg="white", lwd=2)