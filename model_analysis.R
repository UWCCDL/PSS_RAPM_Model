# Model analysis
source("functions.R")
sims <- read.table(unzip("new-simulations.zip"),
                                header=T,
                                sep=","
)

# Fix two problems. First, MaxTime is repeated

sims$MaxTime.1 <- NULL

# Then, sometimes problems are actually "solved" when they exceed MaxTime. 

sims$SolutionsLeft[sims$Time > sims$MaxTime] <- sims$SolutionsLeft[sims$Time > sims$MaxTime] + 1
sims$Solved[sims$SolutionsLeft > 0] <- 0

#maxtime <- read.table("time.txt", header=T, sep="\t")
#sims$MaxTime <- maxtime$MaxTime[1:9292757]

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

c1 <- subset(sims, sims$NCorrect==1)
c2 <- subset(sims, sims$NCorrect==2)

layout(matrix(1:2, ncol=2))
sub <- subset(sims,  sims$MaxTime >= 100 & sims$NFeatures <= 7 & sims$NCorrect==1 & sims$Alpha==0.1 & sims$D1>=0.5 & sims$D2>=0.5)
sub <- subset(sub,  sims$D1<=1 & sims$D2<=1)
sub1 <- subset(sub, sims$D1 > 0.75)
sub2 <- subset(sub, sims$D2 > 0.75)
plot.by.2factors(sub1, "Solved", "D2", "NFeatures", rng=c(0,1,0.1), legpos = "bottomright")
grid()
plot.by.2factors(sub2, "Solved", "D1", "NFeatures", rng=c(0,1,0.1))
grid()

reducedvals <- subset(sims, sims$D1 <=1 & sims$D2 <= 1)
plot.by.1factor(sub, "Solved", "D2", rng=c(0.7,1,0.1), legpos = "bottomright")
plot.by.1factor(sub, "Solved", "D1", rng=c(0.7,1,0.1))

z <- function(x) {max(fmri.stimulus(10, onsets=1, durations = x, type="canonical"))}
Z <- Vectorize(z)
k <- aggregate(sub[c("Activity", "Solved")], list(D2=sub$D2), mean)
k$NormActivity <- (k$Activity - min(k$Activity))/(max(k$Activity)-min(k$Activity))
k$BOLD <- Z(k$NormActivity)

plot(k$D2, k$BOLD, type="l", lwd=2, ylab="Predicted BOLD", xlab=expression(lambda[D]))
points(k$D2, k$BOLD, pch=21, cex=1.5, bg="white", lwd=2)