f <- read.table("firestorm.txt", header=T)
source("functions.R")
f$Region <- "Frontal"
f$Region[f$BlockCondition == "StimIPG"] <- "Parietal"
f$Region[f$BlockCondition == "ShamIPG"] <- "Parietal"

f$CorrRT[f$CorrRT ==0] <- NA

f$CorrRT <- f$CorrRT / 1000
f$AllRT <- f$AllRT / 1000

f$Stimulation <- "Stimulation"
f$Stimulation[f$BlockCondition == "ShamMFG"] <- "Sham"
f$Stimulation[f$BlockCondition == "ShamIPG"] <- "Sham"


f$ExpCondition[f$BlockCondition == "ShamIPG"] <- "Parietal, Sham"
f$ExpCondition[f$BlockCondition == "ShamMFG"] <- "Frontal, Sham"
f$ExpCondition[f$BlockCondition == "StimIPG"] <- "Parietal, 40Hz"
f$ExpCondition[f$BlockCondition == "StimMFG"] <- "Frontal, 40Hz"

f$ProblemType <- as.character(f$Problem)
f$ProblemType[f$Problem == "Rel1"] <- "1-Rel"
f$ProblemType[f$Problem == "Rel2"] <- "2-Rel"
f$ProblemType[f$Problem == "Rel3"] <- "3-Rel"

f$Subject <- as.factor(f$Subject)

d <- aggregate(f[c("ACC", "CorrRT", "AllRT")], 
               list(Subject=f$Subject, 
                    Stimulation=f$Stimulation, 
                    Condition=f$ExpCondition, 
                    Region=f$Region,
                    Problem=f$ProblemType),
               mean)

plot.by.3factors(d, "ACC", "Region", "Stimulation", "Problem", rng=c(0.1,1,0.1))


summary(aov(ACC ~ (Region * Stimulation * Problem) + Error(Subject/(Region*Stimulation * Problem)), d))

d$NACC <- asin(sqrt(d$ACC))
summary(aov(NACC ~ (Region * Stimulation * Problem) + Error(Subject/(Region*Stimulation * Problem)), d))


summary(aov(CorrRT ~ (Region * Stimulation * Problem) + Error(Subject/(Region*Stimulation*Problem)), d))
summary(aov(log(CorrRT) ~ (Region * Stimulation * Problem) + Error(Subject/(Region*Stimulation*Problem)), d))

summary(aov(AllRT ~ (Region * Stimulation * Problem) + Error(Subject/(Region*Stimulation*Problem)), d))
summary(aov(log(AllRT) ~ (Region * Stimulation * Problem) + Error(Subject/(Region*Stimulation*Problem)), d))


## Some descriptive statistics

## Graphics

m.acc <- tapply(d$ACC, list(d$Condition, d$Problem), safe.mean)
se.acc <- tapply(d$ACC, list(d$Condition, d$Problem), se)
m.rt <- tapply(d$CorrRT, list(d$Condition, d$Problem), safe.mean)
se.rt <- tapply(d$CorrRT, list(d$Condition, d$Problem), se)
m.allrt <- tapply(d$AllRT, list(d$Condition, d$Problem), safe.mean)
se.allrt <- tapply(d$AllRT, list(d$Condition, d$Problem), se)

cols <- c("brown1", "brown4", "dodgerblue1", "dodgerblue4")
cols <- c("#8d0040", "#a15369", "#279000", "#829d5c")
cols <- c("#fdaa00", "#ffc54e", "#103caa", "#4f74d1")
cols <- c("#fd5b00", "#e87b3d", "#0f3ea9", "#35549b")
cols <- c("#fd5b00", "#e0a686", "#0f3ea9", "#607096")
#cols <- c("#fd5b00", "#e0a686", "#00a678", "#589383")
sel <- 0.05

plot.accuracies <- function() {
xs < -barplot(m.acc, border="white", beside =T, col=cols, 
        ylim =c(0,1.09), 
        legend=T, args.legend=list(x="topright", ncol=2, bty="n", border="white"),
        main="Accuracy by Problem and Condition",
        xlab="Problem Type", ylab="Proportion Correct")
abline(h=1/8, col="black", lwd=2, lty=3)
arrows(xs, m.acc, xs, m.acc + se.acc, angle=90, length = sel)
arrows(xs, m.acc, xs, m.acc - se.acc, angle=90, length = sel)
box(bty="o")
}



plot.rts <- function() {
  xs <- barplot(m.rt, border="white", beside =T, col="white", 
                ylim =c(0,40))
  #grid(nx=0, ny=8)
    barplot(m.rt, border="white", beside =T, col=cols, 
              ylim =c(0,40), 
        legend=T, args.legend=list(x="topleft", ncol=2, bty="n", border="white"),
        main="Response Times by Problem and Condition\n(Correct trials only)",
        xlab="Problem Type", ylab="Solution time (secs)", add=T)
arrows(xs, m.rt, xs, m.rt + se.rt, angle=90, length = sel)
arrows(xs, m.rt, xs, m.rt - se.rt, angle=90, length = sel)
box(bty="o")
}


plot.allrts <- function() {
  xs <- barplot(m.allrt, border="white", beside =T, col=cols, 
        ylim =c(0, 40), 
        legend=T, args.legend=list(x="topleft", ncol=2, bty="n", border="white"),
        main="Response Times by Problem and Condition\n(All trials)",
        xlab="Problem Type", ylab="Solution time (secs)")
arrows(xs, m.allrt, xs, m.allrt + se.allrt, angle=90, length = sel)
arrows(xs, m.allrt, xs, m.allrt - se.allrt, angle=90, length = sel)
box(bty="o")
}

sc <- subset(d, d$Problem == "Logic" & d$Region == "Parietal")
t.test(CorrRT ~ Stimulation, sc)  # Not significant, bt not paired

a <- sc$CorrRT[sc$Stimulation=="Sham"]
b <- sc$CorrRT[sc$Stimulation=="Stimulation"]
t.test(a, b, paired=TRUE)  # DF = 15,  p = 0.55, t = -0.60

# Save images

jpeg("accuracies.jpg", width=750, height=470, res=150, pointsize = 9, units = "px")
plot.accuracies()
dev.off()

jpeg("rts.jpg", width=750, height=470, res=150, pointsize = 9, units = "px")
plot.rts()
dev.off()

jpeg("allrts.jpg", width=750, height=470, res=150, pointsize = 9, units = "px")
plot.allrts()
dev.off()


## Now, let's remove any subject who made mistakes

k <- aggregate(d[c("CorrRT")], list(Subject=d$Subject), mean)
k <- subset(k, !is.na(k$CorrRT))

d2 <- subset(d, d$Subject %in% k$Subject)

summary(aov(ACC ~ (Region * Stimulation * Problem) + Error(Subject/(Region*Stimulation * Problem)), d2))
summary(aov(NACC ~ (Region * Stimulation * Problem) + Error(Subject/(Region*Stimulation * Problem)), d2))


#summary(aov(CorrRT ~ (Region * Stimulation * Problem) + Error(Subject/(Region*Stimulation*Problem)), d2))
#summary(aov(log(CorrRT) ~ (Region * Stimulation * Problem) + Error(Subject/(Region*Stimulation*Problem)), d))

summary(aov(AllRT ~ (Region * Stimulation * Problem) + Error(Subject/(Region*Stimulation*Problem)), d))
summary(aov(log(AllRT) ~ (Region * Stimulation * Problem) + Error(Subject/(Region*Stimulation*Problem)), d))

m2.acc <- tapply(d2$ACC, list(d2$Condition, d2$Problem), safe.mean)
se2.acc <- tapply(d2$ACC, list(d2$Condition, d2$Problem), se)
m2.rt <- tapply(d2$CorrRT, list(d2$Condition, d2$Problem), safe.mean)
se2.rt <- tapply(d2$CorrRT, list(d2$Condition, d2$Problem), se)
m2.allrt <- tapply(d2$AllRT, list(d2$Condition, d2$Problem), safe.mean)
se2.allrt <- tapply(d2$AllRT, list(d2$Condition, d2$Problem), se)


plot.accuracies2 <- function() {
  xs < -barplot(m2.acc, border="white", beside =T, col=cols, 
                ylim =c(0,1.09), 
                legend=T, args.legend=list(x="topright", ncol=2, bty="n", border="white"),
                main="Accuracy by Problem and Condition",
                xlab="Problem Type", ylab="Proportion Correct")
  abline(h=1/8, col="black", lwd=2, lty=3)
  arrows(xs, m2.acc, xs, m2.acc + se2.acc, angle=90, length = sel)
  arrows(xs, m2.acc, xs, m2.acc - se2.acc, angle=90, length = sel)
  box(bty="o")
}



plot.rts2 <- function() {
  xs <- barplot(m2.rt, border="white", beside =T, col="white", 
                ylim =c(0,40))
  #grid(nx=0, ny=8)
  barplot(m2.rt, border="white", beside =T, col=cols, 
          ylim =c(0,40), 
          legend=T, args.legend=list(x="topleft", ncol=2, bty="n", border="white"),
          main="Response Times by Problem and Condition\n(Correct trials only)",
          xlab="Problem Type", ylab="Solution time (secs)", add=T)
  arrows(xs, m2.rt, xs, m2.rt + se2.rt, angle=90, length = sel)
  arrows(xs, m2.rt, xs, m2.rt - se2.rt, angle=90, length = sel)
  box(bty="o")
}


plot.allrts <- function() {
  xs <- barplot(m.allrt, border="white", beside =T, col=cols, 
                ylim =c(0, 40), 
                legend=T, args.legend=list(x="topleft", ncol=2, bty="n", border="white"),
                main="Response Times by Problem and Condition\n(All trials)",
                xlab="Problem Type", ylab="Solution time (secs)")
  arrows(xs, m.allrt, xs, m.allrt + se.allrt, angle=90, length = sel)
  arrows(xs, m.allrt, xs, m.allrt - se.allrt, angle=90, length = sel)
  box(bty="o")
}
