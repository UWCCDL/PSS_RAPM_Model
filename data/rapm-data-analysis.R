library(ppcor)
library(psych)  # Imports paired.ir

#source('functions.R')
# Behavioral experiment 1 (extended data)

behav1 <- read.table("experiment1.txt", header=T, sep="\t")

# Remove all the participants for which we do not have complete data
# 
behav1 <- subset(behav1, !is.na(behav1$RAPM) & !is.na(behav1$AvoidB) & !is.na(behav1$ChooseA) & !is.na(behav1$OS_Score))

#behav1 <- subset(behav1, behav1$Train_length < 120)

# Test various correlations
attach(behav1)
cor.test(RAPM, OS_Score)     # This should be sign., It is (p = 0.03)
cor.test(ChooseA, OS_Score)  # This should not be sign. It isn't (p = 0.08)
cor.test(ChooseA, AvoidB)    # This should not be sign. It isn't (p = 0.11)
cor.test(ChooseA, RAPM)      # This should not be sign. It isn't (p = 0.22)
cor.test(AvoidB, OS_Score)   # This should be sign. It is (p = 0.01*)
cor.test(AvoidB, RAPM)       # This should be sign. It is. (p = 0.0005**)

# Difference between correlations:
paired.r(cor(AvoidB, RAPM), cor(ChooseA, RAPM), cor(AvoidB, ChooseA), n=97)

# Some basic statistics
colMeans(behav1)
sd(ChooseA)
sd(AvoidB)
t.test(ChooseA, AvoidB, paired=T)

# Partial Correlations
pcor.test(AvoidB, RAPM, OS_Score)
pcor.test(OS_Score, RAPM, AvoidB)
detach(behav1)

# Behavioral experiment 2

#behav2 <- read.table("behavioral_experiment_2.txt", header=T)
#behav2 <- subset(behav2, !is.na(behav2$BARAPM) & !is.na(behav2$AvoidB) & !is.na(behav2$ChooseA) & !is.na(behav2$OS_Score))
behav2 <- read.table("experiment2.txt", header=T, sep="\t")
behav2 <- subset(behav2, !is.na(behav2$BARAPM) & !is.na(behav2$AvoidB))
behav2 <- subset(behav2, behav2$Subject != "12560")
#behav2 <- read.table("justin.txt", header=T, sep="\t")
#behav2$BARAPM <- behav2$BAR
behav2$RAPM <- behav2$BARAPM
#behav2 <- subset(behav2, !is.na(behav2$BARAPM) & !is.na(behav2$AvoidB) & !is.na(behav2$ChooseA) & !is.na(behav2$OS_Score))
behav2 <- subset(behav2, behav2$BARAPM > 2 )
#behav2<-subset(behav2, behav2$AB_trials < 120)


# Test various correlations
attach(behav2)
cor.test(BARAPM, OS_Score)     # This should be sign., it is. (p = 0.05)
cor.test(ChooseA, OS_Score)  # This should not be sign. It isn't (p = 0.77)
cor.test(ChooseA, AvoidB)    # This should not be sign. It isn't (p = 0.62)
cor.test(ChooseA, BARAPM)      # This should not be sign. It isn't (p = 0.14)
cor.test(AvoidB, OS_Score)   # This should be sign. It is (p = 0.03*)
cor.test(AvoidB, BARAPM)       # This should be sign. It is. (p = 0.001**)

#pcor.test(behav2$OS_Score, behav2$RAPM, behav2$AvoidB)
# Difference between correlations:
paired.r(cor(AvoidB, RAPM), cor(ChooseA, RAPM), cor(AvoidB, ChooseA), n=97)

# Some basic statistics
colMeans(behav2)
sd(ChooseA)
sd(AvoidB)
t.test(ChooseA, AvoidB, paired=T)

# Partial Correlations
detach(behav2)


## ---------------------------------------------------------------- ##
## FMRI EXPERIMENT
## ---------------------------------------------------------------- ##

ks <- read.table("accuracy_striatum.txt", header=T)

## ---------------------------------------------------------------- ##
## GRAPHICS
## ---------------------------------------------------------------- ##
## Here is the code to produce the relevant graphs, including those
## that make up the paper pictures.
## ---------------------------------------------------------------- ##


plot.correlations <- function(data, xvar, yvar, xlab=xvar, ylab=yvar, corpos=c(0.2, 0.2), xlim=c(0, 1), ...) {
	attach(data)
	mod <- lm(as.formula(paste(yvar, xvar, sep="~")))
	
	range <- (xlim[2] - xlim[1])
	newx <- data.frame(X=seq(xlim[1] - 1/2 *range, xlim[2] + 1/2 * range, range/1000)) 
	names(newx) <- c(xvar)  # Rename to X variable in data frame	
	newy <- predict(mod, newdata=newx, interval="confidence", level=0.95) 	
	detach(data)
	plot(data[[xvar]], data[[yvar]], pch=21, bg="white", col="white", cex=1.5, fg="grey15", xlab="", ylab="", col.axis="grey15", col.lab="grey15", xlim=xlim, ...)
	p <- cor.test(data[[xvar]], data[[yvar]])$p.value
	if (p < 0.05) {
		line <- "red"
		shade <- "#FF222222"
	} else {
		line <- "black"
		shade <- "#22222222"
	}
	polygon(c(newx[[xvar]], rev(newx[[xvar]])), c(newy[,2], rev(newy[,3])), col=shade, border=F)
	points(data[[xvar]], data[[yvar]], pch=21, bg="#11111166", col="white", cex=1.5)
	abline(mod, col=line, lwd=2, lty=3)
	
	r <- round(cor(data[[xvar]], data[[yvar]]), 2)
	#detach(data)
	text(x=corpos[1], y=corpos[2], col=line, labels=substitute(italic(r)(n) == x, list(n=length(data[[xvar]]), x=r)))
	mtext(ylab, side=2, line=2, col="grey15")
	mtext(xlab, side=1, line=2, col="grey15")
}

plot.correlations.size <- function(data, xvar, yvar, xlab=xvar, ylab=yvar, leg=F, legpos="bottom", corpos=c(0.2, 0.2), xlim=c(0, 1), legcols=NA, ...) {
  attach(data)
  mod <- lm(as.formula(paste(yvar, xvar, sep="~")))
  
  range <- (xlim[2] - xlim[1])
  newx <- data.frame(X=seq(xlim[1] - 1/2 *range, xlim[2] + 1/2 * range, range/1000)) 
  names(newx) <- c(xvar)  # Rename to X variable in data frame	
  newy <- predict(mod, newdata=newx, interval="confidence", level=0.95) 	
  detach(data)
  k <- data
  k$Count <- 0
  kz <- aggregate(k$Count, list(xvar=k[[xvar]], yvar=k[[yvar]]), length)
  #print(kz$x)
  plot(kz$xvar, kz$yvar, pch=21, bg="white", col="white", cex=kz$x, 
       fg="grey15", xlab="", ylab="", col.axis="grey15", col.lab="grey15", xlim=xlim, ...)
  p <- cor.test(data[[xvar]], data[[yvar]])$p.value
  significance = ""
  if (p < 0.05) {
    line <- "red"
    shade <- "#FF222222"
    significance = "*"
    if (p < 0.01) {significance = "**"}
    if (p < 0.001) {significance = "***"}
  } else {
    line <- "black"
    shade <- "#22222222"
  }
  bgs=c("#77777799", "#44444499", "#11111199", "#11111199")
  polygon(c(newx[[xvar]], rev(newx[[xvar]])), c(newy[,2], rev(newy[,3])), col=shade, border=F)
  points(kz$xvar, kz$yvar, pch=21, bg=sapply(kz$x, function(x){bgs[x]}), col="white", cex=1.5 * sqrt(kz$x), 
       fg="grey15", xlab="", ylab="", col.axis="grey15", col.lab="grey15", xlim=xlim, ...)
  #points(data[[xvar]], data[[yvar]], pch=21, bg="#11111166", col="white", cex=1.5)
  abline(mod, col=line, lwd=2, lty=3)
  
  r <- round(cor(data[[xvar]], data[[yvar]]), 2)
  #detach(data)
  text(x=corpos[1], y=corpos[2], col=line, labels=substitute(italic(r)(n) == x~s, list(n=length(data[[xvar]]), x=r, s=significance)))
  mtext(ylab, side=2, line=2, col="grey15")
  mtext(xlab, side=1, line=2, col="grey15")
  if (leg) {
    if (is.na(legcols)) {legcols <- max(kz$x)}
    legend(x=legpos, ncol=legcols, legend=paste("n=", sep="", sort(unique(kz$x))), pch=21,
           col="white", pt.bg=sapply(unique(kz$x), function(x){bgs[x]}), pt.cex= 1.5*sqrt(unique(kz$x)),
           bg="#CCCCCC88", box.col = NA)
  }
  box(bty="O")
}


plot.pss.means <- function(data, ...) {
	attach(data)
	us <- c(mean(AvoidB), mean(ChooseA))
	ses <- c(se(AvoidB), se(ChooseA))
	xs <- barplot(us, ...)
	#detach(data)
	arrows(xs, us, xs, us + ses, angle=90, col="grey45", length=0.1)
	arrows(xs, us, xs, us - ses, angle=90, col="grey45", length=0.1)
	text(xs, us + ses + 0.05, label=round(us, 2))	
	detach(data)
}

plot.pss.correlations <- function(data, ...) {
	attach(data)
	mod2 <- lm(ChooseA ~ AvoidB)
	newx <- data.frame(AvoidB=seq(-1/2, 3/2, 0.01)) 	
	newy <- predict(mod2, newdata=newx, interval="confidence", level=0.95) 	
	
	plot(AvoidB, ChooseA, pch=21, bg="white", col="white", cex=1.5, fg="grey15", ylim=c(0, 1), col.axis="grey15", col.lab="grey15", xlim=c(0,1), ...)
	polygon(c(newx$AvoidB, rev(newx$AvoidB)), c(newy[,2], rev(newy[,3])), col="#22222222", border=F)
	points(AvoidB, ChooseA, pch=21, bg="#11111166", col="white", cex=1.5)

	abline(mod2, col="black", lwd=2, lty=3)
	
	r <- round(cor(ChooseA, AvoidB),2)
	detach(data)
	text(x=0.2, y=0.2, col="black", labels=substitute(italic(R) == x, list(x=r)))
	mtext("Choose Accuracy", side=2, line=2, col="grey15")
	mtext("Avoid Accuracy", side=1, line=2, col="grey15")
	
}



figure4 <- function(data=behav1, ...) {
	par(mar=c(3,3,3,2))#, cex.main=1)
	parms <- par()
	layout(matrix(1:4, byrow=T, ncol=2))
	
	# Plot A: Absolute values of Avoid and Choose 
	
	plot.pss.means(data, col="grey85", border="white", ylim=c(0,1), fg="grey15", names=c("Avoid", "Choose"), col.axis="grey15", col.lab="grey15")
	box(bty="o", col="grey15")
	mtext("Mean Value", side=2, line=2)
	title(main="Experiment 1\nMean PSS Values")
	mtext(expression(bold("(A)")), side=3, line=1, at=c(-0.25), col="black", cex=1.25)
	
	# Plot B: Correlations between Avoid and Choose
	
	plot.correlations.size(data, "ChooseA", "AvoidB", corpos = c(0.2,0.1),
	                       xlab="Choose Accuracy", ylab="Avoid Accuracy", leg=F, ...)
	title(main="Experiment 1\n Avoid and Choose Accuracies")
	mtext(expression(bold("(B)")), side=3, line=1, at=c(-0.2), col="black", cex=1.25)
	
	# Plot C
	plot.correlations.size(behav1, "AvoidB", "RAPM", xlab = "Avoid Accuracy", 
	                       ylab = "RAPM Score",  corpos=c(0.25, 32), ylim=c(8,36), leg=T)
	#mtext("Avoid Accuracy", side=1, line=2, col="grey15",cex=1)
	#mtext("RAPM Score", side=2, line=2, col="grey15", cex=1)
	mtext(expression(bold("(A)")), side=3, line=1, at=c(-0.2), col="black", cex=1.5)
	title(main="Experiment 1\nAvoid Accuracy and RAPM")
	
	# Plot D: Choose & Avoid
	
	plot.correlations.size(behav1, "ChooseA", "RAPM", xlab = "",  
	                       ylab="RAPM Score", corpos=c(0.2, 32), ylim=c(8,36), leg=F)
	mtext("Choose Accuracy", side=1, line=2, col="grey15", cex=parms$cex.axis)
	mtext(expression(bold("(B)")), side=3, line=1, at=c(-0.2), col="black", cex=1.5)
	title(main="Experiment 1\nChoose Accuracy and RAPM")
	
	
}


figure5 <- function(data=behav2, ...) {
  par(mar=c(3,3,3,2))#, cex.main=1)
  parms <- par()
  layout(matrix(1:4, byrow=T, ncol=2))
  

  # Plot A: Absolute values of Avoid and Choose 
  
  plot.pss.means(behav2, col="grey85", border="white", ylim=c(0,1), fg="grey15", names=c("Avoid", "Choose"), col.axis="grey15", col.lab="grey15")
  box(bty="o", col="grey15")
  mtext("Mean Value", side=2, line=2)
  title(main="Experiment 2\nMean PSS Results")
  mtext(expression(bold("(A)")), side=3, line=1, at=c(-0.25), col="black", cex=1.25)
  
  # Plot B: Correlations between Avoid and Choose
  
  plot.correlations.size(behav2, "ChooseA", "AvoidB", 
                         xlab="Choose Accuracy", ylab="Avoid Accuracy", leg=F, ...)
  title(main="Experiment 2\n Avoid and Choose Accuracies")
  mtext(expression(bold("(B)")), side=3, line=1, at=c(-0.2), col="black", cex=1.25)
  
  
  # Plot C: Avoid and RAPM
  
  plot.correlations.size(behav2, "AvoidB", "RAPM", xlab = "", ylab = "", corpos=c(0.2, 11), ylim=c(0,12), leg=T)
  mtext("Avoid Accuracy", side=1, line=2, col="grey15", cex=parms$cex.axis)
  mtext("Abbreviated RAPM Score", side=2, line=2, col="grey15", cex=parms$cex.axis)
  mtext(expression(bold("(C)")), side=3, line=1, at=c(-0.2), col="black", cex=1.5)
  title(main="Experiment 2\nAvoid Accuracy and RAPM")
  
  # Plot D: Choose and RAPM
  
  plot.correlations.size(behav2, "ChooseA", "RAPM", xlab = "", ylab = "", corpos=c(0.2, 11), ylim=c(2,12))
  mtext("Choose Accuracy", side=1, line=2, col="grey15", cex=parms$cex.axis)
  mtext("Abbreviated RAPM Score", side=2, line=2, col="grey15", cex=parms$cex.axis)
  mtext(expression(bold("(D)")), side=3, line=1, at=c(-0.2), col="black", cex=1.5)
  title(main="Experiment 2\nChoose Accuracy and RAPM")
}

png("Fig4ABCD_cs.png", res = 300, units="in", width=6, height=6)
figure4()
dev.off()

png("Fig5ABCD_cs.png", res = 300, units="in", width=6, height=6)
figure5()
dev.off()

figure6D <- function() {
  oldmar <- par("mar")
  par(mar=c(3,3,1,1))
  plot.correlations.size(ks, "Mean", "Accuracy", xlab="Mean Striatal Activity", ylab="Overall RAPM Accuracy", xlim = c(-0.2, 0.6), ylim=c(0.1, 0.7), corpos=c(0.4, 0.6))
  par(mar=oldmar)
}

tiff("Fig6D_cogsci.tiff", res = 300, units="in", width=3, height=3)
figure6D()
dev.off()



figureS1 <- function() {
	parms <- par()
	par(mar=c(3,3,3,1)) #, cex.main=1)
	layout(matrix(1:4, ncol=2, byrow=T), widths=c(1, 1))
	hist(behav1$OS_Score, breaks=5, col="grey75", border="white", xlim=c(0,80), ylim=c(0,40), main="Distribution\nof Working Memory")
	box(bty="o", col="grey15")
	mtext(expression(bold("(A)")), side=3, line=1, at=c(-16), col="black", cex=1.25)
	mtext("Frequency", side=2, line=2, col="grey15", cex=parms$cex)
	mtext("Working Memory Span Score", side=1, line=2, col="grey15", cex=parms$cex)

	hist(behav1$RAPM, breaks=5, col="grey75", border="white", xlim=c(0,40), ylim=c(0,40), main="Distributions\nof RAPM")
	box(bty="o", col="grey15")
	mtext(expression(bold("(B)")), side=3, line=1, at=c(-8), col="black", cex=1.25)
	mtext("Frequency", side=2, line=2, col="grey15", cex=parms$cex)
	mtext("RAPM Score", side=1, line=2, col="grey15", cex=parms$cex)
	
	plot.correlations.size(behav1, "OS_Score", "RAPM", xlab="", ylab="", 
	                       xlim=c(0, 80), ylim=c(8, 36), legpos="bottomright", corpos=c(15, 34), leg=T)
	mtext(expression(bold("(C)")), side=3, line=1, at=c(-16), col="black", cex=1.25)
	mtext("RAPM Score", side=2, line=2, col="grey15", cex=parms$cex)
	mtext("Working Memory Span Score", side=1, line=2, col="grey15", cex=parms$cex)
	title(main="Working Memory\nand RAPM")
	
	plot.correlations.size(behav1, "AvoidB", "OS_Score", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 80),
	                       legpos="bottomright", leg=T, legcols=2, corpos=c(.2, 57))
	mtext(expression(bold("(D)")), side=3, line=1, at=c(-0.2), col="black", cex=1.25)
	mtext("Avoid Accuracy", side=1, line=2, col="grey15", cex=parms$cex)
	mtext("Working Memory Score", side=2, line=2, col="grey15", cex=parms$cex)
	title(main="Avoid Accuracy\nand Working Memory")
}

tiff("FigS1.tiff", res = 300, units="in", width=6, height=6)
figureS1()
dev.off()




