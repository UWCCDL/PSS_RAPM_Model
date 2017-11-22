# Testbed for model analysis
source("../../PSS_model/functions.R")
#model <- read.table("simulations-devel4-model2-bold.txt", header = T, sep = ",")
#model <- read.table("simulations-devel4-newchoice-bold.txt", header = T, sep = ",")
model <- read.table("simulations-devel4-newchoice-newprobs-newbold.txt", header = T, sep = ",")
names(model) <- c("Ticks", "Alpha", "InitValues", "D1", "D2", "Accuracy", "Latency", "RewardBold", "RpeBold")

model <- subset(model, model$Ticks > 20)

model$RewardActivity <- model$RewardBold / model$Latency

model$RpeActivity <- model$RpeBold / model$Latency

## AXIS.RANGE
## 
## Dynamically calculates the proper min/max range and unit
## for plotting a series of data.
##
axis.range <- function(orig.data, borders=1/2) {
  data <- orig.data[!is.na(orig.data)]
  ymin = min(data)
  ymax = max(data)
  
  # Get the range
  range = ymax - ymin
  
  # Aestetically pleasent mins and maxs 
  ymin  = ymin - range*borders
  ymax  = ymax + range*borders
  
  # Major unit (distance between points)
  yunit = 10^floor(log(range*2, 10))
  
  # Now let's round up the mins and maxs 	
  ymin2 = round(ymin, -1*floor(log(range*2, 10)))
  ymax2 = round(ymax, -1*floor(log(range*2, 10)))
  
  # Be sure that rounding min and max didn't shadow them.
  if (ymin < ymin2) {
    ymin = ymin2 - yunit
  } else {
    ymin = ymin2
  } 
  
  
  if (ymax > ymax2) {
    ymax = ymax2 + yunit
  } else {
    ymax = ymax2
  }
  
  #return the values
  c(ymin, ymax, yunit) 
}

plot.model.parameters <- function(data, variable, factor1, factor2, 
                                  factor2name = factor2, rng=NULL, leg=T, legpos="bottomleft", abs=NULL, main=paste(variable, "by", factor2), subtitle=NULL, points=NULL, colors=NULL, fgs=NULL, lwds=NULL,...) {
  oldmar <- par("mar")
  par(mar=c(3,3,1,1))
  levels <- levels(factor(data[[factor2]]))
  res <- tapply(data[[variable]], factor(data[[factor1]]), mean)
  CEX=2
  if (is.null(rng)) {
    arng=axis.range(res)
  } else {
    arng=rng
  }
  
  x.values = levels(factor(data[[factor1]]))
  x.points = 1:length(x.values)
  
  plot.new()
  plot.window(c(x.points[1], x.points[length(x.points)]), arng[1:2])
  axis(2, at=seq(arng[1], arng[2], arng[3]), labels=seq(arng[1], arng[2], arng[3]))
  axis(1, at=x.points, labels=levels(factor(data[[factor1]])), cex=4)
  box(bty="o")
  
  if(is.null(colors)) {
    colors <- grey(seq(0.25, 0.75, 0.5/max(1, (length(levels)-1))))
  }
  
  if(is.null(points)) {
    points <-rep(21, length(levels))
  }
  
  if (is.null(fgs)) {
    fgs<-rep("black", length(levels))
  }
  
  if (is.null(lwds)) {
    lwds<-rep(2, length(levels))
  }
  
  
  l.points<-c()
  l.colors<-c()
  l.names <-c()
  
  ## The line
  if (!is.null(abs)) {
    abline(h=abs, lty=2)
  }
  
  for (l in seq(length(levels))) {
    sub <- subset(data, data[[factor2]]==levels[l])
    res <- tapply(sub[[variable]], factor(sub[[factor1]]), mean)
    res.se <- tapply(sub[[variable]], factor(sub[[factor1]]), se)
    sub.levels <- levels(factor(sub[[factor1]]))
    sub.x <- match(sub.levels, x.values)
    lines(sub.x, res, cex=CEX, col=colors[l], lwd=lwds[l])
    #arrows(sub.x, res, sub.x, res + res.se, angle=90, length=0.10, col=fgs[l], lwd=lwds[l])
    #arrows(sub.x, res, sub.x, res - res.se, angle=90, length=0.10, col=fgs[l], lwd=lwds[l])
    points(sub.x, res, pch=points[l], cex=CEX, bg=colors[l], lwd=0.5, col="white") #col=colors[l])
    
    # Updates
    l.points<-c(l.points, points[l])
    l.colors<-c(l.colors, colors[l])
    l.names <-c(l.names, paste(levels[l], sep=""))
  }
  #title(main=main, ylab=variable, xlab=factor1, sub=subtitle)
  if (leg) {
    legend(legpos, legend=paste(factor2name, "=", l.names), 
           pch=l.points, lty=1, pt.bg=l.colors, col=l.colors, pt.cex=CEX, bty="n")
  }
  par(mar = oldmar)
}

tiff("~/Fig2C.tiff", res=300, width=3, height=3.5, units = "in", compression = "lzw")
plot.model.parameters(model, "Accuracy", "D1", "Ticks", "Wait time", 
                      leg=F, rng=c(0.5, 1.02, 0.1), legpos = "topleft")
vals <- parse(text = paste("italic(tau) ==", unique(model$Ticks)))
legend(x = 1, y=1.07,
       #ncol = 2,
       legend = vals, 
       lwd = 1, 
       lty = 1, 
       pch = 21,
       pt.cex=2,
       bty = "n",
       y.intersp = 0.6,
       col =   grey(seq(0.25, 0.75, 0.5/max(1, (length(levels(factor(model$Ticks))) - 1)))),
       pt.bg = grey(seq(0.25, 0.75, 0.5/max(1, (length(levels(factor(model$Tick))) - 1)))))
mtext(expression(paste("Impact of Positive Feedback ", pi)), side=1, line=4)
mtext("RAPM Accuracy", side=2, line=3)
dev.off()

tiff("~/Fig2D.tiff", res=300, width=3, height=3.5, units = "in", compression = "lzw")
plot.model.parameters(model, "Accuracy", "D2", "Ticks", "Wait time", leg=F, 
                      rng=c(0.5, 1.02, 0.1), legpos = "topleft")
vals <- parse(text = paste("italic(tau) ==", unique(model$Ticks)))
legend(x = 1, y=1.07, 
       legend = vals, 
       lwd = 1, 
       lty = 1, 
       pch = 21,
       pt.cex=2,
       bty = "n",
       y.intersp = 0.6,
       col =   grey(seq(0.25, 0.75, 0.5/max(1, (length(levels(factor(model$Ticks))) - 1)))),
       pt.bg = grey(seq(0.25, 0.75, 0.5/max(1, (length(levels(factor(model$Tick))) - 1)))))
mtext(expression(paste("Impact of Negative Feedback ", nu)), side=1, line=4)
mtext("RAPM Accuracy", side=2, line=3)
dev.off()

model.redux <- aggregate(model[c("Accuracy", "Latency", "RpeActivity", "RewardActivity", "RpeBold", "RewardBold")], list(Wait=model$Ticks, a=model$D2), mean)

plot.model.bold <- function(data, variable, factor1, factor2, 
                            factor2name = factor2, rng=NULL, xrng=NULL, leg=T, legpos="bottomleft", abs=NULL, main=paste(variable, "by", factor2), subtitle=NULL, points=NULL, colors=NULL, fgs=NULL, lwds=NULL,...) {
  oldmar <- par("mar")
  par(mar=c(3,3,1,1))
  levels <- levels(factor(data[[factor2]]))
  res <- tapply(data[[variable]], factor(data[[factor1]]), mean)
  
  CEX=2
  if (is.null(rng)) {
    arng=axis.range(res)
  } else {
    arng=rng
  }
  
  if (is.null(xrng)) {
    xarng=axis.range(data[[factor1]])
  } else {
    xarng=xrng
  }
  
  plot.new()
  plot.window(xarng[1:2], arng[1:2])
  axis(2, at=seq(arng[1], arng[2], arng[3]), labels=seq(arng[1], arng[2], arng[3]))
  axis(1, cex=4)
  box(bty="o")
  
  if(is.null(colors)) {
    colors <- grey(seq(0.25, 0.75, 0.5/max(1, (length(levels)-1))))
  }
  
  if(is.null(points)) {
    points <-rep(21, length(levels))
  }
  
  if (is.null(fgs)) {
    fgs<-rep("black", length(levels))
  }
  
  if (is.null(lwds)) {
    lwds<-rep(2, length(levels))
  }
  
  
  l.points<-c()
  l.colors<-c()
  l.names <-c()
  
  ## The line
  if (!is.null(abs)) {
    abline(h=abs, lty=2)
  }
  
  for (l in seq(length(levels))) {
    sub <- subset(data, data[[factor2]]==levels[l])
    res <- tapply(sub[[variable]], factor(sub[[factor1]]), mean)
    print(res)
    res.se <- tapply(sub[[variable]], factor(sub[[factor1]]), se)
    sub.levels <- levels(factor(sub[[factor1]]))
    #sub.x <- match(sub.levels, x.values)
    sub.x <- sub.levels
    lines(sub.x, res, cex=CEX, col=colors[l], lwd=lwds[l])
    #arrows(sub.x, res, sub.x, res + res.se, angle=90, length=0.10, col=fgs[l], lwd=lwds[l])
    #arrows(sub.x, res, sub.x, res - res.se, angle=90, length=0.10, col=fgs[l], lwd=lwds[l])
    points(sub.x, res, pch=points[l], lwd=0.5, cex=CEX, bg=colors[l], col="grey85") #col=colors[l])
    
    # Updates
    l.points<-c(l.points, points[l])
    l.colors<-c(l.colors, colors[l])
    l.names <-c(l.names, paste(levels[l], sep=""))
  }
  #title(main=main, ylab=variable, xlab=factor1, sub=subtitle)
  if (leg) {
    legend(legpos, legend=paste(factor2name, "=", l.names), pch=l.points, lty=1, pt.bg=l.colors, col=l.colors, pt.cex=CEX, bty="n")
  }
  par(mar=oldmar)
}


tiff("~/Fig2E.tiff", res=300, width=3, height=3.5, units = "in", compression = "lzw")
plot.model.bold(model.redux, "Accuracy", "RpeActivity", "Wait", "Wait time", rng=c(0.5, 1.02, 0.1), 
                legpos = "topright", leg=F, xrng=c(-4, 1, 1))
vals <- parse(text = paste("italic(tau) ==", unique(model.redux$Wait)))
legend(x = -1.6, y=1.07, 
       legend = vals, 
       lwd = 1, 
       lty = 1, 
       pch = 21,
       pt.cex=2,
       bty = "n",
       y.intersp = 0.6,
       col =   grey(seq(0.25, 0.75, 0.5/max(1, (length(levels(factor(model.redux$Wait))) - 1)))),
       pt.bg = grey(seq(0.25, 0.75, 0.5/max(1, (length(levels(factor(model.redux$Wait))) - 1)))))
mtext("Simulated BOLD Signal", side=1, line=4)
mtext("RAPM Accuracy", side=2, line=3)
dev.off()
