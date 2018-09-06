library(matlab)
library(colorspace)
# Testbed for model analysis
#source("../../PSS_model/functions.R")

## ----------------------------------------------------------------
## Color palette 
## ----------------------------------------------------------------

kolors <- heat_hcl(5)


#model <- read.table(unzip("simulations-across-features/simulations-across-features.zip"), 
#                    header = T, sep = ",")
#names(model) <- c("Ticks", "Alpha", "InitValues", "Features", "D1", "D2", "Accuracy", "Latency", "RewardBold", "RpeBold")

#model4 <- read.table("simulations/simulations-devel4-newchoice-newprobs-newbold.txt", header=T, sep=",")
#names(model4) <- c("Ticks", "Alpha", "InitValues",  "D1", "D2", "Accuracy", "Latency", "RewardBold", "RpeBold")

#model4$Features <- 4

#model <- merge(model, model4, all=T)
#model <- subset(model, model$Ticks > 20)
model <- read.table("simulations-final-newtau/simulations-final-newtau.txt", header=T, sep=",")
names(model) <- c("Ticks", "Alpha", "InitValues", "Features", "D1", "D2", "Accuracy", "Latency", "RewardBold", "RpeBold")
model$RewardActivity <- model$RewardBold / model$Latency
model$RpeActivity <- model$RpeBold / model$Latency

md <- aggregate(model[c("Latency", "Accuracy")], 
                list(Ticks=model$Ticks,
                     Alpha=model$Alpha,
                     Features=model$Features,
                     InitValues=model$InitValues,
                     D1=model$D1,
                     D2=model$D2),
                mean)

md4 <- subset(md, md$Features == 4)
model4 <- subset(model, model$Features==4)
model.redux <- aggregate(model4[c("Accuracy", "Latency", "RpeActivity", "RewardActivity", "RpeBold", "RewardBold")], 
                         list(Wait=model4$Ticks, a=model4$D2), mean)

md$Beta1 <- 0
md$Beta2 <- 0
for (t in unique(md$Ticks)) {
  for (a in unique(md$Alpha)) {
    for (i in unique(md$InitValues)) {
      for (d1 in unique(md$D1)) {
        for (d2 in unique(md$D2)) {
          vals <- subset(md, md$Ticks == t &
                         md$Alpha == a &
                         md$InitValues == i &
                         md$D1 == d1 & 
                         md$D2 == d2)
          vals <- vals[order(vals$Features),]
          m1 <- lm(vals$Accuracy ~ vals$Features)
          m2 <- lm(vals$Latency ~ vals$Features)
          md$Beta1[md$Ticks == t &
                     md$Alpha == a &
                     md$InitValues == i &
                     md$D1 == d1 & 
                     md$D2 == d2] <- m1$coefficients[2]
          
          md$Beta2[md$Ticks == t &
                     md$Alpha == a &
                     md$InitValues == i &
                     md$D1 == d1 & 
                     md$D2 == d2] <- m2$coefficients[2]
        }
      } 
    }
  } 
}

getcol<-function(num) {
  t<-unique(md$Ticks)
  kolors[t==num][1]
}

vgetcol <- Vectorize(getcol)

figure.psp <- function() {
  oldmar <- par("mar")
  par(mar=c(4,4,2,0.5))
  plot(md$Beta1, md$Beta2, xlim=c(-0.25, 0.25), 
       ylim=c(-7,7), pch=21,  #3 
       #col=NA, 
       #bg="#EE222211",
       #col="#EE222211", 
       col=vgetcol(md$Ticks),
       cex=0.1, 
       xlab=c("Accuracy Beta"), 
       ylab="Latency Beta",
       main="Model Flexibility")
  grid()
  abline(h=0, lty=3, lwd=2, col="darkgrey")
  abline(v=0, lty=3, lwd=2, col="darkgrey")
  par(mar=oldmar)
  vals <- parse(text = paste("italic(tau) ==", unique(md$Ticks)))
  
  legend(x = 0, y=0,
         legend = vals, 
         lwd = 0, 
         lty = 1, 
         pch = 21,
         pt.cex=1,
         bty = "n",
         y.intersp = 0.6,
         col =   kolors, 
         pt.bg = kolors) 
}

png("FigPSP.png", res=300, width=3.5, height=3.5, units = "in")
figure.psp()
dev.off()


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
  par(mar=c(3,4,2,1))
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

plot.model.bold <- function(data, variable, factor1, factor2, 
                            factor2name = factor2, rng=NULL, xrng=NULL, leg=T, legpos="bottomleft", abs=NULL, main=paste(variable, "by", factor2), subtitle=NULL, points=NULL, colors=NULL, fgs=NULL, lwds=NULL,...) {
  oldmar <- par("mar")
  par(mar=c(3,4,2,1))
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
    colors <- kolors #grey(seq(0.25, 0.75, 0.5/max(1, (length(levels)-1))))
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


# Comparison to human data: Matzen et al. (2010)
# and Santarnecchi et al (2013).

matzen <- c(86.8965517241379,
            74.8275862068966,
            55,
            38.1034482758621
)/100

spm <- c(90.5172413793104,
         72.5862068965517,
         NA,
         53.7931034482759
)/100

santarnecchi <- 1 - c(4.938271604938273,
                      15.432098765432103,
                      41.04938271604939,
                      42.592592592592595)/100
santarnecchi.rt <- c(5.0, 10.0, 14.6, 23.0)


figure3A <- function() {
  plot.model.parameters(md, "Accuracy", "Features", "Ticks",
                        rng=c(0.25,1.02, 0.25), legpos = "topleft", 
                        factor2name = "Wait Time", leg=F,
                        colors = kolors)
  # polygon(x=c(1:4, 4:1),
  #         y=c(hd_acc_ms + hd_acc_sds,
  #             rev(hd_acc_ms - hd_acc_sds)),
  #         border=NA,
  #         col="#88888822")
  # lines(x=1:4, y=hd_acc_ms, lwd=2, lty=3, col="black")
  lines(x=1:4, y=matzen, lty=3, col="grey", lwd=2)
  #points(x=1:4, y=matzen, pch=0, col="grey", cex=3, lwd=5, bg="grey")

  lines(x=1:4, y=santarnecchi, lty=3, col="black", lwd=2)
  #points(x=1:4, y=santarnecchi, pch=0, col="darkgrey", cex=3, lwd=5, bg="darkgrey")
  
    
  #lines(x=1:4, y=spr, lwd=2, lty=3, col="darkgrey")
  #points(x=1:4, y=spr, pch=0, col="darkgrey", cex=2)
  
  
  vals <- parse(text = paste("italic(tau) ==", unique(model$Ticks)))
  legend(x = 1, y=0.5,
         #ncol = 2,
         legend = vals, 
         lwd = 1, 
         lty = 1, 
         pch = 21,
         pt.cex=2,
         bty = "n",
         y.intersp = 0.6,
         col =   kolors, #grey(seq(0.25, 0.75, 0.5/max(1, (length(levels(factor(md$Ticks))) - 1)))),
         pt.bg = kolors) #grey(seq(0.25, 0.75, 0.5/max(1, (length(levels(factor(md$Ticks))) - 1)))))
  
  # legend(x = 1.25, y=1.07,
  #        legend = c("Matzen (2010)", "Santarnecchi\n(2013)"), 
  #        lwd = 2, 
  #        lty = 3, 
  #        #pch = 21,
  #        pt.cex=2,
  #        bty = "n",
  #        y.intersp = 0.6,
  #        col =   c("darkgrey", "black"),
  #        pt.bg = kolors) #grey(seq(0.25, 0.75, 0.5/max(1, (length(levels(factor(md$Ticks))) - 1)))))
  mtext("Number of Features", side=1, line=4, cex=par("cex"))
  mtext("Problem Accuracy", side=2, line=2.5, cex=par("cex"))
}


figure3B <- function() {
  plot.model.parameters(md, "Latency", "Features", "Ticks", 
                        rng=c(0,30,5), legpos = "topleft", 
                        factor2name = "Wait Time", leg=F,
                        colors = kolors)
  # polygon(x=c(1:4, 4:1),
  #         y=c(hd_ms + hd_sds,
  #             rev(hd_ms - hd_sds)),
  #         border=NA,
  #         col="#99999922")
  #lines(x=1:4, y=hd_ms, lwd=2, lty=3, col="black")
  
  lines(x=1:4, y=santarnecchi.rt, lty=3, col="black", lwd=2)
  #points(x=1:4, y=santarnecchi.rt, pch=0, col="darkgrey", cex=3, lwd=5, bg="darkgrey")
  
  vals <- parse(text = paste("italic(tau) ==", unique(model$Ticks)))
  legend(x = 1, y=32,
         #ncol = 2,
         legend = vals, 
         lwd = 1, 
         lty = 1, 
         pch = 21,
         pt.cex=2,
         bty = "n",
         y.intersp = 0.6,
         col =   kolors, #grey(seq(0.25, 0.75, 0.5/max(1, (length(levels(factor(md$Ticks))) - 1)))),
         pt.bg = kolors) #grey(seq(0.25, 0.75, 0.5/max(1, (length(levels(factor(md$Ticks))) - 1)))))
  mtext("Number of Features", side=1, line=4, cex=par("cex"))
  mtext("Problem Solution Time", side=2, line=2.5, cex=par("cex"))
}


figure3C <- function() {
  plot.model.parameters(md4, "Accuracy", "D1", "Ticks", "Wait time", 
                        leg=F, rng=c(0.25, 1.02, 0.25), legpos = "topleft",
                        colors = kolors)
  
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
         col =   kolors, #grey(seq(0.25, 0.75, 0.5/max(1, (length(levels(factor(model$Ticks))) - 1)))),
         pt.bg = kolors) #grey(seq(0.25, 0.75, 0.5/max(1, (length(levels(factor(model$Tick))) - 1)))))
  #mtext(expression(paste("Impact of Positive Feedback ", pi)), side=1, line=4, cex=par("cex"))
  mtext(expression(paste("Impact of Positive Feedback ", italic(R), "+")), side=1, line=4, cex=par("cex"))
  mtext("Problem Accuracy", side=2, line=2.5, cex=par("cex"))

}


figure3D <- function() {
  plot.model.parameters(md4, "Accuracy", "D2", "Ticks", "Wait time", leg=F, 
                        rng=c(0.25, 1.02, 0.25), legpos = "topleft",
                        colors=kolors)
  vals <- parse(text = paste("italic(tau) ==", unique(model$Ticks)))
  legend(x = 1, y=1.07, 
         legend = vals, 
         lwd = 1, 
         lty = 1, 
         pch = 21,
         pt.cex=2,
         bty = "n",
         y.intersp = 0.6,
         col =   kolors, #grey(seq(0.25, 0.75, 0.5/max(1, (length(levels(factor(model$Ticks))) - 1)))),
         pt.bg = kolors)#grey(seq(0.25, 0.75, 0.5/max(1, (length(levels(factor(model$Tick))) - 1)))))
  #mtext(expression(paste("Impact of Negative Feedback ", nu)), side=1, line=4, cex=par("cex"))
  mtext(expression(paste("Impact of Negative Feedback ", italic(R), "-")), 
        side=1, line=4, cex=par("cex"))
  mtext("Problem Accuracy", side=2, line=2.5, cex=par("cex"))
}


figure3E <- function() {
  plot.model.bold(model.redux, "Accuracy", "RpeActivity", "Wait", "Wait time", rng=c(0.25, 1.02, 0.25), 
                legpos = "topright", leg=F, xrng=c(-4, 1, 1))
  vals <- parse(text = paste("italic(tau) ==", unique(model.redux$Wait)))
  legend(x = -1.6, y=1.07, 
         legend = vals, 
         lwd = 1, 
         lty = 1, 
         pch = 21,
         pt.cex=2,
         ncol = 1,
         bty = "n",
         y.intersp = 0.6,
         col =   kolors, #grey(seq(0.25, 0.75, 0.5/max(1, (length(levels(factor(model.redux$Wait))) - 1)))),
         pt.bg = kolors)#grey(seq(0.25, 0.75, 0.5/max(1, (length(levels(factor(model.redux$Wait))) - 1)))))
  mtext("Simulated BOLD Signal", side=1, line=4, cex = par("cex"))
  mtext("Problem Accuracy", side=2, line=2.5, cex=par("cex"))
}

png("Fig3A.png", res=300, width=3, height=3.5, units = "in")
figure3A()
dev.off()

png("Fig3B.png", res=300, width=3, height=3.5, units = "in")
figure3B()
dev.off()

png("Fig3C.png", res=300, width=3, height=3.5, units = "in")
figure3C()
dev.off()

png("Fig3D.png", res=300, width=3, height=3.5, units = "in")
figure3D()
dev.off()


png("Fig3E.png", res=300, width=3, height=3.5, units = "in")
figure3E()
dev.off()

figure3 <- function() {
  layout(matrix(c(1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 7), nrow = 2, byrow = T))
  plot.new()
  figure3A()
  # Each letter is offset 27.5% from the x axis. So, if you take the axis range (1...4, 1..6)
  # and subtract 27.5% of the range from the minium (1, 0, or -4), you get the right
  # coordinate. For a range of 4, it is 1.1; for a range of 6, it is 1.65
  mtext(expression(bold("(A)")), side=3, line=1, at=c(-0.1), col="black", cex=1.5*par("cex"))
  figure3B()
  mtext(expression(bold("(B)")), side=3, line=1, at=c(-0.1), col="black", cex=1.5*par("cex"))
  plot.new()
  figure3C()
  mtext(expression(bold("(C)")), side=3, line=1, at=c(-0.65), col="black", cex=1.5*par("cex"))
  figure3D()
  mtext(expression(bold("(D)")), side=3, line=1, at=c(-0.65), col="black", cex=1.5*par("cex"))
  figure3E()
  mtext(expression(bold("(E)")), side=3, line=1, at=c(-5.65), col="black", cex=1.5*par("cex"))
}

png("Fig3.png", width=6, height = 5, res=300, units = "in")
figure3()
dev.off()