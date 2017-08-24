## SE  [STANDARD ERROR OF THE MEAN]
##
se <- function(data) {
	sd(data[!is.na(data)])/sqrt(length(data[!is.na(data)])-1)
}

safe.mean <- function(data) {
	mean(data[!is.na(data)])
}

## DFMERGE
##
## A utility to merge two data frames
##
dfmerge <- function(df1, df2) {
	nms  <- names(df1)
	lst  <- list()
	swap <- list()
	
	for (name in nms) {
		swap <- list(name = c(array(df1[[name]]), array(df2[[name]])))
		lst <- c(lst, swap)
	}
	names(lst) <- nms
	data.frame(lst)
}

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


convert.dual.task <- function(data) {
	data[data==TRUE] <- 'Dual'
	data[data==FALSE] <- 'IGT'
	data
}


detect <- function(p, data) {
	sel <- subset(data, data$Participant==p)
	ph1 <- levels(factor(sel$DualTask[sel$Phase==1]))
	ph2 <- levels(factor(sel$DualTask[sel$Phase==2]))
	c(ph1, ph2)
}


update.conds <- function(data) {
	rows <- dim(data)[1]
	data[['Condition']] <- rep.int(0, rows)
	data[['DualTaskFirstPhase']] <- rep.int(0, rows)
	data[['DualTaskSecondPhase']] <- rep.int(0, rows) 
	swap <- NULL
	for (p in levels(factor(data$Participant))) {
		swap <- detect(p, data)
		data$Condition[data$Participant==p] <- paste(swap[1], swap[2], sep="-")
		data$DualTaskFirstPhase[data$Participant==p] <- swap[1]
		data$DualTaskSecondPhase[data$Participant==p] <- swap[2]
	}
	data
}

gmb <- function(vec) {
	l1 = vec[vec=='A' | vec=='B']
	l2 = vec[vec=='C' | vec=='D']
	length(l2)-length(l1)
}


# Long e verbose, but useful to exclude participants
# specified on the fly.
#
exclude.participants <- function(data, set) {
	k <- data
	for (i in set) {
		k <- subset(k, k$Participant != i)
	}
	k
}

cont <- function(vals) {
	if (vals[1] > 0) {
		diff(vals)
	} else if (vals[1] == 0) {
		NA #vals[2]
	} else {
		-1*diff(vals)
	}
}

euclid <- function(vals) {
	as <- length(vals[vals=='A'])
}


## PLOTS A DEPENDENT VARIABLE OVER A FACTOR 
##
plot.by.1factor <- function(data, variable, factor, rng=NULL, legpos="bottomleft", abs=NULL, ...) {
	res   <- tapply(data[[variable]], factor(data[[factor]]), mean)
	upper <- res + tapply(data[[variable]], factor(data[[factor]]), se)
	lower <- res - tapply(data[[variable]], factor(data[[factor]]), se) 
	CEX=1.6
	
	if (is.null(rng)) {
		arng=axis.range(c(upper, lower))
	} else {
		arng=rng
	}
	
	xpoints = 1:length(levels(factor(data[[factor]])))
	
	plot.new()
	plot.window(c(1, xpoints[length(xpoints)]), arng[1:2])
	axis(2, at=seq(arng[1], arng[2], arng[3]), labels=seq(arng[1], arng[2], arng[3]))
	axis(1, at=xpoints, labels=levels(factor(data[[factor]])), cex=4)
	box(bty="o")
	
	#colors <- grey(seq(0, 1, length(levels)-1))
	#points <- seq(21, 21+length(levels)-1, 1)
	
	## The line
	if (!is.null(abs)) {
		abline(h=abs, lty=2)
	}

	lines(xpoints, res, cex=CEX)
	arrows(xpoints, res, xpoints, upper, angle=90, length=0.10) #col=colors[j])
	arrows(xpoints, res, xpoints, lower, angle=90, length=0.10) #col=colors[j])
	points(xpoints, res, pch=21, lwd=CEX, cex=CEX, bg="black") 
		
	title(main=paste(variable, "by", factor), ylab=variable, xlab=factor, ...)
	legend(legpos, legend=paste(variable, "+/- SEM"), pch=21, lty=1, pt.bg="black", pt.cex=CEX, bty="n")
}

plot.by.2factors <- function(data, variable, factor1, factor2, rng=NULL, legpos="bottomleft", abs=NULL, main=paste(variable, "by", factor2), subtitle=NULL, points=NULL, colors=NULL, fgs=NULL, lwds=NULL,...) {
	levels <- levels(factor(data[[factor2]]))
	res <- tapply(data[[variable]], factor(data[[factor1]]), mean)
	CEX=1.6
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
		colors <- grey(seq(0, 1, 1/max(1, (length(levels)-1))))
	}
	
	if(is.null(points)) {
		points <-rep(21, length(levels))
	}
	
	if (is.null(fgs)) {
		fgs<-rep("black", length(levels))
	}

	if (is.null(lwds)) {
		lwds<-rep(1, length(levels))
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
		lines(sub.x, res, cex=CEX, col=fgs[l], lwd=lwds[l])
		arrows(sub.x, res, sub.x, res + res.se, angle=90, length=0.10, col=fgs[l], lwd=lwds[l])
		arrows(sub.x, res, sub.x, res - res.se, angle=90, length=0.10, col=fgs[l], lwd=lwds[l])
		points(sub.x, res, pch=points[l], lwd=CEX, cex=CEX, bg=colors[l], col=fgs[l])
		
		# Updates
		l.points<-c(l.points, points[l])
		l.colors<-c(l.colors, colors[l])
		l.names <-c(l.names, paste(levels[l], sep=""))
	}
	title(main=main, ylab=variable, xlab=factor1, sub=subtitle)
	legend(legpos, legend=l.names, pch=l.points, lty=1, pt.bg=l.colors, pt.cex=CEX, bty="n")
}


plot.by.3factors <- function(data, variable, factor1, factor2, factor3, rng=NULL, legpos="bottomleft", abs=NULL, ...) {
	levels1 <- levels(factor(data[[factor2]]))
	levels2 <- levels(factor(data[[factor3]]))
	
	res <- tapply(data[[variable]], factor(data[[factor1]]), mean)
	CEX=1.6
	if (is.null(rng)) {
		arng=axis.range(res)
	} else {
		arng=rng
	}
	
	x.values = levels(factor(data[[factor1]]))
	x.points = 1:length(x.values)

	plot.new()
	plot.window(c(1, x.points[length(x.points)]), arng[1:2])
	axis(2, at=seq(arng[1], arng[2], arng[3]), labels=seq(arng[1], arng[2], arng[3]))
	axis(1, at=x.points, labels=levels(factor(data[[factor1]])), cex=4)
	box(bty="o")
	
	colors <- grey(seq(0, 1, length(levels1)-1))
	points <- seq(21, 21+length(levels1)-1, 1)
	lines  <- seq(1, 1+length(levels2), 1)
	
	l.points<-c()
	l.colors<-c()
	l.lines <-c()
	l.names <-c()
	
	## The line
	if (!is.null(abs)) {
		abline(h=abs, lty=2)
	}

	for (l in seq(length(levels1))) {
		for (j in seq(length(levels2))) {
			#write(file=stderr(), paste(levels1[l], levels2[j], sep=":::"))
			sub <- subset(data, data[[factor2]]==levels1[l] & data[[factor3]]==levels2[j])
			res <- tapply(sub[[variable]], factor(sub[[factor1]]), mean)
			#print(res)
			res.se <- tapply(sub[[variable]], factor(sub[[factor1]]), se)
			sub.levels <- levels(factor(sub[[factor1]]))
			sub.x <- match(sub.levels, x.values)
			lines(sub.x, res, cex=CEX, lty=lines[j])
			arrows(sub.x, res, sub.x, res + res.se, angle=90, length=0.10) #col=colors[j])
			arrows(sub.x, res, sub.x, res - res.se, angle=90, length=0.10) #col=colors[j])
			points(sub.x, res, pch=points[l], lwd=CEX, cex=CEX, bg=colors[l]) 
		
			# Updates
			l.points<-c(l.points, points[l])
			l.colors<-c(l.colors, colors[l])
			l.lines <-c(l.lines, lines[j])
			l.names <-c(l.names, paste(factor2, ": ", levels1[l], ", ", factor3, ": ", levels2[j], sep=""))
		}
	}
	title(main=paste(variable, " by ", factor1, ", ", factor2, " and ", factor3, sep=""), ylab=variable, xlab=factor1, ...)
	legend(legpos, legend=l.names, pch=l.points, lty=l.lines, pt.bg=l.colors, pt.cex=1, bty="n")
}



## PLOTS 2 VARIABLES ON TWO DIFFERENT AXIS

plot2.by.1factor <- function(data, variable1, variable2, factor, rng1=NULL, rng2=NULL, legpos="bottomleft", abs=NULL, ...) {
	res1   <- tapply(data[[variable1]], factor(data[[factor]]), mean)
	upper1 <- res1 + tapply(data[[variable1]], factor(data[[factor]]), se)
	lower1 <- res1 - tapply(data[[variable1]], factor(data[[factor]]), se) 
	
	res2   <- tapply(data[[variable2]], factor(data[[factor]]), mean)
	upper2 <- res2 + tapply(data[[variable2]], factor(data[[factor]]), se)
	lower2 <- res2 - tapply(data[[variable2]], factor(data[[factor]]), se) 
	
	CEX=1.6
	
	if (is.null(rng1)) {
		arng1=axis.range(c(upper1, lower1))
	} else {
		arng1=rng1
	}
	
	if (is.null(rng2)) {
		arng2=axis.range(c(upper2, lower2))
	} else {
		arng2=rng2
	}

	ext1 <- arng1[2]-arng1[1]
	ext2 <- arng2[2]-arng2[1]

	# Now, find a way to rescale the Right Y on the Left Y
	
	
	xpoints = 1:length(levels(factor(data[[factor]])))
	
	plot.new()
	plot.window(c(1, xpoints[length(xpoints)]), arng1[1:2])
	## X axis
	axis(1, at=xpoints, labels=levels(factor(data[[factor]])), cex=4)
	
	## Left Y
	axis(2, at=seq(arng1[1], arng1[2], arng1[3]), labels=seq(arng1[1], arng1[2], arng1[3]))
	
	## Right Y
	print(ext1)
	print(ext2)
	print(arng2[3])
	
	print(ext1*arng2[3]/ext2)
	print(seq(arng1[1], arng1[2], ext1*arng2[3]/ext2))
	axis(4, at=seq(arng1[1], arng1[2], ext1*arng2[3]/ext2), labels=seq(arng2[1], arng2[2], arng2[3]))
	
	box(bty="o")
	
	#colors <- grey(seq(0, 1, length(levels)-1))
	#points <- seq(21, 21+length(levels)-1, 1)
	
	## The line
	if (!is.null(abs)) {
		abline(h=abs, lty=2)
	}

	lines(xpoints, res1, cex=CEX)
	arrows(xpoints, res1, xpoints, upper1, angle=90, length=0.10) #col=colors[j])
	arrows(xpoints, res1, xpoints, lower1, angle=90, length=0.10) #col=colors[j])
	points(xpoints, res1, pch=21, lwd=CEX, cex=CEX, bg="black") 

	f <- ext1/ext2   # conversion factor
	a <- arng1[1]    # Intercept
	
	sres2 <- res2*f + a
	
	lines(xpoints, sres2, cex=CEX)
	arrows(xpoints, sres2, xpoints, f*upper2+a, angle=90, length=0.10) #col=colors[j])
	arrows(xpoints, sres2, xpoints, f*lower2+a, angle=90, length=0.10) #col=colors[j])
	points(xpoints, sres2, pch=21, lwd=CEX, cex=CEX, bg="white") 
		
	title(main=paste(variable1, "and", variable2, "by", factor), ylab=variable1, xlab=factor, ...)
	legend(legpos, legend=paste(c(variable1, variable2), "+/- SEM"), pch=21, lty=1, pt.bg=c("black", "white"), pt.cex=CEX, bty="n")
	mtext(variable2, side=4, las=0)
#	mtext("Responses per Second", side=4, line=2, las=0, cex=0.8)

}


plot.cor <- function(data, var1, var2, group=NULL, xrng=NULL, yrng=NULL, lines=TRUE) {
	CEX=1.6
	
	#colors <- grey(seq(0, 1, length(levels)-1))
	
	if (is.null(xrng) ) {
		xrng <- axis.range(data[[var2]], borders=1/100)
	}
	#xrng[1] <- min(data[[var2]])
	#xrng[2] <- max(data[[var2]])
	if (is.null(yrng)) {
		yrng <- axis.range(data[[var1]], borders=1/100)
	}
	xpoints = 1:length(levels(factor(data[[var2]])))
	
	plot.new()
	plot.window(xrng[1:2], yrng[1:2])
	axis(2, at=seq(yrng[1], yrng[2], yrng[3]), labels=seq(yrng[1], yrng[2], yrng[3]))
	axis(1, at=seq(xrng[1], xrng[2], xrng[3]), labels=seq(xrng[1], xrng[2], xrng[3]))
	
	#points(data[[var2]], data[[var1]], pch=20, lwd=CEX, cex=CEX)
	
	groups <- unique(data[[group]])
	colors <- rainbow(length(groups))
	offsets <- c(-0.1, 0.1)
	
	for (i in seq(length(groups))) {
		#print(g)
		sub <- subset(data, data[[group]] == groups[i])
		print(sub[[var1]])
		points(sub[[var2]]+offsets[i], sub[[var1]], pch=20, col=colors[i], lwd=CEX, cex=CEX)
		
		if (lines==TRUE) {
			coefs <- lm(paste(var1, "~", var2), sub)$coefficients
			abline(a=coefs[1], b=coefs[2], col=colors[i], lty=2)
		}
	}
	box(bty="o")
	
}

plot.bars<-function(data, var1, factor1, bar.color='black', arrow.color='grey', ...) {
	h<-tapply(data[[var1]], data[[factor1]], mean)
	s<-tapply(data[[var1]], data[[factor1]], se)
	x<-barplot(h, col=bar.color, border=arrow.color, ...)
arrows(x, h, x, h+s, angle=90, length=0.075, col=arrow.color, lwd=2)
	arrows(x, h, x, h-s, angle=90, length=0.075, col=arrow.color, lwd=2)
	x
}


plot.2bars<-function(data, var1, factor1, factor2, bar.color='black', arrow.color='grey', ...) {
	h<-tapply(data[[var1]], list(factor1=data[[factor1]], factor2=data[[factor2]]), mean)
	s<-tapply(data[[var1]], list(factor1=data[[factor1]], factor2=data[[factor2]]), se)
	x<-barplot(h, border=arrow.color, beside=T, ...)
	print(s)
	for (i in range(1, length(unique(levels(factor(data[[factor1]])))))) {
		print(i)
		print(h[i,])
		arrows(x[i,], h[i,], x[i,], h[i,]+s[i,], angle=90, length=0.075, col=arrow.color, lwd=2)
		arrows(x[i,], h[i,], x[i,], h[i,]-s[i,], angle=90, length=0.075, col=arrow.color, lwd=2)
	}
	x
}

