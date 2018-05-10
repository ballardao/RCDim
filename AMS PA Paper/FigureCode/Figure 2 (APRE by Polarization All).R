setwd("~/Dropbox/Dimensionality/PAFinal/FigureCode/")
source("PlottingSubfunctionsBW.R")

## Read in the simulation results
setwd("~/Dropbox/Dimensionality/PAFinal/SimulationsMainText/SimulationResults/")
results <- read.csv("AggregateOutput.csv")
results2 <- read.csv("AggregateOutput2.csv")
results <- rbind(results, results2)


setwd("~/Dropbox/Dimensionality/PAFinal/PaperFiles/TexFiles/")
postscript(paper="special", width=7, height=5, file="~/Dropbox/Dimensionality/PAFinal/PaperFiles/TexFiles/Figure2.ps",  pagecentre=TRUE, onefile=FALSE, horizontal=FALSE)
this.data <- subset(results,  partySeparation==0  & nSeparateDimensions>=0 & label==1 & nDimensions<=10 & beta%in%c(.5, 1, 1.5) & radius%in%c(7,9,11))
par(mfrow=c(1,2))
par(mgp=c(1,0,0), mar=c(3,2,1,1), tcl=.05)
with(this.data, sub.plotter.raw(apreD1, apreD2, apreD3, nDimensions, .ylab="APRE", .xlab="# Dimensions",  expression(paste("APRE")), .1))
legend("topright", c("APRE1", "APRE2", "APRE3"),  col=c("gray10", "gray40", "gray60"), pch=c(19,17,7), lty=c(1,2,4), cex=.9, bty="n")
text(7, .7, "Region of \n Empirical APRE1", cex=.8)
with(this.data, sub.plotter.diff(apreD1, apreD2, apreD3, apreD4, nDimensions, .ylab= expression(paste(Delta, "APRE")), .xlab="# Dimensions", expression(paste(Delta, "APRE")), .1, myYlim=c(0, .45)))
legend("topright", c("APRE2-APRE1", "APRE3-APRE2", "APRE4-APRE3"),  col=c("gray10", "gray40", "gray60"),, pch=c(19,17,7), lty=c(1,2,4), cex=.9, bty="n")
text(7, .05, "Region of \n Empirical APRE3-APRE2", cex=.8)
dev.off()
