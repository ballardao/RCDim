rm(list=ls())
setwd("~/Dropbox/Dimensionality/PAFinal/FigureCode/")
source("PlottingSubfunctions.R")

## Read in the simulation results
setwd("~/Dropbox/Dimensionality/PAFinal/SimulationsMainText/SimulationResults/")
results <- read.csv("AggregateOutput.csv")
results2 <- read.csv("AggregateOutput2.csv")
results <- rbind(results, results2)


setwd("~/Dropbox/Dimensionality/PAFinal/PaperFiles/TexFiles/")
postscript(paper="special", width=7, height=7, file="~/Dropbox/Dimensionality/PAFinal/PaperFiles/TexFiles/Figure4.ps",  pagecentre=TRUE, onefile=FALSE, horizontal=FALSE)
psValues <-  names(table(results$partySeparation))
psValues
table(results$beta)
par(mfrow=c(2,4))
par(mgp=c(1,0,0), mar=c(3,2,1.5,1), tcl=.05)
temp <- c(.5, 1, 1.5)
this.data1 <- subset(results,  partySeparation%in%psValues[1:15]  & nSeparateDimensions%in%1:4 & label==1 & nDimensions%in%c(1) & radius%in%c(7:11) & beta%in%temp)
this.data2 <- subset(results,  partySeparation%in%psValues[1:15]  & nSeparateDimensions%in%1:4 & label==1 & nDimensions%in%c(7)& radius%in%c(7:11) & beta%in%temp)
this.data3 <- subset(results,  partySeparation%in%psValues[1:15]  & nSeparateDimensions%in%1:4 & label==1 & nDimensions%in%c(15)& radius%in%c(7:11)& beta%in%temp)
this.data4 <- subset(results,  partySeparation%in%psValues[1:15]  & nSeparateDimensions%in%1:4 & label==1 & nDimensions%in%c(4)& radius%in%c(7:11)& beta%in%temp)
with(this.data1, sub.plotter.raw(apreD1, apreD2, apreD3, partySeparation, .ylab="APRE", .xlab="Party Separation", expression(paste("p=1")), .1))
legend("bottomright", c("APRE1", "APRE2", "APRE3"),  col=c("red", "blue", "darkgreen"), pch=c(19,17,7), lty=c(1,2,4), cex=.9, bty="n")
text(4, .4, "Region of \n Empirical APRE1", cex=.8)
with(this.data4, sub.plotter.raw(apreD1, apreD2, apreD3, partySeparation, .ylab="APRE", .xlab="Party Separation", expression(paste("p=4")), .1))
legend("bottomright", c("APRE1", "APRE2", "APRE3"),  col=c("red", "blue", "darkgreen"), pch=c(19,17,7), lty=c(1,2,4), cex=.9, bty="n")
with(this.data2, sub.plotter.raw(apreD1, apreD2, apreD3, partySeparation, .ylab="APRE", .xlab="Party Separation", expression("p=7"), .1))
legend("bottomright", c("APRE1", "APRE2", "APRE3"),  col=c("red", "blue", "darkgreen"), pch=c(19,17,7), lty=c(1,2,4), cex=.9, bty="n")
with(this.data3, sub.plotter.raw(apreD1, apreD2, apreD3, partySeparation, .ylab="APRE", .xlab="Party Separation", expression("p=15"), .1))
legend("bottomright", c("APRE1", "APRE2", "APRE3"),  col=c("red", "blue", "darkgreen"), pch=c(19,17,7), lty=c(1,2,4), cex=.9, bty="n")
with(this.data1, sub.plotter.diff(apreD1, apreD2, apreD3, apreD4, partySeparation, .ylab=expression(paste(Delta, "APRE")), .xlab="Party Separation",  expression(paste("p=1")), .1))
legend("topright", c("APRE2-APRE1", "APRE3-APRE2", "APRE4-APRE3"),  col=c("red", "blue", "darkgreen"),, pch=c(19,17,7), lty=c(1,2,4), cex=.9, bty="n")
text(4, .05, "Region of \n Empirical APRE3-APRE2", cex=.8)
with(this.data4, sub.plotter.diff(apreD1, apreD2, apreD3, apreD4, partySeparation, .ylab=expression(paste(Delta, "APRE")), .xlab="Party Separation",  expression(paste("p=4")), .1))
legend("topright", c("APRE2-APRE1", "APRE3-APRE2", "APRE4-APRE3"),  col=c("red", "blue", "darkgreen"),, pch=c(19,17,7), lty=c(1,2,4), cex=.9, bty="n")
with(this.data2, sub.plotter.diff(apreD1, apreD2, apreD3, apreD4, partySeparation, .ylab=expression(paste(Delta, "APRE")), .xlab="Party Separation", expression("p=7"), .1))
legend("topright", c("APRE2-APRE1", "APRE3-APRE2", "APRE4-APRE3"),  col=c("red", "blue", "darkgreen"),, pch=c(19,17,7), lty=c(1,2,4), cex=.9, bty="n")
with(this.data3, sub.plotter.diff(apreD1, apreD2, apreD3, apreD4, partySeparation, .ylab=expression(paste(Delta, "APRE")), .xlab="Party Separation", expression("p=15"), .1))
legend("topright", c("APRE2-APRE1", "APRE3-APRE2", "APRE4-APRE3"),  col=c("red", "blue", "darkgreen"),, pch=c(19,17,7), lty=c(1,2,4), cex=.9, bty="n")
dev.off()





