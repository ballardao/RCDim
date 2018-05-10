rm(list=ls())
setwd("~/Dropbox/Dimensionality/PAFinal/FigureCode/")
source("PlottingSubfunctions.R")

## Read in the simulation results
setwd("~/Dropbox/Dimensionality/PAFinal/SimulationsMainText/SimulationResults/")
results <- read.csv("AggregateOutput.csv")
results2 <- read.csv("AggregateOutput2.csv")
results <- rbind(results, results2)


# Figure 7
sub.plot1 <- function(){
  redResults <- subset(results, nDimensions%in%c(1) & partySeparation>=0 & partySeparation<=4.5 & partySeparation>=1 & nSeparateDimensions==1 & label==1  & nRollCallsScaled>50 & radius%in%c(9,11) & beta%in%c(.5, 1, 1.5))
  redResults2 <- subset(results, nDimensions%in%c(1) & partySeparation>=0 & partySeparation<=4.5 & partySeparation>=1 & nSeparateDimensions==1 & label%in%c(2:3) & nRollCallsScaled>50 & radius%in%c(9,11) & beta%in%c(.5, 1, 1.5))
  dim(redResults)
  par(mgp=c(1,0,0), mar=c(3,2,1.5,1), tcl=.05)
  with(redResults,  x <<- partySeparation+.03)
  with(redResults, y1 <<- apreD1)
  with(redResults2, y2<<-apreD1)
  with(redResults2,  x2 <<- partySeparation-.03)
  y1<<-y1[order(x)];  y2<<-y2[order(x2)];  x2<<-x2[order(x2)];x<<-x[order(x)]
    my.fun <- function(.y1=y1, .y2=y2, .y3=y3){
    plot(NULL, col="gray70", pch=1, cex=.9, ylim=c(0, .95), xlim=c(1.5,4.5), main=expression(paste("Simulated APRE1 (P =",1,")", sep="")), ylab="APRE1", xlab="Party Separation")
    points(unique(x), sapply(unique(x), function(i) mean(.y1[x==i])), cex=1.2, pch=19, type="b", col="black")
    points(unique(x2), sapply(unique(x2), function(i) mean(y2[x2==i])), cex=1.2, pch=17, type="b", col="black", lty=2)
  }
  my.fun()
 legend("bottomleft", c("All members", "Intra-party only"), col=c("black", "black"), lty=c(1,2), pch=c(19, 17), bty="n")
}
sub.plot1()



sub.plot2 <- function(){
  redResults <- subset(results, nDimensions%in%c(5) & partySeparation>=0 & partySeparation<=4.5 & partySeparation>=1 & nSeparateDimensions%in%c(3) & label==1 & nRollCallsScaled>50 & radius%in%c(9,11) & beta%in%c(1.5))
  redResults2 <- subset(results, nDimensions%in%c(5) & partySeparation>=0 & partySeparation<=4.5 & partySeparation>=1 & nSeparateDimensions%in%c(1:3) & label%in%c(2:3) & nRollCallsScaled>50 & radius%in%c(9,11) & beta%in%c(.5, 1, 1.5))
  par(mgp=c(1,0,0), mar=c(3,2,1.5,1), tcl=.05)
  with(redResults,  x <<- partySeparation+.03)
  with(redResults, y1 <<- apreD1)
  with(redResults2, y2<<-apreD1)
  with(redResults2,  x2 <<- partySeparation-.03)
  y1<<-y1[order(x)];  y2<<-y2[order(x2)];  x2<<-x2[order(x2)];x<<-x[order(x)]
  my.fun <- function(.y1=y1, .y2=y2, .y3=y3){
    plot(NULL, col="gray70", pch=1, cex=.9, ylim=c(0, .95), xlim=c(1.5,4.5), main=expression(paste("Simulated APRE1 (",  "P = 5", ")", sep="")), ylab="APRE1", xlab="Party Separation")
#    points(x2, .y2, col="gray70", pch=2, cex=.9, ylim=c(0, .9))
    points(unique(x), sapply(unique(x), function(i) mean(.y1[x==i])), cex=1.2, pch=19, type="b", col="black")
    points(unique(x2), sapply(unique(x2), function(i) mean(y2[x2==i])), cex=1.2, pch=17, type="b", col="black", lty=2)
#    abline(h=0, col="gray70")
  }
  my.fun()
 legend("topleft", c("All members", "Intra-party only"), col=c("black", "black"), lty=c(1,2), pch=c(19, 17), bty="n")
}
sub.plot2()

sub.plot3 <- function(){
  NOMRed <- subset(NOM, NOM$chamber=="sen" & NOM$whichSubset=="All"  & NOM$nRollCalls>50)
  NOMRed <- NOMRed[order(NOMRed$year),]
  with(NOMRed, plot(year, apreD1, type="p", col="gray70", main=expression(paste("Empirical APRE1")), ylim=c(0, .95), xlim=c(1960, 2010), pch=19, ylab="APRE1", xlab="Year"))
  thisMod <- loess(NOMRed$apreD1~NOMRed$year, span=.5, family="symmetric", degree=2,   control = loess.control(surface = "direct"))
  thisSeq <- seq(from=1945, to=2010, by=2)
  predd <- predict(thisMod, thisSeq, se=TRUE)
  lines(thisSeq, predd$fit, col="black", lwd=2)
  NOMRed <- subset(NOM, NOM$chamber=="sen" & NOM$whichSubset%in%c("Rep Only", "Dem Only"))
  NOMRed <- NOMRed[order(NOMRed$year),]
  with(NOMRed, points(year, apreD1, type="p", col="gray70", pch=17))
  thisMod <- loess(NOMRed$apreD1~NOMRed$year, span=.5, family="symmetric", degree=2,   control = loess.control(surface = "direct"))
  thisSeq <- seq(from=1945, to=2010, by=2)
  predd <- predict(thisMod, thisSeq, se=TRUE)
  lines(thisSeq, predd$fit, col="black", lwd=2, lty=2)
  legend("topleft", c("All members", "Intra-party only"), col=c("black", "black"), lty=c(1,2), pch=c(19, 17), bty="n")
}
sub.plot3()
sub.plot6 <- function(){
  NOMRed <- subset(NOM, NOM$chamber=="sen" & NOM$whichSubset=="All" & NOM$nRollCalls>100)
  NOMRed <- NOMRed[order(NOMRed$year),]
  with(NOMRed, plot(year, apreD2-apreD1, type="p", col="gray70", main=expression(paste("Empirical APRE2-APRE1")), ylim=c(0, .3), ylab="APRE2-APRE1", xlab="Year", xlim=c(1960, 2010), pch=19))
  thisMod <- loess(NOMRed$apreD2-NOMRed$apreD1~NOMRed$year, span=.5, family="symmetric", degree=2,   control = loess.control(surface = "direct"))
  thisSeq <- seq(from=1945, to=2010, by=2)
  predd <- predict(thisMod, thisSeq, se=TRUE)
  lines(thisSeq, predd$fit, col="black", lwd=2)
  NOMRed <- subset(NOM, NOM$chamber=="sen" & NOM$whichSubset%in%c("Rep Only", "Dem Only"))
  NOMRed <- NOMRed[order(NOMRed$year),]
  with(NOMRed, lines(year, apreD2-apreD1, type="p", col="gray70", main="Raw APRE", ylim=c(0, .95), pch=17))
  legend("topleft", c("All members", "Intra-party only"), col=c("black", "black"), lty=c(1,2), pch=c(19,17), bty="n")
  thisMod <- loess(NOMRed$apreD2-NOMRed$apreD1~NOMRed$year, span=.5, family="symmetric", degree=2,   control = loess.control(surface = "direct"))
  thisSeq <- seq(from=1945, to=2010, by=2)
  predd <- predict(thisMod, thisSeq, se=TRUE)
  lines(thisSeq, predd$fit, col="black", lty=2, lwd=2)
}
sub.plot6()


sub.plot4 <- function(){
  redResults <- subset(results, nDimensions%in%c(1) & partySeparation>=0 & partySeparation<=4.5 & partySeparation>=1 & nSeparateDimensions==1 & label==1 & nRollCallsScaled>50 & radius%in%c(9,11)& beta%in%c(.5, 1, 1.5))
  redResults2 <- subset(results, nDimensions%in%c(1) & partySeparation>=0 & partySeparation<=4.5 & partySeparation>=1 & nSeparateDimensions==1 & label%in%c(2:3) & nRollCallsScaled>50 & radius%in%c(9,11) & beta%in%c(.5, 1, 1.5))
  with(redResults,  x <<- partySeparation+.03)
  with(redResults, y1 <<- apreD2-apreD2)
  with(redResults2, y2<<-apreD2-apreD1)
  with(redResults2,  x2 <<- partySeparation-.03)
    y1<<-y1[order(x)];  y2<<-y2[order(x2)];  x2<<-x2[order(x2)];x<<-x[order(x)]
  my.fun <- function(.y1=y1, .y2=y2, .y3=y3){
    plot(NULL, col="gray70", pch=2, cex=.4, ylim=c(0, .3), main=expression(paste("Sim. APRE2-APRE1 (P=1)")), ylab="APRE2-APRE1", xlab="Party separation", xlim=c(1.5,4.5))
    points(unique(x), sapply(unique(x), function(i) mean(.y1[x==i])), cex=1.2, pch=19, type="b", col="black")
    points(unique(x2), sapply(unique(x2), function(i) mean(y2[x2==i])), cex=1.2, pch=17, type="b", col="black", lty=2)
  }
  my.fun()
   legend("topleft", c("All members", "Intra-party only"), col=c("black", "black"), lty=c(1,2), pch=c(19, 17), bty="n")
}
sub.plot4()

sub.plot5 <- function(){
  redResults <- subset(results, nDimensions%in%c(5) & partySeparation>=0 & partySeparation<=4.5 & partySeparation>=1 & nSeparateDimensions%in%3 & label==1& radius%in%c(9,11) & beta%in%c(.5, 1, 1.5))
  redResults2 <- subset(results, nDimensions%in%c(5) & partySeparation>=0 & partySeparation<=4.5 & partySeparation>=1 & nSeparateDimensions%in%3 & label%in%c(2:3)& radius%in%c(9,11) & beta%in%c(.5, 1, 1.5))
  with(redResults,  x <<- partySeparation+.03)
  with(redResults, y1 <<- apreD2-apreD1)
  with(redResults2, y2<<-apreD2-apreD1)
  with(redResults2,  x2 <<- partySeparation-.03)
  my.fun <- function(.y1=y1, .y2=y2, .y3=y3){
        y1<<-y1[order(x)];  y2<<-y2[order(x2)];  x2<<-x2[order(x2)];x<<-x[order(x)]
    plot(NULL, col="pink", pch=2, cex=.4, ylim=c(0, .3), main=expression(paste("Sim. APRE2-APRE1 (", "P=5", ")", sep="")), ylab="APRE2-APRE1", xlab="Party separation", xlim=c(1.5,4.5))
#    points(NULL, col="skyblue", pch=2, cex=.4, ylim=c(0, .9), main=paste("Raw APRE1 (P_d =",1,")", sep=""), ylab="APRE", xlab="Party Sep", xlim=c(1,4))
    points(unique(x), sapply(unique(x), function(i) mean(.y1[x==i])), cex=1.2, pch=19, type="b", col="black")
    points(unique(x2), sapply(unique(x2), function(i) mean(y2[x2==i])), cex=1.2, pch=17, type="b", col="black", lty=2)
#    points(unique(x), sapply(unique(x), function(i) mean(.y1[x==i])), cex=1.2, pch=17, type="b", col="black", lty=2)
    abline(h=0, col="gray70")
  }
  my.fun()
 legend("topleft", c("All members", "Intra-party only"), col=c("black", "black"), lty=c(1,2), pch=c(19, 17), bty="n")
}
sub.plot5()


setwd("~/Dropbox/Dimensionality/PAFinal/PaperFiles/TexFiles/")
postscript(paper="special", width=7, height=7, file="~/Dropbox/Dimensionality/PAFinal/PaperFiles/TexFiles/Figure7.ps",  pagecentre=TRUE, onefile=FALSE, horizontal=FALSE)
par(mfrow=c(2,3))
sub.plot1()
sub.plot2()
sub.plot3()
sub.plot4()
sub.plot5()
sub.plot6()
dev.off()
