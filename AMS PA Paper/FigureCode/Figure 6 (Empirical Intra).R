rm(list=ls())

setwd("~/Dropbox/Dimensionality/PAFinal/EmpiricalCode/DataFiles")

## Load in all of the results from the empirical analysis
filesToLoad <-  list.files("Subresults")
NOM <- c()
for(ff in filesToLoad){
  newNOM <- read.csv(paste("Subresults/", ff, sep = ""))
  NOM <- data.frame(rbind(NOM, newNOM))
  }
NOM <- NOM[, -1]
NOM <- NOM[apply(NOM[, substr(colnames(NOM), 1, 4) == "apre"], 1, min, na.rm = T) > 0, ]
NOM$year <- (NOM$cong-80) * 2 + 1948



subsetsToInclude <- c("Rep Only", "Dem Only")

postscript(paper="special", width=6.5, height=4, file="~/Dropbox/Dimensionality/PAFinal/PaperFiles/TexFiles/EmpIntra.ps",  pagecentre=TRUE, onefile=FALSE, horizontal=FALSE)
#pdf(width=6.5, height=4, file="~/Dropbox/Dimensionality/PARevision/PaperFiles/Figures/EmpIntra.pdf")
NOMRed <- subset(NOM, NOM$chamber=="sen" & is.element(NOM$whichSubset, subsetsToInclude) & NOM$nRollCalls>100)
NOMRed <- NOMRed[order(NOMRed$year),]
par(mfrow=c(1,2))
par(mgp=c(1,0,0), mar=c(3,2,1,1), tcl=.05)
with(NOMRed, plot(year, apreD1, type="p", col="gray10", main=expression(paste("APRE")), ylim=c(0, .9), xlim=c(1945, 2010), pch=19, xlab="Year", ylab="APRE"))
with(NOMRed, points(year, apreD2, type="p", col="gray40", pch=17))
with(NOMRed, points(year, apreD3, type="p", col="gray60", pch=7))
thisMod <- loess(NOMRed$apreD1~NOMRed$year, span=.25, family="symmetric", degree=2,   control = loess.control(surface = "direct"))
thisSeq <- seq(from=1945, to=2010, by=2)
predd <- predict(thisMod, thisSeq, se=TRUE)
lines(thisSeq, predd$fit, col="gray10", lwd=2)
thisMod <- loess(NOMRed$apreD2~NOMRed$year, span=.25, family="symmetric", degree=2,   control = loess.control(surface = "direct"))
thisSeq <- seq(from=1945, to=2010, by=2)
predd <- predict(thisMod, thisSeq, se=TRUE)
lines(thisSeq, predd$fit, col="gray40", lwd=2, lty=2)
thisMod <- loess(NOMRed$apreD3~NOMRed$year, span=.25, family="symmetric", degree=2,   control = loess.control(surface = "direct"))
thisSeq <- seq(from=1945, to=2010, by=2)
predd <- predict(thisMod, thisSeq, se=TRUE)
lines(thisSeq, predd$fit, col="gray60", lwd=2, lty=4)
legend("topleft", c("APRE1", "APRE2", "APRE3"), lwd=1, col=c("gray10", "gray40", "gray60"), lty=c(1,2,4), pch=c(19,17,7),cex=.8, bty="n")
par(xaxt="s")
with(NOMRed, plot(year, apreD2-apreD1, type="p", col="gray10", main=expression(paste(Delta, "APRE")), ylab=expression(paste(Delta, "APRE")), ylim=c(-.01, .25), xlim=c(1945, 2010), pch=19, xlab="Year"))
with(NOMRed, lines(year, apreD3-apreD2, type="p", col="gray40", pch=17))
with(NOMRed, lines(year, apreD4-apreD3, type="p", col="gray60", pch=7))
thisMod <- loess((NOMRed$apreD2-NOMRed$apreD1)~NOMRed$year, span=.25, family="symmetric", degree=2,   control = loess.control(surface = "direct"))
thisSeq <- seq(from=1945, to=2010, by=2)
predd <- predict(thisMod, thisSeq, se=TRUE)
lines(thisSeq, predd$fit, col="gray10", lwd=2)
thisMod <- loess((NOMRed$apreD3-NOMRed$apreD2)~NOMRed$year, span=.25, family="symmetric", degree=2,   control = loess.control(surface = "direct"))
thisSeq <- seq(from=1945, to=2010, by=2)
predd <- predict(thisMod, thisSeq, se=TRUE)
lines(thisSeq, predd$fit, col="gray40", lwd=2, lty=2)
thisMod <- loess((NOMRed$apreD4-NOMRed$apreD3)~NOMRed$year, span=.25, family="symmetric", degree=2,   control = loess.control(surface = "direct"))
thisSeq <- seq(from=1945, to=2010, by=2)
predd <- predict(thisMod, thisSeq, se=TRUE)
lines(thisSeq, predd$fit, col="gray60", lwd=2, lty=4)
legend("topright", c("APRE2 - APRE1", "APRE3 - APRE2", "APRE3 - APRE4"),  lwd=1, col=c("gray10", "gray40", "gray60"), cex=.8, lty=c(1,2,4), pch=c(19, 17, 7), bty="n")
dev.off()
