rm(list=ls())

## Read in the simulation results
setwd("~/Dropbox/Dimensionality/PAFinal/SimulationsMainText/SimulationResults/")
results <- read.csv("AggregateOutput.csv")
results2 <- read.csv("AggregateOutput2.csv")
results <- rbind(results, results2)



resultsAll <- results[results$label==1 & results$nSeparateDimensions<=3 & results$partySeparation<=7 & results$radius%in%c(9,11) & results$beta%in%c(.5, 1, 1.5),]

## These are empirical party unity scores for the 79th-112th Senate taken from:
## ftp://voteview.com/partyunity_house_senate_1-112.dat
pUnity <- c(0.56148
,0.62903
,0.64176
,0.61981
,0.51661
,0.46544
,0.41042
,0.45261
,0.53738
,0.42509
,0.46479
,0.34899
,0.36937
,0.40105
,0.42179
,0.418
,0.45588
,0.4649
,0.46066
,0.41478
,0.51351
,0.41802
,0.44828
,0.51273
,0.60635
,0.66812
,0.53758
,0.56696
,0.51185
,0.62074
,0.60465
,0.57534
,0.75144)



temp <- (resultsAll$pUnity.nPartyUnityVotes/resultsAll$nRollCallsScaled)*100



postscript(paper="special", width=4, height=3, file="~/Dropbox/Dimensionality/PAFinal/PaperFiles/TexFiles/PartyUnity.ps",  pagecentre=TRUE, onefile=FALSE, horizontal=FALSE)
par(yaxt="n", mgp=c(1,0,0), tcl=.25, mar=c(2,.1,.1,.1))
plot(density(temp[resultsAll$partySeparation==2 ], from=0, to=100), ylim=c(0, .055),lwd=2, lty=2, main="", col="darkgreen", xlab="Percent party unity votes", ylab="", cex.main=1, cex.lab=.9)
lines(density(temp[resultsAll$partySeparation==3], from=0, to=100), ylim=c(0,4), lwd=2, lty=3, col="gray40")
lines(density(temp[resultsAll$partySeparation==4], from=0, to=100), ylim=c(0,4), lwd=2, lty=1, col="orange3")
lines(density(pUnity*100), lwd=2, lty=4, col="yellow3")
legend(x="topright", legend=c("D=2", "D=3", "D=4", "Empirical"), lty=c(2,3,1,4), col=c("darkgreen", "gray40", "orange3", "yellow3"), lwd=c(2,2,2,2), bty="n")
dev.off()

for (i in unique(resultsAll$partySeparation)){
print(i)
  print(median(temp[resultsAll$partySeparation==i]))
  
}


plot(resultsAll$partySeparation, temp)


