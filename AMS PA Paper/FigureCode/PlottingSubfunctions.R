setwd("~/Dropbox/Dimensionality/PARevision/Empirical/Empirical_Results/")

### Read in results from real Nominate data
filesToLoad <-  list.files("Subresults")
filesToLoad

NOM <- c()
for(ff in filesToLoad){
  newNOM <- read.csv(paste("Subresults/", ff, sep = ""))
  NOM <- data.frame(rbind(NOM, newNOM))
  }
NOM <- NOM[, -1]
NOM <- NOM[apply(NOM[, substr(colnames(NOM), 1, 4) == "apre"], 1, min, na.rm = T) > 0, ]
NOM$year <- (NOM$cong-80) * 2 + 1948


# Get bounds for gray boxes
NomRed <- subset(NOM, chamber=="sen" & whichSubset=="All")
rect1 <- summary(NomRed$apreD1)[c(1,6)]
rect2 <- summary(NomRed$apreD3-NomRed$apreD2)[c(1,6)]

### Some sub-functions for creating the plots
sub.plotter.raw <- function(.y1, .y2, .y3, .x, .xlab, .ylab, .main, .adj, Prect=TRUE){
  ux <- sort(unique(.x))
  plot(.x, .y1, col="pink", ylim=c(0,1), xlab=.xlab, ylab=.ylab)
  if(Prect){rect(-1, rect1[1], 100, rect1[2], col="gray95")}
  points(.x, .y1, col="pink", ylim=c(0,1), xlab=.xlab, ylab=.ylab)
  points(.x+.adj, .y2, col="skyblue", pch=2)
  points(.x+2*.adj, .y3, col="lightgreen", pch=4)
  points(ux, sapply(ux, function(i) median(.y1[.x==i])), cex=1.2, pch=19, type="b", col="red")
  points(ux+.adj, sapply(ux, function(i) median(.y2[.x==i])), cex=1.2, pch=17, type="b", col="blue", lty=2)
  points(ux+2*.adj, sapply(ux, function(i) median(.y3[.x==i])), cex=1.2, pch=7, type="b", col="darkgreen", lty=4)
  title(.main, line=.6)
#  abline(h=0, col="gray70")
  box()
}
sub.plotter.diff <- function(.y1, .y2, .y3, .y4, .x, .xlab, .ylab, .main, .adj, Prect=TRUE, myYlim = c(0, .35)){
  ux <- sort(unique(.x))
  plot(.x, .y2-.y1, col="pink", ylim=myYlim, xlab=.xlab, ylab=.ylab)
  if(Prect){rect(-1, rect2[1], 100, rect2[2], col="gray95")}
  points(.x, .y2-.y1, col="pink", ylim=c(0,.2), xlab=.xlab, ylab=.ylab)
  points(.x+.adj, .y3-.y2, col="skyblue", pch=2)
  points(.x+2*.adj, .y4-.y3, col="lightgreen", pch=4)
  points(ux, sapply(ux, function(i) median((.y2-.y1)[.x==i])), cex=1.2, pch=19, type="b", col="red")
  points(ux+.adj, sapply(ux, function(i) median((.y3-.y2)[.x==i])), cex=1.2, pch=17, type="b", col="blue", lty=2)
  points(ux+2*.adj, sapply(ux, function(i) median((.y4-.y3)[.x==i])), cex=1.2, pch=7, type="b", col="darkgreen", lty=4)
  title(.main, line=.6)
#  abline(h=0, col="gray70")
  box()
}
