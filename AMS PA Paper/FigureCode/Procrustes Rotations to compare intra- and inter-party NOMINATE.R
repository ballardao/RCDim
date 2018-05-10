rm(list = ls())
setwd("~/Dropbox/Dimensionality/PAFinal/")

#################
# Load packages #
#################

installPackages <- FALSE
toInstall <- c("mvtnorm", "pscl", "wnominate", "ggplot2", "arm",   "snow", 
               "oc", "MASS", "akima", "vegan",
               "plyr", "fields", "foreach",  "multicore","doMC", 
               "wnominate", "ggplot2", "pscl", "wnominate", "ggplot2",
               "parallel", "proxy")
if(installPackages){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

empiricalVoteMargins <- as.matrix(read.csv("EmpiricalCode/DataFiles/Observed Frequency of Vote Margins.csv"))
nTypicalSenateVotes <- 525

parameterSettings <- list(
  nDimensions = 3, 
  nDraws = 5000,
  typicalSenateVote =525,  # c(1000, 2000, 5000),  # A typical recent Senate will range from 500 to 900 or so votes.
  nObservations = c(101),
  normalMean = 0, # This really should not be varied
  normalVariance = 1, # Neither should this
  nSimulations = 1:25,  # We can run several iterations of each setting.
  nSeparateDimensions = 3,  # c(1,2,3,4,999),  # c(1,2,3,4,999), ## 999 means separation on all dimensions
  partySeparation =  c(0, 1, 2, 4, 6),  #  c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5,5.5, 6),
  majorityPartySize = 51,
  scalePartySubsets = c(TRUE),  # This could also be false, in which case the script returns only the chamber-level NOMINATE.
  beta = c(.5,2),
  radius=c(9, 11),
  lop=c(.03)
)


# Combine parameter options into a frame to sweep:
parametersToSweep <- expand.grid(parameterSettings)
dim(parametersToSweep)

### Modify (and remove) some of the parameter settings to sweep ###

# 999 separate dimensions gets recoded to the maximum number of dimensions:
parametersToSweep$nSeparateDimensions[parametersToSweep$nSeparateDimensions == 999] <- parametersToSweep$nDimensions[parametersToSweep$nSeparateDimensions == 999]

# Remove settings where the number of separate dimensions is greater than the total number of dimensions
parametersToSweep <- parametersToSweep[!(parametersToSweep$nDimensions < parametersToSweep$nSeparateDimensions), ]

# Remove all where ndSeparate dimension is greater than 1, but partysepartion = 0
#parametersToSweep <- parametersToSweep[!(parametersToSweep$partySeparation == 0 & parametersToSweep$nSeparateDimension > 1), ]

# Review the final set of parameters:
rownames(parametersToSweep) <- 1:nrow(parametersToSweep)
apply(parametersToSweep, 2, table)

# Read in functions
installPackages <- TRUE
source("SimulationsMainText/SimulationCodeMain.R", local = TRUE, echo = TRUE)

################################# WORK DONE HERE ############################################################

procrustesFitCollection <- list()

for(rrr in 1:nrow(parametersToSweep)){
whichRow <- rrr
print(rrr)
rowParameters <- unlist(parametersToSweep[whichRow, ])
rowParameters
idealPoints <- preferenceGenerator(mPSize = rowParameters["majorityPartySize"],
                                   nObs = rowParameters["nObservations"],
                                   pSep = rowParameters["partySeparation"],
                                   nDim = rowParameters["nDimensions"],
                                   nSDim = rowParameters["nSeparateDimensions"],
                                   normV = rowParameters["normalVariance"])
.radius <- rowParameters["radius"]
proposalPoints <- nBallDraw(nDim=rowParameters["nDimensions"],nDraws = rowParameters["nDraws"],
                            .radius=.radius )
statusQuoPoints <- nBallDraw(nDim=rowParameters["nDimensions"],nDraws = rowParameters["nDraws"],
                             .radius=.radius )
proposalsAndStatusQuos <- list(proposalPoints = proposalPoints, statusQuoPoints = statusQuoPoints)

rollCallMatrix <- generateRollCalls(.iPoints = idealPoints,
                                    pASQ = proposalsAndStatusQuos,
                                    beta=rowParameters["beta"], lop=rowParameters["lop"])
  reverseThese <- sample(1:rowParameters["typicalSenateVote"], rowParameters["typicalSenateVote"]/2, replace=FALSE)
  rollCallMatrix[,reverseThese] <- (rollCallMatrix[,reverseThese]-1)*-1
  ###FLAG: Reverse coding roll calls to ensure easy replication
  # Fit Roll Call Matrix to empirical distributions

partyUnityScores <- partyUnityCalculator(iPoints = idealPoints,
                                         rcMat = rollCallMatrix)
nominateObject <- dwNominateFunction(rcMat = rollCallMatrix,
                                     nDim = rowParameters["nDimensions"],
                                     lop=rowParameters["lop"])
simulationSummary <- summarizeSimulation(wNom = nominateObject,
                                         pUnity = partyUnityScores,
                                         whichRow = whichRow,
                                         label = "fullChamber")
  
  ### Subsection for party-subset scaling ###
  #if(rowParameters["scalePartySubsets"] == 1){
  if(1 == 0){
print("Scaling party subsets...")
    partySubsetSummaries <- partySubsetScaler(iPoints = idealPoints,
                                              rcMat = rollCallMatrix,
                                              nDim = rowParameters["nDimensions"],
                                              pUnity = partyUnityScores,
                                              whichRow = whichRow, numDupes=numDupes,
                                              maxDupes=maxDupes)
    simulationSummary <- data.frame(rbind(simulationSummary, partySubsetSummaries))
   } # End party-subset conditional
  
  rownames(simulationSummary) <- NULL


iPoints = idealPoints
pUnity = partyUnityScores
leftRollCalls <- rollCallMatrix[iPoints$Party == -1, ]
rightRollCalls <- rollCallMatrix[iPoints$Party == 1, ]
leftNominateObject <- dwNominateFunction(rcMat = leftRollCalls,
                                         nDim = rowParameters["nDimensions"],
                                         lop=rowParameters["lop"])
rightNominateObject <- dwNominateFunction(rcMat = rightRollCalls,
                                          nDim = rowParameters["nDimensions"],
                                          lop=rowParameters["lop"])
leftSimulationSummary <-  summarizeSimulation(wNom = leftNominateObject,
                                              pUnity = pUnity,
                                              whichRow = whichRow,
                                              label = "leftPartyOnly")
rightSimulationSummary <-  summarizeSimulation(wNom = rightNominateObject,
                                               pUnity = pUnity,
                                               whichRow = whichRow,
                                               label = "rightPartyOnly")
output <- data.frame(rbind(leftSimulationSummary, rightSimulationSummary))

############################### ROTATION HERE #################################

par(mfcol = c(1, 1))
head(idealPoints)
nominateObject$legislators[, substr(colnames(nominateObject$legislators), 1, 5) == "coord"]
rightNominateObject$legislators[, substr(colnames(rightNominateObject$legislators), 1, 5) == "coord"]
leftNominateObject$legislators[, substr(colnames(leftNominateObject$legislators), 1, 5) == "coord"]

fullNOM3D <- nominateObject$legislators[, substr(colnames(nominateObject$legislators), 1, 5) == "coord"][, 1:3]
rightNOM3D <- rightNominateObject$legislators[, substr(colnames(rightNominateObject$legislators), 1, 5) == "coord"][, 1:3]
leftNOM3D <- leftNominateObject$legislators[, substr(colnames(leftNominateObject$legislators), 1, 5) == "coord"][, 1:3]
fullIP3D <- idealPoints[, 1:3]
rightIP3D <- idealPoints[iPoints$Party == 1, 1:3]
leftIP3D <- idealPoints[iPoints$Party == -1, 1:3]

fullRotation <- procrustes(X = fullIP3D, Y = fullNOM3D)
leftRotation <- procrustes(X = leftIP3D, Y = leftNOM3D)
rightRotation <- procrustes(X = rightIP3D, Y = rightNOM3D)
plot(fullRotation)
plot(leftRotation)
plot(rightRotation)
summary(fullRotation)
summary(leftRotation)
summary(rightRotation)

fullTest <- protest(X = fullIP3D, Y = fullNOM3D)
leftTest <- protest(X = leftIP3D, Y = leftNOM3D)
rightTest <- protest(X = rightIP3D, Y = rightNOM3D)
(fullTest)
(leftTest)
(rightTest)

procFrame <- data.frame(fullSoS = fullTest$ss, fullCorr = fullTest$t0, fullSig = fullTest$signif,
                        leftSoS = leftTest$ss, leftCorr = leftTest$t0, leftSig = leftTest$signif,
                        rightSoS = rightTest$ss, rightCorr = rightTest$t0, rightSig = rightTest$signif)
outFrame <- data.frame(t(rowParameters), procFrame)

procrustesFitCollection[[rrr]] <- outFrame
}

procrustesFitFrame <- do.call(rbind, procrustesFitCollection)
head(procrustesFitFrame)
procrustesFitFrame$combinedSoS <- with(procrustesFitFrame, sqrt(leftSoS^2+rightSoS^2))
procrustesFitFrame$partySepLabel <- paste0("D = ", procrustesFitFrame$partySeparation)

#write.csv(procrustesFitFrame, "/Results of procrustes comparison.csv", row.names = F)

zzp6 <- ggplot(procrustesFitFrame[is.element(procrustesFitFrame$partySeparation, c(0, 2, 4, 6)), ],
               aes(x = fullSoS, y = combinedSoS))
zzp6 <- zzp6 + geom_abline(intercept = 0, slope = 1, lty = 3, colour = "GRAY")
zzp6 <- zzp6 + geom_point()
#zzp6 <- zzp6 + geom_smooth(method = "lm", alpha = 1/4)
zzp6 <- zzp6 + facet_wrap( ~ partySepLabel)
zzp6 <- zzp6 + theme_bw()
zzp6 <- zzp6 + coord_equal()
zzp6 <- zzp6 + xlab("RMSE for Full-Chamber NOMINATE")
zzp6 <- zzp6 + ylab("RMSE for Intra-Party NOMINATE")
#zzp6 <- zzp6 + ggtitle("Comparing fidelity of NOMINATE configurations to \"true\" ideal points")
print(zzp6)
ggsave(plot = zzp6, "PaperFiles/TexFiles/Procrustes_plot.ps", h = 4.5, w = 6.5)
