#################################
## Simulations for main results#
## Author: Jacob Montgomery and David Sparks
## Last Updated: Nov 6, 2013
## Purpose: Define functions for simulation sweep.
#################################

#################
# Load packages #
# Note: If toInstall==TRUE, this code will install all required packages
#################
toInstall <- c("mvtnorm", "pscl",
               "wnominate", "arm",
               "MASS", "akima",
               "plyr", "fields",
               "foreach", "parallel",
               "doMC")
if(installPackages){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

## Read in data file containing 527 quantiles of vote margins from the empirical record
setwd("~/Dropbox/Dimensionality/PAFinal/EmpiricalCode/DataFiles/")
empiricalVoteMargins <- read.csv("Observed Frequency Of Vote Margins.csv") 

#############
# Functions #
#############

# Generate voter preferences / ideal points:
preferenceGenerator <- function(mPSize, nObs, pSep, nDim, nSDim, normV){
  ## mPSize The size of the majority party
  ## nObs The total number of members who will vote
  ## pSep The distance between party means along each separating dimension
  ## nDim The total number of dimensions
  ## nSDim The number of dimensions along which party means are separated
  ## normV Within-party variance along each dimension. 
  voterParty <- rep(x = c(1, -1),
                    times = c(mPSize, nObs - mPSize))
  alpha1 <- rep(x = c(pSep/2, 0) * 1,
                times = c(nSDim, nDim - nSDim))
  alpha2 <- rep(x = c(pSep/2, 0) * -1,
                times = c(nSDim, nDim - nSDim))
  majorityPartyPreferences <- rmvnorm(n = mPSize,
                                      mean = alpha1,
                                      sigma = diag(normV, nDim))
  minorityPartyPreferences <- rmvnorm(n = nObs - mPSize,
                                      mean = alpha2,
                                      sigma = diag(normV, nDim))
  allVoterPreferences <- data.frame(rbind(majorityPartyPreferences, minorityPartyPreferences))
  allVoterPreferences$Party <- (voterParty) 
  return(allVoterPreferences)
  # Returned value is a data frame where the first nDim columns being member preferences on each dimension.
  # The final column of the data frame is the party affiliation (1=Majority, -1=Minority)
}

# Choose one member at random to provide the proposal point
generateProposals <- function(iPoints, nDraws, nDim){
  ## iPoints The ideal points matrix returned from preferenceGenerator
  ## nDraws The number of proposal points to draw
  ## nDim The number of dimensions in the simulation
  nObs <- nrow(iPoints)
  whoProposes <- sample(x = c(1:nObs), size = nDraws, replace = T)
  proposalPoints <- as.matrix(iPoints[whoProposes, 1:nDim])
  return(proposalPoints)
  ## Returned value is a matrix of points in the policy space associated with the proposals
}


# Takes ideal points and cut points, draws proposers and proposals, infers status quo points
generateProposalsAndStatusQuos <- function(pPoints, cPoints, nDraws, nDim){
  ## pPoints A matrix of proposal points
  ## cPoints A matrix of "cut points", points where the separating hyperplane intersects the line connecting the proposal and status quo
  ## nDraws The number of roll calls being drawn
  ## nDim An integer indicating the number of dimensions in the policy space
  statusQuoPoints <- (cPoints - abs(cPoints - pPoints)*(pPoints > cPoints) +
                      abs(cPoints - pPoints)*(pPoints < cPoints))
  statusQuoPoints <- as.matrix(statusQuoPoints)
  output <- list(proposalPoints = pPoints,
                 statusQuoPoints = statusQuoPoints)
  return(output)
  # Returns a list, where the first element is the pPoints matrix entered and the second element is a matrix of status quo points
}


# Draw points random points out of a p-dimensional n-ball (hypersphere)
nBallDraw <- function(nDim, nDraws=25000, .radius=5){
  ## nDim The total number of dimensions
  ## nDraws The number of 
  ## .radius The radius of the hypersphere
  normingFunction<-function(x, radius){
	x<-as.matrix(x)
	numerator<-norm(x, type="F")
	numerator<-numerator/radius
	return(x/numerator)
      }
  X<-matrix(rnorm((nDim+2)*nDraws), ncol=(nDim+2))
    output<-aaply(X, 1, normingFunction, radius=.radius)
   return((matrix(output[, 1:nDim], ncol=nDim)))
    ## Returns a nDraw by nDim matrix of points.
}

# Takes ideal points and an object that contains proposal and status quo points
# Compares the distance between voters and proposal to the distance between voters and status quo
# Returns a roll call matrix as per probabalistic voting.  
generateRollCalls <- function(.iPoints,  pASQ, beta=300, lop){
  ## .iPoints Ideal points of voting members
  ## .pASQ Proposal and status quo points
  ## beta The scaling parameter for probabalistic voting
  ## lop The level of lopsidedness allowed.  Roll Calls with a smaller proportion of minority voters than lop will not be returned.
  nDim <- ncol(pASQ$proposalPoints)
  distanceFromProposal <- rdist(x1 = .iPoints[, 1:nDim],
                                x2 = pASQ$proposalPoints)
  distanceFromStatusQuo <- rdist(x1 = .iPoints[, 1:nDim],
                                 x2 = pASQ$statusQuoPoints)
  util.diff <-    beta*((distanceFromStatusQuo^2)-(distanceFromProposal^2))
  
  probVote <-  matrix(rbinom(n=length(util.diff), prob=pnorm(util.diff), size=1), nrow=nrow(util.diff))
  condition <- colMeans(probVote)>=lop & colMeans(probVote)<=(1-lop)
  rcsWithEnoughDissents<-condition==TRUE
  return(as.matrix(probVote[,rcsWithEnoughDissents]))
  ## Returns a matrix of non-unanimous/non-nearly-unanimous roll calls
}


# A function to take a large matrix of roll calls and select n=typicalSenateVote of them such that
# the roll call margin exactly matches emp
reduceRollCallMatrix<-function(rcMat, .iPoints, typicalSenateVote, lop, emp){
  ## rcMat A matrix of roll call votes
  ## .iPoints Ideal points for members
  ## typicalSenateVote The number of final roll calls to select
  ## emp The empirical distribution of vote margins being targeted
  rcMargins <- colMeans(rcMat, na.rm = T) 
  bestMatch <- proxy::dist(rcMargins, emp)
  
  myFun <- function(x){
    sample(which(x==min(x)),1)
  }
  bestMatch <- apply(bestMatch, 2, myFun) ## Chooses randomly among best matches
  rollCallMat <- rcMat[, bestMatch]
  numDupes <- sum(table(bestMatch)-1)
  maxDupes <- max(table(bestMatch))
  
  rcMatParty1<-rollCallMat[.iPoints$Party==1,]
  rcMatParty2<-rollCallMat[.iPoints$Party==-1,]
  
  output <- list(all=rollCallMat, # The complete empirically matched roll call record
                 firstOnly=rcMatParty1, # The empirically matched record including only party 1 members
                 secondOnly=rcMatParty2,# The empirically matched record including only party 1 members
                 numDupes=numDupes, # The number of roll calls that were duplicates in the total roll call matrix
                 maxDupes=maxDupes) # The maximum number of times any roll call was duplicated 
  return(output)
  # Outputs a list as described above
}


# Take roll-call matrix and party labels, and calculate party unity scores
partyUnityCalculator <- function(iPoints, rcMat){
  partyUnityVotes <- apply(rcMat, 2,
                           function(rr){
                             NYDR <- table(rr, iPoints$Party)
                             NYDRMajority <- NYDR / colSums(NYDR) >= 1/2
                             isPartyUnityVote <- diff(diag(NYDRMajority)) == 0
                             if(prod(dim(NYDR)) < 4){isPartyUnityVote <- NA}
                             isPartyUnityVote
                           }
                           )

  partyUnityVotes <- unlist(partyUnityVotes)
  ## Replace NAs (which are unanimous votes) with FALSE, because they are not Party Unity votes. Because they are unanimous.
  partyUnityVotes[is.na(partyUnityVotes)] <- FALSE

  partyAgreement <- apply(rcMat, 2,
                          function(rr){
                            partyPosition <- round(by(rr, iPoints$Party, mean)[as.character(iPoints$Party)])
                            partyAg <- (partyPosition == rr) * 1
                            return(partyAg)
                          }
                          )

  agreementOnPartyUnityVotes <- partyAgreement[, partyUnityVotes]

  # These are the scores for individuals
  partyUnityScores <- rowMeans(as.matrix(agreementOnPartyUnityVotes))
  partyUnityN <- rowSums(!is.na(as.matrix(agreementOnPartyUnityVotes)))[1]

  # But we save at the legislature level, not the legislator level, so, we'll save the average party unity score
  meanPUScore <- mean(partyUnityScores, na.rm = T)

  output <- list(voterUnityScores = partyUnityScores,
               nPartyUnityVotes = partyUnityN,
               meanPartyUnityScore = meanPUScore)
  return(output)
  }

## Take roll-call matrix and estimate WNOMINATE.
dwNominateFunction <- function(rcMat, nDim, lop){
  nDimensionsToUse <- nDim
  nDimensionsToUse[nDimensionsToUse > 10] <- 10 
  nDimensionsToUse[nDimensionsToUse < 4] <- 4  
  rollCall <- rollcall(data = rcMat,
                       yea = 1,
                       nay = 0)
  nominateObject <- wnominate(rcObject = rollCall,
                              dims = nDimensionsToUse,
                              minvotes = 20,
                              lop = lop,
                              trials = 1,
                              polarity = rep(1, nDimensionsToUse),
                              verbose = FALSE)
  return(nominateObject)
}

### Take all objects created from one parameter set, and select things to save from them.
summarizeSimulation <- function(wNom, pUnity, whichRow, label = "genericLabel", numDupes, maxDupes){
  rowParameters <- unlist(parametersToSweep[whichRow, ])
  nDim <- rowParameters["nDimensions"]  
  names(pUnity$meanPartyUnityScore) <- "meanPartyUnityScore"
  names(pUnity$nPartyUnityVotes) <- "nPartyUnityVotes"
  effectiveNumberOfEigens <- sum(wNom$eigenvalues) ^ 2 / sum(wNom$eigenvalues ^ 2)
  names(effectiveNumberOfEigens) <- "eNEigenvalues"
  first10Eigens <- wNom$eigenvalues[1:10]
  names(first10Eigens) <- paste0("Eigen", 1:10)
                                        
  nDimensionsToUse <- nDim
  nDimensionsToUse[nDimensionsToUse > 10] <- 10  # Artifical capping at 10  ###FLAG###
  nDimensionsToUse[nDimensionsToUse < 4] <- 4  # Artifical floorign at 4 ### Flag### 
  correctClass <- rep(NA, 10)
  names(correctClass) <- paste("correctClassD", c(1:10), sep="")
  correctClass[1:nDimensionsToUse] <- wNom$fits[1:nDimensionsToUse]
  apre <- rep(NA, 10)
  names(apre) <- paste("apreD", c(1:10), sep="")
  apre[1:nDimensionsToUse] <- wNom$fits[(nDimensionsToUse+1):(nDimensionsToUse*2)]
  gmp <- rep(NA, 10)
  names(gmp) <- paste("gmp", c(1:10), sep="")
  gmp[1:nDimensionsToUse] <- wNom$fits[(nDimensionsToUse*2+1):(nDimensionsToUse*3)]
  nLegislatorsScaled <- sum(!is.na(wNom$legislators$coord1D))
  nRollCallsScaled <- sum(!is.na(wNom$rollcalls$midpoint1D))
  
  output <- data.frame(parameterRow = whichRow,
                       t(rowParameters),
                       effectiveNumberOfEigens,
                       t(first10Eigens),
                       t(correctClass),
                       t(apre),
                       t(gmp),
                       pUnity$meanPartyUnityScore,
                       pUnity$nPartyUnityVotes,
                       nLegislatorsScaled,
                       nRollCallsScaled,
                       numDupes,
                       maxDupes,
                       label)
  return(output)
}

## Function to generate party-only subsets.
partySubsetScaler <- function(iPoints, sub, nDim, pUnity, whichRow, lop, numDupes, maxDupes){
  firstOnly <- dwNominateFunction(rcMat = sub$firstOnly,
                                  nDim = nDim,
                                  lop=lop)
  secondOnly <- dwNominateFunction(rcMat = sub$secondOnly,
                                   nDim = nDim,
                                   lop=lop)
  firstOnlySummary <-  summarizeSimulation(wNom = firstOnly,
                                           pUnity = pUnity,
                                           whichRow = whichRow,
                                           label = "firstOnly",
                                           numDupes=numDupes,
                                           maxDupes=maxDupes)
  secondOnlySummary <-  summarizeSimulation(wNom = secondOnly,
                                            pUnity = pUnity,
                                            whichRow = whichRow,
                                            label = "secondOnly",
                                            numDupes=numDupes,
                                            maxDupes=maxDupes)
  output <- data.frame(rbind(firstOnlySummary, secondOnlySummary))
  return(output)
}

####################
### metaFunction ###
# This function combines all of the step functions
# and is what is passing out to parallel run.
# It takes a row number, pulls parameters from parametersToSweep
# and does everything else.

metaFunction <- function(whichRow){
  rowParameters <- unlist(parametersToSweep[whichRow, ])
  totalDraws <- 0
  enough <- FALSE
  iter <- 0
  lop <- rowParameters["lop"]

  ## set up a blank roll call matrix
  rollCallMatrix <- matrix(0, nrow=rowParameters["nObservations"], ncol=1) 

  ## Draw the ideal points of members
  idealPoints <- preferenceGenerator(mPSize = rowParameters["majorityPartySize"],
                                     nObs = rowParameters["nObservations"],
                                     pSep = rowParameters["partySeparation"],
                                     nDim = rowParameters["nDimensions"],
                                     nSDim = rowParameters["nSeparateDimensions"],
                                     normV = rowParameters["normalVariance"])


  while(!enough & iter<5000){ ## While loop to ensure a sufficient number of non-unanimous roll calls are drawn
    proposalPoints <- generateProposals(iPoints=idealPoints, nDraws= rowParameters["nDraws"], nDim = rowParameters["nDimensions"])
    cutPoints <- nBallDraw(nDim=rowParameters["nDimensions"],nDraws = rowParameters["nDraws"],.radius=1 )
    ## Note that this is not the final cutpoint used

    ## Alpha is going to be used to set the oblong nBall so that it encompasses both parties
    alpha <- rep(x = c(rowParameters["partySeparation"]/2, 0) * 1,
                 times = c(rowParameters["nSeparateDimensions"],rowParameters["nDimensions"] -  rowParameters["nSeparateDimensions"]))
    alpha <- alpha+3.5
    for (i in 1:rowParameters["nDimensions"]){
      cutPoints[,i] <- cutPoints[,i]*alpha[i]
    }

    ## now backwards infer the status quo positions
    proposalsAndStatusQuos <- generateProposalsAndStatusQuos(pPoints=proposalPoints, cPoints=cutPoints,
                                                             nDraws= rowParameters["nDraws"], nDim = rowParameters["nDimensions"])

    ## Generate a roll call matrix
    tempRollCallMatrix <- generateRollCalls(.iPoints = idealPoints,
                                            pASQ = proposalsAndStatusQuos,
                                            beta=rowParameters["beta"], lop=rowParameters["lop"])

    ## Randomly select half of them, and switch the proposal and status quo positions 
    reverseThese <- sample(1:dim(tempRollCallMatrix)[2], dim(tempRollCallMatrix)[2]/2, replace=FALSE)
    tempRollCallMatrix[,reverseThese] <- (tempRollCallMatrix[,reverseThese]-1)*-1

    ## Sweep through the resulting roll call matrix at each possible margin and append those with margins
    ## where the main roll call matrix object does not have enough
    for(i in 3:98){
      if( length(which(colMeans(rollCallMatrix)==i/101))<150){
        rollCallMatrix <- cbind(rollCallMatrix,tempRollCallMatrix[,which((colMeans(tempRollCallMatrix))==i/101)])
      }
    }

    ## Drop the blank roll call row created initially
    if(iter==1){rollCallMatrix <- rollCallMatrix[,-which(colMeans(rollCallMatrix)==0)]}
  
    ## Some prints to keep track of things.  We want to keep drawing until we have 150 roll calls at each possible vote margin (3-98)
    ## Once "enough" is set to TRUE, this while loop will break
    print(dim(rollCallMatrix))
    totalDraws <- totalDraws+rowParameters["nDraws"]
    enough <- !any(table(colMeans(rollCallMatrix))<150) 
    iter <- iter+1
    print(iter)
  }

  ## Randomly choose among all roll calls so that they match the empirical record perfectly
  subsets <-   reduceRollCallMatrix(.iPoints=idealPoints,
                                    rcMat=rollCallMatrix,
                                    typicalSenateVote = rowParameters["typicalSenateVote"],
                                    lop=rowParameters["lop"],
                                    emp=empiricalVoteMargins)
  rollCallMatrix <-    subsets$all; numDupes <- subsets$numDupes; maxDupes <- subsets$maxDupes

  partyUnityScores <- partyUnityCalculator(iPoints = idealPoints,
                                           rcMat = rollCallMatrix)
  nominateObject <- dwNominateFunction(rcMat = rollCallMatrix,
                                       nDim = rowParameters["nDimensions"],
                                       lop=rowParameters["lop"])
  simulationSummary <- summarizeSimulation(wNom = nominateObject,
                                           pUnity = partyUnityScores,
                                           whichRow = whichRow,
                                           label = "fullChamber",
                                           numDupes=numDupes,
                                           maxDupes=maxDupes)

  ### Subsection for party-subset scaling ###
  if(rowParameters["scalePartySubsets"] == 1){
    print("Scaling party subsets...")
    partySubsetSummaries <- partySubsetScaler(iPoints = idealPoints,
                                              sub = subsets,
                                              nDim = rowParameters["nDimensions"],
                                              pUnity = partyUnityScores,
                                              whichRow = whichRow,
                                              lop=rowParameters["lop"],
                                              numDupes=numDupes,
                                              maxDupes=maxDupes)
    simulationSummary <- data.frame(rbind(simulationSummary, partySubsetSummaries))
    } # End party-subset conditional
  
  rownames(simulationSummary) <- NULL

  ## This appends this simulation to the output summary file.
  if(whichRow==1){
    cat(colnames(simulationSummary), file=paste("~/Dropbox/Dimensionality/PAFinal/SimulationsMatching2/SimulationResults/", fileStub, ".csv", sep=""),  sep=",")
  }

  for (i in 1:nrow(simulationSummary)){
    cat("\n", file=paste("~/Dropbox/Dimensionality/PAFinal/SimulationsMatching2/SimulationResults/", fileStub, ".csv", sep=""), append=TRUE)
    cat(unlist(simulationSummary[i,]), file=paste("~/Dropbox/Dimensionality/PAFinal/SimulationsMatching2/SimulationResults/", fileStub, ".csv", sep=""), append=TRUE, sep=",")
  }
  return(NULL)
}

#####################################################################################################################
#####################################################################################################################
