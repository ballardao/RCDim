#################################
## Simulations for main results#
## Author: Jacob M. Montgomery and David Sparks
## Last Updated: November 5, 2013
## Purpose: Define functions for simulation sweep (called by ParPortion.R)
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

# Randomly sample (without replacement) from a roll call matrix 
reduceRollCallMatrix<-function(rcMat, .iPoints, typicalSenateVote, lop){
## rcMat A roll call matrix
## .iPoints Ideal point matrix, where one column represents party affiliation
## typicalSenateVote The number of roll calls to be returned
## lop The level of lopsidedness allowed.  Roll Calls with a smaller proportion of minority voters than lop will not be returned.
  firstCondition <- colMeans(rcMat)<=(1-lop) & colMeans(rcMat)>=lop
  rcsWithEnoughDissents<-firstCondition == TRUE 
  useThese<-sample((1:length(rcsWithEnoughDissents))[rcsWithEnoughDissents], size=typicalSenateVote, replace=FALSE)
  reducedRollCallMat <- rcMat[,useThese]
  
  rcMatParty1<-reducedRollCallMat[.iPoints$Party==1,]
  rcMatParty2<-reducedRollCallMat[.iPoints$Party==-1,]
  
  secondCondition<-colMeans(rcMatParty1)<=(1-(2*lop)) & colMeans(rcMatParty1)>=(2*lop) 
  thirdCondition<-colMeans(rcMatParty2)<=(1-(2*lop)) & colMeans(rcMatParty2)>=(2*lop) 

  
  output <- list(all=reducedRollCallMat,
                 firstOnly=rcMatParty1[,secondCondition],
                 secondOnly=rcMatParty2[,thirdCondition])
  return(output)
  ## Returned object contains three roll call matrices.  The first contains the full set of randomly selected roll calls that are
  # not unanimous or nearly unanimous.  The other two contain the roll calls for each party that are not unanimous or nearly unanimous.
  ## 2*lop is used to prevent roll calls not used in the full analysis from being included in the intra-party analyses
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
summarizeSimulation <- function(wNom, pUnity, whichRow, label = "genericLabel"){
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
                       label)
  return(output)
}

## Function to generate party-only subsets.
partySubsetScaler <- function(iPoints, sub, nDim, pUnity, whichRow, lop){
  firstOnly <- dwNominateFunction(rcMat = sub$firstOnly, nDim = nDim,lop=lop)
  secondOnly <- dwNominateFunction(rcMat = sub$secondOnly,nDim = nDim,lop=lop)
  firstOnlySummary <-  summarizeSimulation(wNom = firstOnly,pUnity = pUnity,whichRow = whichRow,label = "firstOnly")
  secondOnlySummary <-  summarizeSimulation(wNom = secondOnly,pUnity = pUnity,whichRow = whichRow,label = "secondOnly")
  output <- data.frame(rbind(firstOnlySummary, secondOnlySummary))
  return(output)
  }


####################
### metaFunction ###
# This function combines all of the step functions
# and is what we should be passing out to parallel processors.
# It takes a row number, pulls parameters from parametersToSweep
# and does everything else.
metaFunction <- function(whichRow){
  rowParameters <- unlist(parametersToSweep[whichRow, ])
  totalDraws <- 0
  enough <- FALSE
  iter <- 0
  lop <- rowParameters["lop"]
  rollCallMatrix <- matrix(0, nrow=rowParameters["nObservations"], ncol=1)

  while(!enough & iter<70){ # While loop to ensure a sufficient number of non-unanimous roll calls are drawn
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
  
  rollCallMatrix <- cbind(rollCallMatrix, generateRollCalls(.iPoints = idealPoints,
                                                            pASQ = proposalsAndStatusQuos,
                                                            beta=rowParameters["beta"], lop=rowParameters["lop"]))
  totalDraws <- totalDraws+rowParameters["nDraws"]
  enough <- dim(rollCallMatrix)[2]>=rowParameters["typicalSenateVote"]
  if(iter==1){rowParameters["nDraws"] <-rowParameters["nDraws"] + 10000}
  iter <- iter+1
}
  subsets <-   reduceRollCallMatrix(.iPoints=idealPoints,
                                    rcMat=rollCallMatrix,
                                    typicalSenateVote = rowParameters["typicalSenateVote"],
                                    lop=rowParameters["lop"])
  rollCallMatrix <-    subsets$all
  

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
  if(rowParameters["scalePartySubsets"] == 1){
    print("Scaling party subsets...")
    partySubsetSummaries <- partySubsetScaler(iPoints = idealPoints,
                                              sub = subsets,
                                              nDim = rowParameters["nDimensions"],
                                              pUnity = partyUnityScores,
                                              whichRow = whichRow,
                                              lop=rowParameters["lop"])
    simulationSummary <- data.frame(rbind(simulationSummary, partySubsetSummaries))
    } # End party-subset conditional
  
  rownames(simulationSummary) <- NULL
  simulationSummary$enough <- enough
  simulationSummary$totalDraws <- totalDraws

  ## This appends this simulation to the output summary file.  You will need to change the file location here and below.
  ## This file will contain the results of all of the simulations when it is completed, BUT MAY REQUIRE SOME HAND CLEANING
  if(whichRow==1){
    cat(colnames(simulationSummary), file=paste("./PAFinal/SimulationsMainText/SimulationResults/", fileStub, ".csv", sep=""),  sep=",")
  }

  for (i in 1:nrow(simulationSummary)){
    cat("\n", file=paste("./PAFinal/SimulationsMainText/SimulationResults/", fileStub, ".csv", sep=""), append=TRUE)
    cat(unlist(simulationSummary[i,]), file=paste("./PAFinal/SimulationsMainText/SimulationResults/", fileStub, ".csv", sep=""), append=TRUE, sep=",")
  }
  return(NULL)
}

#####################################################################################################################
#####################################################################################################################
