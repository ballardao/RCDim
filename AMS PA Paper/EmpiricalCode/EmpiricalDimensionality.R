rm(list = ls())
setwd("~/Dropbox/Dimensionality/PAFinal/EmpiricalCode")

###############
# Set Options #
###############
installPackages <- FALSE
nDim <- 5  
minvotes <- 20
lop = 0.025

### Select KH to test ###
combinationKH <- expand.grid(c("sen"), c(70:112), "kh")
allKH <- gsub(" ", "", apply(combinationKH, 1, paste0, collapse = ""))
head(allKH)

#################
# Install/Load packages #
#################
toInstall <- c("plyr", "reshape", "foreign", "wnominate", "R.utils", "foreach", "parallel", "doMC")
if(installPackages){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

####################
# Define functions #
####################
#checkMember():  Takes vote matrix and returns total number of retained            
#                votes for each member.  Used as wnominate() helper.               
checkMember <- function(votes) {
 apply(votes!=9,1,sum)
  }

#checkVote():   Takes vote matrix and returns total number of retained votes      
#               for each member.  Used as wnominate() helper.                     
checkVote <- function(votes) {
  minorityPercent <- pmin(apply(votes==1,2,sum),apply(votes==6,2,sum))
  minorityPercent <- minorityPercent/apply(votes!=9,2,sum)
  minorityPercent[is.nan(minorityPercent)]<-0    
  return(minorityPercent)
  }

#standardRollCallConversion():   Takes a roll call matrix and recodes such that all observations are
#                                missing=9, yah=1, nay=6                                 
standardRollCallConversion <- function(rcMatrix){
  newRCObject <- rollcall(data = rcMatrix,
                          yea=c(1,2,3), nay=c(4,5,6),
                          missing=c(7,8,9), notInLegis=c(0))
  newRCObject$votes[newRCObject$votes%in%c(0,7,8)] <- 9
  newRCObject$votes[newRCObject$votes%in%c(4,5)] <- 6
  newRCObject$votes[newRCObject$votes%in%c(2,3)] <- 1
  return(newRCObject)
}

# reduceRollCallMatrix(): Check legislators and members for minimum requirements (and remove those who don't meet the standard)
#                         until there is no further change.
#                         Requires that all missing values be coded 9, yay=1, nay=6
 reduceRollCallMatrix <- function(rcObject, .lop=lop){
  memberVotes <- checkMember(rcObject$votes)
  minorityVoteShare <- checkVote(rcObject$votes)
  tempvotes<-rcObject$votes
  oldM <- rcObject$m
  oldN <- rcObject$n
  done <- FALSE
  while(!done){
      memberVotes <- checkMember(tempvotes)
      minorityVoteShare <- checkVote(tempvotes)
      tempvotes <- tempvotes[memberVotes>minvotes,minorityVoteShare>.lop]
      newM <- ncol(tempvotes); newN <- nrow(tempvotes)
      if(newM==oldM & newN==oldN){done <- TRUE}
      oldM <- newM; oldN <- newN

    }
  rcObject$votes <- tempvotes
  rcObject$m <- newM
  rcObject$n <- newN
  return(rcObject)
  }


#summarizeReslts(): Takes the results of a wnominate analysis and calculates a number of fit statistics.
summarizeResults <- function(wNom){
  effectiveNumberOfEigens <- sum(wNom$eigenvalues) ^ 2 / sum(wNom$eigenvalues ^ 2)
  names(effectiveNumberOfEigens) <- "eNEigenvalues"
  first10Eigens <- wNom$eigenvalues[1:10]
    names(first10Eigens) <- paste0("Eigen", 1:10)
  eigenSum <- sum(wNom$eigenvalues)
  nDimensionsToUse <- nDim
  nDimensionsToUse[nDimensionsToUse > 10] <- 10  # Artifical capping at 10  ###FLAG###
  correctClass <- rep(NA, 10)
  names(correctClass) <- paste("correctClassD", c(1:10), sep="")
  correctClass[1:nDimensionsToUse] <- wNom$fits[1:nDimensionsToUse]
  apre <- rep(NA, 10)
  names(apre) <- paste("apreD", c(1:10), sep="")
  apre[1:nDimensionsToUse] <- wNom$fits[(nDimensionsToUse+1):(nDimensionsToUse*2)]
  gmp <- rep(NA, 10)
  names(gmp) <- paste("gmp", c(1:10), sep="")
  gmp[1:nDimensionsToUse] <- wNom$fits[(nDimensionsToUse*2+1):(nDimensionsToUse*3)]
  
  nLegislators <- sum(!is.na(wNom$legislators$coord1D))
  nRollCalls <- sum(!is.na(wNom$rollcalls$midpoint1D))
    
  output <- data.frame(effectiveNumberOfEigens,
                       t(first10Eigens),
                       eigenSum,
                       t(correctClass),
                       t(apre),
                       t(gmp),
                       nLegislators,
                       nRollCalls)
  return(output)
  }

#nominateCalculator(): Takes in a roll call matrix and performs the following steps:
# (1)  Run/summarize an analysis of the entire roll call record
# (2) Run/summarize an analysis of Democrats
# (3) Run/summarize an analysis of Republicans
# (4) Run/summarize an analysis of the entire body using only roll calls included in (2) 
# (5) Run/summarize an analysis of the entire body using only roll calls included in (3)
# (6) Run/summarize an analysis of the entire body using only roll calls included in *both* (2) and (3)
# (7) Return a dataframe with these combined results
nominateCalculator <- function(kh){
  print(kh)
  eachKH <- kh
  theFTP <- paste0("./PnR_KH_files/", eachKH, ".ord")
  importedData <- try(readKH(theFTP)) # Reads in the data using the readKH() function
  whichCong <- as.numeric(gsub("kh", "", substr(kh, 4, 1000))) #Identify the congress number
  importedData <- data.frame(cong = whichCong,
                             importedData$legis.data, importedData$votes)
  colnames(importedData) <- gsub("var", "V", colnames(importedData)) 
  columnsToInclude <- colnames(importedData)[substr(colnames(importedData), 1, 1) == "V"] # Look 
  demInclude <- importedData[, "party"] == "D" # Make a variable indicating Democrats
  repInclude <- importedData[, "party"] == "R" # Make a variable indicating Republicans
  
  allRollCall <- standardRollCallConversion(importedData[, columnsToInclude]) # Recode the roll call matrix (see above)
  allRollCall <- reduceRollCallMatrix(allRollCall) #Reduce the roll call matrix (see above)
  
  
  allPole  <- 1 ## Use the first legislator to set the polarity
  ## Run the nominate procedure  
    allNOMINATE <- try(wnominate(allRollCall, 
                                 dims = nDim, polarity = rep(allPole, nDim), minvotes = minvotes, lop = lop))
    if(class(allNOMINATE) == "try-error"){
      allNOMSummary <- NA
    } else {
      allNOMSummary <- summarizeResults(allNOMINATE) # Summarize the results
    }

  # Repeat this process for the different subsets as described above
  repRollCall <- reduceRollCallMatrix(standardRollCallConversion(importedData[repInclude, columnsToInclude]), .lop=lop*2)
  demRollCall <- reduceRollCallMatrix(standardRollCallConversion(importedData[demInclude, columnsToInclude]), .lop=lop*2)
  repVoteSubset <- importedData[, gsub(" ", ".", colnames(repRollCall$votes))]
  repVotesRollCall <- reduceRollCallMatrix(standardRollCallConversion(repVoteSubset))
  demVoteSubset <- importedData[, gsub(" ", ".", colnames(demRollCall$votes))]
  demVotesRollCall <- reduceRollCallMatrix(standardRollCallConversion(demVoteSubset))
  
  bothPartiesThresholdMeeting <- intersect(colnames(repVotesRollCall$votes),
                                           colnames(demVotesRollCall$votes))
  allIntersectionSubset <- importedData[, gsub(" ", ".", bothPartiesThresholdMeeting)]
  allIntersectionRollCall <- reduceRollCallMatrix(standardRollCallConversion(allIntersectionSubset))
  
  repNOMINATE <- try(wnominate(repRollCall,
                               dims = nDim, polarity = rep(allPole, nDim), minvotes = minvotes, lop = 2*lop))
  demNOMINATE <- try(wnominate(demRollCall,
                               dims = nDim, polarity = rep(allPole, nDim), minvotes = minvotes, lop = 2*lop))
  repVotesNOMINATE <- try(wnominate(repVotesRollCall,
                                    dims = nDim, polarity = rep(allPole, nDim), minvotes = minvotes, lop = lop))
  demVotesNOMINATE <- try(wnominate(demVotesRollCall,
                                    dims = nDim, polarity = rep(allPole, nDim), minvotes = minvotes, lop = lop))
  allIntersectionNOMINATE <- try(wnominate(allIntersectionRollCall,
                                           dims = nDim, polarity = rep(allPole, nDim), minvotes = minvotes, lop = lop))
  
  if(class(demNOMINATE) == "try-error"){demNOMSummary <- NA} else {
    demNOMSummary <- summarizeResults(demNOMINATE)  }
  if(class(repNOMINATE) == "try-error"){repNOMSummary <- NA} else {
    repNOMSummary <- summarizeResults(repNOMINATE)  }
  if(class(demVotesNOMINATE) == "try-error"){demVotesNOMSummary <- NA} else {
    demVotesNOMSummary <- summarizeResults(demVotesNOMINATE)  }
  if(class(repVotesNOMINATE) == "try-error"){repVotesNOMSummary <- NA} else {
    repVotesNOMSummary <- summarizeResults(repVotesNOMINATE)  }
  if(class(allIntersectionNOMINATE) == "try-error"){allIntersectionNOMSummary <- NA} else {
    allIntersectionNOMSummary <- summarizeResults(allIntersectionNOMINATE)  }
  
  outputData <- data.frame(rbind(allNOMSummary, repNOMSummary,
                                 demNOMSummary, repVotesNOMSummary, demVotesNOMSummary,
                                 allIntersectionNOMSummary))
  outputData$whichSubset <- c("All", "Rep Only", "Dem Only", "Rep Votes", "Dem Votes", "Votes dividing both parties")
  outputData$cong = importedData$cong[1]
  outputData$chamber <- substr(eachKH, 1, 3)
  return(outputData)
}



####################
### metaFunction ###
# This function combines all of the steps of the analysis.
# It takes a row number, pulls in the right data frame, runs the analysis, and writes out a .csv file wit the results
metaFunction <- function(whichKH){
  rowParameters <- unlist(allKH[whichKH])
  print(rowParameters)
  simulationSummary <- nominateCalculator(kh = rowParameters)
  write.csv(simulationSummary, paste("./DataFiles/Subresults/", allKH[whichKH], ".csv", sep = ""))
  return(simulationSummary)
}

###########################
# Run the analysis   ######
###########################
to.export <- ls()
registerDoMC(cores=10) # Runs the analysis in parallel
rowsToSweep <- 1:length(allKH)
simulationResults <- foreach(i=rowsToSweep,
                             .errorhandling="pass",
                             .export=to.export,
                             .verbose=FALSE) %dopar%
                          metaFunction(i)




