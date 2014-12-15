rm(list = ls())
setwd("~/Dropbox/Dimensionality")


###############
# Set Options #
###############

installPackages <- FALSE
nCores <-    20
fileStub <- "MatchingOutput6"
set.seed(123456)

##############################
# Set of Parameters to Sweep #
##############################
parameterSettings <- list(
                         nDimensions = c(1, 2, 3, 4,5, 6, 7,8,9,10,15,20), 
                          nDraws = 20000,  # How many roll calls will be generated before reduced down for analysis to a more standard number                           typicalSenateVote =525, #This is the typical number of votes in the senate for the past XX years
                          nObservations = c(101),
                          normalMean = 0, # This really should not be varied
                          normalVariance = 1, # Neither should this
                          nSimulations = 1,  # We can run several iterations of each setting.
                          nSeparateDimensions =  c(1,2,3,4,999), ## 999 means separation on all dimensions
                          partySeparation = c(0, 0.5, 1, 1.5,2, 2.5,3,3.5,4,4.5, 5,5.5,6),
                          majorityPartySize = 51,
                          scalePartySubsets = c(TRUE),  # This could also be false, in which case the script returns only the chamber-level NOMINATE.
                          beta = c(.5, 1, 1.5),
                          radius=c(9), # This is actually not used in this variant
                          lop=c(.025)
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
parametersToSweep <- parametersToSweep[!(parametersToSweep$partySeparation == 0 & parametersToSweep$nSeparateDimension > 1), ]

# Review the final set of parameters:
rownames(parametersToSweep) <- 1:nrow(parametersToSweep)
apply(parametersToSweep, 2, table)

####################
# Read in function #
####################

source("./PAFinal/SimulationsMatching2/SimulationCodeMatching2.R")
results1 <- read.csv("~/Dropbox/Dimensionality/PAFinal/SimulationsMatching2/SimulationResults/MatchingOutput1.csv")
results2 <- read.csv("~/Dropbox/Dimensionality/PAFinal/SimulationsMatching2/SimulationResults/MatchingOutput2.csv")
results3 <- read.csv("~/Dropbox/Dimensionality/PAFinal/SimulationsMatching2/SimulationResults/MatchingOutput3.csv")
results4 <- read.csv("~/Dropbox/Dimensionality/PAFinal/SimulationsMatching2/SimulationResults/MatchingOutput4.csv")
results5 <- read.csv("~/Dropbox/Dimensionality/PAFinal/SimulationsMatching2/SimulationResults/MatchingOutput5.csv")
results <- rbind(results1, results2, results3, results4)

table(results$maxDupes)
table(results$numDupes)

dim(results)
results <- results[c(results$maxDupes<=4 | results$numDupes<=15),]
dim(results)

colnames(results3)==colnames(results4)
(1:nrow(parametersToSweep))[(!1:nrow(parametersToSweep)%in%results$parameterRow)]

##################
# foreach run #
##################

to.export <- c("parametersToSweep", "nBallDraw", "dwNominateFunction",
                           "generateProposalsAndStatusQuos", 
                           "generateRollCalls","metaFunction", "reduceRollCallMatrix",
                           "partyUnityCalculator", "preferenceGenerator", "summarizeSimulation",
                           "rmvnorm", "rdist", "rollcall", "wnominate", "partySubsetScaler", "fileStub", "empiricalVoteMargins")
registerDoMC(cores=nCores)
metaFunction(1)
errors <- foreach(i=(1:nrow(parametersToSweep))[(!1:nrow(parametersToSweep)%in%results$parameterRow)],
# errors <- foreach(i=2:nrow(parametersToSweep),
         .errorhandling="pass",
         .export=to.export,
         .verbose=FALSE) %dopar%
     metaFunction(i)





