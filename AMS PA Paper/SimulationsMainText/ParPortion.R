rm(list = ls())
setwd("~/Dropbox/Dimensionality")

###############
# Set Options #
###############

installPackages <- FALSE
nCores <-    15
fileStub <- "AggregateOutput3"
set.seed(123456)

##############################
# Set of Parameters to Sweep #
##############################

# Input desired parameter settings here:
parameterSettings <- list(
                         nDimensions = c(1, 2, 3, 4,5, 6, 7,8,9,10,15,20), 
                          nDraws = 5000,  # How many roll calls will be generated before reduced down for analysis to a more standard number 
                          typicalSenateVote =525, #This is the typical number of votes in the senate for the past XX years
                          nObservations = c(101), # Number of senators
                          normalMean = 0, # This really should not be varied
                          normalVariance = 1, # Neither should this
                          nSimulations = 1,  # We can run several iterations of each setting.
                          nSeparateDimensions =  c(1,2,3,4), ## 999 means separation on all dimensions
                          partySeparation = c(0, 0.5, 1, 1.5,2, 2.5,3,3.5,4,4.5, 5,5.5,6,6.5,7),
                          majorityPartySize = 51,
                          scalePartySubsets = c(TRUE),  # This could also be false, in which case the script returns only the chamber-level NOMINATE.
                          beta = c(.5, 1, 1.5),
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
parametersToSweep <- parametersToSweep[!(parametersToSweep$partySeparation == 0 & parametersToSweep$nSeparateDimension > 1), ]

# Review the final set of parameters:
rownames(parametersToSweep) <- 1:nrow(parametersToSweep)
apply(parametersToSweep, 2, table)

####################
# Read in function #
####################
## You may need to change some the directories/file names specified here
source("./PAFinal/SimulationsMainText/SimulationCodeMain.R")

## Optionally, you can read in the results from some previous run
#results <- read.csv("./PAFinal/SimulationsMainText/SimulationResults/AggregateOutput.csv")
#results2 <- read.csv("./PAFinal/SimulationsMainText/SimulationResults/AggregateOutput2.csv")
#results <- rbind(results, results2)

##################
# foreach run #
##################

to.export <- c("parametersToSweep", "nBallDraw", "dwNominateFunction",
                           "generateProposalsAndStatusQuos", 
                           "generateRollCalls","metaFunction", "reduceRollCallMatrix",
                           "partyUnityCalculator", "preferenceGenerator", "summarizeSimulation",
                           "rmvnorm", "rdist", "rollcall", "wnominate", "partySubsetScaler", "fileStub")
registerDoMC(cores=nCores)
metaFunction(1) # This needs to be run the first time to create the file that the results will be appended into

## Below line runs the code in parallel.  Note, that this is designed to identify parameter rows not already analyzed.
## Be aware that some parameter rows may not run correctly in a given sweep.  This may need to be run a couple of times (changing the file stub name)


## This line would be used if the "results" object above is created.
#errors <- foreach(i=(1:nrow(parametersToSweep))[(!1:nrow(parametersToSweep)%in%results$parameterRow)],
errors <- foreach(i=(2:nrow(parametersToSweep)),
                  .errorhandling="pass",
                  .export=to.export,
                  .verbose=FALSE) %dopar%
metaFunction(i)

## NOTE: This should probably be done using some kind of SQL database.  If someone wants to tell us how to do that, please do.
## This is a bit hacky, but it works.
