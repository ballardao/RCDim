### Setup
rm(list = ls())

# Load packages
loadLib <- function(toLoad){
    for(lib in toLoad){
        if(! lib %in% installed.packages()[,1])
        { install.packages(lib, repos='http://cran.rstudio.com/') }
        suppressMessages( library(lib, character.only=TRUE) )
    }
}
toLoad <- c(
    'mvtnorm', 'pscl', 'wnominate', 'arm', 'MASS', 'akima', 'plyr', 'fields',
    'foreach', 'parallel', 'tidyverse', 'magrittr', 'hitandrun', 'Rcpp', 'reticulate'
)
loadLib(toLoad)

# Dir
if(Sys.info()['user']=='aob5' | Sys.info()['user']=='Andy'){
    pth <- 'C:/Users/Andy/RCDim/'
}


### Define parameters
dims <- 10
mus <- rep(0, dims)
D <- 3
sig <- diag(dims)
rcN <- 30000

### Generate ideal points
maj <- mvrnorm(51, mus+(D/2), sig)
min <- mvrnorm(50, mus-(D/2), sig)
mcs <- rbind(maj, min)

### Generate roll calls
#sqs <- hypersphere.sample(dims, rcN)
#props <- hypersphere.sample(dims, rcN)

nBallDraw <- function(nDim, nDraws=10000, .radius=5){
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
sqs <- nBallDraw(nDim = dims, nDraws = rcN)
props <- nBallDraw(nDim = dims, nDraws = rcN)


### Calculate probability of voting Yea on each roll call for each member

# Source C++ functions to generate roll call matrices
sourceCpp(file = paste0(pth, 'rcDF.cpp'))


### Generate test RC matrices
rcsMaj <- rcFn(props=props, sqs=sqs, mcs=maj)
rcsMin <- rcFn(props=props, sqs=sqs, mcs=min)
rcs <- rbind(rcsMaj, rcsMin)