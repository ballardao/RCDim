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
dims <- 2
mus <- rep(0, dims)
D <- 3
sig <- diag(dims)
rcN <- 10000

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
sqs <- nBallDraw(nDim = dims)
props <- nBallDraw(nDim = dims)


### Calculate probability of voting Yea on each roll call for each member

# C++ function to compute distance between 
sourceCpp(file = paste0(pth, 'rcDF.cpp'))
#voteFn <- function(party, majority_status = 'majority'){
#    sqD <- apply(party, 1, function(mc){
#        sqs <- apply(sqs, 1, function(sq){
#            abs(mc - sq)^2 %>% sum()
#        })
#        sqs
#    })
#        
#        propD <- apply(props, 1, function(prop){
#            abs(mc - prop)^2 %>% sum()
#        })
#        
#        diff <- propD - sqD 
#        votes <- pnorm(diff) %>%
#            round()
#        votes
#    }) %>% t() %>% as.data.frame()
#    df$status <- majority_status
#    return(df)
#}
rcsMaj <- voteFn(props, sqs, maj)
rcsMin <- voteFn(party = min, majority_status = 'minority')
rcs <- rbind(rcsMaj, rcsMin)


### Take out near-unanimous votes 
names <- names(rcs)[1:(ncol(rcs)-1)]
majNum <- sapply(names, function(name){
    colname <- names[match(name, names)]
    majNum <- table(rcs[,colname]) %>% max()
    return(majNum)
}) %>%
    `>`(95) %>%
    which()
rcs <- rcs[,-majNum]


### Pull out majority status
majStat <- rcs$status
rcs <- dplyr::select(rcs, -status)


### Calculate proportion of bills where each member voted with majority of their party
Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

props <- apply(rcs, 1, function(rc){
    mc <- as.numeric(rc)
    voteM <- as.numeric(rc) %>%
        Mode() %>% 
        unname() %>%
        `-`(mc)
    prop <- length(which(voteM == 0)) / length(voteM)
})

### Calculate distance from the party mean
distsMaj <- apply(maj, 1, function(mc){
    origin <- mus+(D/2)
    dist <- rbind(origin, mc) %>%
        dist() %>%
        as.numeric()
    dist
})
distsMin <- apply(min, 1, function(mc){
    origin <- mus-(D/2)
    dist <- rbind(origin, mc) %>%
        dist() %>%
        as.numeric()
    dist
})
dists <- c(distsMaj, distsMin)


### Evaluate
coefs <- summary(lm(dists ~ props))$coefficients
cor(dists, props)

### Plot
correlation <- cor(dists, props) %>%
    round(2) %>%
    paste0('r = ', .)
coef <- coefs %>%
    .[2,1] %>%
    round(2) %>%
    paste0('B = ', .)
SE <- coefs %>%
    .[2,2] %>% 
    round(2) %>%
    paste0('SE =  ', .) 
plot(props, dists, pch = 19, xlab = 'Proportion of Rolls Voting with Party Majority', ylab = 'Euclidean Distance from Party Mean Ideal Point')
abline(lm(dists ~ props))
if(coefs[2,4] < 0.01){
    title(main = paste0(correlation, ', ', coef, '***, ', SE))
} else if(coefs[2,4] < 0.05){
    title(main = paste0(correlation, ', ', coef, '**, ', SE))
} else if(coefs[2,4] < 0.1){
    title(main = paste0(correlation, ', ', coef, '*, ', SE))
} else {
    title(main = paste0(correlation, ', ', coef, ', ', SE))
}
