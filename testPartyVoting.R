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


### Source rc generation for testing
source(paste0(pth, 'genRCs.R'))


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
