rm(list = ls()) 

setwd("~/Dropbox/Dimensionality/PAFinal/EmpiricalCode/PnR_KH_files/")
library(wnominate)

allSenates <- 60:111
khURLs <- paste(getwd(), "/", "sen", allSenates, "kh.ord", sep = "")
allRCs <- lapply(khURLs, readKH)


# Recoding roll calls to be TRUE/FALSE
allRCs <- lapply(allRCs, function(ll){
  frm <- ll$votes
  frm[is.element(frm, c(0, 7:9))] <- NA
  frm[is.element(frm, 1:3)] <- TRUE
  frm[is.element(frm, 4:6)] <- FALSE
  ll$votes <- frm
  return(ll)
})

# Create an object containing the vote margin for all recorded vote
allMargins <- lapply(allRCs, function(ll){
  colMeans(ll$votes, na.rm = T)
  })

# Calculate the number of votes for each Congress
nVotes <- unlist(lapply(allMargins, length))
mean(nVotes) # This is the where the "Senate typical" 525 votes comes from in the main text


# A vector of margins for all rollcalls in this period
margins <- unlist(allMargins)
margins <- margins[!is.na(margins)]


## Calculate the relevant quantiles
subsetMargins <- margins
subsetMargins <- subsetMargins[(subsetMargins > 0.025) & (subsetMargins < 0.975)]
marginQuantiles <- quantile(subsetMargins, c(1:525)/525)


##
setwd("~/Dropbox/Dimensionality/PAFinal/EmpiricalCode/DataFiles/")
write.csv(marginQuantiles, "Observed Frequency of Vote Margins.csv", row.names = F)

