### R code from vignette source 'baySeq.Rnw'

BiocStyle::latex()
set.seed(102)
options(width = 90)
library(baySeq)
#cl <- NULL

############################################################################################ COUNTA
data(simData)

dyn.load(paste("RPluMA", .Platform$dynlib.ext, sep=""))
source("RPluMA.R")

input <- function(inputfile) {
  parameters <<- read.table(inputfile, as.is=T);
  rownames(parameters) <<- parameters[,1];
    pfix = prefix()
  if (length(pfix) != 0) {
     pfix <<- paste(pfix, "/", sep="")
  }
}

run <- function() {}

output <- function(outputfile) {
simData <- as.matrix(read.table(paste(pfix, parameters["csvfile", 2], sep="/"), sep=","))
replicates <- readLines(paste(pfix, parameters["replicate", 2], sep="/"))
myNDE <- as.integer(readLines(paste(pfix, parameters["group1", 2], sep="/")))
myDE <- as.integer(readLines(paste(pfix, parameters["group2", 2], sep="/")))
simData[1:10,]
groups <- list(NDE = myNDE, DE = myDE)
CD <- new("countData", data = simData, replicates = replicates, groups = groups)
saveRDS(CD, outputfile)
}


#############################################################################################

