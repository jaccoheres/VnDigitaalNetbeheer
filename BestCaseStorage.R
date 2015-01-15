# BestCaseStorage.R
# This script calculates the best-case benefit of storage by calculating the peak load of
# the average of the yearly use profile.
#
# This file is a slightly modified version of run.R. It should complete much faster. However, it
# is still a lazy's man implementation, because not all computations of run.R are necessary. It
# does work tough.
#
# By Werner van Westering MSc.
# Start 14-01-2014

gc(verbose=FALSE)
drive = substr(getwd(),1,3)
path = paste0(drive,"1. Programmeerwerk/Bottum Up Analyse/2. Data")
setwd(paste0(path,"/7. Output"))

print("--Loading packages--")
# Load packages
library(reshape2)
library(plyr)
library(data.table)
library(slam)       #Used for sparse matrices
library(tictoc)     #Because I am a Matlab person
# library(xlsx)
library(ggplot2)
library(doSNOW)
library(utils)

#Load data (To generate this data: run DataPreparation.R)
print("--Loading data--")
load("Connections_NH_v2.RData")

#Trim down the size of the scenario arrays to only contain the average
baseloadperHLD  = matrix(apply(baseloadperHLD,1,mean),dim(baseloadperHLD)[1],10) #The second dimension should be >1 or run.R will somehow crash. ArrRRR!
baseloadperMSR  = matrix(apply(baseloadperMSR,1,mean),dim(baseloadperMSR)[1],10)
baseloadperOSLD = matrix(apply(baseloadperOSLD,1,mean),dim(baseloadperOSLD)[1],10)

nCPUs = 6

#Generate results
# source('run.R')

save.image("BestCaseStorageData.RData")