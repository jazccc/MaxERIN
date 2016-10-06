# Config

setwd("C:\\Users\\MSmith\\Desktop\\JourneyGuide\\R Model\\Deliver09222016JX01")

library("xlsx") # Need two R packages ("xlsx" and "rJava") and Java

library("matrixStats")

# ReadIn

# ++++ Input the expected returns of different assets ++++++++++++++++++++++++

Asset1               <-          read.csv('Cash.csv', header = FALSE)

Asset2               <-          read.csv('Muni.csv', header = FALSE)

Asset3               <-          read.csv('USAggBonds.csv', header = FALSE)

Asset4               <-          read.csv('HYBonds.csv', header = FALSE)

Asset5               <-          read.csv('USLrgCap.csv', header = FALSE)

Asset6               <-          read.csv('USMidCap.csv', header = FALSE)

Asset7               <-          read.csv('USSmCap.csv', header = FALSE)

Asset8               <-          read.csv('EAFEEq.csv', header = FALSE)

Asset9               <-          read.csv('REIT.csv', header = FALSE)

Asset10              <-          read.csv('HF.csv', header = FALSE)


source("MaxERIN_05.R")

