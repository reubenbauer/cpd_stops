#-------------------------------------------------------------------------------
# File Name:    do.R
# Author(s):    Reuben Bauer
# Written:      12/13/2016
# 
# Inputs:       
#
# Outputs:      
#
# Description: 
# 
#	1. Clear workspace
#	2. Set repository
# 	3. Load libaries
#	4. Source functions
#	5. Run project
#
#-------------------------------------------------------------------------------

#---------------------------- Section 1: Preliminaries -------------------------

#-------- clear workspave ---
rm(list = ls())
gc()

#-------- set repository ----
#repository <- "C:/Users/reubenbauer/Documents/Github/cpd_stops/"
repository <- "C:/Users/reuben_bauer/Documents/Github/cpd_stops/"
setwd(repository)

#-------- load libaries -----
library(reshape2)
library(ggplot2)
library(dplyr)
library(data.table)
library(doBy)
library(zoo)
library(plyr)

#-------- source functions --
source("func.R")

#---------------------------- Section 2: Run Project ---------------------------
source("load.R")
source("clean.R")
source("graph.R")

print("KLAAR")
