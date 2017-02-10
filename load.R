#-------------------------------------------------------------------------------
# File Name:    load.R
# Author(s):    Reuben Bauer
# Written:      12/13/2016
# 
# Inputs:       Crimes_-_2001_to_present type.csv
#               FOIA PO55346 Response.csv
#
# Outputs:      crime data type(dataframe)
#               stops type(dataframe)
#
# Description: 
# 
# 	1. Load the criminal records data
#   2. Load in the police data
#
#-------------------------------------------------------------------------------

#---------------------------- Section 1: Preliminaries -------------------------

#-------- set repository ----
# data_repository <- "C:/Users/reubenbauer/Box Sync/cpd_stops/data/"
# data_repository <- "C:/Users/reuben_bauer/Box Sync/Box Sync/cpd_stops/data/"

#---------------------------- Section 2: Load Data -----------------------------
crime <- fread("C:/Users/reubenbauer/Box Sync/cpd_stops/data/Crimes_-_2001_to_present.csv", header = TRUE, stringsAsFactors = FALSE)
stops <- read.csv("FOIA PO55346 Response.csv", header = TRUE, stringsAsFactors = FALSE)
