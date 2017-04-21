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
data_repository <- "C:/Users/reubenbauer/Dropbox (Team Steve)/"
# data_repository <- "C:/Users/reuben_bauer/Box Sync/Box Sync/cpd_stops/data/"

#---------------------------- Section 2: Load Data -----------------------------
crime <- fread(paste0(data_repository,"crimes_2001_present/Crimes_-_2001_to_present.csv"), header = TRUE, stringsAsFactors = FALSE)
stops <- read.csv(paste0(data_repository, "FOIA PO55346/FOIA PO55346 Response.csv"), header = TRUE, stringsAsFactors = FALSE)
cc12 <- read.csv(paste0(data_repository, "FOIA PO59333/2012CONTACTCARDS.csv"), header = TRUE, stringsAsFactors = FALSE)
cc13 <- read.csv(paste0(data_repository, "FOIA PO59333/2013CONTACTCARDS.csv"), header = TRUE, stringsAsFactors = FALSE)
cc14 <- read.csv(paste0(data_repository, "FOIA PO58150/ContactCardData2014.csv"), header = TRUE, stringsAsFactors = FALSE)
cc15 <- read.csv(paste0(data_repository, "FOIA PO58150/ContactCardData2015.csv"), header = TRUE, stringsAsFactors = FALSE)
