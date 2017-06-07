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
#data_repository <- "C:/Users/reubenbauer/Box Sync/personal/cpd_stops/data/"
data_repository <- "C:/Users/reuben_bauer/Box Sync/personal/cpd_stops/data/"

#---------------------------- Section 2: Load Data -----------------------------
crime <- fread(paste0(data_repository, "Crimes_-_2001_to_present.csv"),       header = TRUE, stringsAsFactors = FALSE)
stops <- fread(paste0(data_repository, "FOIA PO55346 Response.csv"),          header = TRUE, stringsAsFactors = FALSE)
cc10  <- fread(paste0(data_repository, "2010CONTACTCARDS.csv"),               header = TRUE, stringsAsFactors = FALSE)
cc11  <- fread(paste0(data_repository, "2011CONTACTCARDS.csv"),               header = TRUE, stringsAsFactors = FALSE)
cc12  <- fread(paste0(data_repository, "2012CONTACTCARDS.csv"),               header = TRUE, stringsAsFactors = FALSE)
cc13  <- fread(paste0(data_repository, "2013CONTACTCARDS.csv"),               header = TRUE, stringsAsFactors = FALSE)
cc14  <- fread(paste0(data_repository, "ContactCardData2014.csv"),            header = TRUE, stringsAsFactors = FALSE)
cc15  <- fread(paste0(data_repository, "ContactCardData2015.csv"),            header = TRUE, stringsAsFactors = FALSE)
