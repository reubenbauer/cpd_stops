#-------------------------------------------------------------------------------
# File Name:    load.R
# Author(s):    Reuben Bauer
# Written:      12/13/2016
# 
# Inputs:       Crimes_-_2001_to_present type(csv)
#
# Outputs:      data type(dataframe)
#
# Description: 
# 
# 	1. Load the criminal records data from the CPD into R.
	#
#-------------------------------------------------------------------------------

#---------------------------- Section 1: Preliminaries -------------------------

#-------- set repository ----
data_repository <- "C:/Users/reubenbauer/Box Sync/cpd_stops/data/"
data_repository <- "C:/Users/reuben_bauer/Box Sync/Box Sync/cpd_stops/data/"

#---------------------------- Section 2: Load Data -----------------------------

data = fread(paste0(data_repository, 
	"Crimes_-_2001_to_present.csv"), header = TRUE, stringsAsFactors = FALSE)
