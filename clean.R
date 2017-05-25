#-------------------------------------------------------------------------------
# File Name:    clean.R
# Author(s):    Reuben Bauer
# Written:      12/13/2016
# 
# Inputs:       data type(dataframe)
#
# Outputs:      narcotics_count type(dataframe)
#               narcotics_count_quarterly type(dataframe)
#				        narc_sub_count type(dataframe)
#				        narc_sub_count_qrtly type(dataframe)
#               stops type(dataframe)
#               stops_quarterly type(dataframe)
#
# Description: 
# 
# 1. rename poorly styled variables
#	2. restrict dataset to narcotics
#	3. convert to dates
#	4. combine small categories
#	5. generate narcotics_count dataframe
#
# 	To Do:
#	1. combine cannabis categories into a single category
# 2. rename stops to stops monthly
#
#-------------------------------------------------------------------------------

#---------------------------- Section 1: Preliminaries -------------------------

#---------------------------- Section 2: Clean Data ----------------------------

#-------- clean stops -------
cc10 <- select(cc10, -V29)
names(cc10) <- names(cc12)
names(cc11) <- names(cc12)
names(cc14) <- names(cc12)
names(cc15) <- names(cc12)
cc <- do.call("rbind", list(cc10, cc11, cc13, cc14, cc14)) # manually deal with two weird observations in cc12
# rm(cc12, cc13, cc14, cc15)
cc$CONTACTDATE <- as.Date(cc$CONTACTDATE, "%d-%b-%y")

#-------- clean narcotics ---
names(crime)[names(crime) == "Primary Type"]<-"crime_type"
narcotics <- filter(crime, crime_type == "NARCOTICS")
narcotics$Date <- as.Date(narcotics$Date, "%m/%d/%Y %H:%M:%S")
narcotics$Month <- as.yearmon(narcotics$Date)
narcotics <- filter(narcotics, Month != "Aug 2016")

#combine small drug categories
narcotics$Description[narcotics$Description == "POSS: HALLUCINOGENS" |
                        narcotics$Description == "POSS: HEROIN(BLACK TAR)" |
                        narcotics$Description == "POSS: LOOK-ALIKE DRUGS" |
                        narcotics$Description == "POSS: METHAMPHETAMINES" |
                        narcotics$Description == "POS: HYPODERMIC NEEDLE" | 
                        narcotics$Description == "POSS: AMPHETAMINES" |
                        narcotics$Description == "POSS: SYNTHETIC DRUGS" |
                        narcotics$Description == "POSS: PCP" |
                        narcotics$Description == "POSS: BARBITUATES" | 
                        narcotics$Description == "POSS: CANNABIS MORE THAN 30GMS" | 
                        narcotics$Description == "POSS: HEROIN(BRN/TAN)"
                      ]<-"POSS: OTHER"

#-------- gen narcotics count monthly -
narcotics_count <- count(narcotics, c("crime_type", "Month"))

# ------- gen narcotics count quarterly - 
narcotics_count$quarter <- 
  as.yearqtr(narcotics_count$Month, format = "%Y-%m-%d")
narcotics_count_quarterly <- ddply(narcotics_count, 
                              .(crime_type, quarter), summarize, Sum=sum(freq))
narcotics_count_quarterly <- filter(narcotics_count_quarterly, quarter != "2016 Q3")

#-------- gen narcotics sub count monthly -
narc_sub_count <- count(narcotics, c("crime_type", "Description", "Month"))
narc_sub_count <- filter(narc_sub_count, 
                         Description != "CALCULATED CANNABIS CONSPIRACY" & 
                           Description != "CANNABIS PLANT" & 
                           Description != "CONT SUBS:FAIL TO MAINT RECORD" &
                           Description != "CRIMINAL DRUG CONSPIRACY" &
                           Description != "DEL CONT SUBS TO PERSON <18" &
                           Description != "DELIVER CANNABIS TO PERSON <18" &
                           Description != "FAIL REGISTER LIC:CONT SUBS" &
                           Description != "FAILURE TO KEEP HYPO RECORDS" &
                           Description != "MANU/DELIVER: HALLUCINOGEN" &
                           Description != "MANU/DELIVER: HEROIN(BRN/TAN)" &
                           Description != "MANU/DELIVER: METHAMPHETAMINES" &
                           Description != "MANU/DELIVER:AMPHETAMINES" &
                           Description != "MANU/DELIVER:BARBITUATES" &
                           Description != "MANU/DELIVER:HEROIN(BLACK TAR)" &
                           Description != "MANU/DELIVER:LOOK-ALIKE DRUG" &
                           Description != "MANU/DELIVER:PCP" &
                           Description != "MANU/DELIVER:SYNTHETIC DRUGS" &
                           Description != "MANU/POSS. W/INTENT TO DELIVER: SYNTHETIC MARIJUANA" &
                           Description != "POSSESSION: SYNTHETIC MARIJUANA" &
                           Description != "SALE/DEL DRUG PARAPHERNALIA" &
                           Description != "SALE/DEL HYPODERMIC NEEDLE" &
                           Description != "ALTER/FORGE PRESCRIPTION" &
                           Description != "ATTEMPT POSSESSION CANNABIS" & 
                           Description != "ATTEMPT POSSESSION NARCOTICS" & 
                           Description != "FORFEIT PROPERTY" & 
                           Description != "FOUND SUSPECT NARCOTICS" & 
                           Description != "MANU/DEL:CANNABIS 10GM OR LESS" & 
                           Description != "MANU/DEL:CANNABIS OVER 10 GMS" & 
                           Description != "MANU/DELIVER: HEROIN (WHITE)" & 
                           Description != "MANU/DELIVER:COCAINE" & 
                           Description != "MANU/DELIVER:CRACK" & 
                           Description != "POSSESSION OF DRUG EQUIPMENT" &
                           #Description != "POSS: CANNABIS 30GMS OR LESS" & 
                           #Description != "POSS: COCAINE" & 
                           #Description != "POSS: CRACK" & 
                           #Description != "POSS: HEROIN(BRN/TAN)" & 
                           #Description != "POSS: HEROIN(WHITE)" & 
                           #Description != "POSS: Other" & 
                           Description != "SOLICIT NARCOTICS ON PUBLICWAY")

#-------- gen narcotics sub count quarterly -
narc_sub_count$quarter <- 
  as.yearqtr(narc_sub_count$Month, format = "%Y-%m-%d")
narc_sub_count_qrtly <- ddply(narc_sub_count, 
                              .(crime_type, Description, quarter), summarize, Sum=sum(freq))
narc_sub_count_qrtly <- filter(narc_sub_count_qrtly, quarter != "2016 Q3")

# ------- gen stops count monthly -
stops <- reshape(stops, varying = c("2003", "2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016"), v.names = "n_stops", times = c("2003", "2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016"), timevar = "year",  direction = "long")
stops <- filter(stops, Month != "Total")
stops$year_mon = paste0(stops$year, "-", stops$id)
stops$year_mon = as.yearmon(stops$year_mon)
stops <- filter(stops, year_mon >= as.yearmon("2004-01") & year_mon <= as.yearmon("2016-07"))
stops <- select(stops, n_stops, year_mon)
names(stops)[names(stops) == "year_mon"]<-"Month"

# ------ gen stops count quarterly -
stops$quarter <- 
  as.yearqtr(stops$Month, format = "%Y-%m-%d")
stops_quarterly <- ddply(stops, 
                              .(quarter), summarize, Sum=sum(n_stops))
stops_quarterly <- filter(stops_quarterly, quarter != "2016 Q3")

