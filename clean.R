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
#
#-------------------------------------------------------------------------------

#---------------------------- Section 1: Preliminaries -------------------------

#---------------------------- Section 2: Clean CC ------------------------------
#-------- append cc ---------
cc10 <- select(cc10, -V29)
names(cc10) <- names(cc12)
names(cc11) <- names(cc12)
names(cc14) <- names(cc12)
names(cc15) <- names(cc12)
cc <- do.call("rbind", list(cc10, cc11, cc12, cc13, cc14, cc15))
rm(cc10, cc11, cc12, cc13, cc14, cc15)

#-------- clean cc ----------
cc$Month <- as.yearmon(cc$CONTACTDATE, "%d-%b-%y")
cc <- select(cc, CONTACTTYPE, DISTRICT, Month)

#-------- gen cc by month and quarter -
# gen cc count monthly
cc_count_monthly <- count(cc, c("Month"))
cc_count_monthly$Type <- "CC"

# gen cc count quarterly - 
cc_count_monthly$quarter   <- as.yearqtr(cc_count_monthly$Month, format = "%Y-%m-%d")
cc_count_qrtly <- ddply(cc_count_monthly, .(Type, quarter), summarize, Sum=sum(freq))
cc_count_qrtly <- filter(cc_count_qrtly, quarter != "2016 Q3" & !is.na(quarter))

# gen cc count monthly by district
cc_count_monthly_dist <- count(cc, c("Month", "DISTRICT"))

#---------------------------- Section 3: Clean Stops ---------------------------
# ------- clean stops -------
stops <- reshape(stops, varying = c("2003", "2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016"), v.names = "n_stops", times = c("2003", "2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016"), timevar = "year",  direction = "long")
stops <- filter(stops, Month != "Total")
stops$year_mon = paste0(stops$year, "-", stops$id)
stops$year_mon = as.yearmon(stops$year_mon)
stops <- filter(stops, year_mon >= as.yearmon("2004-01") & year_mon <= as.yearmon("2016-07"))
stops <- select(stops, n_stops, year_mon)
names(stops)[names(stops) == "year_mon"]<-"Month"

#-------- gen stops by month and quarter -
# gen stops count monthly -
stops_monthly <- stops
rm(stops)

# gen stops count quarterly -
stops_monthly$quarter <- as.yearqtr(stops_monthly$Month, format = "%Y-%m-%d")
stops_qrly <- ddply(stops_monthly,  .(quarter), summarize, Sum=sum(n_stops))
stops_qrtly <- filter(stops_qrly, quarter != "2016 Q3")

#---------------------------- Section 4: Clean Narcotics -----------------------
#-------- clean narcotics ---
names(crime)[names(crime) == "Primary Type"]<-"crime_type"
narcotics <- filter(crime, crime_type == "NARCOTICS")
narcotics$Date <- as.Date(narcotics$Date, "%m/%d/%Y %H:%M:%S")
narcotics$Month <- as.yearmon(narcotics$Date)
narcotics <- filter(narcotics, Month != "Aug 2016")

#combine small drug categories
narcotics$Description[narcotics$Description == "ATTEMPT POSSESSION CANNABIS" |
                        narcotics$Description == "CALCULATED CANNABIS CONSPIRACY" |
                        narcotics$Description == "CANNABIS PLANT" |
                        narcotics$Description == "DELIVER CANNABIS TO PERSON <18" |
                        narcotics$Description == "MANU/DEL:CANNABIS 10GM OR LESS" |
                        narcotics$Description == "MANU/DEL:CANNABIS OVER 10 GMS" |
                        narcotics$Description == "POSS: CANNABIS 30GMS OR LESS" |
                        narcotics$Description == "POSS: CANNABIS MORE THAN 30GMS" |
                        narcotics$Description == "POSSESSION: SYNTHETIC MARIJUANA" |
                        narcotics$Description ==  "MANU/POSS. W/INTENT TO DELIVER: SYNTHETIC MARIJUANA"] <- "POSS: CANNABIS"
narcotics$Description[narcotics$Description == "MANU/DELIVER: HEROIN (WHITE)" |
                        narcotics$Description == "MANU/DELIVER: HEROIN(BRN/TAN)" |
                        narcotics$Description == "MANU/DELIVER:HEROIN(BLACK TAR)" |
                        narcotics$Description == "POSS: HEROIN(BLACK TAR)" |
                        narcotics$Description == "POSS: HEROIN(BRN/TAN)" |
                        narcotics$Description == "POSS: HEROIN(WHITE)"] <- "POSS: HEROIN"
narcotics$Description[narcotics$Description == "MANU/DELIVER: HALLUCINOGEN" |
                        narcotics$Description == "POSS: HALLUCINOGENS"] <- "POSS: HALLUCINOGEN"
narcotics$Description[narcotics$Description == "MANU/DELIVER: METHAMPHETAMINES" |
                        narcotics$Description == "POSS: METHAMPHETAMINES"] <- "POSS: METHAMPHETAMINES"
narcotics$Description[narcotics$Description == "MANU/DELIVER:AMPHETAMINES" |
                        narcotics$Description == "POSS: AMPHETAMINES"] <- "POSS: AMPHETAMINES"
narcotics$Description[narcotics$Description == "MANU/DELIVER:BARBITUATES" |
                        narcotics$Description == "POSS: BARBITUATES"] <- "POSS: BARBITUATES"
narcotics$Description[narcotics$Description == "MANU/DELIVER:COCAINE" |
                        narcotics$Description == "POSS: COCAINE"] <- "POSS: COCAINE"
narcotics$Description[narcotics$Description == "MANU/DELIVER:CRACK" |
                        narcotics$Description == "POSS: CRACK"] <- "POSS: CRACK"
narcotics$Description[narcotics$Description == "MANU/DELIVER:PCP" |
                        narcotics$Description == "POSS: PCP"] <- "POSS: PCP"
narcotics$Description[narcotics$Description == "ATTEMPT POSSESSION NARCOTICS" |
                        narcotics$Description == "DEL CONT SUBS TO PERSON <18" |
                        narcotics$Description == "FOUND SUSPECT NARCOTICS" |
                        narcotics$Description == "MANU/DELIVER:LOOK-ALIKE DRUG" |
                        narcotics$Description == "MANU/DELIVER:SYNTHETIC DRUGS" | 
                        narcotics$Description == "POS: HYPODERMIC NEEDLE" | 
                        narcotics$Description == "POSS: AMPHETAMINES" | 
                        narcotics$Description == "POSS: BARBITUATES" | 
                        narcotics$Description == "POSS: HALLUCINOGEN" | 
                        narcotics$Description == "POSS: LOOK-ALIKE DRUGS" | 
                        narcotics$Description == "POSS: METHAMPHETAMINES" | 
                        narcotics$Description == "POSS: PCP" | 
                        narcotics$Description == "POSS: SYNTHETIC DRUGS" | 
                        narcotics$Description == "POSSESSION OF DRUG EQUIPMENT" | 
                        narcotics$Description == "SALE/DEL DRUG PARAPHERNALIA" | 
                        narcotics$Description == "SALE/DEL HYPODERMIC NEEDLE" | 
                        narcotics$Description == "SOLICIT NARCOTICS ON PUBLICWAY"]<-"OTHER: POSS"
narcotics$Description[narcotics$Description == "ALTER/FORGE PRESCRIPTION" |
                        narcotics$Description == "CONT SUBS:FAIL TO MAINT RECORD" |
                        narcotics$Description == "CRIMINAL DRUG CONSPIRACY" | 
                        narcotics$Description == "FAIL REGISTER LIC:CONT SUBS" |
                        narcotics$Description == "FAILURE TO KEEP HYPO RECORDS" |
                        narcotics$Description == "POSSESSION OF DRUG EQUIPMENT" | 
                        narcotics$Description == "SOLICIT NARCOTICS ON PUBLICWAY" |
                        narcotics$Description == "FORFEIT PROPERTY"]<-"OTHER: NoN POSS"

#-------- gen narcotics by month and quarter -
# gen narcotics count monthly
narcotics_count_monthly <- count(narcotics, c("crime_type", "Month"))

# gen narcotics count quarterly - 
narcotics_count_monthly$quarter   <- as.yearqtr(narcotics_count_monthly$Month, format = "%Y-%m-%d")
narcotics_count_qrtly <- ddply(narcotics_count_monthly, .(crime_type, quarter), summarize, Sum=sum(freq))
narcotics_count_qrtly <- filter(narcotics_count_qrtly, quarter != "2016 Q3")

# gen narcotics sub count monthly -
narc_sub_count_monthly <- count(narcotics, c("crime_type", "Description", "Month"))

# gen narcotics sub count quarterly -
narc_sub_count_monthly$quarter <- as.yearqtr(narc_sub_count_monthly$Month, format = "%Y-%m-%d")
narc_sub_count_qrtly <- ddply(narc_sub_count_monthly, .(crime_type, Description, quarter), summarize, Sum=sum(freq))
narc_sub_count_qrtly <- filter(narc_sub_count_qrtly, quarter != "2016 Q3")

# gen narcotics count monthly district
narcotics_count_monthly_dist <- count(narcotics, c("Month", "District"))
