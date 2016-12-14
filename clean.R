#-------------------------------------------------------------------------------
# File Name:    clean.R
# Author(s):    Reuben Bauer
# Written:      12/13/2016
# 
# Inputs:       data type(dataframe)
#
# Outputs:      narcotics_count type(dataframe)
#				narc_sub_count type(dataframe)
#				narc_sub_count_qrtly type(dataframe)
#
# Description: 
# 
# 	1. rename poorly styled variables
#	2. restrict dataset to narcotics
#	3. convert to dates
#	4. combine small categories
#	5. generate narcotics_count dataframe
#
# 	To Do:
#	1. combine cannabis categories into a single category
#
#-------------------------------------------------------------------------------

#---------------------------- Section 1: Preliminaries -------------------------

#---------------------------- Section 2: Clean Data ----------------------------

#-------- clean narcotics ---
names(data)[names(data) == "Primary Type"]<-"crime_type"
narcotics <- filter(data, crime_type == "NARCOTICS")
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

#-------- gen narcotics count -
narcotics_count <- count(narcotics, c("crime_type", "Month"))

#-------- gen narcotics descriptioncount -
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

narc_sub_count$quarter <- 
	as.yearqtr(narc_sub_count$Month, format = "%Y-%m-%d")
narc_sub_count_qrtly <- ddply(narc_sub_count, 
	.(crime_type, Description, quarter), summarize, Sum=sum(freq))
narc_sub_count_qrtly <- filter(narc_sub_count_qrtly, quarter != "2016 Q3")
