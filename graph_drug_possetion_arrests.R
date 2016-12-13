#narcotics
rm(list=ls())
#repository = "C:/Users/reubenbauer/Box Sync/cpd_stops/data/Crimes_-_2001_to_present.csv"
repository = "C:/Users/reuben_bauer/Box Sync/Box Sync/cpd_stops/data/Crimes_-_2001_to_present.csv"
data = fread(paste0(repository, "data/Crimes_-_2001_to_present.csv"), header = TRUE, stringsAsFactors = FALSE)
names(data)[names(data) == "Primary Type"]<-"crime_type"
narcotics = filter(data, crime_type == "NARCOTICS")
narcotics$Date = as.Date(narcotics$Date, "%m/%d/%Y %H:%M:%S")
narcotics$Month = as.yearmon(narcotics$Date)
narcotics = filter(narcotics, Month != "Aug 2016")
narcotics_count = count(narcotics, c("crime_type", "Month"))
ggplot(data=narcotics_count, aes(x=Month, y=freq)) + geom_line() + geom_point()

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
narcotics_description_count = count(narcotics, c("crime_type", "Description", "Month"))
narcotics_description_count = filter(narcotics_description_count, 
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
                                       #Description != "POSS: HALLUCINOGENS" &
                                       #Description != "POSS: HEROIN(BLACK TAR)" &
                                       #Description != "POSS: LOOK-ALIKE DRUGS" &
                                       #Description != "POSS: METHAMPHETAMINES" &
                                       #Description != "POS: HYPODERMIC NEEDLE" & 
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
                                       #Description != "POSS: AMPHETAMINES" &
                                       Description != "POSSESSION OF DRUG EQUIPMENT" &
                                       #Description != "POSS: SYNTHETIC DRUGS" &
                                       #Description != "POSS: PCP" & 
                                       #Description != "POSS: BARBITUATES" & 
                                       #Description != "POSS: CANNABIS 30GMS OR LESS" & 
                                       #Description != "POSS: CANNABIS MORE THAN 30GMS" & 
                                       #Description != "POSS: COCAINE" & 
                                       #Description != "POSS: CRACK" & 
                                       #Description != "POSS: HEROIN(BRN/TAN)" & 
                                       #Description != "POSS: HEROIN(WHITE)" & 
                                       #Description != "POSS: Other" & 
                                       Description != "SOLICIT NARCOTICS ON PUBLICWAY")


ggplot(data=narcotics_description_count, aes(x=Month, y=freq, colour = Description)) + geom_line() + geom_point() + scale_x_continuous(breaks = 2001:2016)

library(zoo)
narcotics_description_count$quarter <- as.yearqtr(narcotics_description_count$Month, format = "%Y-%m-%d")
narcotics_description_count = ddply(narcotics_description_count, .(crime_type, Description, quarter), summarize, Sum=sum(freq))
narcotics_description_count = filter(narcotics_description_count, quarter != "2016 Q3")

ggplot(data=narcotics_description_count, aes(x=quarter, y=Sum, colour = Description)) + geom_line() + geom_point() + scale_x_continuous(breaks = 2001:2016)
