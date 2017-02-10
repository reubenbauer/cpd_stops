#-------------------------------------------------------------------------------
# File Name:    graphs.R
# Author(s):    Reuben Bauer
# Written:      12/13/2016
# 
# Inputs:       narcotics_count type(dataframe)
#               narcotics_count_quarterly type(dataframe)
#				        narc_sub_count type(dataframe)
#				        narc_sub_count_qrtly type(dataframe)
#               stops type(dataframe)
#               stops_quarterly type(dataframe)
#
# Outputs:      graphs.pdf
#
# Description: 
# 
# 1. Graph Monthly/Quarterly Crimes/Crimes Sub/Stops
# 2. Graph Joint Crimes + Stops
#
#-------------------------------------------------------------------------------

#---------------------------- Section 1: Preliminaries -------------------------

#-------- set repository ----
setwd(paste0(repository, '/output'))

#---------------------------- Section 3: Generate Variables  -------------------

#-------- graph overall narcotics by month -
plot1 <- ggplot(data=narcotics_count, aes(x=Month, y=freq)) + 
  geom_line() + geom_point() + scale_x_continuous(breaks = 2001:2016) + 
  expand_limits(y=0) + xlab("Month") + ylab("Drug Possession Arrests")
ggsave("monthly_narcotics.pdf", plot = plot1)

#-------- graph overall narcotics by quarter -
plot2 <- ggplot(data=narcotics_count_quarterly, aes(x=quarter, y=Sum)) + 
  geom_line() + geom_point() + scale_x_continuous(breaks = 2001:2016) + theme(legend.position="none") + 
  expand_limits(y=0) + xlab("Quarter") + ylab("Drug Possession Arrests")
ggsave("quarterly_narcotics.pdf", plot = plot2)

#-------- graph narcotic subgroups by month -
plot3 <- ggplot(data=narc_sub_count, aes(x=Month, y=freq, colour = Description)) + 
  geom_line() + geom_point() + scale_x_continuous(breaks = 2001:2016) + theme(legend.position="none") + 
  expand_limits(y=0) + xlab("Month") + ylab("Drug Possession Arrests")
ggsave("monthly_narcotics_sub.pdf", plot = plot3)

#-------- graph narcotic subgroups by quarter -
plot4 <- ggplot(data=narc_sub_count_qrtly, aes(x=quarter, y=Sum, colour = Description)) + 
  geom_line() + geom_point() + scale_x_continuous(breaks = 2001:2016) + theme(legend.position="none") + 
  expand_limits(y=0) + xlab("Quarter") + ylab("Drug Possession Arrests")
ggsave("quarterly_narcotics_sub.pdf", plot = plot4)

#-------- graph police stops by month -
plot5 <- ggplot(data=stops, aes(x=Month, y=n_stops)) + 
  geom_line() + geom_point() + scale_x_continuous(breaks = 2003:2016) + theme(legend.position="none") + 
  expand_limits(y=0) + xlab("Month") + ylab("Police Stops")
ggsave("monthly_stops.pdf", plot = plot5)

#-------- graph police stops by quarter -
plot6 <- ggplot(data=stops_quarterly, aes(x=quarter, y=Sum)) + 
  geom_line() + geom_point() + scale_x_continuous(breaks = 2003:2016) + theme(legend.position="none") + 
  expand_limits(y=0) + xlab("Quarter") + ylab("Police Stops")
ggsave("quarterly_stops.pdf", plot = plot6)

#-------- graph police stops and crime by month -
temp <- filter(narcotics_count, Month >= as.yearmon("2004-01"))

plot7 <- ggplot(data=temp, aes(x=Month, y=freq)) + 
  geom_line() + geom_point() + scale_x_continuous(breaks = 2003:2016) + 
  expand_limits(y=0) + xlab("Month") + ylab("Drug Possession Arrests")

png("stops_crimes_monthly.png")
grid.newpage()
grid.draw(rbind(ggplotGrob(plot5), ggplotGrob(plot7), size = "first"))
dev.off()

#-------- graph police stops and crime by quarter -
temp <- filter(narcotics_count_quarterly, quarter >= as.yearqtr("2004-01"))

plot8 <- ggplot(data=temp, aes(x=quarter, y=Sum)) + 
  geom_line() + geom_point() + scale_x_continuous(breaks = 2003:2016) + 
  expand_limits(y=0) + xlab("Quarter") + ylab("Drug Possession Arrests") 

pdf("stops_crimes_quarterly.pdf")
grid.newpage()
grid.draw(rbind(ggplotGrob(plot6), ggplotGrob(plot8), size = "first"))
dev.off()

# To add vertical lines and to add extra text
# + geom_vline(xintercept = as.yearqtr("2016-01"), colour = "red")
# geom_text(aes(x=200, label="\nthe strong cars", y=20), colour="blue", angle=90, text=element_text(size=11)) +



