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
plot1 <- ggplot(data=narcotics_count_monthly, aes(x=Month, y=freq)) + 
  geom_line() + geom_point() + scale_x_continuous(breaks = 2010:2017) + 
  expand_limits(y=0) + xlab("Month") + ylab("Drug Possession Arrests")
ggsave("monthly_narcotics.pdf", plot = plot1)

#-------- graph overall narcotics by quarter -
plot2 <- ggplot(data=narcotics_count_qrtly, aes(x=quarter, y=Sum)) + 
  geom_line() + geom_point() + scale_x_continuous(breaks = 2010:2017) + theme(legend.position="none") + 
  expand_limits(y=0) + xlab("Quarter") + ylab("Drug Possession Arrests")
ggsave("quarterly_narcotics.pdf", plot = plot2)

#-------- graph narcotic subgroups by month -
plot3 <- ggplot(data=narc_sub_count_monthly, aes(x=Month, y=freq, colour = Description)) + 
  geom_line() + geom_point() + scale_x_continuous(breaks = 2010:2017) + theme(legend.position="none") + 
  expand_limits(y=0) + xlab("Month") + ylab("Drug Possession Arrests")
ggsave("monthly_narcotics_sub.pdf", plot = plot3)

#-------- graph narcotic subgroups by quarter -
plot4 <- ggplot(data=narc_sub_count_qrtly, aes(x=quarter, y=Sum, colour = Description)) + 
  geom_line() + geom_point() + scale_x_continuous(breaks = 2010:2017) + theme(legend.position="none") + 
  expand_limits(y=0) + xlab("Quarter") + ylab("Drug Possession Arrests")
ggsave("quarterly_narcotics_sub.pdf", plot = plot4)

#-------- graph police stops by month -
plot5 <- ggplot(data=stops_monthly, aes(x=Month, y=n_stops)) + 
  geom_line() + geom_point() + scale_x_continuous(breaks = 2010:2017) + theme(legend.position="none") + 
  expand_limits(y=0) + xlab("Month") + ylab("Police Stops")
ggsave("monthly_stops.pdf", plot = plot5)

#-------- graph police stops by quarter -
plot6 <- ggplot(data=stops_qrtly, aes(x=quarter, y=Sum)) + 
  geom_line() + geom_point() + scale_x_continuous(breaks = 2010:2017) + theme(legend.position="none") + 
  expand_limits(y=0) + xlab("Quarter") + ylab("Police Stops")
ggsave("quarterly_stops.pdf", plot = plot6)

#-------- graph cc by month -
plot7 <- ggplot(data=cc_count_monthly, aes(x=Month, y=freq)) + 
  geom_line() + geom_point() + scale_x_continuous(breaks = 2010:2017) + 
  expand_limits(y=0) + xlab("Month") + ylab("Contact Cards")
ggsave("monthly_cc.pdf", plot = plot7)

#-------- graph cc by quarter -
plot8 <- ggplot(data=cc_count_qrtly, aes(x=quarter, y=Sum)) + 
  geom_line() + geom_point() + scale_x_continuous(breaks = 2010:2017) + theme(legend.position="none") + 
  expand_limits(y=0) + xlab("Quarter") + ylab("Contact Cards")
ggsave("quarterly_cc.pdf", plot = plot8)

#-------- graph police stops and crime by month -
temp <- filter(narcotics_count_monthly, Month >= as.yearmon("2004-01"))

plot9 <- ggplot(data=temp, aes(x=Month, y=freq)) + 
  geom_line() + geom_point() + scale_x_continuous(breaks = 2010:2017) + 
  expand_limits(y=0) + xlab("Month") + ylab("Narcotics Arrests")

pdf("stops_crimes_monthly.pdf")
grid.newpage()
grid.draw(rbind(ggplotGrob(plot5), ggplotGrob(plot9), size = "first"))
dev.off()

#-------- graph police stops and crime by quarter -
temp <- filter(narcotics_count_qrtly, quarter >= as.yearqtr("2004-01"))

plot10 <- ggplot(data=temp, aes(x=quarter, y=Sum)) + 
  geom_line() + geom_point() + scale_x_continuous(breaks = 2010:2017) + 
  expand_limits(y=0) + xlab("Quarter") + ylab("Narcotics Arrests") 

pdf("stops_crimes_quarterly.pdf")
grid.newpage()
grid.draw(rbind(ggplotGrob(plot6), ggplotGrob(plot10), size = "first"))
dev.off()

rm(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10)

#-------- contact cards by district and month -
for (i in 1:31) {
  plot <- ggplot(data=cc_count_monthly_dist[cc_count_monthly_dist$DISTRICT == i, ], aes(x=Month, y=freq)) + 
    geom_line() + geom_point() + scale_x_continuous(breaks = 2010:2017) + ylim(0, 8000) + 
      xlab("Month") + ylab(paste("Number of Contact Cards: District", i))
  
  ggsave(paste0("cc_district", i, ".pdf"), plot = plot)
}

#-------- narcotics arrests by district and month -
for (i in 1:31) {
  plot <- ggplot(data=narcotics_count_monthly_dist[narcotics_count_monthly_dist$District == i, ], aes(x=Month, y=freq)) + 
    geom_line() + geom_point() + scale_x_continuous(breaks = 2010:2017) + ylim(0, 700) + 
    xlab("Month") + ylab(paste("Number of Narcotics Arrests: District", i))
  
  ggsave(paste0("na_district", i, ".pdf"), plot = plot)
}



