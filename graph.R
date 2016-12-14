#-------------------------------------------------------------------------------
# File Name:    graphs.R
# Author(s):    Reuben Bauer
# Written:      12/13/2016
# 
# Inputs:       narcotics_count type(dataframe)
#				narc_sub_count type(dataframe)
#				narc_sub_count_qrtly type(dataframe)
#
# Outputs:      
#
# Description: 
# 
# 1.
#
#
#-------------------------------------------------------------------------------

#---------------------------- Section 1: Preliminaries -------------------------

#-------- set repository ----
setwd(paste0(repository, '/output'))

#---------------------------- Section 3: Generate Variables  -------------------

#-------- graph overall narcotics -
plot1 <- ggplot(data=narcotics_count, aes(x=Month, y=freq)) + 
	geom_line() + geom_point() + scale_x_continuous(breaks = 2001:2016)
ggsave("monthly_narcotics.pdf", plot = plot1)

#-------- graph narcotic subgroups by month -
plot2 <- ggplot(data=narc_sub_count, aes(x=Month, y=freq, colour = Description)) + 
	geom_line() + geom_point() + scale_x_continuous(breaks = 2001:2016) + theme(legend.position="none")
ggsave("monthly_narcotics_sub.pdf", plot = plot2)

#-------- graph narcotic subgroups by quarter -
plot3 <- ggplot(data=narc_sub_count_qrtly, aes(x=quarter, y=Sum, colour = Description)) + 
	geom_line() + geom_point() + scale_x_continuous(breaks = 2001:2016) + theme(legend.position="none")
ggsave("quarterly_narcotics_sub.pdf", plot = plot3)