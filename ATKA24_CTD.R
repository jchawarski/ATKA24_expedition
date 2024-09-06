### ATKA24 Expedition Dataset ###
### Developed by J. Chawarski, ASL Environmental Sciences
### Project initiated on 09-06-2024


# 1) Export Sv.csv using AZFPLink6
# 2) Import CTD files to calculate mean sound speed per 50 ping
# 3) Adjust Sv and range values using mean sound speed and absorption
# 3B) convert raw ctd files into a basic heave source file (depth.csv) for Echoview
# 4) *Read into echoview, using depth from CTD as heave source
# 5) calculate average sv and some echometric for individual vs layer. 


# *alternative open-source approach to be developed R

# FILE CONVERSION #
# goal is to take ctd depth data and use is to align AZFP nano
# first, create an automated workflow for converting full CTD data into echoview style heave data

require(tidyverse)
require(readxl)

setwd("C:/Users/jchawarski/OneDrive - ASL Environmental Sciences Inc/Projects/Atka Expedition - SW Greenland/ATKA24 DATA")
# single file reading and stripping of variables

stn1 <- read_excel("RBR_CTD_Tu/ATKA24_01_CTD3_full.xlsx", sheet=4, skip=1)

stn1$Depth_date <- format(as.Date(stn1$Time, format = "%Y-%m-%d"), "%m/%d/%Y" )
stn1$Depth_time <- format(stn1$Time, format = "%H:%M:%S")
stn1$Depth_meters <- stn1$Depth
stn1$Depth_status <- 3
stn1 <- stn1[12:15]

write.csv(stn1, "ATKA24_01_CTD3_heave.depth.csv")
