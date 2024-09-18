### ATKA24 Expedition Dataset ###
### Developed by J. Chawarski, ASL Environmental Sciences
### Project initiated on 09-06-2024


# 1) Export Sv.csv using AZFPLink6
# 2) Import CTD files to calculate mean sound speed per 50 ping
# 3) Adjust Sv and range values using mean sound speed and absorption
# 3B) convert raw ctd files into a basic heave source file (depth.csv) for Echoview
# 4) *Read into echoview, using depth from CTD as heave source
# 5) calculate average sv and some echometric for individual vs layer. 


# *alternative open-source approach to be developed in R

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


# read AZFP data as sv.csv exported from AZFP link

# 1) Read in files AZFP files by profile

nano1 <- read.csv("AZFP_nano/Intermediate/ATKA24_01_CTD1_69001_C1_200KHZ.sv.csv", header=F,fileEncoding = "UTF-8-BOM")

nano.meta <- nano1[2:dim(nano1)[1],1:6]      # select the metadata portion 
colnames(nano.meta) <- nano1[1,1:6]         # assign the headers to the metadata

# convert azfp meta datetime

nano.meta$Date<- as.Date(as.character(nano.meta$Ping_date), "%Y-%m-%d")
nano.meta$datetime <- as.POSIXct(paste(nano.meta$Ping_date, nano.meta$Ping_time), format="%Y-%m-%d %H:%M:%S", tz="UTC")


# 2) Read in accompanying CTD file

stn1 <- read_excel("RBR_CTD_Tu/Exported_full_casts/ATKA24_01_CTD1_full.xlsx", sheet=5, skip=1)

stn1$Depth_date <- format(as.Date(stn1$Time, format = "%Y-%m-%d"), "%m/%d/%Y" )
stn1$Depth_time <- format(stn1$Time, format = "%H:%M:%S")
stn1$datetime <- as.POSIXct(stn1$Time,format="%Y-%m-%d %H:%M:%S", tz="UTC")

stn1$interval <- period_to_seconds(lubridate::seconds(stn1$datetime))

# calculate 1 second averages for the CTD data and round the interval to nearest second

stn.sum <- 

stn1 %>% group_by(round(interval)) %>% summarise(depth = mean(Depth),
                                          sound_speed = mean(`Speed of sound`),
                                          conductivity = mean(Conductivity),
                                          salinity = mean(Salinity),
                                          temperature = mean(Temperature),
                                          turbidity = mean(Turbidity))
colnames(stn.sum)[1] <- "interval"

stn1 %>% ggplot(aes(x=Depth, y=Temperature), color="blue") + geom_line() + coord_flip() + scale_x_reverse()
stn.sum %>% ggplot(aes(x=depth, y=temperature), color="blue") + geom_line() + coord_flip() + scale_x_reverse()


# round the AZFP interval to the nearest second

nano.meta$interval <- period_to_seconds(lubridate::seconds(nano.meta$datetime))


# bind the two datasets together

nano.ctd <- nano.meta %>% 
  left_join(., stn.sum, by="interval")
  
# calculate the mean

# Constants for Sv calculation:

eps <- 10*log10(1.071000006050e-02) # dB re 1 Sr
tvr <- 1.698999938965e+02 # transmit voltage response, dB re 1uPa/Volt at 1 m, read as TVR 
vtx < - 1.114000015259e+02 # voltage sent to the transducer, or transmit voltage, read as VTX from XML
a <- 2.319999970496e-02 # slope of the detector response (Volt/dB), read as DS from XML
el_max <- 1.423999938965e+02 # Echo Level at the transducer that produces full scale output, read as EL from XML

c <-1450.5     # original sound speed
coeff_abs <- 0.0230 # original coefficient of absorption
t <- 0.003        # pulse duration (s)

y <- -19.7   # two-way beam angle  --- Adjust based on transducer 

f <- 200000 # frequency in Hz

# 200 kHz quad transducer
c <-1450.5     # original sound speed
coeff_abs <- 0.0230 # original coefficient of absorption
t <- 0.003        # pulse duration (s)
y <- -21               # two-way beam angle  --- Adjust based on transducer 
f_nominal <- 365000 # nominal frequency in Hz   -- Adjust based on transducer
f <- 365000        # center frequency         NOTE: for wideband data, these are different values


# calculate the absorption coefficient for each 50 m depth interval

coeff_abs_new <-swSoundAbsorption(frequency= f_nominal,    # new absorption coefficient
                                  salinity = sal,
                                  temperature = temp,
                                  pressure = p, 
                                  pH =8,
                                  formulation = "francois-garrison") 



# view data and make any adjustments



# adjust Sv using new sound speed and absorption coefficients
# DEFAULT SOUND SPEED = 1450.5 m/s and DEFAULT ABS COEFF. = 0.0230


  # for each depth, calculate the mean sound speed, temperature, and salinity for the 0-50 below that depth interval. 

# publish dataset with profile name

write.csv(------, "ATKA24_01_AZFP_sv_corr.csv")

# publish a summary dataset with Sv_mean and inertia/dispersion of backscatter (i.e. layers vs single targets)

write.csv(------, "ATKA24_01_AZFP_summary.csv")


# publish an image for each profile

ggsave("ATKA24_01_AZFP_sv_corr.tiff", plot=p1, device = "tiff")
azfp.final$Station <- "RG19_51"
write.csv(azfp.final, "Clean Data/RG19_51_azfp-CTD_correctedSv.csv")



# 3) Standardize the time format, and convert to unique interval

azfp.meta$Interval <- period_to_seconds(lubridate::seconds(azfp.meta$datetime))
CTD$Interval <- period_to_seconds(lubridate::seconds(CTD$datetime))

# CTD was collected at


azfp.ctd <- azfp.meta %>% 
  left_join(., CTD, by="Interval") %>%     # join ctd and azfp data.frames by matching time cases
  mutate_all(., as.numeric) %>%           # convert all values to numeric
  group_by(Ping_index) %>%                # there are multiple values per ping so they need to be average to azfp sampling resolution
  summarise_all(., mean) %>%               # calculate mean value for each parameter per ping
  na_interpolation(.)







