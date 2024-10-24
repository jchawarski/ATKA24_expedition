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

nano <- read.csv("AZFP_nano/Intermediate/ATKA24_01_CTD1_69001_C1_200KHZ.sv.csv", header=F,fileEncoding = "UTF-8-BOM")

nano.meta <- nano[2:dim(nano)[1],1:6]      # select the metadata portion 
colnames(nano.meta) <- nano[1,1:6]         # assign the headers to the metadata

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
                                          turbidity = mean(Turbidity),
                                          pressure = mean(Pressure))
colnames(stn.sum)[1] <- "interval"
stn.sum$datetime <- as.POSIXct(stn.sum$interval ,origin = "1970-01-01",tz = "UTC")

# calculate mean temperature, salinity, pressure to a range of 50 m below probe
# define function to calculate rolling mean for a given window size
# accurate sounds speed adjustments 
rolling_mean <- function(x, depth, window = 50) {
  sapply(1:length(x), function(i) {
    # Define the range for averaging (depth[i] to depth[i] + window)
    lower_bound <- depth[i]
    upper_bound <- lower_bound + window
    
    # Get the values within this depth range
    idx <- which(depth >= lower_bound & depth <= upper_bound)
    
    # If no values fall in the range, return NA
    if(length(idx) == 0) return(NA)
    
    # Calculate the mean
    mean(x[idx], na.rm = TRUE)
  })
}

stn.sum$temperature_50m_mean <- rolling_mean(stn.sum$temperature, stn.sum$depth)
stn.sum$salinity_50m_mean <- rolling_mean(stn.sum$salinity, stn.sum$depth)
stn.sum$soundspeed_50m_mean <- rolling_mean(stn.sum$sound_speed, stn.sum$depth)
stn.sum$pressure_50m_mean <- rolling_mean(stn.sum$pressure, stn.sum$depth)

# Assign constants for calculating a rolling absorption coefficient
# 200 kHz quad transducer
c <-1450.5     # original sound speed
coeff_abs <- 0.0230 # original coefficient of absorption
t <- 0.003        # pulse duration (s)
y <- 1.071000006050e-02 # steridians BP from XML file - two-way beam angle  --- Adjust based on transducer 
f <- 200000 # frequency in Hz



# calculate the absorption coefficient for each 50 m depth interval
require(oce)
coeff_abs_new <-swSoundAbsorption(frequency= f,    # new absorption coefficient
                                  salinity = stn.sum$salinity_50m_mean,
                                  temperature = stn.sum$temperature_50m_mean,
                                  pressure = stn.sum$pressure_50m_mean, 
                                  pH =8,
                                  formulation = "francois-garrison") 

stn.sum$coeff_abs_50m <- coeff_abs_new


# plots full temperature profile (downcast and upcast)
stn.sum %>% 
  ggplot(aes(x=depth, y=temperature), color="blue") +
  geom_line() + 
  coord_flip() + scale_x_reverse()

# plots the profiler depth and time
stn.sum %>% 
  ggplot(aes(x=datetime, y=-depth)) +
  geom_line() + theme_bw()

#plots the sound speed profile
stn.sum %>% ggplot(aes(x=depth, y=sound_speed)) + 
  geom_line() + 
  coord_flip() + 
  scale_x_reverse() + 
  theme_bw()


# round the AZFP interval to the nearest second
nano.meta$interval <- period_to_seconds(lubridate::seconds(nano.meta$datetime))


# bind the two datasets together
nano.ctd <- nano.meta %>% 
  left_join(., stn.sum, by="interval")

# Constants for Sv calculation:          - most of these are not needed as they fall under the 'power' terms
# psi <- 10*log10(1.071000006050e-02) # dB re 1 Sr
#tvr <- 1.698999938965e+02 # transmit voltage response, dB re 1uPa/Volt at 1 m, read as TVR 
#vtx < - 1.114000015259e+02 # voltage sent to the transducer, or transmit voltage, read as VTX from XML
#a <- 2.319999970496e-02 # slope of the detector response (Volt/dB), read as DS from XML
#el_max <- 1.423999938965e+02 # Echo Level at the transducer that produces full scale output, read as EL from XML


#equi_beam_angle <- 10^((y+20*log10(f_nominal/f))/10)     # this is the previous equation using a broadband transducer   
equi_beam_angle <- 10*log10(y)


# Isolate the Sv matrix from the exported file
nano.sv <- nano[2:dim(nano)[1],7:dim(nano)[2]]  # selects only the Sv matrix from the exported file
nano.sv[nano.sv==""] <- -999                    # replaces the blank cells of matrix
nano.sv <- matrix(as.numeric(unlist(nano.sv)),nrow=nrow(nano.sv))


# Set variables for range matrix calculation - This works
Range_start <- as.numeric(nano.meta$Range_start)[1] # all values in the df are the same.
Range_stop <- as.numeric(nano.meta$Range_stop)[1]
Sample_count <- as.numeric(nano.meta$Sample_count)[1]

# calculate range for each horizontal sample
# this is the predicted range based on the default values entered in AZFPlink during deployment
nano.range <- sapply(1:dim(nano.sv)[2], function(i) Range_start + ((Range_stop-Range_start)/Sample_count)*(i+0.5))    
nano.range <- matrix(nano.range, nrow=dim(nano.sv)[1], ncol = dim(nano.sv)[2], byrow = T) # creates the full range matrix

# TVG Range Correction
# Need to include a TVG range correction
# According to Echoview half a pulse length is appropriate, confirm with Steve
# Estimate correction offset value

  Fs <- 64000 # Hz, digitization rate, or the frequency at which the analog-to-digital converter is sampling
  samperf <-  1/Fs # microseconds between samples
  TVG_corr <- (t/samperf)*(1/2)  # samples pulse length divided by samples, divided by two
  
# how do I apply this value to range???
  

# calculate time matrix - this uses a constant sounds speed
# calculates the time for the signal to return based on the pulse length and nominal sound speed
time.mat <- apply(nano.range,1:2, function(i)(c*t/4+i)*2/c) 
#assign sound speed from CTD data
c_new <- nano.ctd$soundspeed_50m_mean

# calculate a new range matrix - this should use a variable sound speed as the probe is moving. range needs to be adjusted per ping
# the per-ping adjustment to sounds speed is only valid for the portion of the profile where we have measured sound speed
# should trim the acoustic profile ahead of time to avoid create NA values?
nano.range_new <- apply(time.mat, 2, function(i) i*c_new/2-c_new*t/4)   #new range matrix  - solving for range (R) in 20logR-2aR-10log(c*t*)

nano.range_new[1000:1005,1:5] # double check that dynamic sound speed results in a dynamic range result



# calculate nominal power matrix - isolates the power terms, removing any variables that include impacted by sounds speed. 
power<- nano.sv-20*log10(nano.range)-2*(coeff_abs)*(nano.range)+10*log10((c*t*y)/2) 

power[1000:1005,1:10]


# Calculate Sv - Sv = power + spherical spreading loss + absorption loss - volume reverberation coefficient
  # spherical spreading = 20log10(R) = 20*log10(nano.range_new)  -- this is a range matrix
    spread_loss <- 20*log10(nano.range_new)
  # absorption loss = 2ar = 2*nano.ctd$coeff_abs_50m*nano.range_new -- this is a vector that uses absorption calculated out to 50 m from transducer
    abs_loss <- 2*nano.ctd$coeff_abs_50m*nano.range_new 
  # volume reverberation coefficient = 10log10(c*t*psi/2) = 10*log10(c_new*t*y/2)
    reverb_coeff <- 10*log10(c_new*t*y/2)

#Sv_new <- power+20*log10(nano.range_new) + 2*coeff_abs_new*nano.range_new  # -10*log10(c_new*t*equi_beam_angle/2) 

# COMPUTATIONALLLY INTENSIVE - not working
Sv_new <- apply(power, 2, function(i) i+spread_loss+abs_loss-reverb_coeff) # subtract the final term from the rest of the calculation


# add a calibration offset to all Sv values to account for system bandwidth effects for volume backscatter
# see section 11.5 in AZFP manual, table 3. 
Sv_offset <- 1.2 # dB --- this value is for a 300 microsecond pulse length at 200 kHz

#add the Sv offset to the entire Sv matrix prior to calculating means
Sv_new <- apply(Sv_new, 2, function(i) i-Sv_offset) # not tested


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







