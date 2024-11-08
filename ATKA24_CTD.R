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


#### Create a heave file from CTD - for use in Echoview only ####


stn1 <- read_excel("RBR_CTD_Tu/Exported_full_casts/ATKA24_01_CTD3_full.xlsx", sheet=4, skip=1)

stn1$Depth_date <- format(as.Date(stn1$Time, format = "%Y-%m-%d"), "%m/%d/%Y" )
stn1$Depth_time <- format(stn1$Time, format = "%H:%M:%S")
stn1$Depth_meters <- stn1$Depth
stn1$Depth_status <- 3
stn1 <- stn1[12:15]

write.csv(stn1, "ATKA24_01_CTD3_heave.depth.csv")




#### AZFP DATA PROCESSING PIPELINE ####

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

# 3) Create matching CTD-AZFP file - 
#     calculate 1 second averages for the CTD data and round the interval to nearest second

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

# 4) Calculate 50m measurements for AZFP calibration

  # calculate mean temperature, salinity, pressure to a range of 50 m below probe
  # define function to calculate rolling mean for a given window size
  # accurate sounds speed adjustments 
  rolling_mean <- function(x, depth, window = 50)    
    {
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

# 5) # Assign nominal constants for calculating a rolling absorption coefficient
      # 200 kHz quad transducer
      c <-1450.5     # original sound speed
      coeff_abs <- 0.0230 # original coefficient of absorption
      t <- 0.0003        # pulse duration (s)
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
      # assign new absorption coefficient
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

#assign sound speed from CTD data
c_new <- nano.ctd$soundspeed_50m_mean


# Constants for Sv calculation:          - most of these are not needed as they fall under the 'power' terms
# psi <- 10*log10(1.071000006050e-02) # dB re 1 Sr
#tvr <- 1.698999938965e+02 # transmit voltage response, dB re 1uPa/Volt at 1 m, read as TVR 
#vtx < - 1.114000015259e+02 # voltage sent to the transducer, or transmit voltage, read as VTX from XML
#a <- 2.319999970496e-02 # slope of the detector response (Volt/dB), read as DS from XML
#el_max <- 1.423999938965e+02 # Echo Level at the transducer that produces full scale output, read as EL from XML


#equi_beam_angle <- 10^((y+20*log10(f_nominal/f))/10)     # this is the previous equation using a broadband transducer   
#equi_beam_angle <- 10*log10(y)


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

# calculate time matrix - this uses a constant sounds speed
# calculates the time for the signal to return based on the pulse length and nominal sound speed
time.mat <- apply(nano.range,1:2, function(i)(c*t/4+i)*2/c) 
time.mat[1000:1005,1:10]


# Range correction based on sound speed
# calculate a new range matrix - this should use a variable sound speed as the probe is moving. range needs to be adjusted per ping
# the per-ping adjustment to sounds speed is only valid for the portion of the profile where we have measured sound speed
# should trim the acoustic profile ahead of time to avoid create NA values?
nano.range_new <- apply(time.mat, 2, function(i) i*c_new/2-c_new*t/4)   #new range matrix  - solving for range (R) in 20logR-2aR-10log(c*t*)
    # test to make sure it worked 
    hist(nano.range_new[,4411])   # histogram of maximum ranges
    nano.range_new[1000:1005,1:5] # subset
    summary(nano.range_new[,4411]) # includes NAs for pings without CTD measurements

#  TVG Range Correction - Need to figure out how to apply this to range measurements
#  Need to include a TVG range correction
#  According to Echoview half a pulse length is appropriate, confirm with Steve
#  Estimate correction offset valuen - two ways to do this
    
    # 1) estimate half a pulse length
    #Fs <- 64000 # Hz, digitization rate, or the frequency at which the analog-to-digital converter is sampling
    #samperf <-  1/Fs # microseconds between samples
    #TVG_corr <- (t/samperf)*(1/2)*Range_stop/Sample_count # meters, pulse length divided by samples, divided by two, converted to meters
    #TVGr_corr <- TVG_corr # meters per sample
    
    # or
    
    # 2) use the standard method - #(c*t)/4 # in m which would correctly translate to
    TVG_corr <- c_new*t/4   # in m --- substract this from the range matrix
    
# subtract TVG range correction from each column in range matrix
nano.range_new <- sweep(nano.range_new, 1, TVG_corr, "-")
summary(nano.range_new[,4411]) # includes NAs for pings without CTD measurements

# calculate nominal power matrix - isolates the power terms, removing any variables that include impacted by sounds speed. 
power <- nano.sv-20*log10(nano.range)-2*(coeff_abs)*(nano.range)+10*log10((c*t*y)/2) 

power[1000:1005,1:10]


# Calculate Sv - Sv = power + spherical spreading loss + absorption loss - volume reverberation coefficient
  # spherical spreading = 20log10(R) = 20*log10(nano.range_new)  -- this is a range matrix
    spread_loss <- 20*log10(nano.range_new)
  # absorption loss = 2ar = 2*nano.ctd$coeff_abs_50m*nano.range_new -- this is a vector that uses absorption calculated out to 50 m from transducer
    abs_loss <- 2*nano.ctd$coeff_abs_50m*nano.range_new 
  # volume reverberation coefficient = 10log10(c*t*psi/2) = 10*log10(c_new*t*y/2)
    # first calculate the adjusted beam angle
    y_adj <- y*(c/c_new)^2 # from Demer et al (2015) recommendation
    reverb_coeff <- 10*log10(c_new*t*y_adj/2)  
    reverb_coeff <- matrix(reverb_coeff, nrow = nrow(power), ncol = ncol(power), byrow = FALSE)
    #check data
    reverb_coeff[1000:1005,1:10]
#Sv_new <- power+20*log10(nano.range_new) + 2*coeff_abs_new*nano.range_new  # -10*log10(c_new*t*equi_beam_angle/2) 

# COMPUTATIONALLLY INTENSIVE - not working might need to rework into invdividual steps
#Sv_new <- apply(power, 2, function(i) i+spread_loss+abs_loss-reverb_coeff) # subtract the final term from the rest of the calculation

Sv_new <- power + spread_loss + abs_loss - reverb_coeff

# add a calibration offset to all Sv values to account for system bandwidth effects for volume backscatter
# see section 11.5 in AZFP manual, table 3. 
Sv_offset <- 1.2 # dB --- this value is for a 300 microsecond pulse length at 200 kHz

#add the Sv offset to the entire Sv matrix prior to calculating means
Sv_new <- apply(Sv_new, 2, function(i) i+Sv_offset) # works

#### END OF ADJUSTMENTS ####

# Check data
    # compute mean values 
    Sv_new_mean <- t(data.frame(apply(Sv_new, 1, function(i) log10(mean(10^i, na.rm=T)))))
    Sv_old_mean <- t(data.frame(apply(nano.sv, 1, function(i) log10(mean(10^i, na.rm=T)))))
    
    plot(Sv_new_mean[1,700:1300], type="l")
    lines(Sv_old_mean[1,700:1300], type = "l", col = "red")

#plot individual pings to compare corrected and uncorrected data

    # select individual ping
    sv_new.ping <- data.frame(Sv_new[1200,], nano.range_new[1200,])
    colnames(sv_new.ping) <- c("Sv", "range")
    sv_old.ping <- data.frame(nano.sv[1200,], nano.range[1200,])
    colnames(sv_old.ping) <- c("Sv", "range")
    
    # plot Sv vs Range
    sv_new.ping %>% 
      ggplot(aes(x=range, y=Sv)) + geom_line(color="black") + 
      geom_line(data=sv_old.ping, aes(x=range, y=Sv), color="red", inherit.aes = F) + 
      xlab("Range [m]") + ylab("Sv [dB]") + 
      theme_bw()


# TRIM dataset to include only the downcast

stn1 <- read_excel("RBR_CTD_Tu/Exported_trimmed_downcasts/ATKA24_01_CTD1.xlsx", sheet=3, skip=1)

stn1$Depth_date <- format(as.Date(stn1$Time, format = "%Y-%m-%d"), "%m/%d/%Y" )
stn1$Depth_time <- format(stn1$Time, format = "%H:%M:%S")
stn1$datetime <- as.POSIXct(stn1$Time,format="%Y-%m-%d %H:%M:%S", tz="UTC")
stn1$interval <- period_to_seconds(lubridate::seconds(stn1$datetime))

stn.sum <- 
  stn1 %>% group_by(round(interval)) %>%           summarise(depth = mean(Depth),
                                                   sound_speed = mean(`Speed of sound`),
                                                   conductivity = mean(Conductivity),
                                                   salinity = mean(Salinity),
                                                   temperature = mean(Temperature),
                                                   turbidity = mean(Turbidity),
                                                   pressure = mean(Pressure))
colnames(stn.sum)[1] <- "interval"

# bind the nano metadata back with the Sv file.
nano.sv_corrected <- cbind(nano.meta, Sv_new)
#double-check
nano.sv_corrected[1:10,1:10]


#filter the echogram by the intervals of the trimmed downcast
nano.sv_corrected <- nano.sv_corrected %>% filter(interval %in% stn.sum$interval)

nano.sv_corrected <- cbind(stn.sum$depth, nano.sv_corrected) 
# rename the depth column
colnames(nano.sv_corrected)[1] <- "depth"

# filter range measurements and compute mean ranges for whole dataset
nano.range_corrected <- cbind(nano.meta,nano.range_new)
nano.range_corrected <- nano.range_corrected %>% filter(interval %in% stn.sum$interval) # filters range matrix data to downcast only
#double check
nano.range_corrected[1:10,1:10]

# select only the range values corresponding to the trimmed dataset
nano.range_corrected <- nano.range_corrected[,10:ncol(nano.range_corrected)]

nano.range_mean <- apply(nano.range_corrected, 2, mean, na.rm=T)    # computes mean range values among all pings

# add the range values as temporary column names for each Sv value
colnames(nano.sv_corrected)[11:ncol(nano.sv_corrected)] <- nano.range_mean
#double-check
nano.sv_corrected[1:10,1:10]

# compute the average interval in depth measurements

intervals <- diff(nano.sv_corrected$depth)
summary(intervals)

# define depth intervals 
interval_width <- 0.5
max_depth <- max(nano.sv_corrected$depth)
max_depth_rounded <- ceiling(max_depth / interval_width) * interval_width
d_breaks <- seq(0, max_depth_rounded, by = interval_width)


interval_width <- 0.5
max_range <- max(nano.range_mean)
max_range_rounded <- ceiling(max_range / interval_width) * interval_width
r_breaks <- seq(0, max_range_rounded, by = interval_width)


### prepare an averaged profile - currently with min, max values. 

# ECHOMETRIC - sample entropy

# Load the package
library(pracma)


r <- 0.2 * sd(sv_new.ping$Sv) # Tolerance level (20% of standard deviation)

sample_entropy(Value, edim=2, r=0.2*sd(Value))
# True depth averaged Sv and echometric calculations
# one idea is to create a range mean column




stn1.profile <- nano.sv_corrected %>% 
                reshape2::melt(., by= colnames(nano.sv_corrected)[1:10], 
                               measure.vars = colnames(nano.sv_corrected)[11:ncol(nano.sv_corrected)], # Columns to melt
                               variable.name = "Variable", 
                               value.name = "Value") %>%
  
              # assign depth interval
  
                                mutate(r_interval = cut(as.numeric(as.character(Variable)),
                                r_breaks, 
                                #labels = r_breaks,
                                include.lowest = TRUE, 
                                right = FALSE)) %>%
              
              # assign range interval
  
                                mutate(d_interval = cut(depth,
                                d_breaks, 
                                #labels = d_breaks,
                                include.lowest = TRUE, 
                                right = FALSE)) %>%
              
              # grouping by depth range combo
              
                                group_by(d_interval, r_interval) %>% 
                                
              # compute mean value of Sv in each 
              # depth-range bin
              # carrying over the min time and interval for each bin
  
                                summarize(Sv_mean = log10(mean(10^Value, na.rm=T)),
                                          Sv_min = min(Value, na.rm=T),
                                          Sv_max = max(Value, na.rm=T),
                                          datetime = min(datetime),
                                          Interval = min(interval)) %>%
              
              # extract min depth from interval string
  
                                mutate(depth = min(as.numeric(unlist(regmatches(d_interval, gregexpr("[0-9.]+", d_interval)))))) %>%
              
              # extract min range from interval string
  
                                mutate(range = sapply(r_interval, function(r_interval) {
                                  if (!is.na(r_interval)) {
                                    # Extract numbers and take the minimum
                                    numbers <- as.numeric(unlist(regmatches(r_interval, gregexpr("-?[0-9]+\\.?[0-9]*", r_interval))))
                                    min(numbers, na.rm = TRUE)  # Return the minimum value
                                  } else {
                                    NA  # Return NA for NA labels
                                  }
                                })) %>%
  
              # compute true depth by combining depth and range
  
                                mutate(depth_true = depth + range)




# Plot results
require(showtext)
font_add_google("Barlow") # add ASL font type
showtext_auto() # gives showtext permission to overwrite ggplot default

Sv_label <- expression(paste("S"["v"]," [dB re 1 m" ^-1,"]"))

# FULL ECHOGRAM

      p1 <-
        
      stn1.profile %>%
      ggplot(aes(x = datetime, y = depth_true, fill = Sv_mean)) +
        geom_tile() +
        #scale_fill_viridis_c() +
        scale_fill_viridis_b(
          option = "D",              # Use a specific viridis palette
          direction = 1,            # Reverse colors if desired
          begin = 0.0, end = 1,        # Full palette range
          breaks = seq(-100, -50, by = 5),
          guide = guide_colorbar(    # Customize the color bar in the legend
            frame.colour = "black",  # Black border around the color bar
            frame.linewidth = 0.5,    # Thickness of the border
            barwidth = unit(0.4, "cm"),
            barheight = unit(5, "cm"), 
            ticks.colour = "black",  # Black ticks
            ticks.linewidth = 0.5 ,  
            ticks= FALSE,
            ticks.length = unit(1, "cm"),
            title.position = "right",
            title.theme = element_text(angle = 270, hjust = 0.5, vjust = -1.5) 
        )) + 
        labs(fill = Sv_label) + 
        scale_y_reverse(expand = c(0,0)) + 
        scale_x_datetime(date_labels = "%H:%M:%S", date_breaks = "1 min", expand = c(0,0)) + 
        labs(title = "Profile Echogram", x = "Time", y = "Depth [m]") +
        theme_bw()  + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+  # Rotate text 45 degrees
        theme(plot.margin = unit(c(0.5, 0, 0, 0.5), "cm"), 
              text = element_text("Barlow"))  
      
      p2 <-
       
      stn1.profile %>%
        group_by(depth_true) %>%
        filter(range > 0.5) %>%
        summarise(sv = log10(mean(10^Sv_mean, na.rm=T)),
                  min = min(Sv_min, na.rm = T),
                  max = max(Sv_max, na.rm = T)) %>%
                  #entropy = sample_entropy(10^(Sv_mean/10), edim=2, r=0.2*sd(10^(Sv_mean/10)))) %>%
        ggplot(aes(x = depth_true, y = sv)) +
        scale_y_continuous() + 
          geom_line() + 
        #  geom_line(aes(x = depth_true, y=min), inherit.aes = F , color="red") + 
        #  geom_line(aes(x = depth_true, y=max), inherit.aes = F , color="blue") + 
          coord_flip()+ scale_x_reverse(expand = c(0,0)) +
        labs(title = "MVBS Plot", y = Sv_label, x = "")+
        theme_bw()+ 
        theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0), "cm"), 
              text = element_text("Barlow"))
         
  
require(cowplot) 

final.plot <- 
      
plot_grid(p1,p2, align="hv", rel_widths = c(3:2), vjust=0.5)



### PUBLISH THE RESULTS ###

# not quite there yet - problems with text size on the png
ggsave2("ATKA24_01_AZFP_sv_corr.png", 
       plot=final.plot, 
       device = "png",
       width = 10,
       height = 4, 
       units = "in",
       dpi=150)



write.csv(------, "ATKA24_01_AZFP_sv_corr.csv")

# publish a summary dataset with Sv_mean and inertia/dispersion of backscatter (i.e. layers vs single targets)

write.csv(------, "ATKA24_01_AZFP_summary.csv")


# publish an image for each profile

ggsave("ATKA24_01_AZFP_sv_corr.tiff", plot=p1, device = "tiff")
azfp.final$Station <- "RG19_51"
write.csv(azfp.final, "Clean Data/RG19_51_azfp-CTD_correctedSv.csv")








