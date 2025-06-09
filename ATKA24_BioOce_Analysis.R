# ATKA 24 Expedition - Oceanographic sections of biological parameters

require(tidyverse)
require(oce)

setwd("C:/Users/jchawarski/OneDrive - ASL Environmental Sciences Inc/Projects/Atka Expedition - SW Greenland")

# downcasts only

CTD <- read.csv("ATKA24 DATA/All Parameters Profiles/ATKA24_01_CTD1_allparameters.csv")





      files <- list.files(path="ATKA24 DATA/All Parameters Profiles", full.names = TRUE, pattern= "*.csv") 

      # filter just files with profiles from Istertup Kangertiva
      files <- files[4:16]
      files <- files[-3]
      
      CTD <- lapply(files, function(i){read.csv(i)})                          # concatenates and trims upcast from all CTD files into large list
     
      meta.tbl <- setNames(data.frame(matrix(ncol = 2, nrow = length(files))), c("Site", "ID")) # create empty df for summary data
      meta.tbl[,1] <-  paste(substr(files, 37, 49))             # selects and trims file name, put in in blank matrix
      uniqueID <- c(1:length(files))                                                                   # create unique ID by cast
      meta.tbl[,2] <-  uniqueID                                                             # assigns a unique ID
      
      
      # READ in the sites
      
      sites <- read.csv("C:/Users/jchawarski/OneDrive - ASL Environmental Sciences Inc/Projects/Atka Expedition - SW Greenland/ATKA24 DATA/ATKA24_CTD_sites.csv")
      
      # modify site df to have matching Site identifier to then add Lat/Lon info to CTDs for sections/spatial analysis
      
      sites$Station <- sprintf("%02d", sites$Station)
      sites$Site <- paste(sites$Cruise, sites$Station, sites$Operation, sep="_")
      
      
      meta.tbl <- sites %>% dplyr::select(Site, Latitude, Longitude) %>% right_join(meta.tbl, by="Site")
      
      
      CTD.all <- lapply(CTD, function(i){ rbind(unique(data.frame(i))) }) %>%  # creates a list of dataframes (of each cast)
        mapply(cbind, ., ID = uniqueID, SIMPLIFY = F) %>%                         # adds ID column with uniqueID to each df in list
        bind_rows() %>%                                                        # binds all dfs
        left_join(meta.tbl, ., by="ID")                                      # joins 
     
      
      
      # optional - create a summary table of each profile 
      CTD.sum.tbl <-  CTD.all %>% filter(depth <= 100) %>%               # filters all data below 100 m depth
        group_by(Site) %>%                               # group fnx, allows headers to carry over
        summarise(
          avgT = mean(temperature), 
          avgS = mean(salinity), 
          lat = mean(Latitude, na.rm = T),
          lon = mean(Longitude, na.rm = T)) 
      
      
      # Read in acoustic profiles and process them into smoothed Sv profiles for making section plots
      
      files <- list.files(path="ATKA24 DATA/AZFP_nano/Final", full.names = TRUE, pattern= "*.csv") 
      
      svdat <- read.csv("ATKA24 DATA/AZFP_nano/Final/ATKA24_01_CTD1_AZFP_0.5m_Sv_corr.csv")
      
      Sv_label <- expression(paste("Sv [dB re 1/m]"))
      
      p5 <- 
        svdat %>%
        group_by(depth_true) %>%
        filter(range > 0.5) %>%
        filter(depth_true < 150) %>%
        summarise(sv = log10(mean(10^Sv_mean, na.rm=T)),
                  min = min(Sv_min, na.rm = T),
                  max = max(Sv_max, na.rm = T)) %>%
        
        ggplot(aes(x = depth_true, y = sv)) +
        scale_y_continuous() + 
        geom_line() + 
        #  geom_line(aes(x = depth_true, y=min), inherit.aes = F , color="red") + 
        #  geom_line(aes(x = depth_true, y=max), inherit.aes = F , color="blue") + 
        coord_flip()+ scale_x_reverse() +
        ylab(Sv_label) + 
        theme_2
      
      
      
      
      
      
      
      
      
      
      
     # create an oceanographic section
      IK.section <- CTD.all %>% filter(Site %in% c("ATKA24_02_CTD",
                                                   "ATKA24_03_CTD",
                                                   "ATKA24_05_CTD",
                                                   "ATKA24_06_CTD",
                                                   "ATKA24_07_CTD",
                                                   "ATKA24_08_CTD",
                                                   "ATKA24_09_CTD",
                                                   "ATKA24_10_CTD",
                                                   "ATKA24_11_CTD", 
                                                   "ATKA24_12_CTD",
                                                   "ATKA24_13_CTD",
                                                   "ATKA24_14_CTD")) %>%
        
        dplyr::select(salinity, temperature, pressure, relative.light, CLW_chl_flu, turbidity, CLW_turb, Longitude, Latitude, Site) %>%
        mutate(longitude = Longitude,
               latitude = Latitude,
               station = Site,
               fluorescence = CLW_chl_flu,
               light = relative.light,
               turbidity.RBR = turbidity,
               turbidity.CLW = CLW_turb) %>%
        dplyr::select(-Longitude, -Latitude, -Site, -CLW_chl_flu, -relative.light, -turbidity, -CLW_turb )
      
      
      station_list <- split(IK.section, IK.section$station)
      
      ctd_list <- lapply(station_list, function(IK.section) {
        # Create initial CTD object
        ctd <- as.ctd(
          salinity = IK.section$salinity,
          temperature = IK.section$temperature,
          pressure = IK.section$pressure-10,
          longitude = unique(IK.section$longitude),
          latitude = unique(IK.section$latitude)
        )
        
        # Identify and add any additional variables (excluding core CTD ones)
        core_vars <- c("salinity", "temperature", "pressure", "longitude", "latitude", "station", "time")
        extra_vars <- setdiff(names(IK.section), core_vars)
        
        for (var in extra_vars) {
          ctd[[var]] <- IK.section[[var]]
        }
        
        return(ctd)
      })
      
      
      ctd.section <- as.section(ctd_list)
      


      plot(ctd.section, which = "turbidity.RBR", ztype = "image", ylim= c(200,0), showStations = TRUE)
      
                  col = oceColorsJet,              # color palette
                  contour = F,                  # add contour lines
                  filledContour = TRUE,            # use filled contours
                  zlab = "Fluorescence (RFU)",     # label for z-axis
                  


      

      
      ctd.section <- as.section(ctd_list)
      
      plot(ctd.section, which='fluorscence', xtype='distance', ztype = "image", showStations = TRUE)
      

    # 2) Load in profiles 

    # 3) Generate Section