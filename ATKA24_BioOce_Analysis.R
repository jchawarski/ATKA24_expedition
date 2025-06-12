# ATKA 24 Expedition - Oceanographic sections of biological parameters

require(tidyverse)
require(oce)

setwd("C:/Users/jchawarski/OneDrive - ASL Environmental Sciences Inc/Projects/Atka Expedition - SW Greenland")

      

      
      

      # Istertup Kangertiva    filter just files with profiles along fjord
      files <- list.files(path="ATKA24 DATA/All Parameters Profiles", full.names = TRUE, pattern= "*.csv") 
      files <- files[4:17]
      files <- files[-3]
      
      
      #Nagtivit Kangertiva - filter files with profiles from along left fjord
      files <- list.files(path="ATKA24 DATA/All Parameters Profiles", full.names = TRUE, pattern= "*.csv") 
      files <- files[c(18, 23:26, 32:34, 37:40)]
      
      
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
     { CTD.sum.tbl <-  CTD.all %>% filter(depth <= 100) %>%               # filters all data below 100 m depth
        group_by(Site) %>%                               # group fnx, allows headers to carry over
        summarise(
          avgT = mean(temperature), 
          avgS = mean(salinity), 
          lat = mean(Latitude, na.rm = T),
          lon = mean(Longitude, na.rm = T)) }
      
      
      # READ IN ACOUSTIC PROFILES and process them into smoothed Sv profiles for making section plots
      
      files <- list.files(path="ATKA24 DATA/AZFP_nano/Final", full.names = TRUE, pattern= "*.csv") 
      
      
      SV <- lapply(files, function(i){read.csv(i)})                          # concatenates and trims upcast from all CTD files into large list
      
      # create empty df for metadata
      meta.tbl <- setNames(data.frame(matrix(ncol = 2, nrow = length(files))), c("Site", "ID"))
      meta.tbl[,1] <-  paste(substr(files, 29, 41))             # selects and trims file name, put in in blank matrix
      #adjust names of first 3 sites
      meta.tbl$Site[1:3] <- c("ATKA24_01_CTD1", 
                              "ATKA24_01_CTD2", 
                              "ATKA24_01_CTD3")
      
      uniqueID <- c(1:length(files))     # create unique ID by cast
      meta.tbl[,2] <-  uniqueID          # assigns a unique ID
      
      
      SV.all <- lapply(SV, function(i){ rbind(unique(data.frame(i))) }) %>%  # creates a list of dataframes (of each cast)
        mapply(cbind, ., ID = uniqueID, SIMPLIFY = F) %>%                    # adds ID column with uniqueID to each df in list
        bind_rows() %>%                                                      # binds all dfs
        left_join(meta.tbl, ., by="ID") %>%                                  # joins 
        
        # compute averages for each depth bin
        
        group_by(Site, depth_true) %>%
        filter(range > 0.5) %>%
        #filter(depth_true < 150) %>%
        summarise(sv = log10(mean(10^Sv_mean, na.rm=T)),
                  min = min(Sv_min, na.rm = T),
                  max = max(Sv_max, na.rm = T)) %>%
        left_join(sites, by="Site")
        
      
      
      
      
      
      
      
      
      # plot profiles
      
      filter(Site %in% c("ATKA24_02_CTD",
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
      
      
        
      
      SV.all %>% filter(Site %in% c("ATKA24_36_CTD")) %>%
        #filter(depth_true >150) %>%
        ggplot(aes(x = depth_true, y = sv)) +
        scale_y_continuous() + 
        geom_line() + 
        #  geom_line(aes(x = depth_true, y=min), inherit.aes = F , color="red") + 
        #  geom_line(aes(x = depth_true, y=max), inherit.aes = F , color="blue") + 
        coord_flip()+ scale_x_reverse() +
        ylab(Sv_label) 
        #theme_2 + 
        #facet_wrap(~Site)
      
      
      
      
        # site wise filtering
      
      SV.smooth <- 
        
      
      SV.all %>%
            
         
           # SELECT only Isterup Kangertivat Sites
        
        #   { filter(Site %in% c("ATKA24_02_CTD",
          #                     "ATKA24_03_CTD",
           #                    "ATKA24_05_CTD",
            #                   "ATKA24_06_CTD",
             #                  "ATKA24_07_CTD",
              #                 "ATKA24_08_CTD",
               #                "ATKA24_09_CTD",
                #               "ATKA24_10_CTD",
                 #              "ATKA24_11_CTD", 
                  #             "ATKA24_12_CTD",
                   #            "ATKA24_13_CTD",
                    #           "ATKA24_14_CTD", 
                     #          "ATKA24_15_CTD")) %>% }
            
            filter(Site %in% unique(CTD.all$Site)) %>% 
        
        
            # REMOVE depths below seafloor and identify seafloor depths - Isterup Kangertivat
        
          #  filter(
           #   case_when(
            #    Site == "ATKA24_02_CTD" ~ depth_true < 438,    # ATKA24_02_CTD bathy depth is 439 m
             #   Site == "ATKA24_05_CTD" ~ depth_true < 352,    # ATKA24_05_CTD bathy depth is 352 m
              #  Site == "ATKA24_06_CTD" ~ depth_true < 396,    # ATKA24_06_CTD bathy depth is 397 m
               # Site == "ATKA24_07_CTD" ~ depth_true < 423,    # ATKA24_07_CTD bathy depth is 423 m
                #Site == "ATKA24_08_CTD" ~ depth_true < 434,    # ATKA24_08_CTD bathy depth is 434 m
  #              Site == "ATKA24_09_CTD" ~ depth_true < 262,    # ATKA24_09_CTD bathy depth is 262 m
   #             Site == "ATKA24_12_CTD" ~ depth_true < 257,    # ATKA24_02_CTD bathy depth is 257 m
    #            Site == "ATKA24_13_CTD" ~ depth_true < 254,    # no bathy depth
     #           Site == "ATKA24_14_CTD" ~ depth_true < 162,    # ATKA24_02_CTD bathy depth is 162 m
      #          Site == "ATKA24_15_CTD" ~ depth_true < 302,    # no bathy depth
       #         TRUE ~ TRUE
        #        )
         #     ) %>%
          
             # REMOVE depths below seafloor and identify seafloor depths - Nagtivit Kangertivat
        
            filter(
              case_when(
                Site == "ATKA24_21_CTD" ~ depth_true < 357,
                Site == "ATKA24_23_CTD" ~ depth_true < 320,    # ATKA24_23_CTD bathy depth is 320 m
                Site == "ATKA24_24_CTD" ~ depth_true < 308,    # ATKA24_24_CTD bathy depth is 308 m
                Site == "ATKA24_30_CTD" ~ depth_true < 350,
                Site == "ATKA24_31_CTD" ~ depth_true < 405,
                Site == "ATKA24_36_CTD" ~ depth_true < 250,
                Site == "ATKA24_37_CTD" ~ depth_true < 205,    # ATKA24_37_CTD bathy depth is 205 m
                TRUE ~ TRUE
              )
            ) %>%
        
        
        
          
          # DESPIKE using a median filter with a 101 sample (50 m window)
          
          group_by(Site) %>%
            mutate(sv_smooth = despike(sv, 
                                       reference = "median", 
                                       n=1, 
                                       k=101, 
                                       replace="reference")) %>%
          ungroup() %>%
          
          
          # SMOOTH with a moving average
        
          group_by(Site)  %>%
            mutate(sv_smooth = SMA(sv_smooth, n=30)) %>%
        
          
          ungroup() # %>%
        
        
          # PLOT all profiles from section
        
        
            ggplot(aes(x = depth_true, y = sv_smooth)) +
            scale_y_continuous() + 
            geom_line() + 
            #  geom_line(aes(x = depth_true, y=min), inherit.aes = F , color="red") + 
            #  geom_line(aes(x = depth_true, y=max), inherit.aes = F , color="blue") + 
            coord_flip()+ scale_x_reverse() +
            ylab(Sv_label) +
            #theme_2 + 
            facet_wrap(~Site)
      
      
      # basic section       
      {station_list <- split(SV.smooth, SV.smooth$Site)
      
      ctd_list <- lapply(station_list, function(SV.smooth) {
        # Create initial CTD object
        ctd <- as.ctd(
          #salinity = SV.smooth$salinity,
          #temperature = SV.smooth$temperature,
          #backscatter = SV.smooth$sv_smooth,
          pressure = SV.smooth$depth_true,
          longitude = unique(SV.smooth$Longitude),
          latitude = unique(SV.smooth$Latitude)
        )
        
        return(ctd)
      })
      
            }
      
      
      
      
      
      
      
     # create an oceanographic section
      IK.section <- CTD.all %>% filter(Site %in% c(unique(SV.smooth$Site))) %>%
        
        
   #     filter(Site %in% c("ATKA24_03_CTD",
           #                                        "ATKA24_05_CTD",
            #                                       "ATKA24_06_CTD",
             #                                      "ATKA24_07_CTD",
              #                                     "ATKA24_08_CTD",
               #                                    "ATKA24_02_CTD",
                #                                   "ATKA24_09_CTD",
                 #                                  "ATKA24_10_CTD",
                  #                                 "ATKA24_11_CTD", 
                   #                                "ATKA24_12_CTD",
                    #                               "ATKA24_13_CTD",
                     #                              "ATKA24_14_CTD",
                      #                             "ATKA24_15_CTD")) %>%
        
        dplyr::select(depth, salinity, temperature, pressure, relative.light, CLW_chl_flu, turbidity, CLW_turb, Longitude, Latitude, Site) %>%
        mutate(longitude = Longitude,
               latitude = Latitude,
               station = Site,
               fluorescence = CLW_chl_flu,
               light = relative.light,
               turbidity.RBR = turbidity,
               turbidity.CLW = CLW_turb) %>%
        dplyr::select(-Longitude, -Latitude, -Site, -CLW_chl_flu, -relative.light, -turbidity, -CLW_turb )
      
      
      # AVERAGE each measurement into a discrete depth interval for joining with SV dataset
      
      IK.summary <- IK.section %>%
        # 1. Create depth interval bins (e.g., 0–0.5, 0.5–1.0, etc.)
        mutate(
          depth_bin = cut(
            depth,
            breaks = seq(0, max(depth, na.rm = TRUE) + 0.5, by = 0.5),
            right = FALSE,
            include.lowest = TRUE
          ),
          depth_true = as.numeric(sub("\\[|\\)|\\]", "", sub(",.*", "", depth_bin))),   # extract lower bound
        
           # ARRANGE in geographic order!
        #  station = factor(station, levels = c("ATKA24_03_CTD",
          #                                     "ATKA24_05_CTD",
           #                                    "ATKA24_06_CTD",
            #                                   "ATKA24_07_CTD",
             #                                  "ATKA24_08_CTD",
              #                                 "ATKA24_02_CTD",
               #                                "ATKA24_09_CTD",
                #                               "ATKA24_10_CTD",
                 #                              "ATKA24_11_CTD", 
                  #                             "ATKA24_12_CTD",
                   #                            "ATKA24_13_CTD",
                    #                           "ATKA24_14_CTD", 
                     #                          "ATKA24_15_CTD")) 
          
          ) %>%
        
        # 2. Group by bin_lower (numeric lower bound)
        group_by(station, depth_true) %>%
        
        # 3. Summarise variables
        summarise(
          salinity = mean(salinity, na.rm = TRUE),
          temperature = mean(temperature, na.rm = TRUE),
          pressure = mean(pressure, na.rm = TRUE),
          fluorescence = mean(fluorescence, na.rm = TRUE),
          light = mean(light, na.rm = TRUE),
          turbidity.RBR = mean(turbidity.RBR, na.rm = TRUE),
          turbidity.CLW = mean(turbidity.CLW, na.rm = TRUE),
          .groups = "drop"
        )  %>%
        
        mutate(Site = station) %>%
        
        right_join(SV.smooth, by=c("Site", "depth_true")) %>%
        
        dplyr::select(-Site, -Station)

      # CREATE OCEANOGRAPHIC SECTION
      
      station_list <- split(IK.summary, IK.summary$station)
      
      ctd_list <- lapply(station_list, function(IK.summary) {
        # Create initial CTD object
        ctd <- as.ctd(
          salinity = IK.summary$salinity,
          temperature = IK.summary$temperature,
          pressure = IK.summary$pressure-10,
          longitude = unique(IK.summary$Longitude),
          latitude = unique(IK.summary$Latitude),
          station = unique(IK.summary$station)
        )
        
        # Identify and add any additional variables (excluding core CTD ones)
        core_vars <- c("salinity", "temperature", "pressure", "longitude", "latitude", "station", "time")
        extra_vars <- setdiff(names(IK.summary), core_vars)
        
        for (var in extra_vars) {
          ctd[[var]] <- IK.summary[[var]]
        }
        
        return(ctd)
      })
      
      
      ctd.section <- as.section(ctd_list)
      
      
      # modify the bottom depth
      
      ik.lat <- unique(IK.summary$Latitude)
      ik.lon <- unique(IK.summary$Longitude)
      ik.sites <- unique(IK.summary$station)

      
      dist <- geodDist(ik.lon, ik.lat, alongPath=F)
      
      bottom <- get.depth(bath, x=ik.lon, y=ik.lat, locator=F)
      
      # IK
      bottom$sites <- ik.sites[1:13]
      # NK
      bottom$sites <- ik.sites[1:11]
      
      depths <- c(316, #03
                  352, #05
                  397, #06
                  423, #07
                  434, #08
                  439, #02
                  262, #09
                  403, #10,
                  361, #11
                  257, #12
                  254, #13
                  162, #14
                  302) #15
      
      
      # NK
      
      depths <- -bottom$depth
      
      # assign depth to each station
      
      
      for (i in seq_along(ctd.section[["station"]])) {
        ctd.section[["station"]][[i]][["metadata"]][["waterDepth"]] <- depths[i]
      }
      
      
      
      
      
      # BASIC SECTION plotting using native oce plotting function

      station_names <- as.character(unique(IK.summary$station)[1:13])
      
      plot(ctd.section, labels = station_names, 
           which = "sv_smooth",
           ztype = "contour", 
           ytype = "depth")
      
      # SV 
      plot(ctd.section, labels = station_names, 
           which = "sv_smooth",
           ztype = "image", 
           ytype = "depth",
           ylim = c(400,0),
           zbreaks = seq(-90, -55, 2), zcol = oceColorsViridis
      )
      # Turbidity 
      plot(ctd.section, labels = station_names, 
           which = "turbidity.RBR",
           ztype = "image", 
           ytype = "depth",
           ylim = c(400,0),
           zbreaks = seq(0, 3, 0.2), zcol = oceColorsTemperature
      )
      # Fluorescence
      plot(ctd.section, labels = station_names, 
           which = "fluorescence",
           ztype = "image", 
           ytype = "depth",
           ylim = c(400,0),
           zbreaks = seq(0, 3, 0.05), zcol = oceColorsChlorophyll
      )
      
      # Light
      plot(ctd.section, labels = station_names, 
           which = "light",
           ztype = "image", 
           ytype = "depth",
           ylim = c(400,0),
           zbreaks = seq(40, 190, 10), zcol = oceColorsPAR
      )
      
      
      
      
      zbreaks = seq(-80, -55, 5), zcol = oceColorsTemperature
      
      
      # ADVANCED SECTION plotting using imagep function
      
      s <- sectionGrid(ctd.section, p='levitus')
      
      nstation <- length(s[['station']])
      p <- unique(s[['pressure']])
      np <- length(p)
      T <- S <- array(NA, dim=c(nstation, np))
      for (i in 1:nstation) {
        T[i, ] <- s[['station']][[i]][['temperature']]
        S[i, ] <- s[['station']][[i]][['salinity']]
      }
      
      distance <- unique(s[['distance']])
      par(mfrow=c(2, 1))
      imagep(distance, p, T, col=oceColorsTemperature, flipy=TRUE)
      imagep(distance, p, S, col=oceColorsSalinity, flipy=TRUE)

      par(mfrow=c(2, 1))
      Tcm <- colormap(T, breaks=seq(-0.5, 0.5, 0.1), col=oceColorsTemperature)
      Scm <- colormap(S, breaks=seq(28, 34, 0.2), col=oceColorsSalinity)
      imagep(distance, p, T, colormap=Tcm, flipy=TRUE,
             ylab='p [dbar]', filledContour=TRUE,
             zlab='temperature [degC]')
      imagep(distance, p, S, colormap=Scm, flipy=TRUE,
             xlab='distance [km]', ylab='p [dbar]', filledContour=TRUE,
             zlab='salinity')
      
      
      
      # ADDING BATHYMETRIC DATA to section plot
      
      
      # get depth points from GEBCO bathy using and then modify with true water depth. 
      
      
      ik.lat <- unique(IK.summary$Latitude)
      ik.lon <- unique(IK.summary$Longitude)
      
      sites <- unique(IK.summary$station)    
      
            dist <- geodDist(ik.lon, ik.lat, alongPath=F)

            bottom <- get.depth(bath, x=ik.lon, y=ik.lat, locator=F)
            
            plot(ctd.section, 
                 which = "",
                 ztype = "image", 
                 ylim= c(500,0),
                 showStations = TRUE,
                 showBottom = TRUE)
            
            
            
            polygon(c(dist, min(dist), max(dist)), c(-bottom$depth, 10000, 10000), col='grey') 
      
      
      mb.bath <- read_sf("Mapping/atka_georeferenced_bathy.mbtiles")
      