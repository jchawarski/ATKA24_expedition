# ATKA 24 Expedition - Oceanographic sections of biological parameters

require(tidyverse)
require(oce)

setwd("C:/Users/jchawarski/OneDrive - ASL Environmental Sciences Inc/Projects/Atka Expedition - SW Greenland")

      
Sv_label <- expression(paste("Sv [dB re 1 m-1]"))

      
      

      # Istertup Kangertiva    filter just files with profiles along fjord
      files <- list.files(path="ATKA24 DATA/All Parameters Profiles", full.names = TRUE, pattern= "*.csv") 
      files <- files[4:17]
      files <- files[-3]
      
      
      #Nagtivit Kangertiva - filter files with profiles from along left fjord
      files <- list.files(path="ATKA24 DATA/All Parameters Profiles", full.names = TRUE, pattern= "*.csv") 
      files <- files[c(18, 23:26, 32:34, 37:40)]
      
      #All files
      files <- list.files(path="ATKA24 DATA/All Parameters Profiles", full.names = TRUE, pattern= "*.csv") 
      
      
      
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
      
      
      SV <- lapply(files, function(i){read.csv(i)})                          # concatenates all AZFP-nano files into large list
      
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
require(TTR)
      
      
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
        
            filter(
              case_when(
                Site == "ATKA24_02_CTD" ~ depth_true < 438,    # ATKA24_02_CTD bathy depth is 439 m
                Site == "ATKA24_05_CTD" ~ depth_true < 352,    # ATKA24_05_CTD bathy depth is 352 m
                Site == "ATKA24_06_CTD" ~ depth_true < 396,    # ATKA24_06_CTD bathy depth is 397 m
                Site == "ATKA24_07_CTD" ~ depth_true < 423,    # ATKA24_07_CTD bathy depth is 423 m
                Site == "ATKA24_08_CTD" ~ depth_true < 434,    # ATKA24_08_CTD bathy depth is 434 m
                Site == "ATKA24_09_CTD" ~ depth_true < 262,    # ATKA24_09_CTD bathy depth is 262 m
                Site == "ATKA24_12_CTD" ~ depth_true < 257,    # ATKA24_02_CTD bathy depth is 257 m
                Site == "ATKA24_13_CTD" ~ depth_true < 254,    # no bathy depth
                Site == "ATKA24_14_CTD" ~ depth_true < 162,    # ATKA24_02_CTD bathy depth is 162 m
                Site == "ATKA24_15_CTD" ~ depth_true < 302,    # no bathy depth
                TRUE ~ TRUE
                )
              ) %>%
          
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
        
        
        # assign fjords to sites
        
          SV.smooth <- SV.smooth %>% mutate(fjord = case_when(Station %in% c("02", "03", "04","05","06", "07", "08","09", "10", "11", "12", "13") ~ "IK",
                                                            Station %in% c("16", "17", "18", "21", "22", "23", "24", "29", "30", "31", "32", "36", "36", "38") ~ "NK", 
                                                            Station %in% c("47", "48", "49", "50") ~ "Sermilik",
                                                            FALSE ~ NA))
        
        

          # PLOT all profiles from section
          
          sv.plot <- 
          
          SV.smooth %>% filter(!fjord %in% NA) %>% filter(!fjord %in% "Sermilik") %>%
            filter(!Site %in% "ATKA24_18_CTD") %>%
            
            ggplot(aes(x = depth_true, y = sv_smooth, color= fjord, group=Station)) +
            scale_y_continuous(breaks = seq(-95,-55,5), expand = c(0,0)) + 
            scale_color_manual(values = c( "#215F9A", "#80350E")) + 
            geom_line(size=1, alpha=0.8) + 
            #  geom_line(aes(x = depth_true, y=min), inherit.aes = F , color="red") + 
            #  geom_line(aes(x = depth_true, y=max), inherit.aes = F , color="blue") + 
            coord_flip()+ 
            scale_x_reverse(limits=c(400,0), breaks = seq(0,400,50), expand = c(0,0)) +
            ylab(Sv_label) + 
            xlab("Depth [m]") + 
             
            theme_bw()  + theme(legend.position = "none")

            ggsave("ATKA24_IKNK_Sv_profiles.png", plot=sv.plot, height=5, width=3, dpi=400)
 
          
 # LIGHT AND ACOUSTIC ANALYSIS
          
  # create a summary table that defines a threshold for the upper level of the krill scattering layer
          # use the rule to define upper scattering layer depth such that any value below 80 m that crosses the -75dB threshold
         
          
          # basic threshold based algo 
        sl.depth <- SV.smooth %>%  filter(fjord %in% c("IK", "NK")) %>% 
          group_by(Site) %>%
          arrange(depth_true) %>%
          filter(depth_true >= 80, !is.na(sv_smooth), sv_smooth >= -75) %>%
          slice(1) %>%
          ungroup() %>%
          select(Site, depth_true, fjord, sv_smooth)
                
          
          sl.depth %>% ggplot() + geom_histogram(aes(x=depth_true, fill=fjord))
          
          # more complex elbow based approach - NEEDS WORK, still not selecting the elbow properly
          sl.depth <- SV.smooth %>% filter(fjord %in% c("IK", "NK")) %>%
            group_by(Site) %>%
            arrange(depth_true) %>%
            filter(depth_true >= 50, !is.na(sv_smooth)) %>%
            mutate(
              future_max_sv = cummax(rev(sv_smooth)) |> rev(),
              delta_sv = future_max_sv - sv_smooth
            ) %>%
            filter(delta_sv >= 10) %>%              # must increase by ≥ 5 later
            slice_min(sv_smooth, n = 1) %>%         # pick the elbow (lowest Sv)
            ungroup() %>%
            select(Site, depth_true,fjord, sv_smooth, delta_sv)
          
          # another attempt
          change_points <- SV.smooth %>%
            filter(fjord %in% c("IK", "NK")) %>%
            group_by(Site, fjord) %>%
            arrange(depth_true) %>%
            filter(depth_true >= 50, !is.na(sv_smooth)) %>%
            mutate(
              sv_20m = sv_smooth[findInterval(depth_true + 15, depth_true)],
              delta_20m = sv_20m - sv_smooth
            ) %>%
            filter(delta_20m >= 5) %>%
            summarise(
              change_depth = first(depth_true),
              change_sv    = first(sv_smooth),
              delta_20m    = first(delta_20m),
              .groups = "drop"
            ) 
          
            light_at_sv_depth <- change_points %>%
            inner_join(CTD.all, by = "Site") %>%
            mutate(depth_diff = abs(depth - change_depth)) %>%
            group_by(Site, fjord.x) %>%
            slice_min(depth_diff, n = 1, with_ties = FALSE) %>%
            ungroup() %>%
            select(
              Site,
              fjord.x,
              change_depth,
              depth_light = depth,
              relative.light,
              CLW_chl_flu,
              CLW_chl_a,
              CLW_turb
            )  
          
          
       sv.light.dat <- light_at_sv_depth %>% dplyr::filter(!Site %in% c("ATKA24_36_CTD", "ATKA24_38_CTD", "ATKA24_29_CTD", "ATKA24_24_CTD", "ATKA24_04_CTD"))  
       
       
       # start values for nls
       start_vals <- list(
         a = max(sv.light.dat$relative.light, na.rm = TRUE) - min(sv.light.dat$relative.light, na.rm = TRUE),
         c = min(sv.light.dat$relative.light, na.rm = TRUE),
         b = 1 / diff(range(sv.light.dat$change_depth, na.rm = TRUE))
       )
       
       # fit the exponential decay model: relative.light decreases with depth
       fit <- nls(
         relative.light ~ a * exp(-b * change_depth) + c,
         data = sv.light.dat,
         start = start_vals
       )
       
       # create new data for smooth curve
       newdat <- tibble(
         change_depth = seq(
           min(sv.light.dat$change_depth, na.rm = TRUE),
           max(sv.light.dat$change_depth, na.rm = TRUE),
           length.out = 200
         )
       )
       
       # predict from the nls model
       newdat <- newdat %>% mutate(fit = predict(fit, newdata = newdat))
       
       
       
        light.sv.plot <-    
       sv.light.dat %>%
         ggplot(aes(x = change_depth, y = relative.light, color = fjord.x)) +
         geom_point(size=2) +
         scale_color_manual(values = c(  "#215F9A", "#80350E")) + 
         geom_line(data = newdat, aes(x = change_depth, y = fit), linewidth = 1, inherit.aes = F, alpha=0.5, color="grey50") +
         #scale_y_continuous(expand = c(0,0)) +
         #scale_x_continuous(expand = c(0,0)) +
         xlab("Upper Limit SL Depth (m)") +
         ylab("Light [rel]") +
         theme_bw() + theme(legend.position = "none")
       
     ggsave("ATKA24_light-sv_plot.png", width=3, height=3, dpi=300)  
       
     SV.smooth %>% filter(fjord %in% (c( "NK", "IK"))) %>% #filter(Site %in% "ATKA24_21_CTD") %>%
       
       ggplot(aes(x = depth_true, y = sv_smooth, color= Site, group=Station)) +
       scale_y_continuous() + 
       
       geom_line() + 
       geom_point(data=change_points, aes(x=change_depth, y=change_sv), inherit.aes=F) + 
       #  geom_line(aes(x = depth_true, y=min), inherit.aes = F , color="red") + 
       #  geom_line(aes(x = depth_true, y=max), inherit.aes = F , color="blue") + 
       coord_flip()+ scale_x_reverse() +
       
       ylab(Sv_label) + 
       xlab("Depth [m]") + 
       xlim(400,0) + 
       theme_bw()  + 
     facet_wrap(~Site)
     
     
     
     ### plot light profiles
     fjords <- SV.smooth %>% group_by(Site) %>% summarise(fjord = first(fjord))
     CTD.all <- CTD.all %>% left_join(fjords, by="Site")
     
     p1 <- 
    CTD.all %>% filter(fjord %in% c("IK", "NK")) %>% dplyr::filter(between(depth,0,150)) %>%
     ggplot(aes(x = depth, y = relative.light, color=fjord, group=Site)) +
       #scale_y_sqrt() + 
       geom_point() + 
       #geom_smooth(aes(group=fjord)) + 
       #  geom_line(aes(x = depth_true, y=min), inherit.aes = F , color="red") + 
       #  geom_line(aes(x = depth_true, y=max), inherit.aes = F , color="blue") + 
       coord_flip()+ scale_x_reverse() + #xlim(200,0) +  
      theme_bw() + theme(legend.position = "none")
     
      ### plot turbidity profiles
     p2 <- 
     CTD.all %>% filter(fjord %in% c("IK", "NK")) %>%
       ggplot(aes(x = depth, y = turbidity, color=fjord, group=Site)) +
       #scale_y_sqrt() + 
       geom_line() + 
       #geom_smooth(aes(group=fjord)) + 
       #  geom_line(aes(x = depth_true, y=min), inherit.aes = F , color="red") + 
       #  geom_line(aes(x = depth_true, y=max), inherit.aes = F , color="blue") + 
       coord_flip()+ scale_x_reverse() + xlim(200,0) +  
       theme_bw() + theme(legend.position = "none")
     
     
     ### plot fluorescence profiles
     
     p3 <-
     CTD.all %>% filter(fjord %in% c("IK", "NK")) %>%
       ggplot(aes(x = depth, y = CLW_chl_a, color=fjord, group=Site)) +
       #scale_y_sqrt() + 
       geom_line() + 
       #geom_smooth(aes(group=fjord)) + 
       #  geom_line(aes(x = depth_true, y=min), inherit.aes = F , color="red") + 
       #  geom_line(aes(x = depth_true, y=max), inherit.aes = F , color="blue") + 
       coord_flip()+ scale_x_reverse() + xlim(100,0) +  ylim(0,4) + 
       theme_bw()
     
     # plot temperature profiles
     
     CTD.all %>% filter(fjord %in% c("IK", "NK")) %>% filter(between(depth, 0, 10)) %>%
       ggplot(aes(x = depth, y = temperature, color=fjord, group=Site)) +
       #scale_y_sqrt() + 
       geom_line() + 
       #geom_smooth(aes(group=fjord)) + 
       #  geom_line(aes(x = depth_true, y=min), inherit.aes = F , color="red") + 
       #  geom_line(aes(x = depth_true, y=max), inherit.aes = F , color="blue") + 
       coord_flip()+ scale_x_reverse() + xlim(10,0) +  
       theme_bw()
     
     
     
     # Temperature
     temp.plot <- 
     
     CTD.all %>% filter(fjord %in% c("IK", "NK")) %>% #filter(!Site %in% "ATKA24_13_CTD") %>%
       ggplot(aes(x = depth, y = temperature, color=fjord, group=Site)) +
       geom_line() + 
       scale_color_manual(values = c(  "#215F9A", "#80350E")) + 
       
       coord_flip()+ scale_x_reverse(breaks = seq(0,300, 50), limits=c(300,0), expand = c(0,0)) +   
       scale_y_continuous(limits = c(-1.5, 6), expand = c(0,0)) + 
       xlab("Depth [m]") + ylab("Temp. [°C]") + 
       theme_bw() + theme(legend.position = "none")
     
     ggsave("ATKA24_temp_profiles.png", plot=temp.plot, height=3, width=3, dpi=400)
     
     
     
     
     # Fluorescence
     
     flu.plot <- 
     CTD.all %>% filter(fjord %in% c("IK", "NK")) %>%
       ggplot(aes(x = depth, y = CLW_chl_a, color=fjord, group=Site)) +
       #scale_y_sqrt() + 
       geom_line() + 
       scale_color_manual(values = c(  "#215F9A", "#80350E")) + 
       coord_flip()+ 
       scale_x_reverse(breaks = seq(0,100, 25), limits=c(100,0), expand = c(0,0)) +
       scale_y_continuous(limits = c(0, 4), expand = c(0,0)) + 
       ylab("Chl-a [μg/L]") + xlab("Depth [m]") + 
       theme_bw() + theme(legend.position = "none")
     
     ggsave("ATKA24_flu_profiles.png", plot=flu.plot, height=3, width=3, dpi=400)
     
     
     require(cowplot)
     plot_grid(p1,p2,p3, rel_widths = c(2,1,1), align="hv", nrow=1)
     
     
      
          

      
##### CREATE OCEANOGRAPHIC SECTIONS
     
     
     #Isertup Kangertiva
          {

      IK.section <- CTD.all %>% filter(Site %in% c(unique(SV.smooth$Site))) %>%
        
        
        filter(Site %in% c(                        "ATKA24_03_CTD",
                                                   "ATKA24_05_CTD",
                                                   "ATKA24_06_CTD",
                                                   "ATKA24_07_CTD",
                                                   "ATKA24_08_CTD",
                                                   "ATKA24_02_CTD",
                                                   "ATKA24_09_CTD",
                                                   "ATKA24_10_CTD",
                                                   "ATKA24_11_CTD", 
                                                   "ATKA24_12_CTD",
                                                   "ATKA24_13_CTD",
                                                   "ATKA24_14_CTD")) %>%
                                                   #"ATKA24_15_CTD")) %>%
        
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
          station = factor(station, levels = c("ATKA24_03_CTD",
                                               "ATKA24_05_CTD",
                                               "ATKA24_06_CTD",
                                               "ATKA24_07_CTD",
                                               "ATKA24_08_CTD",
                                               "ATKA24_02_CTD",
                                               "ATKA24_09_CTD",
                                               "ATKA24_10_CTD",
                                               "ATKA24_11_CTD", 
                                               "ATKA24_12_CTD",
                                               "ATKA24_13_CTD",
                                               "ATKA24_14_CTD" )) 
                                               #"ATKA24_15_CTD")) 
          
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
      
      
      #create string of depths corresponding to each station
      
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
                  162) #14
                  #302) #15
      
      # assign depth to each station
      
      for (i in seq_along(ctd.section[["station"]])) {
        ctd.section[["station"]][[i]][["metadata"]][["waterDepth"]] <- depths[i]
      }
      
     }

      
     #NAGTIVIT KANGERTIVAT
          { 
         NK.section <- CTD.all %>% filter(Site %in% c(unique(SV.smooth$Site))) %>%
           
           filter(fjord %in% "NK") %>%
    
          filter(!Site %in% "ATKA24_29_CTD") %>%
           
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
         
         NK.summary <- NK.section %>%
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
             station = factor(station, levels = c("ATKA24_18_CTD",
                                                  "ATKA24_17_CTD",
                                                  "ATKA24_16_CTD",
                                                  "ATKA24_21_CTD",
                                                  "ATKA24_23_CTD",
                                                  "ATKA24_24_CTD",
                                                  #"ATKA24_29_CTD",
                                                  "ATKA24_30_CTD",
                                                  "ATKA24_31_CTD", 
                                                  "ATKA24_32_CTD",
                                                  "ATKA24_36_CTD",
                                                  "ATKA24_38_CTD" )) 
             #"ATKA24_15_CTD")) 
             
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
         
         station_list <- split(NK.summary, NK.summary$station)
         
         ctd_list <- lapply(station_list, function(NK.summary) {
           # Create initial CTD object
           ctd <- as.ctd(
             salinity = NK.summary$salinity,
             temperature = NK.summary$temperature,
             pressure = NK.summary$pressure-10,
             longitude = unique(NK.summary$Longitude),
             latitude = unique(NK.summary$Latitude),
             station = unique(NK.summary$station)
           )
           
           # Identify and add any additional variables (excluding core CTD ones)
           core_vars <- c("salinity", "temperature", "pressure", "longitude", "latitude", "station", "time")
           extra_vars <- setdiff(names(NK.summary), core_vars)
           
           for (var in extra_vars) {
             ctd[[var]] <- NK.summary[[var]]
           }
           
           return(ctd)
         })
         
         
         ctd.section <- as.section(ctd_list)
         
         
}
     
     
     
      # BASIC SECTION plotting using native oce plotting function

      station_names <- as.character(unique(NK.summary$station)[1:13])
      
      plot(ctd.section, labels = station_names, 
           which = "sv_smooth",
           ztype = "contour", 
           ytype = "depth")
      
      # Acoustic backscatter (SV)
      
      png(
        "ATKA_24_NK_Sv_section.png",
        width = 2400,
        height = 1600,
        res = 300
      )
      
      plot(ctd.section, labels = station_names, 
           which = "sv_smooth",
           ztype = "image", 
           ytype = "depth",
           ylim = c(400,0),
           legend.loc = "",
           showStations = T,
           #stationIndices = 1:12,
           zbreaks = seq(-90, -55, 1), 
           zcol = oceColorsViridis)
      
    dev.off()

      # Turbidity
    
    
      png(
        "ATKA_24_NK_turb_section.png",
        width = 2400,
        height = 1600,
        res = 300
      )
    
    
      plot(ctd.section, labels = station_names, 
           which = "turbidity.RBR",
           ztype = "image", 
           ytype = "depth",
           legend.loc = "",
           ylim = c(400,0),
           zbreaks = seq(0, 2, 0.2), zcol = oceColorsTemperature
      )
      
      
      dev.off()
      
      
      # Fluorescence
      
      
      png(
        "ATKA_24_IK_flu_section.png",
        width = 2400,
        height = 1600,
        res = 300
      )
      
      
      
      plot(ctd.section, labels = station_names, 
           which = "fluorescence",
           ztype = "image", 
           ytype = "depth",
           ylim = c(400,0),
           legend.loc = "",
           zbreaks = seq(0, 3, 0.05), zcol = oceColorsChlorophyll
      )
      
      
      
      dev.off()
      
      
      # Light
      
      png(
        "ATKA_24_NK_light_section.png",
        width = 2400,
        height = 1600,
        res = 300
      )
      
      
      plot(ctd.section, labels = station_names, 
           which = "light",
           ztype = "image", 
           ytype = "depth",
           legend.loc = "",
           ylim = c(400,0),
           zbreaks = seq(40, 190, 10), zcol = oceColorsPAR
      )
      
      
      dev.off()
      

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      

      
      
       
      

