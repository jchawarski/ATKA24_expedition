# ATKA Map

library(tidyverse)
library(marmap)
library(rgdal)
library(rgeos)
library(raster)
library(maptools)
library(sf)
library(ggOceanMaps)
library(ggOceanMapsData)


setwd("C:/Users/jchawarski/OneDrive - ASL Environmental Sciences Inc/Projects/Atka Expedition - SW Greenland/ATKA24 DATA/Mapping")



#### FULL MAP ####

  # BASIC overview map with ggoceanmaps function

  basemap(limits =  c(-40, -37, 66.5, 65), 
          land.size=0.5, 
          land.col="lemonchiffon3",
          bathymetry = F,
          bathy.style = "poly_blues", 
          glaciers=F, 
          legend.position = "none",
          rotate=T) + 
  
  # DETAILED overview map with high res coastline and most recent GEBCO bathymetry
    
    # READ the coastline file and create a polygon
    
    coast <- readOGR("Greenland_Coast/Greenland_coast.shp") # BAS sourced Greenland coastline shapefile - this is an rgdal fxn
    # Data Projection: WGS 84 NSIDC Sea Ice Polar Stereographic North, EPSG 3413
    #change the crf
    st_crs(coast) 
    coast <- spTransform(coast, CRS("+init=epsg:4326")) # assign the coastline CRS to match other data (bathymetry etc)
    CP <- as(extent(-39.5, -37.5,65.4, 65.8), "SpatialPolygons") # create a polygon bounding box
    proj4string(CP) <- CRS(proj4string(coast)) # match the projections
    coastline <- gIntersection(CP, coast, byid=TRUE) # clip the coastline to boundaries of polygon
    #plot(coast) # plots entire greenland coast
  
    # READ in the bathymetry data
    
    bath <- readGEBCO.bathy("ATKAbathy/gebco_2024_n66.5_s65.0_w-40.0_e-37.0.nc")  # your GEBCO sourced netcdf - this is a marmap fxn
    bath.f <- fortify.bathy(bath) # this is a marmap fxn
    
    
    # READ in the sites
    
    sites <- read.csv("C:/Users/jchawarski/OneDrive - ASL Environmental Sciences Inc/Projects/Atka Expedition - SW Greenland/ATKA24 DATA/ATKA24_CTD_sites.csv")
    
    # ASSIGN bathymetric breaks and labels for plot
    
    brk <- c(0, -50, -100, -150, -250,-500, -750, -1000)   #define bathymetry breaks
    brk.labels <-c("0-50", "50-100", "100-150",  "150-250", "250-500", "500-750", "750-1000")
    
    # PLOT

      ggplot() +
      geom_contour_filled(data = bath.f,                       #bathymetry - plot this as the bottom layer... add coast and stations after
                          aes(x=x, y=y, z=z),
                          breaks=brk,
                          linewidth=c(0.1),
                          colour="grey70") +
        
      scale_fill_brewer(palette = "Blues", name="Depth [m]", labels=brk.labels  ) + 
      
    
      geom_polygon(data= coastline,                    #coast - high resolution coastline
                         aes(x=long, 
                             y=lat,
                             group=group),
                         fill= "grey60", 
                         colour ="grey50",
                         inherit.aes = F,
                         size=0.5) +
        
        geom_spatial_point(data=sites, aes(x=Longitude, y=Latitude), color="red", 
                           inherit.aes = F, crs=4326, shape=16) +
        
  
        xlab("Longitude") +
        ylab("Latitude") + 
        
        coord_sf(crs = 4326) + 
        scale_x_continuous(limits=c(-39.5, -37.75), expand = c(0, 0)) +
        scale_y_continuous(limits=c(65.4, 65.8), expand = c(0, 0)) + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1))
      

      
      
      
#### FJORD MAPS ####
      
# ISERTUP KANGERTIVA
  
  # DETAILED overview map with high res coastline and most recent GEBCO bathymetry
    
    # READ the coastline file and create a polygon
    
    coast <- readOGR("Greenland_Coast/Greenland_coast.shp") # BAS sourced Greenland coastline shapefile - this is an rgdal fxn
    # Data Projection: WGS 84 NSIDC Sea Ice Polar Stereographic North, EPSG 3413
    CP <- as(extent(-39.5, -38.8, 65.55, 65.8), "SpatialPolygons") # create a polygon bounding box
    proj4string(CP) <- CRS(proj4string(coast)) # match the projections
    coastline <- gIntersection(CP, coast, byid=TRUE) # clip the coastline to boundaries of polygon

    # READ in the bathymetry data
    
    bath <- readGEBCO.bathy("ATKAbathy/gebco_2024_n66.5_s65.0_w-40.0_e-37.0.nc")  # your GEBCO sourced netcdf - this is a marmap fxn
    bath.f <- fortify.bathy(bath) # this is a marmap fxn
    
    
    # READ in the sites
    
    sites <- read.csv("C:/Users/jchawarski/OneDrive - ASL Environmental Sciences Inc/Projects/Atka Expedition - SW Greenland/ATKA24 DATA/ATKA24_CTD_sites.csv")
    
    # ASSIGN bathymetric breaks and labels for plot
    
    brk <- c(0, -50, -100, -150, -250,-500, -750, -1000)   #define bathymetry breaks
    brk.labels <-c("0-50", "50-100", "100-150",  "150-250", "250-500", "500-750", "750-1000")
    
    # PLOT

      ggplot() +
      geom_contour_filled(data = bath.f,                       #bathymetry - plot this as the bottom layer... add coast and stations after
                          aes(x=x, y=y, z=z),
                          breaks=brk,
                          linewidth=c(0.1),
                          colour="grey70") +
        
      scale_fill_brewer(palette = "Blues", name="Depth [m]", labels=brk.labels  ) + 
      
    
      geom_polygon(data= coastline,                    #coast - high resolution coastline
                         aes(x=long, 
                             y=lat,
                             group=group),
                         fill= "grey60", 
                         colour ="grey50",
                         inherit.aes = F,
                         size=0.5) +
        
        geom_spatial_label_repel(data=sites, aes(x=Longitude, y=Latitude, label= Station), color="red", 
                           inherit.aes = F, crs=4326, shape=16) +
        
  
        xlab("Longitude") +
        ylab("Latitude") + 
        ggtitle("Istertup Kangertiva") + 
        
        coord_sf(crs = 4326) + 
        scale_x_continuous(limits=c(-39.5, -38.8), expand = c(0, 0)) +
        scale_y_continuous(limits=c(65.55, 65.8), expand = c(0, 0)) + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1))
      
      
  # NAGTIVIT KANGERTIVAT
      
      # DETAILED overview map with high res coastline and most recent GEBCO bathymetry
      
      # READ the coastline file and create a polygon
      
      coast <- readOGR("Greenland_Coast/Greenland_coast.shp") # BAS sourced Greenland coastline shapefile - this is an rgdal fxn
      # Data Projection: WGS 84 NSIDC Sea Ice Polar Stereographic North, EPSG 3413
      CP <- as(extent(-38.75, -38.3, 65.55, 65.7), "SpatialPolygons") # create a polygon bounding box
      proj4string(CP) <- CRS(proj4string(coast)) # match the projections
      coastline <- gIntersection(CP, coast, byid=TRUE) # clip the coastline to boundaries of polygon
      
      # READ in the bathymetry data
      
      bath <- readGEBCO.bathy("ATKAbathy/gebco_2024_n66.5_s65.0_w-40.0_e-37.0.nc")  # your GEBCO sourced netcdf - this is a marmap fxn
      bath.f <- fortify.bathy(bath) # this is a marmap fxn
      
      
      # READ in the sites
      
      sites <- read.csv("C:/Users/jchawarski/OneDrive - ASL Environmental Sciences Inc/Projects/Atka Expedition - SW Greenland/ATKA24 DATA/ATKA24_CTD_sites.csv")
      
      # ASSIGN bathymetric breaks and labels for plot
      
      brk <- c(0, -50, -100, -150, -250,-500, -750, -1000)   #define bathymetry breaks
      brk.labels <-c("0-50", "50-100", "100-150",  "150-250", "250-500", "500-750", "750-1000")
      
      # PLOT
      
      ggplot() +
        geom_contour_filled(data = bath.f,                       #bathymetry - plot this as the bottom layer... add coast and stations after
                            aes(x=x, y=y, z=z),
                            breaks=brk,
                            linewidth=c(0.1),
                            colour="grey70") +
        
        scale_fill_brewer(palette = "Blues", name="Depth [m]", labels=brk.labels  ) + 
        
        
        geom_polygon(data= coastline,                    #coast - high resolution coastline
                     aes(x=long, 
                         y=lat,
                         group=group),
                     fill= "grey60", 
                     colour ="grey50",
                     inherit.aes = F,
                     size=0.5) +
        
        geom_spatial_label(data=sites, aes(x=Longitude, y=Latitude, label= Station), color="red", 
                                 inherit.aes = F, crs=4326, shape=16, size=2) +
        
        
        xlab("Longitude") +
        ylab("Latitude") + 
        ggtitle("Nagtivit Kangertivat") + 
        
        coord_sf(crs = 4326) + 
        scale_x_continuous(limits=c(-38.75, -38.3), expand = c(0, 0)) +
        scale_y_continuous(limits=c(65.55, 65.7), expand = c(0, 0)) + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              panel.border = element_rect(colour = "black", fill=NA, size=1))
      
      
              
      -38.75, -38.3, 65.55, 65.75
      
      
      
      
      
      
      
      
      
      
            
      plot(bath)
      
      get.transect(bath, locator=T)





ggsave("map.png", width=6, height=6, units="in", dpi=1600, bg="white")


require(cowplot)
plot_grid(p1, p2, ncol=2)


basemap(limits = 80, land.size=0.5, bathymetry = T, bathy.style = "contour_grey",  
        glaciers=F, legend.position = "right", projection.grid = F, land.col = "grey80", land.border.col = "grey80")  +
  scale_fill_discrete(breaks = c(500,1000) )
# geom_spatial_label(data=sites, aes(lat,lon, label=Station.number), size=2.5, shape=24, color="#FF9900", fill="#990000")
