# ATKA Map

require(ggOceanMaps)
require(tidyverse)

sites <- sites %>% filter(!Station.number %in% 59)

p1 <- 
  
  basemap(limits =  c(-40, -30, 80, 55), land.size=0.5, land.col="lemonchiffon3", bathymetry = T, bathy.style = "poly_blues", glaciers=T, legend.position = "none") 


ggsave("map.png", width=6, height=6, units="in", dpi=1600, bg="white")


require(cowplot)
plot_grid(p1, p2, ncol=2)


basemap(limits = 80, land.size=0.5, bathymetry = T, bathy.style = "contour_grey",  
        glaciers=F, legend.position = "right", projection.grid = F, land.col = "grey80", land.border.col = "grey80")  +
  scale_fill_discrete(breaks = c(500,1000) )
# geom_spatial_label(data=sites, aes(lat,lon, label=Station.number), size=2.5, shape=24, color="#FF9900", fill="#990000")
