# FULL ECHOGRAM
require(ggRetro) # needed for floating axes   #https://github.com/albert-ying/ggRetro
require(ggsci) # needed for nature publishing group color palettes

require(TTR)

p1 <-
  
  stn.profile %>%
  filter(between(Interval, 1723494563, 1723495390 )) %>%
  ggplot(aes(x = datetime, y = range, fill = Sv_mean)) +
  geom_tile(width=2) +
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
  labs(title = "Profile Echogram", x = "Time", y = "Range [m]") +
  theme_bw()  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  # Rotate text 45 degrees
  theme(plot.margin = unit(c(0.5, 0, 0, 0.5), "cm"), 
        text = element_text("Barlow"))  

p1 <-
  
  stn.profile %>%
  filter(between(Interval, 1723494563, 1723495390 )) %>%
  
  ggplot(aes(x = datetime, y = depth_true, fill = Sv_mean)) +
  geom_tile(width=2) +
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
  
  stn.profile %>%
  filter(between(Interval, 1723494563, 1723495390 )) %>%
  
  group_by(depth_true) %>%
  filter(range > 0.5) %>%
  
  
  summarise(sv = log10(mean(10^Sv_mean, na.rm=T)),
            min = min(Sv_min, na.rm = T),
            max = max(Sv_max, na.rm = T)) %>%
  
  mutate(sv_smooth = despike(sv, 
                             reference = "median", 
                             n=1, 
                             k=101, 
                             replace="reference")) %>%
  
  mutate(sv_smooth = SMA(sv_smooth, n=30)) %>%
  
  
  ggplot(aes(x = depth_true, y = sv_smooth)) +
  scale_y_continuous() + 
  geom_line() + 
  
  #  geom_line(aes(x = depth_true, y=min), inherit.aes = F , color="red") + 
  #  geom_line(aes(x = depth_true, y=max), inherit.aes = F , color="blue") + 
  coord_flip()+ scale_x_reverse(expand = c(0,0)) +
  labs(title = "MVBS Plot", y = Sv_label, x = "")+
  theme_bw()+ 
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0), "cm"), 
        text = element_text("Barlow"))


# identical plot, but smoothed profile

stn.profile %>%
  filter(between(Interval, 1723494563, 1723495390 )) %>%
  
  group_by(depth_true) %>%
  filter(range > 0.5) %>%
  
  
  summarise(sv = log10(mean(10^Sv_mean, na.rm=T)),
            min = min(Sv_min, na.rm = T),
            max = max(Sv_max, na.rm = T)) %>%
  
  mutate(sv_smooth = despike(sv, 
                             reference = "median", 
                             n=1, 
                             k=101, 
                             replace="reference")) %>%
  
  mutate(sv_smooth = SMA(sv_smooth, n=30)) %>%
  
  
  ggplot(aes(x = depth_true, y = sv_smooth)) +
  scale_y_continuous() + 
  geom_line(aes(x= depth_true, y = sv), color="black") + 
  geom_line(color="red") + 
  
  
  #  geom_line(aes(x = depth_true, y=min), inherit.aes = F , color="red") + 
  #  geom_line(aes(x = depth_true, y=max), inherit.aes = F , color="blue") + 
  coord_flip()+ scale_x_reverse(expand = c(0,0)) +
  labs(title = "MVBS Plot", y = Sv_label, x = "")+
  theme_bw()+ 
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0), "cm"), 
        text = element_text("Barlow"))



# create overlapping profile plots

require(ggRetro) # needed for floating axes   #https://github.com/albert-ying/ggRetro
require(ggsci) # needed for nature publishing group color palettes
 
  
plot.col <-  "darkgreen"  

p <-
stn.sum %>% 
  ggplot(aes(x=depth, y=turbidity)) +
  geom_line(color=plot.col) + 
  ylab("Turbidity [NTU]") +
  xlab("Depth [meters]") + 
  #coord_flip() + scale_x_reverse() + 
theme_classic(base_size = 16) 


plot1 <- 
p |> base_mode() +  theme( axis.title.x = element_text(angle = 180),
                           axis.text.x = element_text(angle = 180),
                           axis.title.y = element_text(color=plot.col),
                           axis.line.y = element_line(color=plot.col),
                           axis.ticks.y = element_line(color=plot.col),
                           axis.text.y = element_text(color=plot.col, angle=90),
                           rect = element_rect(fill = "transparent"))

ggsave("ProfilePlot_turb.png", plot = plot1, height= 4, width = 4, dpi = 300)

require(tidyquant)
require(oce)
plot.col <-  "purple"  


p <- 
stn.profile %>%
  group_by(depth_true) %>%
  filter(range > 0.5) %>%
  
  
  summarise(sv = log10(mean(10^Sv_mean, na.rm=T)),
            min = min(Sv_min, na.rm = T),
            max = max(Sv_max, na.rm = T)) %>%
  
  mutate(sv_smooth = despike(sv, 
                             reference = "median", 
                             n=1, 
                             k=101, 
                             replace="reference")) %>%

  mutate(sv_smooth = SMA(sv_smooth, n=30)) %>%
    
    ggplot(aes(x=depth_true, y=sv_smooth)) +
    geom_line(color=plot.col) + 
    #geom_ma(aes(x=datetime, y=-detide_curr/100), ma_fun = SMA, n = 48, color=algae2, linetype="solid") + 
  
    #labs(y = Sv_label) +
    ylab("Sv,200kHz") +
    xlab("Depth [meters]") + 
    #coord_flip() + scale_x_reverse() + 
    theme_classic(base_size = 16) 
  
plot1 <- 
  p |> base_mode() +  theme( axis.title.x = element_text(angle = 180),
                             axis.text.x = element_text(angle = 180),
                             axis.title.y = element_text(color=plot.col),
                             axis.line.y = element_line(color=plot.col),
                             axis.ticks.y = element_line(color=plot.col),
                             axis.text.y = element_text(color=plot.col, angle=90))
                            # rect = element_rect(fill = "transparent"))

ggsave("ProfilePlot_sv.png", plot = plot1, height= 4, width = 4, dpi = 300)


# PSD Slope
# IK

plot1 <- 
nPSD_IK_summary %>%
  filter(between(Depth, 0,300)) %>%
  filter(Profile %in% "atka24_02") %>%
  ggplot(aes(y=slope, x=Depth)) + geom_point(color="#004676") + geom_line(color="#004676") + 
  
  #scale_x_reverse() + coord_flip() + 
  ylim(-4,-1.5) + xlim(0,300) + 
  theme_classic(base_size = 16) +   theme(
    axis.text.y = element_text(angle = 90), 
    axis.text.x = element_text(angle = 90),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA)
  )

ggsave(
  "ProfilePlot_PSDslope_ATKA02.png",
  plot = plot1,
  height = 4,
  width = 4,
  dpi = 300,
  bg = "transparent"
)

 
p |> base_mode() +  theme(axis.text.y = element_text(angle=90), rect = element_rect(fill = "transparent"))

# NK
plot1 <- 
  nPSD_NK_summary %>%
  filter(between(Depth, 0,300)) %>%
  filter(Profile %in% "atka24_22") %>%
  ggplot(aes(y=slope, x=Depth)) + geom_point(color="#761C00") + geom_line(color="#761C00") + 
  #scale_x_reverse() + coord_flip() + 
  ylim(-4,-1.5) + xlim(0,300) +  
theme_classic(base_size = 16) + 
  
  theme(
    axis.text.y = element_text(angle = 90), 
    axis.text.x = element_text(angle = 90),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA)
  )

ggsave(
  "ProfilePlot_PSDslope_ATKA22.png",
  plot = plot1,
  height = 4,
  width = 4,
  dpi = 300,
  bg = "transparent"
)
  

ggsave("ProfilePlot_PSDslope_ATKA22.png", plot = plot1, height= 4, width = 4, dpi = 300)


# and now plot the comparison of sv plots so they are identical in style and convention

svdat <- read.csv("AZFP_nano/Final/ATKA24_02_CTD_AZFP_0.5m_Sv_corr.csv")

Sv_label <- expression(paste("Sv [dB re 1/m]"))

plot1 <- 
  svdat %>%
  group_by(depth_true) %>%
  filter(range > 0.5) %>%
  filter(depth_true < 280) %>%
  summarise(sv = log10(mean(10^Sv_mean, na.rm=T)),
            min = min(Sv_min, na.rm = T),
            max = max(Sv_max, na.rm = T)) %>%
    mutate(sv_smooth = despike(sv, 
                               reference = "median", 
                               n=1, 
                               k=101, 
                               replace="reference")) %>%
    
    mutate(sv_smooth = SMA(sv_smooth, n=30)) %>%
    
  
  ggplot(aes(x = depth_true, y = sv_smooth)) +
  scale_y_continuous() + 
  geom_line(color="#004676") + 
    #geom_line(color="#761C00") +   
  #  geom_line(aes(x = depth_true, y=min), inherit.aes = F , color="red") + 
  #  geom_line(aes(x = depth_true, y=max), inherit.aes = F , color="blue") + 
  #coord_flip()+ scale_x_reverse() +
  ylab(Sv_label) + xlim(0,300) + 
    ylim(-90, -55) + 
    theme_classic(base_size = 16) + 
    
    theme(
      axis.text.y = element_text(angle = 90), 
      axis.text.x = element_text(angle = 90),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background  = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA)
    )
    
  
  ggsave("ProfilePlot_SvSmooth_ATKA02.png", plot = plot1, height= 4, width = 4, dpi = 300)
  

