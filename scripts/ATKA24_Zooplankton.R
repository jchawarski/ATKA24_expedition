###################################
### ATKA24 ZOOPLANKTON ANALYSIS ###
###################################


# prepared by J. Chawarski, ASL Environmental Sciences
# The purpose of this script is to perform a community analysis of spatial differences in commmunity structure
# of zooplankton communities captured using a vertically hauled ring net during the ATKA24 expedition to
# SE Greenland


# load packages
require(tidyverse)
require(oce)
require(readxl)
library(vegan)
library(reshape2)


# load data
zoo <- read_xlsx(path = "C:/Users/jchawarski/OneDrive - ASL Environmental Sciences Inc/Projects/Atka Expedition - SW Greenland/ATKA24 DATA/Zooplankton Net Samples/ATKA-ZooplSamples-final.xlsx", sheet= 1)

zoo_clean <- zoo %>% filter(Sample.ID %in% c("Atka2402","Atka2403","Atka2408","Atka2410","Atka2415","Atka2419","Atka2424","Atka2433","Atka2440","Atka2443"))

zoo_clean <- zoo_clean %>% 
                mutate(fjord = case_when(Sample.ID %in% c("Atka2402","Atka2403","Atka2408","Atka2410","Atka2415") ~ "IK",
                                         Sample.ID %in% c("Atka2419","Atka2424","Atka2433","Atka2440","Atka2443") ~ "NK"))

##### Community analysis #####

# Convert long-format data to community matrix
comm <- zoo_clean %>%
  group_by(Sample.ID, Taxa.2, fjord, Station) %>%
  summarise(Abundance = sum(Abundance.Ind.m3, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(
    names_from = Taxa.2,
    values_from = Abundance,
    values_fill = 0
  ) %>%
  column_to_rownames("Sample.ID")


# compute Bray-Curtis distance
bray_dist <- vegdist(comm[3:13], method = "bray")

# perform NMDS
zoo_mds <- metaMDS(bray_dist, k = 2,
                   trymax = 100,
                   autotransform = T)

# inspect stress
# Your best stress value is 0.03726951.
# This is well below 0.05, indicating that the two-dimensional NMDS configuration provides an excellent representation of the rank-order relationships in your Bray–Curtis dissimilarity matrix.
# In practical terms, your two-dimensional ordination captures the differences in community composition between stations with very little distortion.

# NMDS PLOT - Results
{
plot(zoo_mds, type = "n",   
     xlim = c(-0.5, 0.6),
     ylim = c(-0.3, 0.3))

points(zoo_mds, display = "sites",
       pch = 21,
       bg = "steelblue",
       cex = 1.5)

points(zoo_mds, display = "sites",
       pch = 21,
       bg = c("steelblue", "tomato")[as.factor(comm$fjord)],
       cex = 1.5)

text(zoo_mds,
     display = "sites",
     labels = comm$Station,
     pos = 3,
     cex = 0.7)

# Add species vectors

envfit_results <- envfit(zoo_mds, comm[3:14], permutations = 999)

plot(envfit_results, p.max = 0.05, col = "red")


ordiellipse(zoo_mds, groups = comm$fjord,
            kind = "se",
            conf = 0.95,
            draw = "polygon",
            border = c("steelblue", "tomato"),
            col = c(adjustcolor("steelblue", alpha.f = 0.15),
                    adjustcolor("tomato", alpha.f = 0.15)),
            lwd = 2)

legend("topright",
       legend = unique(comm$fjord),
       pch = 21,
       pt.bg = c("steelblue", "tomato"),
       bty = "n")
}


# PERMANOVA

adonis2(bray_dist ~ fjord,
        data = comm,
        permutations = 999,
        method = "bray")


# Compute within fjord dispersion

disp <- betadisper(bray_dist, comm$fjord)

# Test differences in dispersion
anova(disp)

# Permutation test
permutest(disp, permutations = 999)


# SIMPER
sim <- simper(comm[3:14], group = comm$fjord,
              permutations = 999)
summary(sim)


# What happens when we start to remove sites that are less representative of hydropgraphic regime,
# or introduce multivariate tests that include light levels etc


###### ABUNDANCE PLOTS #######

zoo_clean %>%
  filter(Taxa.1 %in% "Calanus") %>%
  group_by(fjord) %>% 
  mutate(Station = factor(Station, levels = c("3", "2", "8", "10", "15", "19", "24", "33", "40", "43"))) %>%
  ggplot(aes(x=Station, y=Abundance.Ind.m3, fill=Taxa.1)) + 
  geom_col() + 
  scale_fill_viridis_d(option = "turbo", direction = -1, name = "Taxonomy") + 
  ylab(expression(paste("ind ", m^{-3}))) + 
  #ylim(0, 250) + 
  theme_bw()

zoo_clean %>%
  group_by(Sample.ID) %>%
  mutate(relative_abundance = Abundance.Ind.m3 / sum(Abundance.Ind.m3, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(fjord) %>% 
  mutate(Station = factor(Station, levels = c("3", "2", "8", "10", "15", "19", "24", "33", "40", "43"))) %>%
  ggplot(aes(x=Station, y=relative_abundance, fill=Taxa.1)) + geom_col() + scale_fill_viridis_d(option = "turbo")



# Sized based metrics

size_metrics <- zoo_clean %>%
  mutate(
    size_mid = sqrt(Size.min * Size.max),
    log_width = log10(Size.max) - log10(Size.min),
    total_biomass = Ind.ugC * Abundance.Ind.m3,
    nbss = Ind.ugC / log_width
  ) %>%
  group_by(Station, fjord) %>%
  summarise(
    mean_size = weighted.mean(size_mid, total_biomass, na.rm=T),
    stn_biomass = sum(total_biomass),
    .groups = "drop"
  )

size_metrics %>%
  mutate(Station = factor(Station, levels = c("3", "2", "8", "10", "15", "19", "24", "33", "40", "43"))) %>%
  ggplot(aes(x=Station, y=stn_biomass, fill=fjord)) + geom_col() + scale_fill_viridis_d(option = "turbo")


# Size-based community analysis


comm_taxa_size <- zoo_clean %>%
  mutate(
    Taxa_Size = paste(
      Taxa.2,
      Size.min,
      Size.max,
      sep = "_"
    )
  ) %>%
  group_by(Sample.ID, Station, fjord, Taxa_Size) %>%
  summarise(
    Abundance = sum(Abundance.Ind.m3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Taxa_Size,
    values_from = Abundance,
    values_fill = 0
  )


size_mat <- comm_taxa_size %>%
  select(-Sample.ID, -Station, -fjord) %>%
  as.data.frame()

rownames(size_mat) <- comm_taxa_size$Sample.ID

size_dist <- vegdist(size_mat, method = "bray")

size_mds <- metaMDS(
  size_dist,
  k = 2,
  trymax = 100,
  autotransform = FALSE
)

##### SIZE-BASED NMDS PLOT #####

{
  plot(size_mds, type = "n",
       xlim = c(-0.5, 0.6),
       ylim = c(-0.3, 0.3),
       main = "Zooplankton size structure")
  
  # Plot sampling stations, coloured by fjord
  points(size_mds,
         display = "sites",
         pch = 21,
         bg = c("steelblue", "tomato")[
           as.factor(size_wide$fjord)
         ],
         col = "black",
         cex = 1.5)
  
  # Add station labels
  text(size_mds,
       display = "sites",
       labels = size_wide$Station,
       pos = 3,
       cex = 0.7)
  
  # Fit size-class vectors
  sizefit_results <- envfit(
    size_mds,
    size_mat,
    permutations = 999
  )
  
  # Plot significant size-class vectors
  plot(sizefit_results,
       p.max = 0.05,
       col = "red")
  
  # Add fjord ellipses
  ordiellipse(
    size_mds,
    groups = size_wide$fjord,
    kind = "se",
    conf = 0.95,
    draw = "polygon",
    border = c("steelblue", "tomato"),
    col = c(
      adjustcolor("steelblue", alpha.f = 0.15),
      adjustcolor("tomato", alpha.f = 0.15)
    ),
    lwd = 2
  )
  
  # Legend
  legend(
    "topright",
    legend = levels(as.factor(size_wide$fjord)),
    pch = 21,
    pt.bg = c("steelblue", "tomato"),
    bty = "n"
  )
}

# PERMANOVA

adonis2(size_dist ~ fjord,
        data = comm_taxa_size,
        permutations = 999,
        method = "bray")


# Compute within fjord dispersion

disp <- betadisper(size_dist, comm_taxa_size$fjord)

# Test differences in dispersion
anova(disp)

# Permutation test
permutest(disp, permutations = 999)


# SIMPER
sim_size <- simper(
  size_mat,
  group = comm_taxa_size$fjord,
  permutations = 999
)

summary(sim_size)


# What happens when we start to remove sites that are less representative of hydropgraphic regime,

