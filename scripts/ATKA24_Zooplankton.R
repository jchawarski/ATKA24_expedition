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


# plot results

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



# PERMANOVA

adonis2(bray_dist ~ fjord,
        data = comm,
        permutations = 999,
        method = "bray")


# Dispesion

disp <- betadisper(bray_dist, comm$fjord)

# Test differences in dispersion
anova(disp)

# Permutation test
permutest(disp, permutations = 999)



# SIMPER

sim <- simper(comm[3:14], group = comm$fjord,
              permutations = 999)
summary(sim)

