##########################
### PARTICLE ANALYSIS ####
##########################

# prepared by J. Chawarski, Biological Oceanographer, ASL Environmental Sciences
# last edit 11.11.2026


#load libraries
library(janitor)
library(tidyverse)
library(data.table)
library(here)

# load cleaning function for particle database variable names
clean_names <- function(x) {
  x |>
    # remove units in brackets
    str_remove("\\[.*?\\]") |>
    # remove extra spaces
    str_squish() |>
    # replace bad microns symbol
    str_replace_all("�m", "um") |>
    # remove "LPM " prefix space (keep LPM)
    str_replace("^LPM\\s+", "LPM ") |>
    # convert >ranges into "gt16.4"
    str_replace(">\\s*(\\d+\\.?\\d*)", "gt\\1") |>
    # extract the range inside parentheses: (x-y um)
    str_replace(".*\\(([^)]+)\\).*", "\\1") |>
    # replace spaces in range
    str_replace_all(" ", "") |>
    # replace micron symbol after dash
    str_replace("um", ".um") |>
    # handle mm case
    str_replace("mm", ".mm") |>
    # add LPM prefix *only if original x had LPM*
    (\(nm){
      ifelse(
        grepl("LPM", x, ignore.case = TRUE),
        ifelse(
          grepl("biovolume", x, ignore.case = TRUE),
          paste0("LPM.biovolume.", nm),
          paste0("LPM.", nm)
        ),
        nm   # leave unchanged if no LPM in original name
      )
    })()
}

###### LOAD DATA
# data were exported from ecopart (reduced mode) by fjord

# read in all particle profile metadata - reduced
meta <-  as.data.frame(fread(here("data/particles/IK_Ecopart/export_reduced_20251114_22_33/export_reduced_20251114_22_33_Export_metadata_summary.tsv")))

# NOTE: Ecopart mistakenly changed the header of first column from capitol P to lowercase p in "Profile", column 1
colnames(meta)[1] <- "Profile"

##### Isertup Kangertivat #####
# read in particles data from IK fjord

# list all files in the IK_Ecopart folder containing the term "PAR" indicating that they are particle profiles - REDUCED
files <- list.files(path=here("data/particles/IK_Ecopart/export_reduced_20251114_22_33"), full.names = TRUE, pattern = "PAR.*\\.tsv$")

# read in all the files as dataframes, clean names, and limit the size
PART <- lapply(
  files,
  function(i) {
    df <- as.data.frame(fread(i))
    
    # clean column names
    colnames(df) <- clean_names(colnames(df))
    
    # keep only columns 1:50  (if fewer exist, keep all) - 
    # keeps only the particle abundances- to expand if biovolume is desired
    df <- df[, seq_len(min(50, ncol(df)))]
    
    df
  }
)


# IK Section Particles
# Define and arrange profiles in geographical order from glacial front to fjord entrance
# Note: numeric part of profile names indicate chronological order in which profiles were collected.
PART.IK <- lapply(PART, function(i){ rbind(unique(data.frame(i))) }) %>%  # creates a list of dataframes (of each cast)
  bind_rows() %>%                                                        # binds all dfs
  left_join(meta, ., by="Profile") %>%                                      # joins 
  mutate(
    Site = factor(Site, levels = c("ATKA24_03",
                                   "ATKA24_05",
                                   "ATKA24_06",
                                   "ATKA24_07",
                                   "ATKA24_08",
                                   "ATKA24_02",
                                   "ATKA24_09",
                                   "ATKA24_10",
                                   "ATKA24_11", 
                                   "ATKA24_12",
                                   "ATKA24_13",
                                   "ATKA24_14", 
                                   "ATKA24_15"))) 

##### Compute Normalized Particle Size Distribution #####

## Normalize Abundance

# to compute normalized particle size distribution (PSD), we need the normalized abundance 
# computed as the total number of particles in each ECD size (L^-1 um^-1) class,
# normalized by the cumulative volume sampled (L) and the ECD bin width (um). This is done with the following equation

## The Ecopart database outputs Equivalent Spherical Diameter (ECD) and Biovolume for each size class. 
##  $$ ECD =  2 * \sqrt{\frac{area}{\pi}} $$
##  $$ Biovolume = \frac{1}{6} * \pi * ECD^3 $$

{
  nPSD <- PART.IK %>% 
    
    dplyr::select(Profile, Depth, Sampledvol.ume, LPM.32.64.um:LPM.0.512.1.02.mm) %>% pivot_longer(cols= c(-Profile, -Depth, -Sampledvol.ume)) %>%
    
    # re-factor size classes by increasing size
    mutate(
      name = factor(
        name,
        levels = c(
          "LPM.32.64.um",
          "LPM.64.128.um",
          "LPM.128.256.um",
          "LPM.256.512.um",
          "LPM.0.512.1.02.mm"
        ))) %>%
    
    # compute bin width for each size class, normalized using log bin width
    mutate(bin_width = case_when(
      name == "LPM.32.64.um" ~ log10(64)-log10(32),
      name == "LPM.64.128.um" ~ log10(128)-log10(64),
      name == "LPM.128.256.um" ~ log10(256)-log10(128),
      name == "LPM.256.512.um" ~ log10(512)-log10(256),
      name == "LPM.0.512.1.02.mm" ~ log10(1020)-log10(512)
    )) %>%
    
    
    # compute mean size of each class
    mutate(mean_size_class = case_when(
      name == "LPM.32.64.um" ~ sqrt(32*64),
      name == "LPM.64.128.um" ~ sqrt(64*128),
      name == "LPM.128.256.um" ~ sqrt(128*256),
      name == "LPM.256.512.um" ~ sqrt(256*512),
      name == "LPM.0.512.1.02.mm" ~ sqrt(512*1020)
    )) %>%
    group_by(Depth, Profile) %>%
    
    mutate(norm_abund = value / (Sampledvol.ume * bin_width))
  
}


## Computing indices for particle size distributions

# Particle size spectra can be used to explain and predict variability in carbon export efficiency. 
# Larger particles tend to sink faster than small particles. Size-structure perspectives on plankton-trophic 
# interactions provides a simplified connection between biogeochemistry and plantkon ecology.

# Here, we want to compute slope, intercept, and R^2 and save them to a database

# Fit a log-linear regression to each depth 5 m interval
# using the following equation for normalized abundance
# log_{10}(NA_i) =  log_{10}(I) + b*log_{10}(ECD_i)


nPSD_IK_summary <- nPSD %>%
  filter(norm_abund > 0, mean_size_class > 0) %>%
  mutate(
    # Convert particle diameter from um to mm
    diameter_mm = mean_size_class / 1000,
    
    # Calculate individual spherical particle volume (mm3)
    particle_volume = (pi / 6) * diameter_mm^3,
    
    # Calculate biovolume concentration (mm3/L)
    biovolume = value * particle_volume
  ) %>%
  group_by(Profile, Depth) %>%
  summarise(
    # PSD model
    model = list(lm(log10(norm_abund) ~ log10(mean_size_class))),
    
    slope = coef(model[[1]])[2],
    intercept = coef(model[[1]])[1],
    r2 = summary(model[[1]])$r.squared,
    
    # p-value of the slope term
    p_value = summary(model[[1]])$coefficients[2, 4],
    
    # Significance flag
    p_lt_0_05 = p_value < 0.05,
    
    # Integrated particle metrics
    total_abundance = sum(value, na.rm = TRUE),
    total_biovolume = sum(biovolume, na.rm = TRUE),
    
    # Log-transformed versions
    log10_total_abundance = log10(total_abundance),
    log10_total_biovolume = log10(total_biovolume),
    mean_log_norm_abund   = mean(log10(norm_abund), na.rm = TRUE),
    
    .groups = "drop")

# Join summary metrics back to original PSD data
nPSD_IK_with_model <- nPSD %>%
  left_join(nPSD_NK_summary, by = c("Profile", "Depth"))

        ### Write results to CSV
          write.csv(
          nPSD_IK_summary %>% dplyr::select(-model),
          file = here("data/intermediate/ATKA24_IK_nPSD_reduced_modelresults.csv"),
          row.names = FALSE
        )

##### CREATE OCEANOGRAPHIC SECTIONS FROM PSD METRICS    
          
library(dplyr)
library(oce)
library(scico)

# Create ISERTUP KANGERTIVA section
          
# derive station metadata 
IK.summary <- data.frame(unique(PART.IK$Profile), unique(PART.IK$Latitude), unique(PART.IK$Longitude))
colnames(IK.summary) <- c("station", "Latitude", "Longitude")

# Define station order and corresponding water depths
station_depths <- c(
  "atka24_03" = 316,
  "atka24_05" = 352,
  "atka24_06" = 397,
  "atka24_07" = 423,
  "atka24_08" = 434,
  "atka24_02" = 439,
  "atka24_09" = 262,
  "atka24_10" = 403,
  "atka24_11" = 361,
  "atka24_12" = 257,
  "atka24_13" = 254,
  "atka24_14" = 162
)

# Establish station order
station_order <- names(station_depths)

# Match PSD profiles to CTD station metadata
psd_section_data <- nPSD_IK_summary %>%
  left_join(
    IK.summary %>%
      dplyr::select(station, Longitude, Latitude) %>%
      distinct(),
    by = c("Profile" = "station")
  ) %>%
  mutate(
    Profile = factor(Profile, levels = station_order)
  ) %>%
  arrange(Profile, Depth)

# Split PSD data by profile, preserving station order
station_list <- split(
  psd_section_data,
  psd_section_data$Profile,
  drop = TRUE
)

# Create CTD objects for each profile
ctd_list <- lapply(station_list, function(psd) {

  ctd <- as.ctd(
    salinity = rep(NA_real_, nrow(psd)),
    temperature = rep(NA_real_, nrow(psd)),
    pressure = psd$Depth,
    longitude = unique(psd$Longitude),
    latitude = unique(psd$Latitude),
    station = as.character(unique(psd$Profile))
  )

  # Add PSD-derived variables
extra_vars <- c(
  "slope", "intercept", "r2", "p_value", "p_lt_0_05",
  "total_abundance", "total_biovolume",
  "log10_total_abundance", "log10_total_biovolume", "mean_log_norm_abund"
)

  for (var in extra_vars) {
    ctd[[var]] <- psd[[var]]
  }

  return(ctd)
})

# Combine profiles into section
ctd.section <- as.section(ctd_list)

# Assign water depth to each station by station name
for (i in seq_along(ctd.section[["station"]])) {

  station_name <- ctd.section[["station"]][[i]][["metadata"]][["station"]]

  ctd.section[["station"]][[i]][["metadata"]][["waterDepth"]] <-
    unname(station_depths[station_name])
}

####################
##### OPTIONAL #####
####################

# add line to show optical transition depth
# after computing the optical transition depth in the ATKA24_BioOce_Analysis.R script,
# read the optical_transition_depth.csv, or use 'transition_depth' df.
# read the site x fjord from ATKA24_sitexfjord.csv

# Light

## Light section with optical transition depth overlay (oce)
## Requires in the workspace: ctd.section (oce section), transition_depth (from
## optical_transition_depth_v2.R), and optionally station_names.

library(oce)

# ---- 1. Transition depth per Site -------------------------------------------
tr <- transition_depth[, c("Site", "z_transition_m", "z_ci_low", "z_ci_high")]
tr <- tr[!is.na(tr$z_transition_m), ]          # drop sites with no transition

# ---- 2. Station positions and IDs, in section order -------------------------
lon  <- ctd.section[["longitude", "byStation"]]
lat  <- ctd.section[["latitude",  "byStation"]]
stns <- ctd.section@data$station                # list of ctd objects

get_id <- function(s) {
  v <- s@metadata$ID                            # 1) metadata
  if (is.null(v)) v <- s@data$ID                # 2) data column (one value per row)
  if (is.null(v)) v <- s@metadata$station       # 3) usual oce station field
  if (is.null(v)) v <- s@metadata$stationId
  as.character(v[1])                            # one value per station
}
stn_id <- vapply(stns, get_id, character(1))

# ---- 3. Match Sites to section stations by name -----------------------------
idx <- match(trimws(tr$Site), trimws(stn_id))
if (anyNA(idx)) {
  warning("No section station found for: ", paste(tr$Site[is.na(idx)], collapse = ", "))
  tr  <- tr[!is.na(idx), ]
  idx <- idx[!is.na(idx)]
}

# ---- 4. Candidate x definitions (km from first station) ---------------------
cand <- list(
  oce      = tryCatch(as.numeric(ctd.section[["distance", "byStation"]]),
                      error = function(e) NULL),
  along    = geodDist(lon, lat, alongPath = TRUE),
  straight = geodDist(lon[1], lat[1], lon, lat)
)
cand <- Filter(function(v) !is.null(v) && length(v) == length(lon) && !anyNA(v), cand)

# ---- 5. Plot ----------------------------------------------------------------
png("ATKA24_expedition/figs/ATKA_24_IK_light_section.png", width = 2400, height = 1600, res = 300)

plot(ctd.section, labels = stn_id,
     which = "light",
     ztype = "image",
     ytype = "depth",
     xtype = "distance",
     legend.loc = "",
     ylim = c(400, 0),
     zbreaks = seq(40, 190, 10), zcol = oceColorsPAR)

# Pick the candidate whose span matches the plotted x axis
usr   <- par("usr")[1:2]
width <- function(v) diff(range(v))
best  <- names(cand)[which.min(sapply(cand, function(v) abs(width(v) - diff(usr))))]
message("x definition used by the plot: ", best)
print(sapply(cand, range)); print(usr)          # sanity check

x <- cand[[best]][idx]
o <- order(x)

#lines(x[o], tr$z_ci_hi[o], col = "white", lwd = 3, lty="dashed")   # halo for contrast
# lines(x[o], tr$z_ci_low[o], col = "white", lwd = 3, lty="dashed")   # halo for contrast
#lines(x[o], tr$z_transition_m[o], col = "white", lwd = 3)   # halo for contrast
lines(x[o], tr$z_transition_m[o], col = "white", lwd = 1.5, lty="dashed")
points(x[o], tr$z_transition_m[o], pch = 21, bg = "white")

dev.off()      



### PLOT SECTION ###

# Basic section plotting use oce

output_dir <- "C:/Users/jchawarski/OneDrive - ASL Environmental Sciences Inc/Projects/Atka Expedition - SW Greenland/ATKA24_expedition/figs"

# nPSD slope

grDevices::png(
  filename = file.path(output_dir, "ATKA_24_IK_nPSDslope_section.png"),
  width = 2400,
  height = 1600,
  res = 300,
  type = "cairo"
)

cols <- scico(100, palette = "vik")

breaks <- seq(-3.5, -1.5, length.out = 101)


plot(
    ctd.section,
    labels = station_names,
    which = "slope",
    ztype = "image",
    ytype = "depth",
    ylim = c(400, 0),
    legend.loc = "",
    showStations = TRUE,
    zbreaks = breaks,
    zcol = cols
  )
  
grDevices::dev.off()


# nPSD intercept

grDevices::png(
  filename = file.path(output_dir, "ATKA_24_IK_nPSDintercept_section.png"),
  width = 2400,
  height = 1600,
  res = 300,
  type = "cairo"
)

cols <- scico(100, palette = "devon")

plot(
    ctd.section,
    labels = station_names,
    which = "intercept",
    ztype = "image",
    ytype = "depth",
    ylim = c(400, 0),
    legend.loc = "",
    showStations = TRUE,
    zcol = cols
  )
  
grDevices::dev.off()



# Particle Abundance

grDevices::png(
  filename = file.path(output_dir, "ATKA_24_IK_particleabundance_section.png"),
  width = 2400,
  height = 1600,
  res = 300,
  type = "cairo"
)

cols <- scico(100, palette = "lajolla")

#breaks <- seq(-3.5, -1.5, length.out = 101)

plot(
    ctd.section,
    labels = station_names,
    which = "total_abundance",
    ztype = "image",
    ytype = "depth",
    ylim = c(400, 0),
    legend.loc = "",
    showStations = TRUE,
    #zbreaks = breaks,
    zcol = cols
  )
  
grDevices::dev.off()


# Log Particle Abundance

grDevices::png(
  filename = file.path(output_dir, "ATKA_24_IK_logparticleabundance_section.png"),
  width = 2400,
  height = 1600,
  res = 300,
  type = "cairo"
)

cols <- scico(100, palette = "lajolla")

breaks <- seq(3.7, 4.9, length.out = 101)

plot(
    ctd.section,
    labels = station_names,
    which = "log10_total_abundance",
    ztype = "image",
    ytype = "depth",
    ylim = c(400, 0),
    legend.loc = "",
    showStations = TRUE,
    zbreaks = breaks,
    zcol = cols
  )
        lines(x[o], tr$z_transition_m[o], col = "white", lwd = 1.5, lty="dashed")
      points(x[o], tr$z_transition_m[o], pch = 21, bg = "white")

grDevices::dev.off()


# Particle Biovolume

grDevices::png(
  filename = file.path(output_dir, "ATKA_24_IK_particlebiovolume_section.png"),
  width = 2400,
  height = 1600,
  res = 300,
  type = "cairo"
)

cols <- scico(100, palette = "managua", direction = -1)

#breaks <- seq(-3.5, -1.5, length.out = 101)


plot(
    ctd.section,
    labels = station_names,
    which = "total_biovolume",
    ztype = "image",
    ytype = "depth",
    ylim = c(400, 0),
    legend.loc = "",
    showStations = TRUE,
    #zbreaks = breaks,
    zcol = cols
  )

grDevices::dev.off()




