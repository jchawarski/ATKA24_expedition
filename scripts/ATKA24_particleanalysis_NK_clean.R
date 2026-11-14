##########################
### PARTICLE ANALYSIS ####
##########################

# prepared by J. Chawarski, Biological Oceanographer, ASL Environmental Sciences
# Refactored version: configuration at the top, logic in functions, no object overwriting.
#
# Inputs : EcoPart reduced-mode exports (PAR*.tsv), `meta` table, optical_transition_depth.csv
# Outputs: <out_dir>/ATKA24_<fjord>_nPSD_reduced_modelresults.csv, section figures,
#          sessionInfo + input checksums (provenance)
# Run from the project root (here::here() finds it). Use renv::snapshot() to pin packages.

library(tidyverse)   # dplyr, tidyr, stringr, readr, purrr
library(data.table)  # fread
library(here)
library(oce)
library(scico)
source(here("scripts/section_overlay_functions.R"))

############################
### 0. CONFIGURATION     ###
############################

cfg <- list(
  fjord            = "NK",
  ecopart_dir      = here("data/particles/NK_Ecopart"),
  meta_file        = here("data/particles/NK_Ecopart/export_reduced_20251118_22_23_Export_metadata_summary.tsv"), 
  fjord_file       = here("data/ATKA24_sitexfjord.csv"),   # columns: Site, fjord
  transition_file  = here("data/optical_transition_depth.csv"),
  out_dir          = here("data/intermediate"),
  fig_dir          = here("figs/"),
  
  # TODO verify against the raw EcoPart header: if the LPM columns are "[# l-1]" they are
  # already concentrations and must NOT be divided by the sampled volume again.
  lpm_is_per_litre = FALSE,
  
  # Normalisation width of the size classes. "log10" = per log10(size) (as in the original
  # code); "um" = per micrometre (as the original comment said). Slopes differ by exactly 1:
  # slope_per_um = slope_per_log10 - 1.
  norm_width       = "log10",
  
  ref_size_um      = 128,   # intercept is reported at this size (inside the fitted range)
  min_classes      = 4,     # need at least this many non-zero size classes to fit a slope
  ylim             = c(400, 0)
)

# Size classes of the reduced export (names as produced by clean_particle_names + make.names)
size_classes <- tribble(
  ~name,               ~lower_um, ~upper_um,
  "LPM.32.64.um",          32,         64,
  "LPM.64.128.um",         64,        128,
  "LPM.128.256.um",       128,        256,
  "LPM.256.512.um",       256,        512,
  "LPM.0.512.1.02.mm",    512,       1020
)

# Stations in section order (glacier front -> fjord entrance) with water depth.
# NOTE: the original Site factor also listed ATKA24_15, which had no water depth; add a row if needed.
stations <- tribble(
  ~Profile,    ~water_depth_m,
  "atka24_18" , 245,
  "atka24_17" , 270,
  "atka24_16" , 310,
  "atka24_21" , 357,
  "atka24_23" , 320,
  "atka24_24" , 308,
  "atka24_30" , 350,
  "atka24_31" , 405,
  "atka24_32" , 410,
  "atka24_36" , 250,
  "atka24_37" , 205
)


############################
### 1. READ PARTICLE DATA ###
############################

# Clean EcoPart column names (renamed from clean_names() so it does not mask janitor::clean_names)
clean_particle_names <- function(x) {
  x |>
    str_remove("\\[.*?\\]") |>                          # remove units in brackets
    str_squish() |>
    str_replace_all("(µ|μ|�)m", "um") |> # micro sign (or broken encoding) -> "u"
    str_replace("^LPM\\s+", "LPM ") |>
    str_replace(">\\s*(\\d+\\.?\\d*)", "gt\\1") |>      # ">16.4" -> "gt16.4"
    str_replace(".*\\(([^)]+)\\).*", "\\1") |>          # keep the range inside (...)
    str_replace_all(" ", "") |>
    str_replace("(?<=\\d)um$", ".um") |>                # only the size-unit suffix; the old
    str_replace("(?<=\\d)mm$", ".mm") |>                # pattern also hit "vol-um-e" -> "Sampledvol.ume"
    (\(nm) ifelse(
      grepl("LPM", x, ignore.case = TRUE),
      ifelse(grepl("biovolume", x, ignore.case = TRUE),
             paste0("LPM.biovolume.", nm), paste0("LPM.", nm)),
      nm
    ))()
}

read_ecopart_par <- function(file) {
  df <- as.data.frame(fread(file, encoding = "Latin-1"))
  # make.names() is explicit here; before it happened implicitly inside data.frame()
  names(df) <- make.names(clean_particle_names(names(df)), unique = TRUE)
  df |>
    rename(sampled_vol_L = starts_with("Sampledvol")) |>
    dplyr::select(Profile, Depth, sampled_vol_L, starts_with("LPM"))   # by name, not by position 1:50
}

files <- list.files(cfg$ecopart_dir, pattern = "PAR.*\\.tsv$", full.names = TRUE)
stopifnot("no particle files found" = length(files) > 0)
PART <- map_dfr(files, read_ecopart_par) |> distinct()
stopifnot(all(size_classes$name %in% names(PART)))   # fails loudly if names changed

# Profile metadata (position, site). EcoPart writes the first header as "profile".
meta <- as.data.frame(fread(cfg$meta_file))
names(meta)[1] <- "Profile"
profile_meta <- meta |> dplyr::select(Profile, Site, Latitude, Longitude) |> distinct()
stopifnot("more than one metadata row per Profile" = !anyDuplicated(profile_meta$Profile))

# inner_join: only profiles that have particle data (the old left_join(meta, .) kept every
# profile of every fjord and produced rows with NA Site).
PART <- PART |>
  inner_join(profile_meta, by = "Profile") |>
  filter(Profile %in% stations$Profile)

missing_stn <- setdiff(stations$Profile, unique(PART$Profile))
if (length(missing_stn)) warning("Stations without particle data: ", paste(missing_stn, collapse = ", "))
PART <- PART |> mutate(Profile = factor(Profile, levels = stations$Profile))

############################
### 2. nPSD AND METRICS  ###
############################

# Normalised abundance:  NA_i = N_i / (V * width_i)
#   N_i = particles in size class i, V = sampled volume (L), width_i = class width (see cfg$norm_width)
# Biovolume per class uses the geometric-mid-size sphere: (pi/6) * D^3.
# (EcoPart also exports measured "LPM biovolume" columns; those are more accurate if you need
#  absolute biovolume and can replace the sphere approximation here.)
compute_npsd <- function(part, size_classes, lpm_is_per_litre, norm_width) {
  sc <- size_classes |>
    mutate(size_mid_um = sqrt(lower_um * upper_um),
           bin_width   = if (norm_width == "log10") log10(upper_um) - log10(lower_um)
           else upper_um - lower_um,
           particle_volume_mm3 = (pi / 6) * (size_mid_um / 1000)^3)
  
  part |>
    dplyr::select(Profile, Depth, sampled_vol_L, all_of(sc$name)) |>
    pivot_longer(all_of(sc$name), names_to = "name", values_to = "value") |>
    left_join(sc, by = "name") |>
    mutate(vol        = if (lpm_is_per_litre) 1 else sampled_vol_L,
           norm_abund = value / (vol * bin_width),
           biovolume  = value * particle_volume_mm3)
}

# log10(NA) = log10(I_ref) + b * log10(D / D_ref)
# Centring on D_ref (inside the data range) makes the intercept = abundance at D_ref and
# decorrelates it from the slope. intercept_1um is the old definition (value at 1 um).
fit_psd <- function(d, ref_size_um, min_classes) {
  d <- d[is.finite(d$norm_abund) & d$norm_abund > 0, ]
  n <- nrow(d)
  empty <- tibble(n_classes = n, slope = NA_real_, slope_se = NA_real_, intercept = NA_real_,
                  intercept_1um = NA_real_, r2 = NA_real_, p_value = NA_real_)
  if (n < min_classes) return(empty)
  
  x <- log10(d$size_mid_um / ref_size_um)
  y <- log10(d$norm_abund)
  m <- lm(y ~ x)
  s <- summary(m)
  tibble(n_classes     = n,
         slope         = unname(coef(m)[2]),
         slope_se      = s$coefficients[2, 2],
         intercept     = unname(coef(m)[1]),
         intercept_1um = unname(coef(m)[1] - coef(m)[2] * log10(ref_size_um)),
         r2            = s$r.squared,
         p_value       = s$coefficients[2, 4])   # only 'n_classes - 2' degrees of freedom: interpret with care
}

nPSD <- compute_npsd(PART, size_classes, cfg$lpm_is_per_litre, cfg$norm_width)

psd_metrics <- nPSD |>
  group_by(Profile, Depth) |>
  group_modify(~ bind_cols(
    fit_psd(.x, cfg$ref_size_um, cfg$min_classes),
    tibble(total_abundance      = sum(.x$value, na.rm = TRUE),
           total_biovolume      = sum(.x$biovolume, na.rm = TRUE),
           mean_log_norm_abund  = mean(log10(.x$norm_abund[is.finite(.x$norm_abund) & .x$norm_abund > 0])))
  )) |>
  ungroup() |>
  mutate(log10_total_abundance = if_else(total_abundance > 0, log10(total_abundance), NA_real_),
         log10_total_biovolume = if_else(total_biovolume > 0, log10(total_biovolume), NA_real_),
         mean_log_norm_abund   = if_else(is.nan(mean_log_norm_abund), NA_real_, mean_log_norm_abund))

dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(psd_metrics, file.path(cfg$out_dir, sprintf("ATKA24_%s_nPSD_reduced_modelresults2.csv", cfg$fjord)))

############################
### 3. SECTION OBJECT    ###
############################

# One position per profile (n_pos is computed BEFORE lat/lon are averaged)
profile_pos <- PART |>
  group_by(Profile) |>
  summarise(n_pos = n_distinct(Latitude, Longitude),
            Latitude  = mean(Latitude,  na.rm = TRUE),
            Longitude = mean(Longitude, na.rm = TRUE),
            .groups = "drop")
if (any(profile_pos$n_pos > 1)) warning("Position varies within a profile; using the mean position")

psd_vars <- c("slope", "slope_se", "intercept", "intercept_1um", "r2", "p_value", "n_classes",
              "total_abundance", "total_biovolume",
              "log10_total_abundance", "log10_total_biovolume", "mean_log_norm_abund")

stn <- stations |> filter(Profile %in% psd_metrics$Profile)

# oce ctd objects are used only as containers for the PSD variables (hence the NA S and T)
ctd_list <- lapply(seq_len(nrow(stn)), function(i) {
  prof <- stn$Profile[i]
  d    <- psd_metrics |> filter(Profile == prof) |> arrange(Depth)
  pos  <- profile_pos  |> filter(Profile == prof)
  ctd <- as.ctd(salinity    = rep(NA_real_, nrow(d)),
                temperature = rep(NA_real_, nrow(d)),
                pressure    = d$Depth,
                longitude   = pos$Longitude,
                latitude    = pos$Latitude,
                station     = as.character(prof))
  for (v in psd_vars) ctd <- oceSetData(ctd, v, d[[v]])
  oceSetMetadata(ctd, "waterDepth", stn$water_depth_m[i])   # set BEFORE as.section()
})

psd.section <- as.section(ctd_list)    # NOT "ctd.section": that name belongs to the CTD analysis

############################
### 4. TRANSITION DEPTH  ###
############################

# Written by the CTD analysis script: write.csv(transition_depth, <transition_file>, row.names = FALSE)
transition <- read_csv(cfg$transition_file, show_col_types = FALSE)
if (!"fjord" %in% names(transition)) {
  site_fjord <- read_csv(cfg$fjord_file, show_col_types = FALSE) |>
    mutate(.key = site_key(Site)) |> dplyr::select(.key, fjord) |> distinct()
  transition <- transition |>
    mutate(.key = site_key(Site)) |>
    left_join(site_fjord, by = ".key") |>
    dplyr::select(-.key)
}

############################
### 5. SECTION FIGURES   ###
############################

# One entry per figure. overlay = TRUE adds the optical transition depth line.
plot_specs <- list(
  list(which = "slope",                 tag = "nPSDslope",
       zcol = scico(100, palette = "vik"), zbreaks = seq(-3.5, -1.5, length.out = 101)),
  list(which = "intercept",             tag = "nPSDintercept",
       zcol = scico(100, palette = "devon"), zbreaks = seq(1.0, 2.4, length.out = 101)),
  list(which = "total_abundance",       tag = "particleabundance",
       zcol = scico(100, palette = "lajolla")),
  list(which = "log10_total_abundance", tag = "logparticleabundance",
       zcol = scico(100, palette = "lajolla"), zbreaks = seq(3.7, 4.9, length.out = 101)),
  list(which = "total_biovolume",       tag = "particlebiovolume",
       zcol = scico(100, palette = "managua", direction = -1))
)

for (p in plot_specs) {
  plot_section_var(
    psd.section, which = p$which,
    file    = file.path(cfg$fig_dir, sprintf("ATKA_24_%s_%s_section_wOTD.png", cfg$fjord, p$tag)),
    zcol    = p$zcol, zbreaks = p$zbreaks, ylim = cfg$ylim,
    transition = if (isTRUE(p$overlay %||% TRUE)) transition else NULL,
    transition_args = list(fjord = cfg$fjord)
  )
}

# ---- Optional: light section from the CTD analysis ---------------------------
# In the CTD script, save once:  saveRDS(ctd.section, here("data/intermediate/ATKA24_NK_ctd_section.rds"))
ctd_section_file <- here("data/intermediate/ATKA24_NK_ctd_section.rds")
if (file.exists(ctd_section_file)) {
  ctd.section <- readRDS(ctd_section_file)
  plot_section_var(
    ctd.section, which = "light",
    file    = file.path(cfg$fig_dir, sprintf("ATKA_24_%s_light_section.png", cfg$fjord)),
    zcol    = oceColorsPAR, zbreaks = seq(40, 190, 10), ylim = cfg$ylim,
    transition = transition, transition_args = list(fjord = cfg$fjord)
  )
}

############################
### 6. PROVENANCE        ###
############################

writeLines(capture.output(sessionInfo()), file.path(cfg$out_dir, "sessionInfo_particles.txt"))
write_csv(tibble(file = basename(files), md5 = unname(tools::md5sum(files))),
          file.path(cfg$out_dir, "inputs_particles_md5.csv"))