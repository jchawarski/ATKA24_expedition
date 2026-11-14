## section_overlay_functions.R
## Reusable helpers for oce section plots with an optical-transition-depth overlay.
## Usage (in any analysis script):
##   source(here::here("R/section_overlay_functions.R"))
##   plot_section_var(psd.section, "slope", "figs/slope.png", zcol = scico::scico(100, "vik"),
##                    transition = transition, transition_args = list(fjord = "IK"))

suppressPackageStartupMessages(library(oce))

## Normalise site / profile names so "atka24_03", "ATKA24_03" and "ATKA24_03_CTD" all match
site_key <- function(x) toupper(sub("_CTD$", "", trimws(as.character(x))))

## Station keys of a section, in plotting order (NA if a station has no name)
section_station_keys <- function(section) {
  vapply(section@data$station, function(s) {
    v <- s@metadata$station
    if (is.null(v)) v <- s@metadata$stationId
    if (is.null(v)) v <- s@metadata$ID
    if (is.null(v)) v <- s@data$ID
    if (is.null(v)) NA_character_ else site_key(v[1])
  }, character(1))
}

## Candidate x positions (km) for the stations of a section
section_x_candidates <- function(section) {
  lon <- section[["longitude", "byStation"]]
  lat <- section[["latitude",  "byStation"]]
  cand <- list(
    along    = geodDist(lon, lat, alongPath = TRUE),
    straight = geodDist(lon[1], lat[1], lon, lat),
    oce      = tryCatch(as.numeric(section[["distance", "byStation"]]),
                        error = function(e) NULL)
  )
  Filter(function(v) !is.null(v) && length(v) == length(lon) && !anyNA(v), cand)
}

## Add the optical transition depth to an ALREADY DRAWN oce section plot.
##   section : the oce section that was just plotted
##   tr      : data.frame with one row per site (e.g. `transition_depth`)
##   fjord   : keep only rows of this fjord (needs a `fjord` column); NULL = use all rows
## Sites are matched to stations by name (site_key), so other fjords' sites are
## ignored silently. Stations without a transition depth leave a gap in the line.
## Call it straight after plot(); par("usr") is used to check the x definition.
add_transition_line <- function(section, tr,
                                xtype = c("distance", "longitude", "latitude"),
                                x_def = c("auto", "along", "straight", "oce"),
                                key_col = "Site", depth_col = "z_transition_m",
                                ci_cols = c("z_ci_low", "z_ci_high"),
                                fjord = NULL, fjord_col = "fjord",
                                connect_gaps = FALSE, show_ci = FALSE,
                                col = "white", lwd = 1.5, lty = "dashed",
                                pch = 21, bg = "white", cex = 1,
                                verbose = TRUE) {
  xtype <- match.arg(xtype)
  x_def <- match.arg(x_def)
  stopifnot(is.data.frame(tr), all(c(key_col, depth_col) %in% names(tr)))
  
  if (!is.null(fjord)) {
    if (!fjord_col %in% names(tr)) stop("`tr` has no column '", fjord_col, "' to filter by fjord")
    tr <- tr[tr[[fjord_col]] %in% fjord, , drop = FALSE]
  }
  tr$.key <- site_key(tr[[key_col]])
  if (anyDuplicated(tr$.key)) {
    stop("More than one row per site in `tr`: ",
         paste(unique(tr$.key[duplicated(tr$.key)]), collapse = ", "))
  }
  
  # one entry per station, in section order
  stn_key <- section_station_keys(section)
  m <- match(stn_key, tr$.key)
  if (all(is.na(m))) {
    stop("No site in `tr` matches a station in the section.\n  section: ",
         paste(head(stn_key, 5), collapse = ", "), " ...\n  tr:      ",
         paste(head(tr$.key, 5), collapse = ", "), " ...")
  }
  z_raw <- tr[[depth_col]][m]
  
  # x position of each station, consistent with how the section was drawn
  x <- switch(
    xtype,
    longitude = section[["longitude", "byStation"]],
    latitude  = section[["latitude",  "byStation"]],
    distance  = {
      cand <- section_x_candidates(section)
      if (x_def == "auto") {
        usr <- par("usr")[1:2]
        gap <- vapply(cand, function(v) abs(diff(range(v)) - diff(usr)), numeric(1))
        pick <- names(cand)[which.min(gap)]
        if (min(gap) > 0.25 * diff(usr))
          warning("No x definition matches the plot axis well; check the line positions.")
        if (verbose) message("x definition matched to the plot: ", pick)
        cand[[pick]]
      } else {
        if (is.null(cand[[x_def]])) stop("x_def '", x_def, "' is not available for this section")
        cand[[x_def]]
      }
    }
  )
  
  o  <- order(x)
  xs <- x[o]
  zs <- z_raw[o]
  ok <- is.finite(zs)
  
  if (show_ci && all(ci_cols %in% names(tr))) {
    lo <- tr[[ci_cols[1]]][m][o]
    hi <- tr[[ci_cols[2]]][m][o]
    segments(xs[ok], lo[ok], xs[ok], hi[ok], col = col, lwd = lwd)
  }
  if (connect_gaps) lines(xs[ok], zs[ok], col = col, lwd = lwd, lty = lty)
  else              lines(xs,     zs,     col = col, lwd = lwd, lty = lty)
  points(xs[ok], zs[ok], pch = pch, bg = bg, cex = cex)
  
  if (verbose && any(!is.finite(z_raw)))
    message("Stations without a transition depth: ",
            paste(stn_key[!is.finite(z_raw)], collapse = ", "))
  
  invisible(data.frame(station = stn_key[o], x = xs, z = zs))
}

## Draw one section variable to a PNG, optionally with the transition-depth line.
##   which           : variable name in the section (checked before plotting)
##   transition_args : passed to add_transition_line() (e.g. list(fjord = "IK", show_ci = TRUE))
##   plot_args       : extra/override arguments for plot.section()
plot_section_var <- function(section, which, file, zcol, zbreaks = NULL,
                             transition = NULL, transition_args = list(),
                             plot_args = list(), xtype = "distance",
                             ylim = c(400, 0),
                             width = 2400, height = 1600, res = 300) {
  available <- unique(unlist(lapply(section@data$station, function(s) names(s@data))))
  if (!which %in% available) {
    stop("'", which, "' is not in this section. Available: ",
         paste(sort(available), collapse = ", "))
  }
  
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(file, width = width, height = height, res = res, type = "cairo")
  on.exit(grDevices::dev.off(), add = TRUE)
  
  args <- c(list(section, which = which, ztype = "image", ytype = "depth",
                 xtype = xtype, ylim = ylim, legend.loc = "",
                 showStations = TRUE, zcol = zcol),
            if (!is.null(zbreaks)) list(zbreaks = zbreaks))
  args[names(plot_args)] <- plot_args
  do.call(plot, args)
  
  if (!is.null(transition)) {
    ta <- modifyList(list(xtype = xtype), transition_args)
    do.call(add_transition_line, c(list(section, transition), ta))
  }
  invisible(file)
}
