proj.path <- getwd()
data <- fread(file.path(proj.path, "output", "data", "ml_oilgas_panel_1km_2018_2024_leakage_safe.csv"))

x <- data

# Maps
plot_dir <- file.path(proj.path, "output", "plot")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# 1. Pick month and clean obvious missing codes
# ---------------------------------------------------------
plot_dt <- copy(x[year == 2024 & month == 4])
plot_dt[plot_dt == -9999] <- NA

# ---------------------------------------------------------
# 2. Convert centroids to sf points
# ---------------------------------------------------------
pts <- st_as_sf(
  plot_dt,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

# transform to UTM 38N so 1 km cells are in meters
pts_utm <- st_transform(pts, 32638)

# ---------------------------------------------------------
# 3. Rebuild 1x1 km grid cells from centroids
# ---------------------------------------------------------
make_square <- function(point_geom, cell_size = 1000) {
  xy <- st_coordinates(point_geom)
  x <- xy[1]
  y <- xy[2]
  half <- cell_size / 2
  
  coords <- matrix(
    c(
      x - half, y - half,
      x + half, y - half,
      x + half, y + half,
      x - half, y + half,
      x - half, y - half
    ),
    ncol = 2,
    byrow = TRUE
  )
  
  st_polygon(list(coords))
}

grid_polys <- st_sfc(
  lapply(st_geometry(pts_utm), make_square),
  crs = 32638
)

grid_sf <- st_sf(
  st_drop_geometry(pts_utm),
  geometry = grid_polys
)

# transform back to lat/lon for plotting
grid_sf_ll <- st_transform(grid_sf, 4326)

# ---------------------------------------------------------
# 4. Variables to map
# ---------------------------------------------------------
vars_to_map <- c(
  "ntl",
  "lst_night_c",
  "cf_cvg",
  "ndvi",
  "ndbi",
  "B11_med",
  "swir_nir_ratio",
  "oil_signature_score",
  "builtup_index"
)

vars_to_map <- vars_to_map[vars_to_map %in% names(grid_sf_ll)]

# ---------------------------------------------------------
# 5. Pretty labels
# ---------------------------------------------------------
pretty_title <- c(
  ntl = "Night Lights",
  lst_night_c = "Night Land Surface Temperature (°C)",
  cf_cvg = "Cloud-Free Coverage",
  ndvi = "NDVI",
  ndbi = "NDBI",
  B11_med = "SWIR Band (B11)",
  swir_nir_ratio = "SWIR / NIR Ratio",
  oil_signature_score = "Oil Signature Score",
  builtup_index = "Built-Up Index"
)

pretty_fill <- c(
  ntl = "Log night lights",
  lst_night_c = "Night LST (°C)",
  cf_cvg = "Log cloud-free coverage",
  ndvi = "NDVI",
  ndbi = "NDBI",
  B11_med = "Log SWIR band (B11)",
  swir_nir_ratio = "Log SWIR/NIR",
  oil_signature_score = "Oil signature score",
  builtup_index = "Built-up index"
)

# ---------------------------------------------------------
# 6. Transformation rules
# ---------------------------------------------------------
log1p_vars <- c(
  "ntl",
  "cf_cvg"
)

log_vars <- c(
  "B11_med",
  "swir_nir_ratio"
)

centered_vars <- c(
  "ndvi",
  "ndbi",
  "builtup_index",
  "oil_signature_score"
)

level_vars <- c(
  "lst_night_c"
)

# ---------------------------------------------------------
# 7. Helper: transformed plot values
# ---------------------------------------------------------
make_plot_value <- function(x, varname) {
  v <- copy(x[[varname]])
  v <- as.numeric(v)
  
  if (varname %in% log1p_vars) {
    v[v <= -1] <- NA_real_
    return(log1p(v))
  }
  
  if (varname %in% log_vars) {
    v[v <= 0] <- NA_real_
    return(log(v))
  }
  
  return(v)
}

# ---------------------------------------------------------
# 8. Bounding box for tight crop
# ---------------------------------------------------------
bb <- st_bbox(grid_sf_ll)
xpad <- (bb$xmax - bb$xmin) * 0.025
ypad <- (bb$ymax - bb$ymin) * 0.025

# ---------------------------------------------------------
# 9. Clean map theme
# ---------------------------------------------------------
theme_map_polished <- function() {
  theme_void() +
    theme(
      plot.title = element_text(size = 14.5, face = "bold", hjust = 0.5),
      legend.position = "none",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(8, 10, 8, 8)
    )
}

# ---------------------------------------------------------
# 10. Main plotting function
# ---------------------------------------------------------
make_pretty_map <- function(sf_dt, varname) {
  sf_plot <- copy(sf_dt)
  sf_plot$plot_value <- make_plot_value(sf_plot, varname)
  
  if (all(is.na(sf_plot$plot_value))) {
    return(NULL)
  }
  
  title_txt <- if (varname %in% names(pretty_title)) pretty_title[[varname]] else varname
  fill_txt  <- if (varname %in% names(pretty_fill))  pretty_fill[[varname]]  else varname
  
  
  q <- quantile(sf_plot$plot_value, probs = c(0.02, 0.98), na.rm = TRUE)
  if (any(!is.finite(q)) || diff(q) == 0) {
    q <- range(sf_plot$plot_value, na.rm = TRUE)
  }
  
  # centered diverging maps
  if (varname %in% centered_vars) {
    lim <- max(abs(q), na.rm = TRUE)
    
    p <- ggplot(sf_plot) +
      geom_sf(aes(fill = plot_value), color = NA, linewidth = 0) +
      scale_fill_gradient2(
        low = "#3b0f70",
        mid = "#f7f7f7",
        high = "#fdae61",
        midpoint = 0,
        limits = c(-lim, lim),
        oob = squish,
        na.value = "grey92",
        name = fill_txt
      ) +
      coord_sf(
        xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
        ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
        expand = FALSE
      ) +
      labs(
        title = title_txt      
        ) +
      theme_map_polished()
    
    return(p)
  }
  
  # sequential maps
  p <- ggplot(sf_plot) +
    geom_sf(aes(fill = plot_value), color = NA, linewidth = 0) +
    scale_fill_viridis_c(
      option = "magma",
      limits = q,
      oob = squish,
      na.value = "grey92",
      name = fill_txt
    ) +
    coord_sf(
      xlim = c(bb["xmin"] - xpad, bb["xmax"] + xpad),
      ylim = c(bb["ymin"] - ypad, bb["ymax"] + ypad),
      expand = FALSE
    ) +
    labs(
      title = title_txt
    ) +
    theme_map_polished()
  
  p
}

# ---------------------------------------------------------
# 11. Generate and save all plots
# ---------------------------------------------------------
plot_list <- list()

for (v in vars_to_map) {
  message("Making plot: ", v)
  
  p <- make_pretty_map(grid_sf_ll, v)
  
  if (!is.null(p)) {
    plot_list[[v]] <- p
    
    ggsave(
      filename = file.path(plot_dir, paste0(v, "_map_polished.png")),
      plot = p,
      width = 8.8,
      height = 6.4,
      dpi = 500,
      bg = "white"
    )
  }
}

# ---------------------------------------------------------
# 12. Print all plots
# ---------------------------------------------------------
for (nm in names(plot_list)) {
  print(plot_list[[nm]])
}






































# ---------------------------------------------------------
# 1. Build lightweight panel
# ---------------------------------------------------------
base_dt <- data[, .(
  grid_id,
  year,
  month,
  date = as.Date(sprintf("%04d-%02d-01", year, month)),
  ntl,
  lst_night_c,
  cf_cvg,
  ndvi,
  ndbi,
  B11_med,
  swir_nir_ratio,
  oil_signature_score,
  builtup_index
)]

# ---------------------------------------------------------
# 2. Replace obvious missing codes
# ---------------------------------------------------------
num_cols <- setdiff(names(base_dt), c("grid_id", "year", "month", "date"))
for (j in num_cols) {
  set(base_dt, i = which(base_dt[[j]] == -9999), j = j, value = NA_real_)
}

# ---------------------------------------------------------
# 3. Collapse duplicate grid-month rows if any
# ---------------------------------------------------------
dup_check <- base_dt[, .N, by = .(grid_id, date)][N > 1]

if (nrow(dup_check) > 0) {
  message("Found duplicate grid_id-date rows. Collapsing by mean.")
  
  base_dt <- base_dt[
    , c(
      lapply(.SD, function(x) {
        if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
      }),
      .(year = year[1], month = month[1])
    ),
    by = .(grid_id, date),
    .SDcols = num_cols
  ]
  
  setcolorder(base_dt, c("grid_id", "year", "month", "date", num_cols))
}

setorder(base_dt, grid_id, date)

# ---------------------------------------------------------
# 4. Variables to plot
# ---------------------------------------------------------
vars_to_plot <- intersect(c(
  "ntl",
  "lst_night_c",
  "cf_cvg",
  "ndvi",
  "ndbi",
  "B11_med",
  "swir_nir_ratio",
  "oil_signature_score",
  "builtup_index"
), names(base_dt))

pretty_title <- c(
  ntl = "Night Lights",
  lst_night_c = "Night Land Surface Temperature (°C)",
  cf_cvg = "Cloud-Free Coverage",
  ndvi = "NDVI",
  ndbi = "NDBI",
  B11_med = "SWIR Band (B11)",
  swir_nir_ratio = "SWIR / NIR Ratio",
  oil_signature_score = "Oil Signature Score",
  builtup_index = "Built-Up Index"
)

theme_ts_clean <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 15, face = "bold"),
      plot.subtitle = element_text(size = 10.5, color = "grey35"),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# ---------------------------------------------------------
# 5. Variable-specific transforms
# ---------------------------------------------------------
transform_vec <- function(x, varname) {
  x <- as.numeric(x)
  
  if (varname %in% c("ntl", "cf_cvg")) {
    x[x < 0] <- NA_real_
    return(log1p(x))
  }
  
  if (varname %in% c("B11_med", "swir_nir_ratio")) {
    x[x <= 0] <- NA_real_
    return(log(x))
  }
  
  x
}

# ---------------------------------------------------------
# 6. Helper to build monthly aggregate plot data
# ---------------------------------------------------------
make_monthly_series <- function(dt, value_col, smooth_k = 6) {
  out <- dt[
    !is.na(get(value_col)),
    .(
      mean_value   = mean(get(value_col), na.rm = TRUE),
      median_value = median(get(value_col), na.rm = TRUE),
      p25          = quantile(get(value_col), 0.25, na.rm = TRUE),
      p75          = quantile(get(value_col), 0.75, na.rm = TRUE),
      n_cells      = sum(!is.na(get(value_col)))
    ),
    by = date
  ][order(date)]
  
  out[, mean_smooth := frollmean(mean_value, smooth_k, align = "center", na.rm = TRUE)]
  out[, p25_smooth  := frollmean(p25,        smooth_k, align = "center", na.rm = TRUE)]
  out[, p75_smooth  := frollmean(p75,        smooth_k, align = "center", na.rm = TRUE)]
  
  out
}

# ---------------------------------------------------------
# 7. Main loop
# ---------------------------------------------------------
for (v in vars_to_plot) {
  message("Making series for: ", v)
  
  title_txt <- if (v %in% names(pretty_title)) pretty_title[[v]] else v
  
  dtv <- base_dt[, .(
    grid_id,
    date,
    month,
    value = get(v)
  )]
  
  # transform the raw variable
  dtv[, value := transform_vec(value, v)]
  
  # one row per grid-month
  dtv <- dtv[
    , .(
      value = {
        z <- value
        if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE)
      }
    ),
    by = .(grid_id, date, month)
  ]
  
  # -------------------------------------------------------
  # A. RAW SERIES
  # -------------------------------------------------------
  monthly_raw <- make_monthly_series(dtv, "value", smooth_k = 6)
  
  p_raw <- ggplot(monthly_raw, aes(x = date, y = mean_smooth)) +
    geom_ribbon(aes(ymin = p25_smooth, ymax = p75_smooth), alpha = 0.18) +
    geom_line(linewidth = 0.9) +
    geom_point(aes(y = mean_value), size = 0.9, alpha = 0.65) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title = paste0(title_txt, " — Raw"),
      subtitle = "Raw monthly level with 6-month smoothing",
      x = NULL,
      y = NULL
    ) +
    theme_ts_clean()
  
  ggsave(
    filename = file.path(plot_dir, paste0(v, "_timeseries_raw.png")),
    plot = p_raw,
    width = 8.5,
    height = 5,
    dpi = 400,
    bg = "white"
  )
  
  # -------------------------------------------------------
  # B. DESEASONALIZED SERIES
  # Remove each grid's average calendar-month effect
  # -------------------------------------------------------
  dt_ds <- copy(dtv)
  dt_ds[, month_mean := mean(value, na.rm = TRUE), by = .(grid_id, month)]
  dt_ds[is.nan(month_mean), month_mean := NA_real_]
  dt_ds[, value_ds := value - month_mean]
  
  monthly_ds <- make_monthly_series(dt_ds, "value_ds", smooth_k = 6)
  
  p_ds <- ggplot(monthly_ds, aes(x = date, y = mean_smooth)) +
    geom_ribbon(aes(ymin = p25_smooth, ymax = p75_smooth), alpha = 0.18) +
    geom_line(linewidth = 0.9) +
    geom_point(aes(y = mean_value), size = 0.9, alpha = 0.65) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title = paste0(title_txt, " — Deseasonalized"),
      subtitle = "Grid-specific month-of-year effects removed; 6-month smoothing",
      x = NULL,
      y = NULL
    ) +
    theme_ts_clean()
  
  ggsave(
    filename = file.path(plot_dir, paste0(v, "_timeseries_deseasonalized.png")),
    plot = p_ds,
    width = 8.5,
    height = 5,
    dpi = 400,
    bg = "white"
  )
  
  # -------------------------------------------------------
  # C. ANOMALY SERIES
  # Same as deseasonalized here: deviation from each grid's
  # usual value in that calendar month
  # -------------------------------------------------------
  dt_anom <- copy(dtv)
  dt_anom[, clim := mean(value, na.rm = TRUE), by = .(grid_id, month)]
  dt_anom[is.nan(clim), clim := NA_real_]
  dt_anom[, anomaly := value - clim]
  
  monthly_anom <- make_monthly_series(dt_anom, "anomaly", smooth_k = 6)
  
  p_anom <- ggplot(monthly_anom, aes(x = date, y = mean_smooth)) +
    geom_ribbon(aes(ymin = p25_smooth, ymax = p75_smooth), alpha = 0.18) +
    geom_line(linewidth = 0.9) +
    geom_point(aes(y = mean_value), size = 0.9, alpha = 0.65) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title = paste0(title_txt, " — Anomaly"),
      subtitle = "Deviation from each grid's usual calendar-month level; 6-month smoothing",
      x = NULL,
      y = NULL
    ) +
    theme_ts_clean()
  
  ggsave(
    filename = file.path(plot_dir, paste0(v, "_timeseries_anomaly.png")),
    plot = p_anom,
    width = 8.5,
    height = 5,
    dpi = 400,
    bg = "white"
  )
  
  # print to screen
  print(p_raw)
  print(p_ds)
  print(p_anom)
  
  rm(dtv, dt_ds, dt_anom,
     monthly_raw, monthly_ds, monthly_anom,
     p_raw, p_ds, p_anom)
  gc()
}