# reading in files
proj.path <- getwd()
data_1 <- fread(file.path(proj.path, "data", "kri_kirkuk_monthly_raw_panel_1km_combined_2018_2024.csv"))
flare <- fread(file.path(proj.path, "data", "flares.csv"))

# Training period used ONLY for thresholds / normalization choices
train_end_year <- 2020L

# Spatial labeling parameters
utm_crs        <- 32638  # UTM Zone 38N
label_radius_m <- 1000   # 1 km

# Sentinel quality filter
min_s2_count   <- 2L

# Small constant for ratios / logs
eps <- 1e-6







# Clean flare data and create flare sites ####

# replace missing value code
flare[flare == -9999] <- NA

# standardize column names
setnames(
  flare,
  old = c("latitude", "longitude"),
  new = c("lat", "lon"),
  skip_absent = TRUE
)

# remove missing coordinates
flare <- flare[!is.na(lat) & !is.na(lon)]

# remove weak detections
flare <- flare[dtc_freq >= 3]

# remove missing or non-positive flare volume
flare <- flare[!is.na(flr_volume) & flr_volume > 0]
flare <- flare[!is.na(year)]
flare[, flare_year_date := as.Date(sprintf("%04d-01-01", as.integer(year)))]


# If avg_temp / clr_obs missing in schema, create them safely
if (!"avg_temp" %in% names(flare)) flare[, avg_temp := NA_real_]
if (!"clr_obs"  %in% names(flare)) flare[, clr_obs  := NA_real_]

# Aggregate to flare-site-by-year first
flare_site_year <- flare[, .(
  detections    = .N,
  mean_volume   = mean(flr_volume, na.rm = TRUE),
  mean_temp     = mean(avg_temp, na.rm = TRUE),
  mean_dtc_freq = mean(dtc_freq, na.rm = TRUE),
  mean_clr_obs  = mean(clr_obs, na.rm = TRUE)
), by = .(lon, lat, year)]

# Persistent flare sites by location over full flare sample
flare_sites <- flare[, .(
  detections_total = .N,
  first_year       = min(year, na.rm = TRUE),
  last_year        = max(year, na.rm = TRUE),
  mean_volume      = mean(flr_volume, na.rm = TRUE),
  mean_temp        = mean(avg_temp, na.rm = TRUE),
  mean_dtc_freq    = mean(dtc_freq, na.rm = TRUE),
  mean_clr_obs     = mean(clr_obs, na.rm = TRUE)
), by = .(lon, lat)]

# keep persistent flare sites
flare_sites <- flare_sites[detections_total >= 2]

# Keep only site-years belonging to persistent sites
flare_site_year <- merge(
  flare_site_year,
  flare_sites[, .(lon, lat, detections_total, first_year, last_year)],
  by = c("lon", "lat"),
  all.x = TRUE
)

flare_site_year <- flare_site_year[!is.na(detections_total)]

# Convert to sf for later time-consistent labeling
flare_site_year_sf <- st_as_sf(
  flare_site_year,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

flare_sites_sf <- st_as_sf(
  flare_sites,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)










# Clean Google Earth panel data ####
data_1[data_1 == -9999] <- NA


required_panel_cols <- c("grid_id", "year", "month", "lon", "lat")
missing_required <- setdiff(required_panel_cols, names(data_1))
if (length(missing_required) > 0) {
  stop(sprintf("Missing required columns in panel data: %s",
               paste(missing_required, collapse = ", ")))
}

data_1 <- data_1[!is.na(grid_id) & !is.na(year) & !is.na(month) & !is.na(lat) & !is.na(lon)]

data_1[, year  := as.integer(year)]
data_1[, month := as.integer(month)]

data_1 <- data_1[month >= 1 & month <= 12]

data_1[, date := as.Date(sprintf("%04d-%02d-01", year, month))]

setorder(data_1, grid_id, date)

# Quality filter: keep rows with at least some S2 support, or missing s2_count
if ("s2_count" %in% names(data_1)) {
  data_1 <- data_1[is.na(s2_count) | s2_count >= min_s2_count]
} else {
  data_1[, s2_count := NA_real_]
}

# Ensure optional cols exist so downstream code is stable
optional_cols <- c("B4_med", "B8_med", "B11_med", "ndvi", "ndbi", "ntl", "lst_night_c", "cf_cvg")
for (v in optional_cols) {
  if (!v %in% names(data_1)) data_1[, (v) := NA_real_]
}



# Clamp negative ntl to zero if desired; safer if raw source should be nonnegative
data_1[, ntl_clean := fifelse(!is.na(ntl) & ntl < 0, 0, ntl)]

# Log night lights
data_1[, ntl_log := log1p(ntl_clean)]

# Built-up / industrial proxy
data_1[, builtup_index := ndbi - ndvi]

# Spectral ratios
data_1[, nir_red_ratio  := B8_med  / (B4_med  + eps)]
data_1[, swir_nir_ratio := B11_med / (B8_med  + eps)]
data_1[, swir_red_ratio := B11_med / (B4_med  + eps)]

# Interactions
data_1[, heat_light_interaction  := lst_night_c * ntl_log]
data_1[, built_light_interaction := builtup_index * ntl_log]
data_1[, heat_built_interaction  := lst_night_c * builtup_index]

# Composite score
data_1[, oil_signature_score := ntl_log + builtup_index + swir_nir_ratio + lst_night_c - ndvi]







# Temporal anomaly features (within-grid)

data_1[, `:=`(
  ntl_grid_mean      = mean(ntl_log, na.rm = TRUE),
  lst_grid_mean      = mean(lst_night_c, na.rm = TRUE),
  ndvi_grid_mean     = mean(ndvi, na.rm = TRUE),
  ndbi_grid_mean     = mean(ndbi, na.rm = TRUE),
  b11_grid_mean      = mean(B11_med, na.rm = TRUE),
  swir_nir_grid_mean = mean(swir_nir_ratio, na.rm = TRUE)
), by = grid_id]

data_1[, `:=`(
  ntl_anom      = ntl_log - ntl_grid_mean,
  lst_anom      = lst_night_c - lst_grid_mean,
  ndvi_anom     = ndvi - ndvi_grid_mean,
  ndbi_anom     = ndbi - ndbi_grid_mean,
  b11_anom      = B11_med - b11_grid_mean,
  swir_nir_anom = swir_nir_ratio - swir_nir_grid_mean
)]







# Temporal persistence / rolling features

# Rolling means including current month
data_1[, ntl_roll3_raw      := frollmean(ntl_log,         n = 3, align = "right", na.rm = TRUE), by = grid_id]
data_1[, ntl_roll6_raw      := frollmean(ntl_log,         n = 6, align = "right", na.rm = TRUE), by = grid_id]
data_1[, lst_roll3_raw      := frollmean(lst_night_c,     n = 3, align = "right", na.rm = TRUE), by = grid_id]
data_1[, lst_roll6_raw      := frollmean(lst_night_c,     n = 6, align = "right", na.rm = TRUE), by = grid_id]
data_1[, b11_roll3_raw      := frollmean(B11_med,         n = 3, align = "right", na.rm = TRUE), by = grid_id]
data_1[, swir_nir_roll3_raw := frollmean(swir_nir_ratio,  n = 3, align = "right", na.rm = TRUE), by = grid_id]

# One-period lags of raw series
data_1[, ntl_lag1      := shift(ntl_log,         1L), by = grid_id]
data_1[, lst_lag1      := shift(lst_night_c,     1L), by = grid_id]
data_1[, b11_lag1      := shift(B11_med,         1L), by = grid_id]
data_1[, ndvi_lag1     := shift(ndvi,            1L), by = grid_id]
data_1[, ndbi_lag1     := shift(ndbi,            1L), by = grid_id]
data_1[, swir_nir_lag1 := shift(swir_nir_ratio,  1L), by = grid_id]

# Month-to-month changes (current minus lagged previous)
data_1[, ntl_change      := ntl_log        - ntl_lag1]
data_1[, lst_change      := lst_night_c    - lst_lag1]
data_1[, b11_change      := B11_med        - b11_lag1]
data_1[, ndvi_change     := ndvi           - ndvi_lag1]
data_1[, ndbi_change     := ndbi           - ndbi_lag1]
data_1[, swir_nir_change := swir_nir_ratio - swir_nir_lag1]

# Lagged rolling features for leakage-safe ML
data_1[, ntl_roll3      := shift(ntl_roll3_raw,      1L), by = grid_id]
data_1[, ntl_roll6      := shift(ntl_roll6_raw,      1L), by = grid_id]
data_1[, lst_roll3      := shift(lst_roll3_raw,      1L), by = grid_id]
data_1[, lst_roll6      := shift(lst_roll6_raw,      1L), by = grid_id]
data_1[, b11_roll3      := shift(b11_roll3_raw,      1L), by = grid_id]
data_1[, swir_nir_roll3 := shift(swir_nir_roll3_raw, 1L), by = grid_id]






# Train-period thresholds
train_subset <- data_1[year <= train_end_year]

if (nrow(train_subset) == 0) {
  stop("Training subset is empty. Check train_end_year and panel years.")
}

safe_quantile <- function(x, p = 0.90) {
  if (all(is.na(x))) return(NA_real_)
  as.numeric(quantile(x, probs = p, na.rm = TRUE, type = 7))
}

ntl_cut  <- safe_quantile(train_subset$ntl_log,        0.90)
lst_cut  <- safe_quantile(train_subset$lst_night_c,    0.90)
b11_cut  <- safe_quantile(train_subset$B11_med,        0.90)
swir_cut <- safe_quantile(train_subset$swir_nir_ratio, 0.90)

data_1[, high_ntl      := as.integer(!is.na(ntl_log)        & ntl_log        >= ntl_cut)]
data_1[, high_lst      := as.integer(!is.na(lst_night_c)    & lst_night_c    >= lst_cut)]
data_1[, high_b11      := as.integer(!is.na(B11_med)        & B11_med        >= b11_cut)]
data_1[, high_swir_nir := as.integer(!is.na(swir_nir_ratio) & swir_nir_ratio >= swir_cut)]

# Raw rolling persistence counts include current month
data_1[, ntl_persist6_raw      := frollsum(high_ntl,      n = 6, align = "right", na.rm = TRUE), by = grid_id]
data_1[, lst_persist6_raw      := frollsum(high_lst,      n = 6, align = "right", na.rm = TRUE), by = grid_id]
data_1[, b11_persist6_raw      := frollsum(high_b11,      n = 6, align = "right", na.rm = TRUE), by = grid_id]
data_1[, swir_nir_persist6_raw := frollsum(high_swir_nir, n = 6, align = "right", na.rm = TRUE), by = grid_id]

# Lagged persistence counts for ML
data_1[, ntl_persist6      := shift(ntl_persist6_raw,      1L), by = grid_id]
data_1[, lst_persist6      := shift(lst_persist6_raw,      1L), by = grid_id]
data_1[, b11_persist6      := shift(b11_persist6_raw,      1L), by = grid_id]
data_1[, swir_nir_persist6 := shift(swir_nir_persist6_raw, 1L), by = grid_id]






# Seasonality features


data_1[, month_sin := sin(2 * pi * month / 12)]
data_1[, month_cos := cos(2 * pi * month / 12)]






# Convert panel to sf and create flare labels
grid_sf <- st_as_sf(
  unique(data_1[, .(grid_id, lon, lat)]),
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

grid_sf_utm <- st_transform(grid_sf, utm_crs)



# IMPORTANT:
# transform to projected CRS so distance is in meters
# EPSG:32638 is UTM zone 38N, appropriate for much of northern Iraq
flare_site_year_sf_utm <- st_transform(flare_site_year_sf, utm_crs)
flare_sites_sf_utm     <- st_transform(flare_sites_sf, utm_crs)



# Prepare per-grid static minimum distance to any persistent flare site
dist_mat <- st_distance(grid_sf_utm, flare_sites_sf_utm)
grid_sf_utm$dist_to_flare_m <- apply(dist_mat, 1, min)
grid_sf_utm$dist_to_flare_m <- as.numeric(grid_sf_utm$dist_to_flare_m)

grid_labels_static <- as.data.table(st_drop_geometry(grid_sf_utm))[, .(
  grid_id,
  lon,
  lat,
  dist_to_flare_m
)]

# Build flare label by year using only flare sites observed up to that year
panel_years <- sort(unique(data_1$year))
grid_year_labels <- vector("list", length(panel_years))

for (i in seq_along(panel_years)) {
  yy <- panel_years[i]
  
  flare_up_to_y <- flare_site_year_sf_utm[flare_site_year_sf_utm$year <= yy, ]
  
  if (nrow(flare_up_to_y) == 0) {
    flare_label_y <- rep(0L, nrow(grid_sf_utm))
  } else {
    flare_label_y <- as.integer(
      lengths(st_is_within_distance(grid_sf_utm, flare_up_to_y, dist = label_radius_m)) > 0
    )
  }
  
  grid_year_labels[[i]] <- data.table(
    grid_id      = grid_sf_utm$grid_id,
    year         = yy,
    flare_label  = flare_label_y
  )
}

grid_year_labels <- rbindlist(grid_year_labels)

# Merge yearly time-consistent labels onto full month panel
data_1 <- merge(
  data_1,
  grid_year_labels,
  by = c("grid_id", "year"),
  all.x = TRUE
)

# Merge static distance
data_1 <- merge(
  data_1,
  grid_labels_static,
  by = c("grid_id", "lon", "lat"),
  all.x = TRUE
)

# Optional heuristic exploration label

ntl_q75   <- safe_quantile(train_subset$ntl_log,        0.75)
lst_q75   <- safe_quantile(train_subset$lst_night_c,    0.75)
swir_q75  <- safe_quantile(train_subset$swir_nir_ratio, 0.75)
built_q75 <- safe_quantile(train_subset$builtup_index,  0.75)

data_1[, oilgas_candidate := as.integer(
  !is.na(ntl_log) &
    !is.na(lst_night_c) &
    !is.na(swir_nir_ratio) &
    !is.na(builtup_index) &
    ntl_log        > ntl_q75 &
    lst_night_c    > lst_q75 &
    swir_nir_ratio > swir_q75 &
    builtup_index  > built_q75
)]

# Missingness indicators for ML

feature_missing_flags <- c(
  "B4_med", "B8_med", "B11_med",
  "ndvi", "ndbi", "ntl", "lst_night_c",
  "s2_count", "cf_cvg",
  "ntl_roll3", "ntl_roll6", "lst_roll3", "lst_roll6",
  "b11_roll3", "swir_nir_roll3",
  "ntl_change", "lst_change", "b11_change", "ndvi_change",
  "ntl_persist6", "lst_persist6", "b11_persist6", "swir_nir_persist6"
)

for (v in feature_missing_flags) {
  if (v %in% names(data_1)) {
    data_1[, (paste0(v, "_miss")) := as.integer(is.na(get(v)))]
  }
}

# Final ML dataset


ml_data <- data_1[, .(
  # identifiers
  grid_id,
  year,
  month,
  date,
  lon,
  lat,
  
  # raw bands
  B4_med,
  B8_med,
  B11_med,
  
  # raw variables
  ndvi,
  ndbi,
  ntl,
  ntl_clean,
  ntl_log,
  lst_night_c,
  s2_count,
  cf_cvg,
  
  # engineered contemporaneous features
  builtup_index,
  nir_red_ratio,
  swir_nir_ratio,
  swir_red_ratio,
  heat_light_interaction,
  built_light_interaction,
  heat_built_interaction,
  oil_signature_score,
  
  # anomalies
  ntl_anom,
  lst_anom,
  ndvi_anom,
  ndbi_anom,
  b11_anom,
  swir_nir_anom,
  
  # lagged levels
  ntl_lag1,
  lst_lag1,
  b11_lag1,
  ndvi_lag1,
  ndbi_lag1,
  swir_nir_lag1,
  
  # lagged rolling features
  ntl_roll3,
  ntl_roll6,
  lst_roll3,
  lst_roll6,
  b11_roll3,
  swir_nir_roll3,
  
  # changes
  ntl_change,
  lst_change,
  b11_change,
  ndvi_change,
  ndbi_change,
  swir_nir_change,
  
  # lagged persistence
  ntl_persist6,
  lst_persist6,
  b11_persist6,
  swir_nir_persist6,
  
  # seasonality
  month_sin,
  month_cos,
  
  # labels / validation variables
  flare_label,
  dist_to_flare_m,
  oilgas_candidate,
  
  # missing flags
  B4_med_miss,
  B8_med_miss,
  B11_med_miss,
  ndvi_miss,
  ndbi_miss,
  ntl_miss,
  lst_night_c_miss,
  s2_count_miss,
  cf_cvg_miss,
  ntl_roll3_miss,
  ntl_roll6_miss,
  lst_roll3_miss,
  lst_roll6_miss,
  b11_roll3_miss,
  swir_nir_roll3_miss,
  ntl_change_miss,
  lst_change_miss,
  b11_change_miss,
  ndvi_change_miss,
  ntl_persist6_miss,
  lst_persist6_miss,
  b11_persist6_miss,
  swir_nir_persist6_miss
)]

# Optional: explicit split flag for downstream ML
ml_data[, split := fifelse(year <= train_end_year, "train",
                           fifelse(year == (train_end_year + 1L), "validation", "test"))]

# =========================================================
# 13. Save outputs
# =========================================================

fwrite(flare,            file.path(proj.path, "output", "data", "flare_clean_iraq_2012_2019.csv"))
fwrite(flare_sites,      file.path(proj.path, "output", "data", "flare_sites_iraq_2012_2019.csv"))
fwrite(flare_site_year,  file.path(proj.path, "output", "data", "flare_site_year_iraq_2012_2019.csv"))
fwrite(ml_data,          file.path(proj.path, "output", "data", "ml_oilgas_panel_1km_2018_2024_leakage_safe.csv"))
































