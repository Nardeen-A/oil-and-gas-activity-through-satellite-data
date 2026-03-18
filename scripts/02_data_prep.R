# reading in files
proj.path <- getwd()
data_1 <- fread(file.path(proj.path, "data", "kri_kirkuk_monthly_raw_panel_1km_combined_2018_2024.csv"))
flare <- fread(file.path(proj.path, "data", "flares.csv"))








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

# create flare site dataset
flare_sites <- flare[, .(
  detections       = .N,
  first_year       = min(year, na.rm = TRUE),
  last_year        = max(year, na.rm = TRUE),
  mean_volume      = mean(flr_volume, na.rm = TRUE),
  mean_temp        = mean(avg_temp, na.rm = TRUE),
  mean_dtc_freq    = mean(dtc_freq, na.rm = TRUE),
  mean_clr_obs     = mean(clr_obs, na.rm = TRUE)
), by = .(lon, lat)]

# keep persistent flare sites
flare_sites <- flare_sites[detections >= 2]

# convert flare sites to sf
flare_sites_sf <- st_as_sf(
  flare_sites,
  coords = c("lon", "lat"),
  crs = 4326
)











# Clean Google Earth panel data ####


# replace GEE missing code
data_1[data_1 == -9999] <- NA

# remove rows without spatial coordinates
data_1 <- data_1[!is.na(lat) & !is.na(lon)]

# create date variable
data_1[, date := as.Date(sprintf("%04d-%02d-01", year, month))]

# sort panel
setorder(data_1, grid_id, date)

# keep observations with at least some Sentinel-2 support
# you can tighten this later if you want
data_1 <- data_1[is.na(s2_count) | s2_count >= 2]

# Features ####

# log night lights
data_1[, ntl_log := log1p(ntl)]

# industrial / built-up features
data_1[, builtup_index := ndbi - ndvi]

# spectral ratios
data_1[, nir_red_ratio  := B8_med / (B4_med + 1e-6)]
data_1[, swir_nir_ratio := B11_med / (B8_med + 1e-6)]
data_1[, swir_red_ratio := B11_med / (B4_med + 1e-6)]

# interaction features
data_1[, heat_light_interaction := lst_night_c * ntl_log]
data_1[, built_light_interaction := builtup_index * ntl_log]
data_1[, heat_built_interaction := lst_night_c * builtup_index]

# simple oil-vs-urban composite score
data_1[, oil_signature_score := ntl_log + builtup_index + swir_nir_ratio + lst_night_c - ndvi]

# 4. Temporal anomaly features (within-grid)

# grid-level means
data_1[, `:=`(
  ntl_grid_mean = mean(ntl_log, na.rm = TRUE),
  lst_grid_mean = mean(lst_night_c, na.rm = TRUE),
  ndvi_grid_mean = mean(ndvi, na.rm = TRUE),
  ndbi_grid_mean = mean(ndbi, na.rm = TRUE),
  b11_grid_mean = mean(B11_med, na.rm = TRUE),
  swir_nir_grid_mean = mean(swir_nir_ratio, na.rm = TRUE)
), by = grid_id]

# anomalies relative to each grid's own history
data_1[, `:=`(
  ntl_anom = ntl_log - ntl_grid_mean,
  lst_anom = lst_night_c - lst_grid_mean,
  ndvi_anom = ndvi - ndvi_grid_mean,
  ndbi_anom = ndbi - ndbi_grid_mean,
  b11_anom = B11_med - b11_grid_mean,
  swir_nir_anom = swir_nir_ratio - swir_nir_grid_mean
)]

# =========================================================
# 5. Temporal persistence / rolling features
# =========================================================

# rolling means by grid
data_1[, ntl_roll3  := frollmean(ntl_log,      n = 3, align = "right"), by = grid_id]
data_1[, ntl_roll6  := frollmean(ntl_log,      n = 6, align = "right"), by = grid_id]
data_1[, lst_roll3  := frollmean(lst_night_c,  n = 3, align = "right"), by = grid_id]
data_1[, lst_roll6  := frollmean(lst_night_c,  n = 6, align = "right"), by = grid_id]
data_1[, b11_roll3  := frollmean(B11_med,      n = 3, align = "right"), by = grid_id]
data_1[, swir_nir_roll3 := frollmean(swir_nir_ratio, n = 3, align = "right"), by = grid_id]

# lags
data_1[, ntl_lag1 := shift(ntl_log, 1), by = grid_id]
data_1[, lst_lag1 := shift(lst_night_c, 1), by = grid_id]
data_1[, b11_lag1 := shift(B11_med, 1), by = grid_id]
data_1[, ndvi_lag1 := shift(ndvi, 1), by = grid_id]

# changes
data_1[, ntl_change := ntl_log - ntl_lag1]
data_1[, lst_change := lst_night_c - lst_lag1]
data_1[, b11_change := B11_med - b11_lag1]
data_1[, ndvi_change := ndvi - ndvi_lag1]

# persistence indicators using sample-wide high thresholds
ntl_cut  <- data_1[, quantile(ntl_log, 0.90, na.rm = TRUE)]
lst_cut  <- data_1[, quantile(lst_night_c, 0.90, na.rm = TRUE)]
b11_cut  <- data_1[, quantile(B11_med, 0.90, na.rm = TRUE)]
swir_cut <- data_1[, quantile(swir_nir_ratio, 0.90, na.rm = TRUE)]

data_1[, high_ntl      := as.integer(ntl_log >= ntl_cut)]
data_1[, high_lst      := as.integer(lst_night_c >= lst_cut)]
data_1[, high_b11      := as.integer(B11_med >= b11_cut)]
data_1[, high_swir_nir := as.integer(swir_nir_ratio >= swir_cut)]

# rolling persistence counts
data_1[, ntl_persist6      := frollsum(high_ntl,      n = 6, align = "right"), by = grid_id]
data_1[, lst_persist6      := frollsum(high_lst,      n = 6, align = "right"), by = grid_id]
data_1[, b11_persist6      := frollsum(high_b11,      n = 6, align = "right"), by = grid_id]
data_1[, swir_nir_persist6 := frollsum(high_swir_nir, n = 6, align = "right"), by = grid_id]

# =========================================================
# 6. Seasonality features
# =========================================================

data_1[, month_sin := sin(2 * pi * month / 12)]
data_1[, month_cos := cos(2 * pi * month / 12)]

# =========================================================
# 7. Convert panel to sf and create flare labels
# =========================================================

grid_sf <- st_as_sf(
  unique(data_1[, .(grid_id, lon, lat)]),
  coords = c("lon", "lat"),
  crs = 4326
)

# IMPORTANT:
# transform to projected CRS so distance is in meters
# EPSG:32638 is UTM zone 38N, appropriate for much of northern Iraq
grid_sf_utm   <- st_transform(grid_sf, 32638)
flare_sf_utm  <- st_transform(flare_sites_sf, 32638)

# binary label: within 1 km of persistent flare site
grid_sf_utm$flare_label <- lengths(
  st_is_within_distance(grid_sf_utm, flare_sf_utm, dist = 1000)
) > 0

grid_sf_utm$flare_label <- as.integer(grid_sf_utm$flare_label)

# minimum distance to known flare site
dist_mat <- st_distance(grid_sf_utm, flare_sf_utm)
grid_sf_utm$dist_to_flare_m <- apply(dist_mat, 1, min)
grid_sf_utm$dist_to_flare_m <- as.numeric(grid_sf_utm$dist_to_flare_m)

# back to regular data.table
grid_labels <- as.data.table(st_drop_geometry(grid_sf_utm))

# geometry column names may differ after conversion, so rebuild lon/lat from original grid_sf if needed
coords_orig <- as.data.table(st_coordinates(grid_sf))
grid_labels[, `:=`(
  lon = coords_orig$X,
  lat = coords_orig$Y
)]

# keep only what we need
grid_labels <- grid_labels[, .(grid_id, lon, lat, flare_label, dist_to_flare_m)]

# merge labels back into full panel
data_1 <- merge(
  data_1,
  grid_labels,
  by = c("grid_id", "lon", "lat"),
  all.x = TRUE
)

# =========================================================
# 8. Optional: create stronger candidate labels for oil/gas activity
# =========================================================
# flare_label is strict and useful for supervised training.
# You can also build a broader "oilgas_candidate" heuristic for exploration.

data_1[, oilgas_candidate := as.integer(
  ntl_log > quantile(ntl_log, 0.75, na.rm = TRUE) &
    lst_night_c > quantile(lst_night_c, 0.75, na.rm = TRUE) &
    swir_nir_ratio > quantile(swir_nir_ratio, 0.75, na.rm = TRUE) &
    builtup_index > quantile(builtup_index, 0.75, na.rm = TRUE)
)]

# =========================================================
# 9. Keep final ML columns
# =========================================================

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
  
  # raw remote sensing variables
  ndvi,
  ndbi,
  ntl,
  ntl_log,
  lst_night_c,
  s2_count,
  cf_cvg,
  
  # engineered features
  builtup_index,
  nir_red_ratio,
  swir_nir_ratio,
  swir_red_ratio,
  heat_light_interaction,
  built_light_interaction,
  heat_built_interaction,
  oil_signature_score,
  
  # anomaly features
  ntl_anom,
  lst_anom,
  ndvi_anom,
  ndbi_anom,
  b11_anom,
  swir_nir_anom,
  
  # rolling / lag / persistence features
  ntl_roll3,
  ntl_roll6,
  lst_roll3,
  lst_roll6,
  b11_roll3,
  swir_nir_roll3,
  ntl_change,
  lst_change,
  b11_change,
  ndvi_change,
  ntl_persist6,
  lst_persist6,
  b11_persist6,
  swir_nir_persist6,
  
  # seasonality
  month_sin,
  month_cos,
  
  # labels
  flare_label,
  dist_to_flare_m,
  oilgas_candidate
)]

# =========================================================
# 10. Save outputs
# =========================================================

fwrite(flare, file.path(proj.path, "output", "data", "flare_clean_iraq_2012_2019.csv"))
fwrite(flare_sites, file.path(proj.path, "output", "data", "flare_sites_iraq_2012_2019.csv"))
fwrite(ml_data, file.path(proj.path, "output", "data", "ml_oilgas_panel_1km_2018_2024.csv"))
































