library(data.table)

proj.path <- getwd()

# ---------------------------------------------------------
# 1. Read files
# ---------------------------------------------------------
ml_data <- fread(
  file.path(proj.path, "output", "data", "ml_oilgas_panel_1km_2018_2024_leakage_safe.csv")
)

pollution_ml <- fread(
  file.path(proj.path, "output", "data", "pollution_ml_panel_2018_2024.csv")
)

# ---------------------------------------------------------
# 2. Standardize join-column types
# ---------------------------------------------------------
# Keep ml_data grid_id as the master grid_id
ml_data[, grid_id := as.character(grid_id)]

# pollution grid_id may have been generated separately, so do not trust it for merging
pollution_ml[, grid_id := as.character(grid_id)]

# core time fields
ml_data[, year := as.integer(year)]
pollution_ml[, year := as.integer(year)]

ml_data[, month := as.integer(month)]
pollution_ml[, month := as.integer(month)]

# coordinates
ml_data[, lon := as.numeric(lon)]
pollution_ml[, lon := as.numeric(lon)]

ml_data[, lat := as.numeric(lat)]
pollution_ml[, lat := as.numeric(lat)]

# date
ml_data[, date := as.Date(date)]
pollution_ml[, date := as.Date(date)]

# ---------------------------------------------------------
# 3. Optional coordinate rounding safeguard
# ---------------------------------------------------------
# This helps if one file has tiny floating-point differences
ml_data[, `:=`(
  lon_merge = round(lon, 6),
  lat_merge = round(lat, 6)
)]

pollution_ml[, `:=`(
  lon_merge = round(lon, 6),
  lat_merge = round(lat, 6)
)]

# ---------------------------------------------------------
# 4. Drop pollution grid_id before merge
# ---------------------------------------------------------
# We do this because pollution grid_id may not correspond to ml_data grid_id
pollution_ml[, grid_id_pollution := grid_id]
pollution_ml[, grid_id := NULL]

# ---------------------------------------------------------
# 5. Check for duplicate pollution rows by merge key
# ---------------------------------------------------------
dup_pollution <- pollution_ml[
  ,
  .N,
  by = .(year, month, date, lon_merge, lat_merge)
][N > 1]

if (nrow(dup_pollution) > 0) {
  cat("Warning:", nrow(dup_pollution), "duplicate pollution keys found.\n")
  cat("Keeping first row within each year-month-location key.\n")
  
  setorder(pollution_ml, year, month, date, lon_merge, lat_merge)
  pollution_ml <- pollution_ml[
    ,
    .SD[1],
    by = .(year, month, date, lon_merge, lat_merge)
  ]
}

# ---------------------------------------------------------
# 6. Merge pollution onto ML data
# ---------------------------------------------------------
final_ml <- merge(
  ml_data,
  pollution_ml,
  by = c("year", "month", "date", "lon_merge", "lat_merge"),
  all.x = TRUE,
  suffixes = c("", "_pollution")
)

# ---------------------------------------------------------
# 7. Clean up merged coordinates
# ---------------------------------------------------------
# Keep original lon/lat from ml_data as the master coordinates
# pollution lon/lat may come in as duplicate columns depending on schema

if ("lon_pollution" %in% names(final_ml)) final_ml[, lon_pollution := NULL]
if ("lat_pollution" %in% names(final_ml)) final_ml[, lat_pollution := NULL]

# rename back
setnames(final_ml, old = c("lon_merge", "lat_merge"), new = c("lon_merge_key", "lat_merge_key"))

# ---------------------------------------------------------
# 8. Quick merge diagnostics
# ---------------------------------------------------------
pollution_vars <- c(
  "CO", "NO2", "O3", "PM2_5", "SO2",
  "CO_log", "NO2_log", "O3_log", "PM25_log", "SO2_log"
)

pollution_vars_present <- pollution_vars[pollution_vars %in% names(final_ml)]

if (length(pollution_vars_present) > 0) {
  merge_summary <- final_ml[, lapply(.SD, function(x) mean(!is.na(x))), .SDcols = pollution_vars_present]
  cat("\nShare non-missing after merge:\n")
  print(merge_summary)
}

cat("\nRows in ml_data:     ", nrow(ml_data), "\n")
cat("Rows in pollution_ml:", nrow(pollution_ml), "\n")
cat("Rows in final_ml:    ", nrow(final_ml), "\n")

# ---------------------------------------------------------
# 9. Save final merged file
# ---------------------------------------------------------
dir.create(file.path(proj.path, "output", "data"), recursive = TRUE, showWarnings = FALSE)

write_parquet(
  final_ml,
  file.path(proj.path, "output", "data", "ml_oilgas_pollution.parquet")
)

cat("\nSaved:\n")
cat(file.path(proj.path, "output", "data", "ml_oilgas_pollution_panel_2018_2024.csv"), "\n")