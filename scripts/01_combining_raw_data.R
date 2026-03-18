# reading in files
proj.path <- getwd()


# raw files locations have been changed!

# folder containing the yearly csv files
data_dir <- file.path(proj.path, "data")

# get all relevant files
files <- list.files(
  data_dir,
  pattern = "^kri_kirkuk_monthly_raw_panel_1km_\\d{4}\\.csv$",
  full.names = TRUE
)

# keep only non-empty files
files_nonempty <- files[file.info(files)$size > 0]

# read and combine
panel_df <- rbindlist(
  lapply(files_nonempty, function(f) {
    dt <- fread(f)
    
    # if year column is missing, extract it from filename
    if (!"year" %in% names(dt)) {
      dt[, year := as.integer(sub(".*_(\\d{4})\\.csv$", "\\1", basename(f)))]
    }
    
    dt
  }),
  fill = TRUE,
  use.names = TRUE
)

# create grid id if useful for panel work
if (all(c("lon", "lat") %in% names(panel_df))) {
  panel_df[, grid_id := .GRP, by = .(lon, lat)]
}

fwrite(panel_df, file.path(data_dir, "kri_kirkuk_monthly_raw_panel_1km_combined_2012_2024.csv"))

# 2018 to 2024
years <- 2018:2024

files <- file.path(
  proj.path, "data",
  paste0("kri_kirkuk_monthly_raw_panel_1km_", years, ".csv")
)

files <- files[file.exists(files) & file.info(files)$size > 0]

panel_df <- rbindlist(
  lapply(files, function(f) {
    dt <- fread(f)
    if (!"year" %in% names(dt)) {
      dt[, year := as.integer(sub(".*_(\\d{4})\\.csv$", "\\1", basename(f)))]
    }
    dt
  }),
  fill = TRUE,
  use.names = TRUE
)

if (all(c("lon", "lat") %in% names(panel_df))) {
  panel_df[, grid_id := .GRP, by = .(lon, lat)]
}

fwrite(panel_df, file.path(proj.path, "data", "kri_kirkuk_monthly_raw_panel_1km_combined_2018_2024.csv"))





# flaring ####
# folder where the flare files are stored
data_dir <- file.path(proj.path, "data", "Methane_Flaring_Sites_VIIRS_1874")

# list flare files (2012–2019)
files <- list.files(
  data_dir,
  pattern = "^[0-9]{4}_flare_list\\.csv$",
  full.names = TRUE
)

# keep only years 2012–2019
files <- files[as.integer(sub("_flare_list\\.csv", "", basename(files))) %in% 2012:2019]

# read and combine
flare_df <- rbindlist(
  lapply(files, function(f) {
    
    dt <- fread(f)
    
    # add year column from filename
    dt[, year := as.integer(sub("_flare_list\\.csv", "", basename(f)))]
    
    dt
  }),
  fill = TRUE,
  use.names = TRUE
)

flare_df <- flare_df[cntry_name == "Iraq"]

fwrite(flare_df, file.path(proj.path, "data", "flares.csv"))
