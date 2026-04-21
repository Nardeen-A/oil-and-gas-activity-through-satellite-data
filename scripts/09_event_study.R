# =========================================================
# EVENT STUDY: TRUE KIRKUK BOUNDARY FROM GEE
# =========================================================

library(data.table)
library(sf)
library(fixest)
library(lubridate)

# ---------------------------------------------------------
# 1. File paths
# ---------------------------------------------------------
pred_path   <- "E:/UofT/05_Research/00_File/output/data/predictions/grid_month_predictions.csv"
kirkuk_path <- "E:/UofT/05_Research/00_File/data/raw/kirkuk_boundary.geojson"
out_dir     <- "E:/UofT/05_Research/00_File/output/data"

# ---------------------------------------------------------
# 2. Load clean prediction panel
# ---------------------------------------------------------
df <- fread(pred_path)
setDT(df)

required_cols <- c("grid_id", "date", "pred_prob", "lat", "lon")
missing_cols <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

df[, date := as.Date(date)]
df[, date := as.Date(format(date, "%Y-%m-01"))]
setorder(df, grid_id, date)

# ---------------------------------------------------------
# 3. Convert points to sf
# ---------------------------------------------------------
pts <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# ---------------------------------------------------------
# 4. Load Kirkuk boundary exported from GEE
# ---------------------------------------------------------
kirkuk <- st_read(kirkuk_path, quiet = TRUE)

# Make sure CRS matches
kirkuk <- st_transform(kirkuk, st_crs(pts))

# ---------------------------------------------------------
# 5. Spatial join: identify points inside Kirkuk
# ---------------------------------------------------------
# Keep only one name field if available
name_field <- names(kirkuk)
name_field <- name_field[name_field %in% c("ADM1_NAME", "NAME_1", "shapeName")]
if (length(name_field) == 0) {
  pts_joined <- st_join(pts, kirkuk, left = TRUE)
} else {
  pts_joined <- st_join(pts, kirkuk[, name_field[1], drop = FALSE], left = TRUE)
}

# Define control/treatment
# treated = 0 for Kirkuk, 1 otherwise
join_name <- setdiff(names(pts_joined), names(pts))
if (length(join_name) == 0) {
  stop("Spatial join did not add a Kirkuk identifier column.")
}

pts_joined$treated <- as.integer(is.na(pts_joined[[join_name[1]]]))

# ---------------------------------------------------------
# 6. Convert back to data.table
# ---------------------------------------------------------
df_es <- as.data.table(pts_joined)
if ("geometry" %in% names(df_es)) df_es[, geometry := NULL]

# ---------------------------------------------------------
# 7. Create event time
# ---------------------------------------------------------
shock_date <- as.Date("2022-02-01")

df_es[, event_time := 12L * (year(date) - year(shock_date)) +
        (month(date) - month(shock_date))]

# Keep symmetric event window
df_es <- df_es[event_time >= -24 & event_time <= 24]

# ---------------------------------------------------------
# 8. Sanity checks
# ---------------------------------------------------------
cat("\nTreatment split:\n")
print(table(df_es$treated, useNA = "ifany"))

cat("\nTreatment split by event time:\n")
print(dcast(
  df_es[, .N, by = .(event_time, treated)],
  event_time ~ treated,
  value.var = "N",
  fill = 0
))

cat("\nDuplicate grid_id-date rows (should be empty):\n")
print(df_es[, .N, by = .(grid_id, date)][N > 1])

cat("\nUnique grids by treatment:\n")
print(unique(df_es[, .(grid_id, treated)])[, .N, by = treated])

# ---------------------------------------------------------
# 9. Run DiD-style event study
# ---------------------------------------------------------
event_model <- feols(
  pred_prob ~ i(event_time, treated, ref = -1) | grid_id + date,
  data = df_es,
  cluster = ~grid_id
)

cat("\nEvent study summary:\n")
print(summary(event_model))

# ---------------------------------------------------------
# 10. Plot event study
# ---------------------------------------------------------
iplot(
  event_model,
  main = "Event study: non-Kirkuk relative to Kirkuk",
  xlab = "Months relative to February 2022",
  ylab = "Difference in predicted hydrocarbon risk",
  ref.line = 0
)

# ---------------------------------------------------------
# 11. Save objects
# ---------------------------------------------------------
saveRDS(event_model, file.path(out_dir, "event_model_kirkuk_true_boundary.rds"))
fwrite(df_es, file.path(out_dir, "event_study_panel_kirkuk_true_boundary.csv"))

















# ---------------------------------------------------------
# Extract event-study coefficients from fixest model
# ---------------------------------------------------------
ct <- as.data.table(summary(event_model)$coeftable, keep.rownames = "term")
setnames(ct, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"),
         c("estimate", "std_error", "t_value", "p_value"))

# Keep only event-study interaction terms
plot_dt <- ct[grepl("^event_time::", term)]

# Extract event time from term names
plot_dt[, event_time := as.integer(str_extract(term, "-?\\d+"))]

# Confidence intervals
plot_dt[, ci_low  := estimate - 1.96 * std_error]
plot_dt[, ci_high := estimate + 1.96 * std_error]

setorder(plot_dt, event_time)

# ---------------------------------------------------------
# Main event-study plot
# ---------------------------------------------------------
# pick colors
cols <- viridis(3, option = "D")

p_event <- ggplot(plot_dt, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "solid", color = "grey40") +
  geom_vline(xintercept = 0, linewidth = 0.6, linetype = "dashed", color = "grey40") +
  
  # confidence band
  geom_ribbon(
    aes(ymin = ci_low, ymax = ci_high),
    fill = cols[2],
    alpha = 0.25
  ) +
  
  # line + points
  geom_line(color = cols[1], linewidth = 0.9) +
  geom_point(color = cols[1], size = 1.8) +
  
  scale_x_continuous(breaks = seq(-24, 24, by = 4)) +
  labs(
    x = "Months relative to February 2022",
    y = "Difference in predicted hydrocarbon risk"
  ) +
  theme(
    plot.title = element_text(face = "bold")
  )

print(p_event)




event_model <- feols(
  pred_prob ~ i(event_time, treated, ref = -1) | grid_id + date,
  data = df_es
)

V_conley <- vcov_conley(
  event_model,
  lat = ~lat,
  lon = ~lon,
  cutoff = 100
)

coef_vec <- coef(event_model)
se_vec   <- sqrt(diag(V_conley))







