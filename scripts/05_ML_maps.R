# ---------------------------------------------------------
# 1. Load ML predictions
# ---------------------------------------------------------
proj.path <- getwd()
grid_scores <- read.csv(file.path(proj.path, "output", "data", "predictions", "grid_level_scores.csv"))
df <- read.csv(file.path(proj.path, "output", "data", "predictions", "grid_month_predictions.csv"))
discovery_candidates <- read.csv(file.path(proj.path, "output", "data", "candidates", "discovery_candidates.csv"))
strict_candidates <- read.csv(file.path(proj.path, "output", "data", "candidates", "strict_candidates.csv"))
strict_candidates_filtered <- read.csv(file.path(proj.path, "output", "data", "candidates", "strict_candidates_filtered.csv"))



grid_sf <- st_as_sf(grid_scores, coords = c("lon", "lat"), crs = 4326)

# Predicted probability that a grid cell contains oil & gas activity
p_mean <- ggplot(grid_sf) +
  geom_sf(aes(color = mean_risk), size = 1.4, alpha = 0.9) +
  scale_color_viridis_c(
    option = "magma",
    limits = c(0, 1),
    name = "Mean risk"
  ) +
  labs(
    title = "Grid-Level Mean ML Risk",
    subtitle = "Average predicted probability by grid"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.2)
  )

print(p_mean)

p_max <- ggplot(grid_sf) +
  geom_sf(aes(color = max_risk), size = 1.4, alpha = 0.9) +
  scale_color_viridis_c(
    option = "magma",
    limits = c(0, 1),
    name = "Max risk"
  ) +
  labs(
    title = "Grid-Level Maximum ML Risk",
    subtitle = "Maximum predicted probability by grid"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.2)
  )

print(p_max)

###########################################################################################
p_grid_hist <- ggplot(grid_scores, aes(x = mean_risk)) +
  geom_histogram(bins = 40, fill = "#440154FF", color = "white", alpha = 0.9) +
  labs(
    title = "Distribution of Grid-Level Mean Risk",
    x = "Mean risk",
    y = "Count"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(p_grid_hist)

p_grid_hist_max <- ggplot(grid_scores, aes(x = max_risk)) +
  geom_histogram(bins = 40, fill = "#440154FF", color = "white", alpha = 0.9) +
  labs(
    title = "Distribution of Grid-Level Maximum Risk",
    x = "Max risk",
    y = "Count"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(p_grid_hist_max)

grid_scores <- grid_scores %>%
  filter(!(strict_active_months == 0))

p_active_vs_risk <- ggplot(
  grid_scores,
  aes(x = strict_active_months, y = max_risk)
) +
  geom_point(alpha = 0.35, size = 1.8, color = "#440154FF") +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
  labs(
    title = "Persistence and Maximum Predicted Risk",
    subtitle = "Grids active in more strict months tend to have higher model risk",
    x = "Strict active months",
    y = "Maximum risk"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(p_active_vs_risk)

###########################################################################################
df <- df %>%
  filter(!(year == 2018 & month == 1))

if (!"date" %in% names(df)) {
  df$date <- as.Date(sprintf("%04d-%02d-01", df$year, df$month))
} else {
  df$date <- as.Date(df$date)
}

ts_avg <- df[
  is.finite(pred_prob),
  .(avg_pred_prob = mean(pred_prob, na.rm = TRUE)),
  by = date
][order(date)]

p_ts_avg <- ggplot(ts_avg, aes(x = date, y = avg_pred_prob)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Average Predicted Risk Over Time",
    subtitle = "Mean model-predicted probability across all grid-months",
    x = NULL,
    y = "Average predicted risk"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(p_ts_avg)


ts_p90 <- df[
  is.finite(pred_prob),
  .(p90_pred_prob = quantile(pred_prob, 0.90, na.rm = TRUE)),
  by = date
][order(date)]

p_ts_p90 <- ggplot(ts_p90, aes(x = date, y = p90_pred_prob)) +
  geom_line(linewidth = 1) +
  labs(
    title = "90th Percentile Predicted Risk Over Time",
    subtitle = "Upper-tail ML signal across grid-months",
    x = NULL,
    y = "90th percentile risk"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(p_ts_p90)


p_pred_hist <- ggplot(df[is.finite(pred_prob)], aes(x = pred_prob)) +
  geom_histogram(bins = 50, fill = "#440154FF", color = "white", alpha = 0.9) +
  labs(
    title = "Distribution of Monthly Predicted Risk",
    x = "Predicted probability",
    y = "Count"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(p_pred_hist)


heat_dt <- df[
  is.finite(pred_prob),
  .(avg_pred_prob = mean(pred_prob, na.rm = TRUE)),
  by = .(year, month)
][order(year, month)]

p_heat <- ggplot(heat_dt, aes(x = factor(month), y = factor(year), fill = avg_pred_prob)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "magma", name = "Avg risk") +
  labs(
    title = "Seasonality of Predicted Risk",
    subtitle = "Average predicted probability by year and month",
    x = "Month",
    y = "Year"
  ) +
  theme(plot.title = element_text(face = "bold"))

print(p_heat)














discovery_candidates_clean <- discovery_candidates[is.finite(lon) & is.finite(lat)]
discovery_sf <- st_as_sf(discovery_candidates_clean, coords = c("lon", "lat"), crs = 4326)


if ("mean_risk" %in% names(discovery_candidates_clean)) {
  discovery_sf2 <- st_as_sf(discovery_candidates_clean, coords = c("lon", "lat"), crs = 4326)
  
  p_discovery_risk <- ggplot() +
    geom_sf(data = grid_sf, color = "grey85", size = 0.1, alpha = 0.14) +
    geom_sf(data = discovery_sf2, aes(color = mean_risk), size = 2.5, alpha = 0.98) +
    scale_color_viridis_c(option = "cividis", name = "") +
    labs(
      title = "Lowest Threshold Candidates",
      subtitle = "Candidates sites colored by mean probability"
    ) +
    theme(plot.title = element_text(face = "bold"))
  
  print(p_discovery_risk)
}


strict_candidates_clean <- strict_candidates[is.finite(lon) & is.finite(lat)]
strict_sf <- st_as_sf(strict_candidates_clean, coords = c("lon", "lat"), crs = 4326)

if ("max_risk" %in% names(strict_candidates_clean)) {
  strict_sf2 <- st_as_sf(strict_candidates_clean, coords = c("lon", "lat"), crs = 4326)
  
  p_strict_risk <- ggplot() +
    geom_sf(data = grid_sf, color = "grey85", size = 0.1, alpha = 0.14) +
    geom_sf(data = strict_sf2, aes(color = max_risk), size = 3, alpha = 0.98) +
    scale_color_viridis_c(option = "cividis", name = "Max risk") +
    labs(
      title = "Strict Candidates by Risk",
      subtitle = "High-confidence sites colored by maximum model risk"
    ) +
    theme(plot.title = element_text(face = "bold"))
  
  print(p_strict_risk)
}


strict_candidates_filtered_clean <- strict_candidates_filtered[is.finite(lon) & is.finite(lat)]
strict_filtered_sf <- st_as_sf(strict_candidates_filtered_clean, coords = c("lon", "lat"), crs = 4326)

if ("max_risk" %in% names(strict_candidates_filtered_clean)) {
  strict_filtered_sf2 <- st_as_sf(strict_candidates_filtered_clean, coords = c("lon", "lat"), crs = 4326)
  
  p_strict_filtered_risk <- ggplot() +
    geom_sf(data = grid_sf, color = "grey87", size = 0.1, alpha = 0.14) +
    geom_sf(data = strict_filtered_sf2, aes(color = max_risk), size = 3, alpha = 0.98) +
    scale_color_viridis_c(option = "magma", name = "") +
    labs(
      title = "Filtered Strict Candidates",
      subtitle = "Candidate sites colored by maximum probability"
    ) +
    theme(plot.title = element_text(face = "bold"))
  
  print(p_strict_filtered_risk)
}







df[, month_factor := factor(month)]

fe_model <- lm(pred_prob ~ month_factor, data = df)

df[, pred_deseason := residuals(fe_model)]

ts_p97_fe <- df[
  is.finite(pred_deseason),
  .(p97 = quantile(pred_deseason, 0.97, na.rm = TRUE)),
  by = date
][order(date)]

ggplot(ts_p97_fe, aes(date, p97)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Deseasonalized Upper-Tail Risk",
    subtitle = "Month fixed effects removed",
    y = "Risk (demeaned)",
    x = NULL
  )



















# ---------------------------------------------------------
# 1. Initialize output column
# ---------------------------------------------------------
df[, pred_deseason := NA_real_]
df[, row_id := .I]

# ---------------------------------------------------------
# 2. Define regression variables
# ---------------------------------------------------------
reg_vars <- c(
  "pred_prob",
  "grid_id",
  "month",
  "ntl_log",
  "lst_night_c",
  "ndvi",
  "ndbi",
  "cf_cvg",
  "oil_signature_score",
  "swir_nir_ratio",
  "lst_anom",
  "ntl_roll3",
  "lst_roll3",
  "ntl_change",
  "lst_change"
)

# ---------------------------------------------------------
# 3. Build complete-case estimation sample
# ---------------------------------------------------------
work <- df[
  complete.cases(df[, ..reg_vars]),
  c("row_id", "date", reg_vars),
  with = FALSE
]

# convert to data.frame for fixest
work_df <- as.data.frame(work)

# ---------------------------------------------------------
# 4. Estimate FE model
# ---------------------------------------------------------
fe_model <- feols(
  pred_prob ~
    ntl_log +
    lst_night_c +
    ndvi +
    ndbi +
    cf_cvg +
    oil_signature_score +
    swir_nir_ratio +
    lst_anom +
    ntl_roll3 +
    lst_roll3 +
    ntl_change +
    lst_change
  | grid_id + month,
  data = work_df
)

# ---------------------------------------------------------
# 5. Recover exact rows used by fixest and assign residuals
# ---------------------------------------------------------
used_idx <- obs(fe_model)   # row positions in work_df actually used
work_df$pred_deseason <- NA_real_
work_df$pred_deseason[used_idx] <- resid(fe_model)

# map residuals back to original df using row_id
df[match(work_df$row_id, row_id), pred_deseason := work_df$pred_deseason]

# ---------------------------------------------------------
# 6. Build robust upper-tail time series
# ---------------------------------------------------------
ts_risk <- df[
  is.finite(pred_deseason) & !is.na(date),
  {
    q97 <- quantile(pred_deseason, 0.97, na.rm = TRUE)
    .(
      n = .N,
      p97 = q97,
      tail_mean97 = mean(pred_deseason[pred_deseason >= q97], na.rm = TRUE)
    )
  },
  by = date
][order(date)]

# optional: drop months with too few observations
ts_risk <- ts_risk[n >= 30]

# ---------------------------------------------------------
# 7. Smooth
# ---------------------------------------------------------
ts_risk[, p97_smooth := rollmean(p97, k = 3, fill = NA, align = "center")]
ts_risk[, tail_mean97_smooth := rollmean(tail_mean97, k = 3, fill = NA, align = "center")]



ggplot(ts_risk, aes(x = date)) +
  geom_line(aes(y = tail_mean97), linewidth = 0.5, alpha = 0.35) +
  geom_line(aes(y = tail_mean97_smooth), linewidth = 1, color = "#3B0F70") +
  labs(
    title = "Deseasonalized Upper-Tail Oil and Gas Risk",
    subtitle = "Grid and month fixed effects removed; 97th-percentile tail mean with 3-month smoothing",
    x = NULL,
    y = "Residualized risk"
  ) +
  scale_x_date(
  date_breaks = "1 year",
  date_labels = "%Y"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11)
  )






