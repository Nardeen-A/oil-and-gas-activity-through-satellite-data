# =========================================================
# TIME-SERIES + STL + OIL PRICE INTEGRATION
# Assumes your main ML dataframe is already called: df
# and contains at least: date, pred_prob
# =========================================================

library(data.table)
library(ggplot2)
library(scales)
library(forecast)
library(lubridate)

# ---------------------------------------------------------
# 1. Make sure df is a data.table and clean date
# ---------------------------------------------------------
df <- fread("E:/UofT/05_Research/00_File/output/data/predictions/grid_month_predictions.csv")

setDT(df)
df[, date := as.Date(date)]
setorder(df, date)

# Optional safety check
stopifnot("pred_prob" %in% names(df))

# ---------------------------------------------------------
# 2. Collapse grid-level predictions to monthly regional series
# ---------------------------------------------------------
risk_ts <- df[, .(
  mean_risk = mean(pred_prob, na.rm = TRUE)
), by = date]

# If you already created high95 in your ML pipeline, keep it
if ("high95" %in% names(df)) {
  high_ts <- df[, .(
    share_high95 = mean(high95, na.rm = TRUE)
  ), by = date]
  
  risk_ts <- merge(risk_ts, high_ts, by = "date", all = TRUE)
}

setorder(risk_ts, date)

# Standardize date to month-start just in case
risk_ts[, date := as.Date(format(date, "%Y-%m-01"))]

# Calendar helpers
risk_ts[, `:=`(
  year = year(date),
  month_num = month(date),
  month_lbl = month(date, label = TRUE, abbr = TRUE)
)]

# ---------------------------------------------------------
# 3. Seasonal diagnostic plot
# ---------------------------------------------------------
p_seasonal <- ggplot(
  risk_ts,
  aes(x = month_num, y = mean_risk, group = year, color = factor(year))
) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  geom_point(size = 1.6) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Seasonal plot of monthly hydrocarbon-risk series",
    subtitle = "Each line represents one year",
    x = NULL,
    y = "Mean predicted risk",
    color = "Year"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

print(p_seasonal)

# ---------------------------------------------------------
# 4. STL decomposition
# ---------------------------------------------------------
risk_ts_obj <- ts(
  risk_ts$mean_risk,
  start = c(year(min(risk_ts$date)), month(min(risk_ts$date))),
  frequency = 12
)

stl_fit <- stl(risk_ts_obj, s.window = "periodic", robust = TRUE)

risk_ts[, trend_component     := as.numeric(stl_fit$time.series[, "trend"])]
risk_ts[, seasonal_component  := as.numeric(stl_fit$time.series[, "seasonal"])]
risk_ts[, remainder_component := as.numeric(stl_fit$time.series[, "remainder"])]

# Deseasonalized series
risk_ts[, risk_deseason := mean_risk - seasonal_component]

# STL base plot
plot(stl_fit)

# ---------------------------------------------------------
# 5. Load oil prices
# ---------------------------------------------------------
oil <- fread("E:/UofT/05_Research/00_File/data/raw/MCOILWTICO.csv")

setnames(
  oil,
  old = c("observation_date", "MCOILWTICO"),
  new = c("date", "oil_price")
)

oil[, date := as.Date(date)]
oil[, oil_price := as.numeric(oil_price)]
oil <- oil[is.finite(oil_price)]

# Make sure oil dates are also month-start
oil[, date := as.Date(format(date, "%Y-%m-01"))]

# Already monthly, but this keeps structure clean
oil_monthly <- oil[, .(
  oil_price = mean(oil_price, na.rm = TRUE)
), by = date]

setorder(oil_monthly, date)

# ---------------------------------------------------------
# 6. Merge deseasonalized risk with oil prices
# ---------------------------------------------------------
plot_dt <- merge(
  risk_ts[, .(date, mean_risk, risk_deseason)],
  oil_monthly,
  by = "date",
  all = FALSE
)

setorder(plot_dt, date)

# ---------------------------------------------------------
# 7. Normalize both series to pre-shock mean = 100
# ---------------------------------------------------------
shock_date <- as.Date("2022-02-01")

base_risk_raw <- plot_dt[date < shock_date, mean(mean_risk, na.rm = TRUE)]
base_risk_ds  <- plot_dt[date < shock_date, mean(risk_deseason, na.rm = TRUE)]
base_oil      <- plot_dt[date < shock_date, mean(oil_price, na.rm = TRUE)]

plot_dt[, risk_raw_index      := 100 * mean_risk / base_risk_raw]
plot_dt[, risk_deseason_index := 100 * risk_deseason / base_risk_ds]
plot_dt[, oil_index           := 100 * oil_price / base_oil]

# ---------------------------------------------------------
# 8. Main plot: deseasonalized risk + oil prices
# ---------------------------------------------------------
plot_long <- rbind(
  plot_dt[, .(date, series = "Deseasonalized hydrocarbon risk", value = risk_deseason_index)],
  plot_dt[, .(date, series = "WTI oil price", value = oil_index)]
)

p_main <- ggplot(plot_long, aes(x = date, y = value, linetype = series)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = shock_date, linetype = "dashed") +
  annotate(
    "text",
    x = as.Date("2022-03-01"),
    y = max(plot_long$value, na.rm = TRUE) * 0.96,
    label = "Feb. 2022 shock",
    hjust = 0,
    size = 4
  ) +
  scale_y_continuous(
    name = "Index (pre-2022 average = 100)",
    labels = label_number(accuracy = 1)
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(
    title = "Deseasonalized hydrocarbon activity risk and oil prices",
    subtitle = "Monthly series indexed to pre-shock means",
    x = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

print(p_main)

# ---------------------------------------------------------
# 9. Optional comparison plot: raw risk vs deseasonalized risk vs oil
# ---------------------------------------------------------
compare_long <- rbind(
  plot_dt[, .(date, series = "Raw risk", value = risk_raw_index)],
  plot_dt[, .(date, series = "Deseasonalized risk", value = risk_deseason_index)],
  plot_dt[, .(date, series = "WTI oil price", value = oil_index)]
)

p_compare <- ggplot(compare_long, aes(x = date, y = value, linetype = series)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = shock_date, linetype = "dashed") +
  scale_y_continuous(
    name = "Index (pre-2022 average = 100)",
    labels = label_number(accuracy = 1)
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(
    title = "Raw risk, deseasonalized risk, and oil prices",
    subtitle = "Comparison of monthly indexed series",
    x = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

print(p_compare)



# Define colors manually from viridis palette
cols <- viridis(10, option = "D")

ggplot() +
  # Oil price (background / reference)
  geom_line(
    data = plot_dt,
    aes(x = date, y = oil_index, color = "WTI oil price"),
    linewidth = 0.9,
    alpha = 0.8
  ) +
  # Risk (main variable)
  geom_line(
    data = plot_dt,
    aes(x = date, y = risk_deseason_index, color = "Hydrocarbon risk"),
    linewidth = 1.3
  ) +
  # Shock line
  geom_vline(
    xintercept = as.Date("2022-02-01"),
    linetype = "dashed",
    linewidth = 0.8,
    color = "black"
  ) +
  # Annotation
  annotate(
    "text",
    x = as.Date("2022-03-01"),
    y = max(plot_dt$oil_index, na.rm = TRUE) * 0.92,
    label = "Feb. 2022 shock",
    hjust = 0,
    size = 4
  ) +
  scale_color_manual(
    values = c(
      "Hydrocarbon risk" = cols[1],
      "WTI oil price" = cols[5]
    )
  ) +
  scale_y_continuous(
    name = element_blank(),
    breaks = seq(50, 200, 25)
  ) +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y-%m"
  ) +
  labs(
    x = NULL,
    color = NULL
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1)
  )

# ---------------------------------------------------------
# 10. Save final monthly series for paper / regressions
# ---------------------------------------------------------
fwrite(
  risk_ts,
  "E:/UofT/05_Research/00_File/output/data/monthly_risk_stl_components.csv"
)

fwrite(
  plot_dt,
  "E:/UofT/05_Research/00_File/output/data/monthly_risk_oil_stl_series.csv"
)