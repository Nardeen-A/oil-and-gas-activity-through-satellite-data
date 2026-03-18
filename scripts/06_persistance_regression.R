# =========================================================
# OIL & GAS SATELLITE PROJECT:
# WITHIN-REGION EXPOSURE DESIGN
# =========================================================
proj.path <- getwd()
df <- read.csv(file.path(proj.path, "output", "data", "predictions", "grid_month_predictions.csv"))

setDT(df)

df[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
setorder(df, grid_id, date)

df <- df[!is.na(grid_id) & !is.na(date) & is.finite(pred_prob)]

# Month fixed effects created ONCE in df
df[, month_fe := factor(format(date, "%m"))]

# =========================
# 2. Define high-risk states
# =========================
p95_cutoff <- df[, quantile(pred_prob, 0.95, na.rm = TRUE)]
p90_cutoff <- df[, quantile(pred_prob, 0.90, na.rm = TRUE)]
p99_cutoff <- df[, quantile(pred_prob, 0.99, na.rm = TRUE)]

df[, high95 := as.integer(pred_prob >= p95_cutoff)]
df[, high90 := as.integer(pred_prob >= p90_cutoff)]
df[, high99 := as.integer(pred_prob >= p99_cutoff)]

# =========================
# 3. Construct lags
# =========================
df[, high95_lag1 := shift(high95, 1L), by = grid_id]
df[, high95_lag2 := shift(high95, 2L), by = grid_id]
df[, high95_lag3 := shift(high95, 3L), by = grid_id]

df[, high90_lag1 := shift(high90, 1L), by = grid_id]
df[, high99_lag1 := shift(high99, 1L), by = grid_id]

df[, pred_prob_lag1 := shift(pred_prob, 1L), by = grid_id]

# =========================
# 4. Estimation samples
# =========================
reg_dt      <- df[!is.na(high95_lag1)]
reg_dt_lag2 <- df[!is.na(high95_lag1) & !is.na(high95_lag2)]
reg_dt_lag3 <- df[!is.na(high95_lag1) & !is.na(high95_lag2) & !is.na(high95_lag3)]
reg_cont    <- df[!is.na(pred_prob_lag1)]
reg90       <- df[!is.na(high90_lag1)]
reg99       <- df[!is.na(high99_lag1)]

# =========================
# 5. Progressive regressions
# =========================

# Model 1: Simple OLS
m1 <- feols(
  high95 ~ high95_lag1,
  cluster = ~grid_id,
  data = reg_dt
)

# Model 2: Month FE
m2 <- feols(
  high95 ~ high95_lag1 | month_fe,
  cluster = ~grid_id,
  data = reg_dt
)

# Model 3: Grid FE
m3 <- feols(
  high95 ~ high95_lag1 | grid_id,
  cluster = ~grid_id,
  data = reg_dt
)

# Model 4: Grid FE + Month FE
m4 <- feols(
  high95 ~ high95_lag1 | grid_id + month_fe,
  cluster = ~grid_id,
  data = reg_dt
)

# Model 5: Two lags
m5 <- feols(
  high95 ~ high95_lag1 + high95_lag2 | grid_id + month_fe,
  cluster = ~grid_id,
  data = reg_dt_lag2
)

# Model 6: Three lags
m6 <- feols(
  high95 ~ high95_lag1 + high95_lag2 + high95_lag3 | grid_id + month_fe,
  cluster = ~grid_id,
  data = reg_dt_lag3
)

# Model 7: Continuous persistence
m7 <- feols(
  pred_prob ~ pred_prob_lag1 | grid_id + month_fe,
  cluster = ~grid_id,
  data = reg_cont
)

# Model 8: Top 10% robustness
m8 <- feols(
  high90 ~ high90_lag1 | grid_id + month_fe,
  cluster = ~grid_id,
  data = reg90
)

# Model 9: Top 1% robustness
m9 <- feols(
  high99 ~ high99_lag1 | grid_id + month_fe,
  cluster = ~grid_id,
  data = reg99
)

# =========================
# 6. Print summaries
# =========================
print(summary(m1))
print(summary(m2))
print(summary(m3))
print(summary(m4))
print(summary(m5))
print(summary(m6))
print(summary(m7))
print(summary(m8))
print(summary(m9))

# =========================
# 7. Transition probabilities
# =========================
transitions_95 <- reg_dt[, .(
  p_stay_high = mean(high95[high95_lag1 == 1], na.rm = TRUE),
  p_enter_high = mean(high95[high95_lag1 == 0], na.rm = TRUE),
  unconditional_high = mean(high95, na.rm = TRUE)
)]

print(transitions_95)

# Clean transition table for plotting
plot_transition <- data.table(
  state = factor(
    c("Previously low-risk", "Previously high-risk"),
    levels = c("Previously low-risk", "Previously high-risk")
  ),
  probability = c(
    transitions_95$p_enter_high,
    transitions_95$p_stay_high
  )
)

# =========================
# 8. Nice plot
# =========================
p <- ggplot(plot_transition, aes(x = state, y = probability)) +
  geom_col(width = 0.62, alpha = 0.9) +
  geom_text(
    aes(label = sprintf("%.1f%%", 100 * probability)),
    vjust = -0.6,
    size = 5
  ) +
  scale_y_continuous(
    limits = c(0, max(plot_transition$probability) * 1.15),
    labels = function(x) paste0(round(100 * x), "%")
  ) +
  labs(
    title = "Persistence of High-Risk Oil and Gas Signals",
    subtitle = "Probability a grid cell is high-risk this month, by last month’s status",
    x = NULL,
    y = "Probability of high-risk status"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(p)

ggsave(
  filename = "persistence_transition_plot.png",
  plot = p,
  width = 8,
  height = 5,
  dpi = 300
)

# =========================
# 9. Entry dynamics
# =========================
df[, new_high95 := as.integer(high95 == 1 & high95_lag1 == 0)]
df[is.na(new_high95), new_high95 := 0]

entry_dt <- df[, .(
  n_new_high95 = sum(new_high95, na.rm = TRUE),
  share_new_high95 = mean(new_high95, na.rm = TRUE),
  n_high95 = sum(high95, na.rm = TRUE),
  share_high95 = mean(high95, na.rm = TRUE),
  n_cells = .N
), by = date][order(date)]

# Optional second plot: monthly entries
p_entry <- ggplot(entry_dt, aes(x = date, y = n_new_high95)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Monthly Emergence of New High-Risk Cells",
    subtitle = "New entries into the top 5% of predicted oil/gas risk",
    x = NULL,
    y = "Number of new high-risk cells"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_entry)

ggsave(
  filename = "new_highrisk_entries_over_time.png",
  plot = p_entry,
  width = 8,
  height = 5,
  dpi = 300
)

# =========================
# 10. Concentration statistics
# =========================
df[, rank_prob := frank(-pred_prob, ties.method = "first"), by = date]
df[, n_date := .N, by = date]
df[, top5 := as.integer(rank_prob <= ceiling(0.05 * n_date))]
df[, top1 := as.integer(rank_prob <= ceiling(0.01 * n_date))]

concentration_dt <- df[, .(
  share_signal_top5 = sum(pred_prob[top5 == 1], na.rm = TRUE) / sum(pred_prob, na.rm = TRUE),
  share_signal_top1 = sum(pred_prob[top1 == 1], na.rm = TRUE) / sum(pred_prob, na.rm = TRUE)
), by = date][order(date)]

print(summary(concentration_dt))

cat("Average share of total predicted signal from top 5% of cells:",
    concentration_dt[, mean(share_signal_top5, na.rm = TRUE)], "\n")
cat("Average share of total predicted signal from top 1% of cells:",
    concentration_dt[, mean(share_signal_top1, na.rm = TRUE)], "\n")


# LaTeX version
se_list_main <- list(
  se(m1),
  se(m2),
  se(m3),
  se(m4),
  se(m5),
  se(m6)
)

# =========================================================
# CLEAN REGRESSION TABLE WITH FIXEST
# =========================================================

r_1 <- etable(
  m1, m2, m3, m4, m5, m6,
  tex = TRUE,
  file = "persistence_progressive_table.tex",
  title = "Persistence of High-Risk Oil and Gas Signals",
  headers = c("OLS", "Month FE", "Grid FE", "Grid + Month FE", "2 Lags", "3 Lags"),
  dict = c(
    "high95_lag1" = "High-risk (t-1)",
    "high95_lag2" = "High-risk (t-2)",
    "high95_lag3" = "High-risk (t-3)"
  ),
  fitstat = ~ n + r2 + ar2 + wr2,
  extralines = list(
    "Grid FE" = c("No", "No", "Yes", "Yes", "Yes", "Yes"),
    "Month FE" = c("No", "Yes", "No", "Yes", "Yes", "Yes"),
    "Clustered SE" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
  )
)