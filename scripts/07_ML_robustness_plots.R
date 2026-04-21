proj.path <- getwd()
features <- read.csv(file.path(proj.path, "output", "data", "diagnostics", "preferred_model_permutation_importance.csv"))

# ---------------------------------------
# Confusion matrix (your values)
# ---------------------------------------
cm <- matrix(
  c(1811242, 1286, 1366, 3026),
  nrow = 2,
  byrow = TRUE
)

rownames(cm) <- c("Actual 0", "Actual 1")
colnames(cm) <- c("Pred 0", "Pred 1")

# ---------------------------------------
# Convert to long format
# ---------------------------------------
cm_df <- melt(cm)
colnames(cm_df) <- c("Actual", "Predicted", "Count")

# ---------------------------------------
# Plot (RAW counts, colored by magnitude)
# ---------------------------------------
ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = format(Count, big.mark = ",")), size = 7) +
  scale_fill_viridis_c(option = "magma", trans = "log10", alpha = 0.6) +
  labs(
    x = "Predicted",
    y = "Actual",
    fill = "Count"
  ) +
  theme(
    legend.position = "None",
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )






features_plot <- features[order(features$importance_mean, decreasing = TRUE), ]
features_top  <- head(features_plot, 15)

ggplot(features_top, aes(x = reorder(feature, importance_mean), y = importance_mean)) +
  geom_col(fill = "#5B2A86") +
  geom_errorbar(
    aes(
      ymin = pmax(0, importance_mean - importance_std),
      ymax = importance_mean + importance_std
    ),
    width = 0.2,
    linewidth = 0.5
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = element_blank()
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 12.5)
  )






