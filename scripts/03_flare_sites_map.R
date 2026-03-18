# load flare sites if needed
flare_sites <- fread(file.path(proj.path, "data", "flare_sites_iraq_2012_2019.csv"))

# convert to sf object
flare_sites_map <- st_as_sf(flare_sites, coords = c("lon", "lat"), crs = 4326)

# get Iraq map
iraq <- ne_countries(country = "Iraq", scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = iraq, fill = "grey98", color = "grey35", linewidth = 0.5) +
  geom_sf(
    data = flare_sites_map,
    aes(color = mean_volume),
    size = 2.8,
    alpha = 0.95
  ) +
  scale_color_viridis_c(
    option = "plasma",
    trans = "log10",
    guide = guide_colorbar(
      barheight = unit(50, "pt"),
      barwidth = unit(8, "pt")
    ),
    name = "Mean flare volume"
  ) +
  coord_sf(
    xlim = c(41.8, 46.2),
    ylim = c(34.0, 37.4),
    expand = FALSE
  ) +
  labs(
    title = "Observed Flare Sites in Southern Kurdistan and Kirkuk",
    subtitle = "VIIRS Nightfire, 2012–2019"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "grey30"),
    legend.position = c(0.88, 0.33),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )