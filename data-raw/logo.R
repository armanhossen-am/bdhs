# Builds man/figures/logo.png (hex sticker) from the bundled 2022 boundaries.
library(ggplot2)

b <- sf::st_union(bdhs::bdhs_boundaries(2022))
b <- sf::st_transform(b, 32646)
bb <- sf::st_bbox(b)
# Scale the outline into the unit hexagon.
geom <- sf::st_geometry(b)
ctr <- sf::st_centroid(geom)
scale <- 1.05 / max(bb["xmax"] - bb["xmin"], bb["ymax"] - bb["ymin"])
shape <- (geom - ctr) * scale + c(0, 0.12)

hex <- data.frame(
  x = cos(seq(pi / 6, 2 * pi + pi / 6, length.out = 7)),
  y = sin(seq(pi / 6, 2 * pi + pi / 6, length.out = 7))
)
circle <- data.frame(
  x = 0.30 * cos(seq(0, 2 * pi, length.out = 200)) - 0.08,
  y = 0.30 * sin(seq(0, 2 * pi, length.out = 200)) + 0.12
)

p <- ggplot() +
  geom_polygon(data = hex, aes(x, y), fill = "#006a4e", colour = "#f42a41", linewidth = 2.2) +
  geom_polygon(data = circle, aes(x, y), fill = "#f42a41", alpha = 0.9) +
  geom_sf(data = sf::st_sf(geometry = shape), fill = NA, colour = "white", linewidth = 0.6) +
  annotate("text", x = 0, y = -0.55, label = "bdhs", colour = "white",
           size = 17, fontface = "bold", family = "sans") +
  coord_sf(xlim = c(-1, 1), ylim = c(-1, 1), expand = FALSE, datum = NA) +
  theme_void() +
  theme(plot.background = element_rect(fill = "transparent", colour = NA))

ggsave("man/figures/logo.png", p, width = 4, height = 4 * 2 / sqrt(3) * sqrt(3) / 2 * 1.1547,
       dpi = 150, bg = "transparent")
