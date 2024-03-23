# SETUP --------------------------

## install packman if necessary
if (!`pacman` %in% installed.packages()) {{ install.packages(`pacman`) }
}

## load librarys ----
pacman::p_load(sf, rnaturalearth, tidyverse, here, h3jsr)

# WORKFLOW --------------------------

## get germany polygon  ----
germany_polygon <- rnaturalearth::ne_countries(
  scale = "large", country = "germany", returnclass = "sf"
) %>%
  st_transform(4326) %>%
  select(name, geometry)

st_write(
  germany_polygon,
  here::here(
    "data/country-shapes/germany",
    "germany-polygon.gpkg"
  )
)

germany_polygon_plot <- ggplot2::ggplot(data = germany_polygon) +
  ggplot2::geom_sf(color = "lightgrey", fill = "lightgrey") +
  ggplot2::theme_void()

ggplot2::ggsave(
  plot = germany_polygon_plot,
  here::here(
    "data/country-shapes/germany",
    "germany-polygon.png"
  ),
  width = 30,
  height = 30,
  units = "cm"
)

## get hexagons of germany ----

pacman::p_load(h3jsr)

# create hexagon grid based on germany polygon

germany_polygon <- st_read(
  here(
    "data/country-shapes/germany",
    "germany-polygon.gpkg"
  )
)

# mid resolution
hex_germany <- germany_polygon %>%
  polygon_to_cells(res = 6) %>%
  cell_to_polygon(simple = FALSE) %>%
  mutate(country = "germany")

st_write(
  hex_germany,
  here::here(
    "data/country-shapes/germany",
    "germany-hexagon-res-mid.gpkg"
  )
)

hex_map_res_mid <- germany_polygon_plot +
  ggplot2::geom_sf(data = hex_germany)

ggplot2::ggsave(
  plot = hex_map_res_mid,
  here::here(
    "data/country-shapes/germany",
    "germany-hexagon-res-mid.png"
  ),
  width = 30,
  height = 30,
  units = "cm"
)

# high resolution
hex_germany_high_res <- germany_polygon %>%
  polygon_to_cells(res = 8) %>%
  cell_to_polygon(simple = FALSE) %>%
  mutate(country = "germany")

st_write(
  hex_germany_high_res,
  here::here(
    "data/country-shapes/germany",
    "germany-hexagon-res-high.gpkg"
  )
)

hex_map_res_high <- germany_polygon_plot +
  ggplot2::geom_sf(data = hex_germany_high_res)

ggplot2::ggsave(
  plot = hex_map_res_high,
  here::here(
    "data/country-shapes/germany",
    "germany-hexagon-res-high.png"
  ),
  width = 30,
  height = 30,
  units = "cm"
)

# END --------------------------
