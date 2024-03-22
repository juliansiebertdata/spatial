# SETUP --------------------------

## Load the required libraries ----

pacman::p_load(dplyr, janitor, readxl, sf, rnaturalearth, here, ggtext)

## simulate data ----

set.seed(123)


# Simulate yield outcomes for two treatments (control and product)
yield_data <- tibble(
  trial_id = rep(1:100, each = 2),
)

yield_data <- yield_data %>%
  mutate(
    treatment = rep(c("control", "product"), times = 100),
    yield = rnorm(200, mean = 100, sd = 10)
  )

# Generate longitude and latitude values for each trial field
trial_coords <- tibble(
  trial_id = 1:100,
  longitude = runif(100, min = 5.87, max = 15.04),
  latitude = runif(100, min = 47.27, max = 55.06)
)

# Combine yield data and trial coordinates
combined_data <- left_join(yield_data, trial_coords, by = "trial_id")

relative_yield <- combined_data %>%
  group_by(trial_id) %>%
  mutate(
    relative_yield = (yield[treatment == "product"] - yield[treatment == "control"]) / yield[treatment == "control"],
    yield_relative_perc = relative_yield * 100
  ) %>%
  ungroup()

relative_yield <- st_as_sf(relative_yield,
  coords = c("longitude", "latitude"),
  crs = 4326
)


# CREATE PLOT -----------------------------------------------------------------

# get shape of germany map ---
germany <- rnaturalearth::ne_countries(scale = "large", country = "germany", returnclass = "sf") %>%
  st_transform(4326) %>%
  select(geometry)

# make basic germany map ---
base_map <- ggplot2::ggplot(data = germany) +
  ggplot2::geom_sf(color = "lightgrey", fill = "lightgrey") +
  ggplot2::theme_void()

# Filter points inside Germany multipolygon
filtered_data <- st_intersection(relative_yield, germany)

# Plot filtered data
filtered_map <- base_map +
  ggplot2::geom_sf(
    data = filtered_data,
    shape = 21,
    fill = "#53b57f",
    color = "transparent",
    size = 4
  ) +
  ggplot2::geom_sf(
    data = filtered_data,
    shape = 21,
    color = "lightgrey",
    fill = "transparent",
    size = 4
  )

filtered_map


## load soil data -----

soil_data <- readxl::read_xlsx(here::here("data", "soil-sample-sites-germany.xlsx")) %>% janitor::clean_names()

soil_data <- soil_data %>%
  select(-bundesland, -probenahme_monat, -probenahme_jahr, -boden_var_typ_sub)


soil_lab_data <- readxl::read_xlsx(here::here("data", "soil-labvalues-germany.xlsx")) %>% janitor::clean_names()

soil_lab_data <- soil_lab_data %>%
  filter(tiefenstufe_untergrenze <= 30) %>%
  select(
    point_id, tiefenstufe_untergrenze, p_h_h2o, tc, toc, tn, trd_fb,
    sand, ton, schluff
  )


soil_tbl <- left_join(soil_lab_data, soil_data, by = "point_id")

soil_tbl <- st_as_sf(soil_tbl, coords = c("xcoord", "ycoord"), crs = 32632) %>%
  st_transform(4326)

## plot soil_tbl on germany_map ----

soil_map <- base_map +
  geom_sf(
    data = soil_tbl,
    shape = 21,
    fill = "#FFA500",
    color = "black",
    size = 1,
    alpha = 0.3
  )

soil_map

# mapview(soil_tbl)

## get hexmap of soil data ----

pacman::p_load(h3jsr)

hex <- germany %>%
  polygon_to_cells(res = 6) %>%
  cell_to_polygon(simple = FALSE)

hex_map <- base_map +
  geom_sf(data = hex)

## make fancy map ----

glimpse(soil_tbl)

soil <- soil_tbl %>%
  filter(tiefenstufe_untergrenze < 30) %>%
  mutate(
    sand = as.numeric(sand) / 100,
    ton = as.numeric(ton) / 100,
    schluff = as.numeric(schluff) / 100
  ) %>%
  tidyr::drop_na(sand, schluff, ton) %>%
  mutate(
    clay = ton,
    silt = schluff,
    sand = sand,
    x = clay,
    y = silt,
    z = sand
  ) %>%
  select(
    point_id = point_id,
    clay, silt, sand,
    x, y, z
  ) %>%
  mutate(clay_cl = case_when(
    clay < 0.36666666 ~ "A",
    clay < 0.6333333 ~ "B",
    TRUE ~ "C"
  )) %>%
  mutate(silt_cl = case_when(
    silt < 0.36666666 ~ "A",
    silt < 0.6333333 ~ "B",
    TRUE ~ "C"
  )) %>%
  mutate(sand_cl = case_when(
    sand < 0.36666666 ~ "A",
    sand < 0.6333333 ~ "B",
    TRUE ~ "C"
  )) %>%
  mutate(cl = glue::glue("{clay_cl}{silt_cl}{sand_cl}")) %>%
  mutate(cl2 = case_when(
    cl == "CAA" ~ "p1",
    cl == "BAA" ~ "p2",
    cl == "BAB" ~ "p3",
    cl == "AAB" ~ "p4",
    cl == "AAC" ~ "p5",
    cl == "BBA" ~ "p6",
    cl == "ABA" ~ "p7",
    cl == "ABB" ~ "p8",
    cl == "ACA" ~ "p9"
  ))


## give soil attribute to hexes based on nearest sample location ----

# Calculate centroids of polygons
hex_centroids <- st_centroid(hex)

# Find nearest points in soil to centroids and add their data to hex_centroids
# then add polygon geometrys back to the data
soil_hex <- st_join(hex_centroids, soil, join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  left_join(hex, by = "h3_address") %>%
  st_as_sf()


## color palette based on cl2 ----
pal_fill2 <- c(
  "p1" = "#01a0c6",
  "p2" = "#3EBAD4",
  "p3" = "#8ACCC5",
  "p4" = "#feffab",
  "p5" = "#feff00",
  "p6" = "#6DACCF",
  "p7" = "#f882be",
  "p8" = "#ffb4a9",
  "p9" = "#f21c8d"
)


soil_de <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = soil_hex,
    aes(fill = cl2),
    color = alpha("white", 0)
  ) +
  ggplot2::scale_fill_manual(values = pal_fill2) +
  ggplot2::guides(fill = "none") +
  ggplot2::theme_void() +
  ggplot2::theme(plot.background = element_rect(fill = NA, color = NA))

ggsave(
  plot = soil_de,
  here::here("plots", "soil_map.png"),
  width = 60,
  height = 90,
  units = "cm"
)

# maps for each soil attribute ----

## Clay -----------------------------------------------------------------------

# Create a color palette function
palette_func <- colorRampPalette(colors = c("grey85", "#01a0c6", "grey15"))

# Generate a palette of 10 colors
palette <- palette_func(10)

scales::show_col(palette)

clay_palette <- c(
  "#A9CCD4", "#79BFD0",
  "#49B3CC", "#18A6C8", "#0592B4", "#0D7790",
  "#155C6D"
)

soil_de_clay <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = soil_hex,
    aes(fill = clay),
    color = alpha("white", 0)
  ) +
  ggplot2::scale_fill_gradientn(
    colors = clay_palette,
    name = "Clay Content",
    labels = function(x) paste(x * 100, "%"),
    breaks = seq(0, 0.8, by = 0.2)
  ) +
  ggplot2::guides(fill = guide_coloursteps(
    barwidth = 4,
    barheight = 20,
    label.position = "left",
    ticks = TRUE,
    title.vjust = 5
  )) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.background = element_rect(fill = NA, color = NA),
    legend.position = c(1.1, 0.5),
    legend.title = element_text(face = "bold", size = 22, color = "grey30"),
    legend.text = element_text(size = 18, face = "bold", color = "grey30"),
    legend.justification = "left",
    plot.margin = margin(l = 1, r = 8, t = 1, b = 1, "cm")
  )

ggsave(
  plot = soil_de_clay,
  here::here("plots", "soil_map_clay.png"),
  width = 30,
  height = 30,
  units = "cm"
)

## Sand ------------------------------------------------------------------------
# Create a color palette function
palette_func <- colorRampPalette(colors = c("white", "#feff00", "darkorange"))

# Generate a palette of 10 colors
palette <- palette_func(10)

scales::show_col(palette)

sand_palette <- c(
  "#FEFFC6", "#FEFF8D", "#FEFF55", "#FEFF1C",
  "#FEF200", "#FED800", "#FEBF00"
)

soil_de_sand <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = soil_hex,
    aes(fill = sand),
    color = alpha("white", 0)
  ) +
  ggplot2::scale_fill_gradientn(
    colors = sand_palette,
    name = "Sand Content",
    labels = function(x) paste(x * 100, "%"),
    breaks = seq(0, 0.8, by = 0.2)
  ) +
  ggplot2::guides(fill = guide_coloursteps(
    barwidth = 4,
    barheight = 20,
    label.position = "left",
    ticks = TRUE,
    title.vjust = 5
  )) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.background = element_rect(fill = NA, color = NA),
    legend.position = c(1.1, 0.5),
    legend.title = element_text(face = "bold", size = 22, color = "grey30"),
    legend.text = element_text(size = 18, face = "bold", color = "grey30"),
    legend.justification = "left",
    plot.margin = margin(l = 1, r = 8, t = 1, b = 1, "cm")
  )

ggsave(
  plot = soil_de_sand,
  here::here("plots", "soil_map_sand.png"),
  width = 30,
  height = 30,
  units = "cm"
)

## Silt ------------------------------------------------------------------------
# Create a color palette function
palette_func <- colorRampPalette(colors = c("white", "#f21c8d", "grey15"))

# Generate a palette of 10 colors
palette <- palette_func(10)

scales::show_col(palette)

silt_palette <- c(
  "#FCCCE5", "#F99ACC", "#F667B3", "#F33599",
  "#DB1D81", "#AE1F6A", "#802153"
)

soil_de_silt <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = soil_hex,
    aes(fill = silt),
    color = alpha("white", 0)
  ) +
  ggplot2::scale_fill_gradientn(
    colors = silt_palette,
    name = "Silt Content",
    labels = function(x) paste(x * 100, "%"),
    breaks = seq(0, 0.8, by = 0.2)
  ) +
  ggplot2::guides(fill = guide_coloursteps(
    barwidth = 4,
    barheight = 20,
    label.position = "left",
    ticks = TRUE,
    title.vjust = 5
  )) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.background = element_rect(fill = NA, color = NA),
    legend.position = c(1.1, 0.5),
    legend.title = element_text(face = "bold", size = 22, color = "grey30"),
    legend.text = element_text(size = 18, face = "bold", color = "grey30"),
    legend.justification = "left",
    plot.margin = margin(l = 1, r = 8, t = 1, b = 1, "cm")
  )

ggsave(
  plot = soil_de_silt,
  here::here("plots", "soil_map_silt.png"),
  width = 30,
  height = 30,
  units = "cm"
)


# Legend and distribution of soil attributes -----------------------------------
# install.packages("ggtern")
library(ggtern)

tern_distribution <-
  ggtern::ggtern(soil, aes(x = clay, y = silt, z = sand, color = cl2)) +
  geom_point(alpha = .2) +
  geom_point(shape = 21, alpha = .8) +
  scale_color_manual(values = pal_fill2) +
  scale_T_continuous(limits = c(0.0, 1)) +
  scale_L_continuous(limits = c(0.0, 1)) +
  scale_R_continuous(limits = c(0.0, 1)) +
  guides(color = "none") +
  theme_void() +
  theme_nolabels() +
  theme_custom(
    tern.plot.background = NA,
    tern.panel.background = "#EAEAEA",
    col.grid.minor = NA,
    col.L = NA,
    col.T = NA,
    col.R = NA
  ) +
  labs(
    title = "Soil Texture Distribution",
    subtitle = "of Hexagons inside Germany"
  ) +
  theme(
    plot.title = element_markdown(
      hjust = 0.5,
      vjust = 0.5,
      size = 12,
      face = "bold",
      color = "grey30"
    ),
    plot.subtitle = element_markdown(
      hjust = 0.5,
      vjust = 0.5,
      size = 8,
      color = "grey30"
    )
  )

ggsave(
  plot = tern_distribution,
  here::here("plots", "soil_distribution.png"),
  width = 15,
  height = 15,
  units = "cm"
)

## Make the legend ----

pol1 <- tibble(
  x = c(100 * 2 / 3, 100 * 2 / 3, 100),
  y = c(100 * 1 / 3, 0, 0),
  z = c(0, 1 / 3 * 100, 0),
  col = "p1"
)

pol2 <- tibble(
  x = c(100 * 2 / 3, 100 * 1 / 3, 2 / 3 * 100),
  y = c(100 * 1 / 3, 100 * 1 / 3, 0),
  z = c(0, 100 * 1 / 3, 1 / 3 * 100),
  col = "p2"
)

pol3 <- tibble(
  x = c(100 * 2 / 3, 100 * 1 / 3, 1 / 3 * 100),
  y = c(0, 100 * 1 / 3, 0),
  z = c(100 * 1 / 3, 100 * 1 / 3, 100 * 2 / 3),
  col = "p3"
)

pol4 <- tibble(
  x = c(100 * 1 / 3, 100 * 1 / 3, 0),
  y = c(0, 100 * 1 / 3, 1 / 3 * 100),
  z = c(100 * 2 / 3, 100 * 1 / 3, 100 * 2 / 3),
  col = "p4"
)

pol5 <- tibble(
  x = c(0, 1 / 3 * 100, 0),
  y = c(0, 0, 1 / 3 * 100),
  z = c(100, 2 / 3 * 100, 2 / 3 * 100),
  col = "p5"
)

pol6 <- tibble(
  x = c(100 * 2 / 3, 100 * 1 / 3, 100 * 1 / 3),
  y = c(100 * 1 / 3, 2 / 3 * 100, 100 * 1 / 3),
  z = c(0, 0, 100 * 1 / 3),
  col = "p6"
)

pol7 <- tibble(
  x = c(100 * 1 / 3, 0, 100 * 1 / 3),
  y = c(2 / 3 * 100, 2 / 3 * 100, 100 * 1 / 3),
  z = c(0, 100 * 1 / 3, 100 * 1 / 3),
  col = "p7"
)

pol8 <- tibble(
  x = c(0, 0, 100 * 1 / 3),
  y = c(2 / 3 * 100, 1 / 3 * 100, 100 * 1 / 3),
  z = c(1 / 3 * 100, 100 * 2 / 3, 100 * 1 / 3),
  col = "p8"
)

pol9 <- tibble(
  x = c(0, 1 / 3 * 100, 0),
  y = c(2 / 3 * 100, 2 / 3 * 100, 100),
  z = c(1 / 3 * 100, 0, 0),
  col = "p9"
)

# comnine all polygons
multipol <- pol1 %>%
  bind_rows(pol2) %>%
  bind_rows(pol3) %>%
  bind_rows(pol4) %>%
  bind_rows(pol5) %>%
  bind_rows(pol6) %>%
  bind_rows(pol7) %>%
  bind_rows(pol8) %>%
  bind_rows(pol9) %>%
  mutate(
    x = 10 + x * 0.8,
    y = 10 + y * 0.8,
    z = z * 0.8
  )

legend <- ggtern(multipol, aes(x = x, y = y, z = z, fill = col)) +
  geom_polygon(color = NA) +
  # geom_point(mp,mapping=aes(x=clay,y=silt,z=sand,color=cl))+
  scale_fill_manual(values = pal_fill2) +
  scale_T_continuous(limits = c(.1, .9), breaks = NULL) +
  scale_L_continuous(limits = c(.1, .9), breaks = NULL) +
  scale_R_continuous(limits = c(0, .8), breaks = NULL) +
  guides(fill = "none") +
  labs(
    x = "",
    y = "",
    z = "",
    zarrow = "Sand Content",
    yarrow = "Silt Content",
    xarrow = "Clay Content",
    title = "Color Legend",
    subtitle = "for Soil Texture Classes in<br>mass-% based on particle size.<br>(DIN ISO 11277) "
  ) +
  theme_showarrows() +
  theme(
    tern.axis.arrow.text = element_markdown(
      face = "bold",
      size = 8,
    ),
    tern.axis.ticks.major = element_blank(),
    tern.axis.text = element_blank(),
    plot.title = element_markdown(
      hjust = 0.5,
      vjust = 0.5,
      size = 12,
      face = "bold",
      color = "grey30"
    ),
    plot.subtitle = element_markdown(
      hjust = 0.5,
      vjust = 0.5,
      size = 8,
      color = "grey30"
    )
  )

ggsave(
  plot = legend,
  here::here("plots", "soil_legend.png"),
  width = 15,
  height = 15,
  units = "cm"
)
# devtools::install_version("ggplot2", version = "3.4.4", repos = "http://cran.us.r-project.org")


pacman::p_load(patchwork)

right <- soil_de_clay / soil_de_silt / soil_de_sand

ggsave(plot = right, here::here("plots", "soil_map_facet.png"), width = 30, height = 90, units = "cm")
