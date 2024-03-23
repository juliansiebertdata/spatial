# SETUP --------------------------

## install packman if necessary
if (!"pacman" %in% installed.packages()) {
  install.packages("pacman")
}

## load librarys ----

pacman::p_load(dplyr, janitor, readxl, sf, rnaturalearth, here, ggtext, stringr)

# load soil data ----- ----------------------------------------------------
# data from https://www.openagrar.de/receive/openagrar_mods_00054877
# erste bodenzustandserhebung landwirtschaft - kerndatensatz

soil_data <- readxl::read_xlsx(
  here::here(
    "data/german-soil-assessment/data-raw",
    "soil-sample-sites-germany.xlsx"
  )
) %>% janitor::clean_names()

soil_data <- soil_data %>%
  select(-bundesland, -probenahme_monat, -probenahme_jahr, -boden_var_typ_sub)


soil_lab_data <- readxl::read_xlsx(
  here::here(
    "data/german-soil-assessment/data-raw",
    "soil-labvalues-germany.xlsx"
  )
) %>% janitor::clean_names()

## tidy data ----
soil_lab_data <- soil_lab_data %>%
  filter(tiefenstufe_untergrenze <= 30) %>%
  select(
    point_id,
    tiefenstufe_untergrenze,
    p_h_h2o,
    ec_h2o,
    tc,
    toc,
    tic,
    tn,
    trd_fb,
    sand, ton, schluff,
    bodenart
  )

# join measurements and site metadata based on point_id
soil_tbl <- left_join(soil_lab_data, soil_data, by = "point_id")

# create POINT - geometry
soil_tbl <- st_as_sf(
  soil_tbl,
  coords = c("xcoord", "ycoord"),
  crs = 32632
) %>%
  st_transform(4326)

# rename columns and remove characters from numeric vectors
soil_data_tidy <- soil_tbl %>%
  rename(
    sample_site_id = point_id,
    depth = tiefenstufe_untergrenze,
    ph_water = p_h_h2o,
    ec_water = ec_h2o,
    totalcarbon_g_per_kg = tc,
    organiccarbon_g_per_kg = toc,
    inorganiccarbon_g_per_kg = tic,
    organic_carbon_stock_mg_per_ha_topsoil = kv_0_30,
    organic_carbon_stock_mg_per_ha_subsoil = kv_30_100,
    totalnitrogen_g_per_kg = tn,
    dry_bulk_density_g_per_cm3 = trd_fb,
    sand_perc = sand,
    clay_perc = ton,
    silt_perc = schluff,
    soil_class = bodenart,
    soil_type = hauptbodentyp,
    soil_climate_zone = bodenklimaraum_name,
    landuse_type = landnutzung,
    groundwater_level_cm = grundwa_stand,
    moorland_logical = bze_moor,
    moorland_depth_cm = moormaechtigkeit,
    moorland_peatdepth_cm = torfmaechtigkeit,
    relief_type = reliefformtyp,
    relief_position = lage_im_relief,
    releif_slope = neigung,
    releif_curvature = woelbung,
    releif_exposition = exposition
  ) %>%
  select(-grundwa_stufe) %>%
  mutate(
    groundwater_level_cm = stringr::str_replace_all(groundwater_level_cm, ">", ""),
    across(where(is.character), ~ stringr::str_replace_all(.x, "NA", ""))
  )

## write to xlsx and gpkg ----
openxlsx2::write_xlsx(
  st_drop_geometry(soil_data_tidy %>%
    mutate(
      longitude = st_coordinates(geometry)[, 1],
      latitude = st_coordinates(geometry)[, 2]
    )),
  here::here(
    "data/german-soil-assessment",
    "german_soil_assessment.xlsx"
  ),
  overwrite = TRUE
)

st_write(
  soil_data_tidy,
  here::here(
    "data/german-soil-assessment",
    "german-soil-assessment.gpkg"
  )
)

# CREATE PLOT -----------------------------------------------------------------

## get polygon and hexmap of germany ----

germany_polygon <- st_read(
  here(
    "data/country-shapes/germany",
    "germany-polygon.gpkg"
  )
)

germany_hex <- st_read(
  here(
    "data/country-shapes/germany",
    "germany-hexagon-res-mid.gpkg"
  )
)

base_map <- ggplot2::ggplot(data = germany_polygon) +
  ggplot2::geom_sf(color = "lightgrey", fill = "lightgrey") +
  ggplot2::geom_sf(data = germany_hex) +
  ggplot2::theme_void()

## get data ----
soil_tbl <- st_read(
  here::here(
    "data/german-soil-assessment",
    "german-soil-assessment.gpkg"
  )
)

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
