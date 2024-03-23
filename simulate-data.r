pacman::p_load(dplyr, janitor, sf, here)


# yield data --------------------------------------
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

# read germany polygon
germany_polygon <- st_read(
  here(
    "data/country-shapes/germany",
    "germany-polygon.gpkg"
  )
)

# filter simulated data based on polygon
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
