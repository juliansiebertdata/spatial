# SETUP --------------------------
rm(list = ls())
## install packman if necessary
if (!"pacman" %in% installed.packages()) {{ install.packages("pacman") }
}

## load librarys ----
pacman::p_load(tidyverse, here, sf, ggtext)

## load plot function ----
## make ggplot2 function ----
germany_hex_map <- function(data, fill) {
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = data,
      aes(fill = {{ fill }}),
      color = alpha("white", 0)
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = element_rect(fill = NA, color = NA),
      legend.position = c(1.1, 0.5),
      legend.title = element_text(face = "bold", size = 22, color = "grey30"),
      legend.text = element_text(size = 12, face = "bold", color = "grey30"),
      legend.justification = "left",
      plot.margin = margin(l = 1, r = 1, t = 1, b = 1, "cm"),
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
}


# LOAD DATA --------------------------
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

soil_hex_tbl <- soil_tbl %>%
  mutate(
    sand = as.numeric(sand_perc) / 100,
    clay = as.numeric(clay_perc) / 100,
    silt = as.numeric(silt_perc) / 100
  )

# Calculate centroids of polygons
hex_centroids <- st_centroid(germany_hex)

# Find nearest points in soil to centroids and add their data to hex_centroids
# then add polygon geometrys back to the data
soil_hex_tbl <- st_join(hex_centroids, soil_hex_tbl, join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  left_join(germany_hex, by = "h3_address") %>%
  st_as_sf()


# WORKFLOW --------------------------
cor_data <- soil_hex_tbl %>%
  select_if(is.numeric) %>%
  select(-sample_site_id) %>%
  st_drop_geometry() %>%
  na.omit()

# pacman::p_load(GGally)

cor_plot <- GGally::ggpairs(
  data = cor_data
)

ggsave(
  plot = cor_plot,
  here("plots", "soil-correlation.png"),
  width = 60,
  height = 60,
  units = "cm"
)

# strong correlation between soil organic carbon and nitrogen
# make bivariate hexplot
bivariate_data <- soil_hex_tbl %>%
  select(organiccarbon_g_per_kg, totalnitrogen_g_per_kg) %>%
  drop_na()

# calculate quantiles
quantile(bivariate_data$organiccarbon_g_per_kg, probs = c(0.33, 0.66))
# 14.86, 26.98

quantile(bivariate_data$totalnitrogen_g_per_kg, probs = c(0.33, 0.66))
# 1.39, 2.43

# set thresholds
t1 <- 14.86
t2 <- 26.98
t3 <- 1.39
t4 <- 2.43

bivariate_data <- bivariate_data %>%
  mutate(
    oc_class = case_when(
      organiccarbon_g_per_kg < t1 ~ "o1",
      organiccarbon_g_per_kg < t2 ~ "o2",
      organiccarbon_g_per_kg >= t2 ~ "o3"
    ),
    tn_class = case_when(
      totalnitrogen_g_per_kg < t3 ~ "t1",
      totalnitrogen_g_per_kg < t4 ~ "t2",
      totalnitrogen_g_per_kg >= t4 ~ "t3"
    ),
    cl = paste0(oc_class, tn_class)
  )

# color palette (https://jakubnowosad.com/posts/2020-08-25-cbc-bp2/)
library(pals)

pal <- stevens.greenblue(9)
# barplot(rep(1, length(pal)), col = pal, border = NA, axes = FALSE)

pal_bivar <- c(
  "o1t1" = pal[1],
  "o1t2" = pal[2],
  "o1t3" = pal[3],
  "o2t1" = pal[4],
  "o2t2" = pal[5],
  "o2t3" = pal[6],
  "o3t1" = pal[7],
  "o3t2" = pal[8],
  "o3t3" = pal[9]
)

# plot bivariate hexplot
bivariate_map <- germany_hex_map(bivariate_data, cl) +
  scale_fill_manual(values = pal_bivar) +
  guides(fill = "none")


ggsave(
  plot = bivariate_map,
  here("plots", "carbon-nitro-plot.png"),
  width = 15,
  height = 22,
  units = "cm"
)

## create legend ----

tib <- tibble(
  var_A = rep(c("o1", "o2", "o3"), 3),
  var_B = c(rep("t1", 3), rep("t2", 3), rep("t3", 3)),
  value = paste0(var_A, var_B)
)

leg_pts <- ggplot(data = tib, aes(x = var_A, y = var_B, fill = value)) +
  geom_point(pch = 21, size = 25, color = "grey90") +
  scale_fill_manual(values = pal) +
  guides(fill = "none") +
  coord_fixed(
    xlim = c(0.8, 3.6),
    ylim = c(0.8, 3.5),
    clip = "off"
  ) +
  labs(x = "Organic Carbon [g/kg]", y = "Total Nitrogen [g/kg]") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_markdown(
      size = 12,
      face = "bold",
      color = "grey30"
    ),
    panel.grid.major = element_blank(),
    axis.line.x.bottom = element_line(
      color = "grey30",
      linewidth = 1.8,
      arrow = arrow(
        type = "closed",
        length = unit(0.3, "cm")
      )
    ),
    axis.line.y.left = element_line(
      color = "grey30",
      linewidth = 1.8,
      arrow = arrow(
        type = "closed",
        length = unit(0.3, "cm"),
      )
    ),
    # plot.margin = unit(x = c(2, 8, 8, 2), units = "cm")
  ) +
  annotate(
    geom = "text",
    x = c(1, 3.9, 3.9),
    y = c(3.8, 3.8, 1),
    label = c(
      "Much Nitrogen,\nLess Carbon",
      "Much Nitrogen,\nMuch Carbon",
      "Less Nitrogen,\nMuch Carbon"
    ),
    size = 3,
    fontface = "bold",
    color = "grey30"
  ) +
  annotate(
    geom = "curve",
    x = c(1, 3, 3),
    xend = c(1.2, 3.6, 3.6),
    y = c(3, 3, 1),
    yend = c(3.65, 3.65, 1),
    color = "grey30",
    curvature = 0.15,
    linewidth = 0.8,
    arrow = arrow(
      length = unit(5, "pt"),
      type = "open",
      ends = "last"
    )
  )

ggsave(
  plot = leg_pts,
  here("plots", "carbon-nitro-legend.png"),
  width = 15,
  height = 15,
  units = "cm"
)

## scatter plot ----

scatter <- ggplot(
  bivariate_data,
  aes(
    x = organiccarbon_g_per_kg,
    y = totalnitrogen_g_per_kg,
    fill = cl,
    color = cl
  )
) +
  geom_point(
    shape = 21,
    stroke = 1.2,
    fill = NA
  ) +
  scale_fill_manual(values = pal_bivar) +
  scale_color_manual(values = pal_bivar) +
  guides(fill = "none", colour = "none") +
  theme_classic() +
  xlim(0, 250) +
  ylim(0, 20) +
  geom_smooth(
    inherit.aes = FALSE,
    aes(x = organiccarbon_g_per_kg, y = totalnitrogen_g_per_kg),
    method = "lm",
    se = FALSE,
    color = "grey30",
    size = 1.2
  ) +
  labs(x = "Organic Carbon [g/kg]", y = "Total Nitrogen [g/kg]") +
  ggplot2::theme(
    axis.title = element_markdown(
      size = 12,
      face = "bold",
      color = "grey30"
    ),
    axis.line.x.bottom = element_line(
      color = "grey30",
      linewidth = 1.4,
      arrow = arrow(
        type = "closed",
        length = unit(0.15, "cm")
      )
    ),
    axis.line.y.left = element_line(
      color = "grey30",
      linewidth = 1.4,
      arrow = arrow(
        type = "closed",
        length = unit(0.15, "cm"),
      )
    ),
    plot.background = element_rect(fill = NA, color = NA),
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
  plot = scatter,
  here("plots", "carbon-nitro-scatter.png"),
  width = 30,
  height = 30,
  units = "cm"
)

# combine plots ----
pacman::p_load(patchwork)

layout <- c(
  area(t = 8, l = 1, b = 10, r = 2),
  area(t = 1, l = 1, b = 10, r = 10)
)

bivariate_info <- leg_pts + bivariate_map +
  plot_layout(design = layout)

ggsave(
  plot = bivariate_info,
  here("plots", "carbon-nitro-combo.png"),
  width = 40,
  height = 40,
  units = "cm"
)
