# SETUP --------------------------
rm(list = ls())
## install packman if necessary
if (!"pacman" %in% installed.packages()) {{ install.packages("pacman") }
}

## load librarys ----
pacman::p_load(here, sf)
library(ggplot2)
library(ggtext)
library(tidyverse)

## load plot function ----
## make ggplot2 function ----
germany_hex_map_nolegend <- function(data, fill) {
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = data,
      aes(fill = {{ fill }}),
      color = alpha("white", 0)
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
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
  select(organiccarbon_g_per_kg, totalnitrogen_g_per_kg, landuse_type) %>%
  drop_na(organiccarbon_g_per_kg, totalnitrogen_g_per_kg) %>%
  mutate(landuse_type_log = case_when(
    landuse_type == "A" ~ 0,
    landuse_type == "G" | landuse_type == "SO" ~ 1,
  ))

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
bivariate_map <- germany_hex_map_nolegend(bivariate_data, cl) +
  scale_fill_manual(values = pal_bivar) +
  theme(legend.position = "none")


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

t1 <- 14.86
t2 <- 26.98
t3 <- 1.39
t4 <- 2.43



leg_pts <- ggplot(data = tib, aes(x = var_A, y = var_B, fill = value)) +
  geom_point(pch = 21, size = 30, color = "grey90") +
  scale_fill_manual(values = pal) +
  guides(fill = "none") +
  scale_x_discrete(
    label = c("< 14.9 g/kg", "< 27 g/kg ", "=> 27 g/kg")
  ) +
  scale_y_discrete(
    label = c("< 1.4 g/kg", "< 2.4 g/kg ", "=> 2.4 g/kg")
  ) +
  coord_fixed(
    xlim = c(0.8, 3.6),
    ylim = c(0.8, 3.5),
    clip = "off"
  ) +
  labs(x = "Organic Carbon [g/kg soil]", y = "Total Nitrogen [g/kg soil]") +
  theme_minimal() +
  theme(
    axis.text = element_markdown(
      size = 8,
      face = "bold",
      color = "grey30"
    ),
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
    plot.background = element_rect(fill = NA, color = NA),
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


scatter_plot <- ggplot(
  bivariate_data,
  aes(
    x = organiccarbon_g_per_kg,
    y = totalnitrogen_g_per_kg,
    color = cl,
    fill = cl
  )
) +
  geom_point(
    shape = 21,
    stroke = 1.2,
    fill = NA,
    alpha = 0.6
  ) +
  scale_color_manual(
    values = pal_bivar
  ) +
  guides(colour = "none", fill = "none") +
  theme_classic() +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 10)) +
  scale_x_continuous(
    breaks = c(seq(0, 100, 20)),
    labels = c(seq(0, 100, 20))
  ) +
  scale_y_continuous(
    breaks = c(seq(0, 10, 2)),
    labels = c(seq(0, 10, 2))
  ) +
  labs(x = "Organic Carbon [g/kg]", y = "Total Nitrogen [g/kg]") +
  ggplot2::theme(
    axis.title = element_markdown(
      size = 12,
      face = "bold",
      color = "grey30"
    ),
    axis.text = element_markdown(
      size = 10,
      face = "bold",
      color = "grey30"
    ),
    axis.ticks = element_blank(),
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
  plot = scatter_plot,
  here("plots", "carbon-nitro-scatter.png"),
  width = 30,
  height = 30,
  units = "cm"
)

# combine plots ----
# detach("package:geomtextpath", unload = TRUE)
library(patchwork)

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

# all three combined
# Define the layout
layout <- c(
  area(t = 1, l = 1, b = 6, r = 3), # Area for scatter_plot
  area(t = 7, l = 2, b = 11, r = 3), # Area for legend
  area(t = 1, l = 4, b = 11, r = 10) # Area for bivariate_map
)

# Create the patchwork object
all_plots <- scatter_plot + leg_pts + bivariate_map + plot_layout(design = layout)


ggsave(
  plot = all_plots,
  here("plots", "carbon-nitro-final.png"),
  width = 55,
  height = 38,
  units = "cm"
)

# lm and quantile regression ----

## lm ----

lm_model <- lm(
  totalnitrogen_g_per_kg ~ organiccarbon_g_per_kg,
  data = bivariate_data
)

lm_model %>% broom::tidy()

## median regression ----

library(quantreg)

quant_model <- rq(
  totalnitrogen_g_per_kg ~ organiccarbon_g_per_kg,
  data = bivariate_data,
  tau = c(0.25, 0.5, 0.75)
)

quant_model %>% broom::tidy()

scatter_plot_lm <- scatter_plot +
  coord_cartesian() +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth(
    inherit.aes = FALSE,
    aes(x = organiccarbon_g_per_kg, y = totalnitrogen_g_per_kg),
    method = "lm",
    formula = y ~ x,
    color = "black",
    linewidth = 1.2
  ) +
  geom_quantile(
    inherit.aes = FALSE,
    aes(x = organiccarbon_g_per_kg, y = totalnitrogen_g_per_kg),
    stat = "quantile",
    formula = y ~ x,
    quantiles = c(0.05, 0.5, 0.95),
    color = "darkred",
    linewidth = 1.2,
    linetype = "dashed"
  )

ggsave(
  plot = scatter_plot_lm,
  here("plots", "carbon-nitro-scatter-model.png"),
  width = 30,
  height = 30,
  units = "cm"
)

# -> lm model underestimates the effect of organic carbon on total nitrogen
remotes::install_version("ggplot2", version = "< 3.5.0")


## facet by land use ----

facet_land_use <- bivariate_data %>%
  mutate(landuse_type_log = case_when(
    landuse_type == "A" ~ "Agriculture",
    landuse_type == "G" ~ "Grassland and Special Culture",
    landuse_type == "SO" ~ "Grassland and Special Culture"
  )) %>%
  germany_hex_map_nolegend(data = ., cl) +
  scale_fill_manual(values = pal_bivar) +
  facet_wrap(~landuse_type_log) +
  theme(
    legend.position = "none",
    strip.text = element_markdown(
      size = 12,
      face = "bold",
      color = "grey30"
    ),
    strip.placement = "bottom"
  )

layout_landuse <- c(
  area(t = 8, l = 1, b = 10, r = 2),
  area(t = 1, l = 1, b = 10, r = 10)
)

landuse_facet <- leg_pts + facet_land_use +
  plot_layout(design = layout_landuse) +
  plot_annotation(
    title = "Organic Carbon and Total Nitrogen by Land Use",
    theme = theme(
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
  )


ggsave(
  plot = landuse_facet,
  here("plots", "carbon-nitro-combo-patchwork.png"),
  width = 65,
  height = 30,
  units = "cm"
)

ggsave(
  plot = facet_land_use,
  here("plots", "carbon-nitro-landuse.png"),
  width = 40,
  height = 20,
  units = "cm"
)

## logistic regression ----

boxplot <- ggplot(
  bivariate_data,
  aes(x = as.factor(landuse_type_log), y = totalnitrogen_g_per_kg)
) +
  geom_boxplot() +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  theme_bw()

scatter_plot_landuse <- scatter_plot +
  coord_cartesian() +
  scale_x_continuous() +
  scale_y_continuous() +
  facet_wrap(~ as.factor(landuse_type_log)) +
  gghighlight::gghighlight()

ggsave(
  plot = boxplot,
  here("plots", "carbon-nitro-boxplot.png"),
  width = 15,
  height = 15,
  units = "cm"
)

glm(
  formula = as.factor(landuse_type_log) ~ organiccarbon_g_per_kg + totalnitrogen_g_per_kg,
  family = "binomial",
  data = bivariate_data
) %>%
  gtsummary::tbl_regression(exponentiate = TRUE)
