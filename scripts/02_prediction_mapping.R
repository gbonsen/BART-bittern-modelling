# --------------------------------------------------------------------------------------
# 02_prediction_mapping.R
# --------------------------------------------------------------------------------------
# Bittern BART Prediction: Mapping, Scenarios, and PDP Thresholds
# Author: Gavin Bonsen
# --------------------------------------------------------------------------------------

# ---- LOAD LIBRARIES ----
library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(dbarts)
library(purrr)


# ---- LOAD SPATIAL DATA ----
anae <- st_read("path/to/ANAE_clipped.shp")
wetlands <- st_read("path/to/wetlandAreas_dissolved.shp")
floodplain <- st_read("path/to/floodplain_clipped.shp")
bart_model.final <- readRDS("bart_model_final.rds")
target_crs <- 28355
anae <- st_transform(anae, target_crs)
wetlands <- st_transform(wetlands, target_crs)
floodplain <- st_transform(floodplain, target_crs)


# ---- CREATE GRID FOR PREDICTIONS ----
res_m <- 500
grid <- st_make_grid(wetlands, cellsize = res_m, what = "polygons") %>%
  st_as_sf() %>%
  st_filter(wetlands)
grid_vect <- vect(grid)


# ---- CALCULATE DISTANCES TO FEATURES ----
floodplain_vect <- vect(floodplain)
min_dist_floodplain <- apply(terra::distance(grid_vect, floodplain_vect), 1, min)

get_min_distance <- function(anae_layer, type) {
  veg <- anae_layer %>% filter(Veg_Palust == type) %>% vect()
  apply(terra::distance(grid_vect, veg), 1, min)
}

min_dist_rrg <- get_min_distance(anae, "River red gum")
min_dist_lignum <- get_min_distance(anae, "Lignum")
min_dist_gsf <- get_min_distance(anae, "Aquatic grass/sedge/forb")

tev <- anae %>% filter(Veg_Palust == "Tall emergent aquatic")
tev_rast <- rasterize(vect(tev), rast(grid_vect, resolution = res_m), field = 1, background = 0)


# ---- LOAD TRAINING DATA AND SCALE VALUES ----
original_df <- read.csv("bitterns_dailyPres_allVars.csv")
original_df$WMA_area_inundated_ha <- original_df$WMA_inundated_ha

mu_sd <- original_df %>%
  summarise(
    buffer_mu = mean(buffer_inundated_ha, na.rm = TRUE),
    buffer_sd = sd(buffer_inundated_ha, na.rm = TRUE),
    WMA_mu = mean(WMA_area_inundated_ha, na.rm = TRUE),
    WMA_sd = sd(WMA_area_inundated_ha, na.rm = TRUE),
    weir1_mu = mean(Weir1_Flow_ML, na.rm = TRUE),
    weir1_sd = sd(Weir1_Flow_ML, na.rm = TRUE),
    weir2_mu = mean(Weir2_Flow_ML, na.rm = TRUE),
    weir2_sd = sd(Weir2_Flow_ML, na.rm = TRUE),
    rrg_mu = mean(River_red_gum_dist, na.rm = TRUE),
    rrg_sd = sd(River_red_gum_dist, na.rm = TRUE),
    lignum_mu = mean(Lignum_dist, na.rm = TRUE),
    lignum_sd = sd(Lignum_dist, na.rm = TRUE),
    gsf_mu = mean(Aquatic_GSF_dist, na.rm = TRUE),
    gsf_sd = sd(Aquatic_GSF_dist, na.rm = TRUE)
  ) %>% as.list()


# ---- CONSTRUCT GRID WITH UN-SCALED VALUES ----
grid_df <- st_as_sf(grid_vect) %>%
  mutate(
    RiverRedGum_dist = min_dist_rrg,
    Lignum_dist = min_dist_lignum,
    AquaticGSF_dist = min_dist_gsf,
    floodplain_dist = min_dist_floodplain,
    TallEmergent_presence = terra::extract(tev_rast, grid_vect)[, 2]
  )


# ---- SCALING FUNCTION FOR MANUAL STANDARDISATION ----
scale_manual <- function(x, mean_val, sd_val) {
  (x - mean_val) / sd_val
}

grid_df.scaled <- grid_df %>%
  mutate(
    buffer_inundated_ha = scale_manual(500, mu = mu_sd$buffer_mu, sd = mu_sd$buffer_sd), # NOTE: Manually scaling fixed value (500 ha inundation) for prediction grid
    WMA_area_inundated_ha = scale_manual(10000, mu = mu_sd$WMA_mu, sd = mu_sd$WMA_sd),
    Weir1_Flow_ML = scale_manual(4000, mu = mu_sd$weir1_mu, sd = mu_sd$weir1_sd),
    Weir2_Flow_ML = scale_manual(3000, mu = mu_sd$weir2_mu, sd = mu_sd$weir2_sd),
    River_red_gum_dist = scale_manual(RiverRedGum_dist, mu = mu_sd$rrg_mu, sd = mu_sd$rrg_sd),
    Aquatic_GSF_dist = scale_manual(AquaticGSF_dist, mu = mu_sd$gsf_mu, sd = mu_sd$gsf_sd),
    Lignum_dist = scale_manual(Lignum_dist, mu = mu_sd$lignum_mu, sd = mu_sd$lignum_sd),
    Tall_emergent_presence = TallEmergent_presence
  ) %>%
  select(
    buffer_inundated_ha, WMA_area_inundated_ha, Weir1_Flow_ML,
    Weir2_Flow_ML, River_red_gum_dist, Lignum_dist,
    Aquatic_GSF_dist, Tall_emergent_presence, geometry
  )


# ---- PREDICT BITTERN USE ----
# Requires fitted model: bart_model.final
predictors <- names(grid_df.scaled)[names(grid_df.scaled) != "geometry"]
pred_matrix <- predict(bart_model.final, newdata = as.matrix(st_drop_geometry(grid_df.scaled)))
grid_df$predicted_prob <- colMeans(pred_matrix)


# ---- MAP: PREDICTED BITTERN USE ----
grid_polys <- grid_df %>% st_intersection(floodplain)
ggplot() +
  geom_sf(data = wetlands, fill = "grey95", color = NA) +
  geom_sf(data = WMAs, fill = NA, color = "darkred", linetype = "dashed", size = 0.3) +
  geom_sf(data = grid_polys, aes(fill = predicted_prob), color = NA) +
  geom_sf(data = floodplain, fill = NA, color = "black", size = 0.4) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  labs(title = "Predicted Bittern Use (Final BART Model)") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_blank(),
    legend.position = "right", legend.title = element_blank()
  )


# ---- SCENARIO THRESHOLD PDPs ----

scenario_thresholds <- tibble::tibble(
  Predictor = c("Buffer inundated (ha)", "WMA inundated (1000 ha)", 
                "Weir 1 flow (GL/day)", "Weir 2 flow (GL/day)"),
  Minimal = c(600, 25, 3.5, 6),
  Optimal = c(1020, 54, 6.5, 20)
)

threshold_vars <- c("buffer_inundated_ha", "WMA_area_inundated_ha",
                    "Weir1_Flow_ML", "Weir2_Flow_ML")

threshold_df <- pdp_ci_df %>%
  filter(Variable %in% threshold_vars) %>%
  mutate(
    Predictor = recode(Variable,
                       "buffer_inundated_ha" = "Buffer inundated (ha)",
                       "WMA_area_inundated_ha" = "WMA inundated (1000 ha)",
                       "Weir1_Flow_ML" = "Weir 1 flow (GL/day)",
                       "Weir2_Flow_ML" = "Weir 2 flow (GL/day)"),
    RealValue = Value,
    Predictor = factor(Predictor, levels = c(
      "Buffer inundated (ha)", "WMA inundated (1000 ha)",
      "Weir 1 flow (GL/day)", "Weir 2 flow (GL/day)"
    ))
  )

scenario_thresholds$Predictor <- factor(scenario_thresholds$Predictor, levels = levels(threshold_df$Predictor))

ggplot(threshold_df, aes(x = RealValue, y = Probability)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "lightblue", alpha = 0.3) +
  geom_line(color = "blue", linewidth = 1.2) +
  facet_wrap(~Predictor, scales = "free_x") +
  geom_vline(data = scenario_thresholds, aes(xintercept = Minimal),
             color = "orange", linetype = "dashed", linewidth = 0.8) +
  geom_vline(data = scenario_thresholds, aes(xintercept = Optimal),
             color = "darkgreen", linetype = "dashed", linewidth = 0.8) +
  geom_text(data = scenario_thresholds, aes(x = Minimal, y = 0.95, label = "Minimal"),
            color = "orange", angle = 90, vjust = -0.3, hjust = 1, size = 3.5) +
  geom_text(data = scenario_thresholds, aes(x = Optimal, y = 0.95, label = "Optimal"),
            color = "darkgreen", angle = 90, vjust = -0.3, hjust = 1, size = 3.5) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    y = "Predicted probability of bittern use",
    x = "Predictor value (units)"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank())


# ---- SCENARIO-BASED PREDICTION MAPS ----

# Define real-world scenario inputs
scenarios <- tibble::tibble(
  scenario = c("Minimal", "Optimal"),
  buffer_inundated_ha = c(600, 1020),
  WMA_area_inundated_ha = c(25000, 54000),
  Weir1_Flow_ML = c(3500, 6500),
  Weir2_Flow_ML = c(6000, 20000)
) %>%
  mutate(
    buffer_inundated_ha = scale_manual(buffer_inundated_ha, mu_sd$buffer_mu, mu_sd$buffer_sd),
    WMA_area_inundated_ha = scale_manual(WMA_area_inundated_ha, mu_sd$WMA_mu, mu_sd$WMA_sd),
    Weir1_Flow_ML = scale_manual(Weir1_Flow_ML, mu_sd$weir1_mu, mu_sd$weir1_sd),
    Weir2_Flow_ML = scale_manual(Weir2_Flow_ML, mu_sd$weir2_mu, mu_sd$weir2_sd)
  )

# Apply scenarios to grid
scenario_grids <- scenarios %>%
  group_by(scenario) %>%
  group_split() %>%
  map_df(function(scn) {
    grid_df %>%
      mutate(
        buffer_inundated_ha = scn$buffer_inundated_ha,
        WMA_area_inundated_ha = scn$WMA_area_inundated_ha,
        Weir1_Flow_ML = scn$Weir1_Flow_ML,
        Weir2_Flow_ML = scn$Weir2_Flow_ML,
        scenario = scn$scenario
      )
  })

# Rename predictors to match trained model
scenario_grids <- scenario_grids %>%
  rename(
    River_red_gum_dist = RiverRedGum_dist,
    Aquatic_GSF_dist = AquaticGSF_dist,
    Tall_emergent_presence = TallEmergent_presence
  )

# Predict
model_vars <- c("buffer_inundated_ha", "WMA_area_inundated_ha",
                "Weir1_Flow_ML", "Weir2_Flow_ML",
                "River_red_gum_dist", "Lignum_dist", "Aquatic_GSF_dist",
                "Tall_emergent_presence")

x_test <- scenario_grids %>%
  st_drop_geometry() %>%
  select(all_of(model_vars)) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()

scenario_grids$predicted_prob <- colMeans(predict(bart_model.final, newdata = x_test))

# Plot: Scenario prediction maps
ggplot(scenario_grids) +
  geom_sf(data = floodplain, fill = NA, color = "black", linewidth = 0.1) +
  geom_sf(aes(fill = predicted_prob), color = NA) +
  facet_wrap(~scenario) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )

