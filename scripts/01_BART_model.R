# --------------------------------------------------------------------------------------
# 01_BART_model.R
# --------------------------------------------------------------------------------------
# Bittern Modelling using Bayesian Additive Regression Trees (BART)
# Author: Gavin Bonsen
# Description: Prepares data and fits BART models to bittern presence data
# --------------------------------------------------------------------------------------

# ---- LOAD LIBRARIES ----
library(sf)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(purrr)
library(dbarts)
library(DHARMa)
library(corrplot)


# ---- LOAD DATA ----
# NOTE: Paths and wetland names are placeholders — update as needed
detectors <- read.csv("path/to/site_summary.csv") %>% 
  filter(WetlandArea %in% c("Wetland1", "Wetland2")) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  st_transform(28355)

bitterns <- read.csv("path/to/bitterns_occ.csv")
inundation <- read.csv("path/to/inundationBuffers.csv")
flows <- read.csv("path/to/river_flows.csv")  # Example path


# ---- PREPARE SPATIAL BUFFERS ----
buffers <- st_buffer(detectors, dist = 2000)


# ---- DAILY PRESENCE ----
bitterns_daily <- bitterns %>%
  group_by(Region, Session, SiteCode, SiteName, Date) %>%
  summarise(Presence = max(Presence, na.rm = TRUE), .groups = "drop")


# ---- JOIN INUNDATION ----
inundation <- inundation %>%
  rename(Date = date, SiteName = site_name, SiteCode = site_code)

bitterns_daily <- bitterns_daily %>%
  left_join(inundation, by = c("SiteCode", "SiteName", "Date")) %>%
  group_by(SiteCode, SiteName) %>%
  arrange(Date) %>%
  ungroup()


# ---- JOIN FLOW DATA ----
flows <- flows %>%
  rename(Date = date)
bitterns_daily <- left_join(bitterns_daily, flows, by = "Date")


# ---- VEGETATION DISTANCES & PROPORTIONS ----
veg_metrics <- read.csv("path/to/veg_proportions.csv")
bitterns_daily <- bitterns_daily %>%
  left_join(veg_metrics, by = c("SiteCode", "SiteName"))


# ---- SCALE PREDICTORS ----
predictors_all <- c(
  "River_red_gum_dist", "Black_box_dist", "Lignum_dist", "Tall_emergent_dist", "Aquatic_GSF_dist",
  "Lignum_presence", "Tall_emergent_presence", "Aquatic_GSF_presence",
  "buffer_inundated_ha", "WMA_inundated_ha", "Weir1_Flow_ML", "Weir2_Flow_ML"
)

bitterns_daily <- bitterns_daily %>%
  mutate(across(all_of(predictors_all), scale))


# ---- COLLINEARITY CHECK ----
predictor_matrix <- bitterns_daily %>%
  select(all_of(predictors_all)) %>%
  drop_na()
corr_matrix <- cor(predictor_matrix)
corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.8)


# ---- SELECT NON-COLLINEAR VARIABLES ----
high_corr <- which(abs(corr_matrix) > 0.7 & abs(corr_matrix) < 1, arr.ind = TRUE)
high_corr_vars <- unique(c(rownames(corr_matrix)[high_corr[, 1]], colnames(corr_matrix)[high_corr[, 2]]))
predictors_final <- setdiff(predictors_all, high_corr_vars)


# ---- PREPARE FOR BART ----
bart_data <- bitterns_daily %>%
  filter(!is.na(Presence)) %>%
  mutate(pres_bin = as.integer(Presence > 0)) %>%
  select(pres_bin, all_of(predictors_final)) %>%
  drop_na()


# ---- FIT INITIAL BART MODEL ----
set.seed(42)
bart_model_initial <- bart2(
  formula = pres_bin ~ .,
  data = bart_data,
  keepTrees = TRUE
)


# ---- VARIABLE IMPORTANCE FILTER ----
var_counts <- colMeans(bart_model_initial$varcount)
top_vars <- names(var_counts[var_counts > 25])

message("Variables used in >25 trees:\n", paste(top_vars, collapse = ", "))


# ---- REFIT FINAL MODEL ----
bart_data_top <- bart_data %>% select(pres_bin, all_of(top_vars))
bart_model_final <- bart2(
  formula = pres_bin ~ .,
  data = bart_data_top,
  keepTrees = TRUE
)


# ---- RESIDUAL DIAGNOSTICS ----
preds <- predict(bart_model_final, type = "ev")
sim_res <- createDHARMa(simulatedResponse = matrix(rbinom(n = length(preds), size = 1, prob = preds), ncol = 1), 
                        observedResponse = bart_data_top$pres_bin,
                        fittedPredictedResponse = preds)
plot(sim_res)
testTemporalAutocorrelation(sim_res, time = 1:length(preds))


# ---- PARTIAL DEPENDENCE PLOTS ----
get_pdp_data <- function(var, model, data, grid.resolution = 20) {
  grid_vals <- seq(min(data[[var]], na.rm = TRUE), max(data[[var]], na.rm = TRUE), length.out = grid.resolution)
  pdp_preds <- lapply(grid_vals, function(val) {
    newdata <- data
    newdata[[var]] <- val
    plogis(predict(model, newdata = newdata, type = "ev"))
  })
  bind_rows(lapply(seq_along(grid_vals), function(i) {
    probs <- pdp_preds[[i]]
    data.frame(
      variable = var,
      value = grid_vals[i],
      mean = mean(probs),
      lower = quantile(probs, 0.1),
      upper = quantile(probs, 0.9)
    )
  }))
}

pdp_all <- map_dfr(top_vars, get_pdp_data, model = bart_model_final, data = bart_data_top)


# ---- PLOT PDPs ----
ggplot(pdp_all, aes(x = value, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "darkgreen", alpha = 0.2) +
  geom_line(color = "darkgreen") +
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = "Partial Dependence Plots", y = "Predicted Probability") +
  theme_minimal(base_size = 13)


# ---- SAVE MODEL ----
saveRDS(bart_model_final, "bart_model_final.rds")
