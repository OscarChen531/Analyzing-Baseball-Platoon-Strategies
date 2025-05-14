# platoon_code.R
# Analyzing Baseball Platoon Strategies: Integrating Bat Speed, Swing Length, Spray Angle, Clustering, and Performance Metrics
# Author: Wei Chieh Chen
# Date: April 30, 2025

# Load libraries
library(tidyr)
library(factoextra)
library(MASS)
library(ggplot2)
library(summarytools)
library(baseballr)
library(gridExtra)
library(dplyr)
set.seed(123)

# Set theme for visualizations
theme_set(theme_minimal(base_size = 12) + 
            theme(
              plot.title = element_text(hjust = 0.5, face = "bold"),
              axis.title = element_text(face = "bold"),
              legend.position = "bottom",
              panel.grid.minor = element_blank()
            ))

# Load data with error handling
data <- tryCatch({
  read.csv("Final Project/data/statcast_pitch_swing_data_20240402_20241030_with_arm_angle.csv")
}, error = function(e) {
  stop("Failed to load CSV. Ensure file is in working directory: ", e)
})

# Check column names
required_cols <- c("player_name", "bat_speed", "swing_length", "launch_angle", "launch_speed", 
                   "pitch_type", "arm_angle", "estimated_ba_using_speedangle", 
                   "estimated_slg_using_speedangle", "woba_value", "stand", "p_throws", 
                   "balls", "strikes", "on_1b", "on_2b", "on_3b", "hc_x", "hc_y", "type", 
                   "description", "zone", "plate_x", "plate_z", "sz_top", "sz_bot", "events",
                   "game_pk", "at_bat_number", "pitch_number", "game_type")
missing_cols <- setdiff(required_cols, colnames(data))
if (length(missing_cols) > 0) {
  stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
}

# Calculate plate discipline metrics
plate_discipline <- data %>%
  mutate(
    sz_top = ifelse(is.na(sz_top), median(sz_top, na.rm = TRUE), sz_top),
    sz_bot = ifelse(is.na(sz_bot), median(sz_bot, na.rm = TRUE), sz_bot),
    is_strike_zone = zone %in% 1:9,
    is_strike_zone_plate = plate_x >= -0.83 & plate_x <= 0.83 & plate_z >= sz_bot & plate_z <= sz_top,
    is_strike_zone = ifelse(is.na(is_strike_zone), is_strike_zone_plate, is_strike_zone),
    is_swing = description %in% c("hit_into_play", "foul", "swinging_strike", "foul_tip")
  ) %>%
  group_by(player_name) %>%
  summarise(
    total_pitches = n(),
    total_swings = sum(is_swing, na.rm = TRUE),
    in_zone_pitches = sum(is_strike_zone, na.rm = TRUE),
    out_zone_pitches = sum(!is_strike_zone, na.rm = TRUE),
    in_zone_swings = sum(is_swing & is_strike_zone, na.rm = TRUE),
    out_zone_swings = sum(is_swing & !is_strike_zone, na.rm = TRUE),
    Z_Swing = ifelse(in_zone_pitches > 0, (in_zone_swings / in_zone_pitches) * 100, NA),
    O_Swing = ifelse(out_zone_pitches > 0, (out_zone_swings / out_zone_pitches) * 100, NA),
    Swing = (total_swings / total_pitches) * 100,
    .groups = "drop"
  ) %>%
  filter(total_pitches >= 10) %>%
  mutate(
    Z_Swing = ifelse(is.na(Z_Swing) | !is.finite(Z_Swing), 67, Z_Swing),
    O_Swing = ifelse(is.na(O_Swing) | !is.finite(O_Swing), 30, O_Swing),
    Swing = ifelse(is.na(Swing) | !is.finite(Swing), 47, Swing)
  ) %>%
  select(player_name, Z_Swing, O_Swing, Swing)

# Preprocess data for clustering
data_clustering <- data %>%
  mutate(
    player_name = ifelse(stand == "L", paste0(player_name, "-L"), paste0(player_name, "-R")),
    spray_angle = ifelse(
      type == "X" & !is.na(hc_x) & !is.na(hc_y),
      atan2(hc_y - 200, hc_x - 125) * 180 / pi,
      NA
    ),
    adj_spray_angle = ifelse(stand == "L", -spray_angle, spray_angle),
    count = paste(balls, strikes, sep = "-")
  ) %>%
  filter(type == "X" | description %in% c("hit_into_play", "foul", "swinging_strike", "foul_tip")) %>%
  dplyr::select(player_name, bat_speed, swing_length, launch_angle, launch_speed, pitch_type, 
                arm_angle, estimated_ba_using_speedangle, estimated_slg_using_speedangle, 
                woba_value, stand, p_throws, balls, strikes, on_1b, on_2b, on_3b, 
                spray_angle, adj_spray_angle, count, events, game_pk, at_bat_number) %>%
  mutate(
    arm_angle = ifelse(is.na(arm_angle), median(arm_angle, na.rm = TRUE), arm_angle),
    base_runners = paste(ifelse(is.na(on_1b), 0, 1), ifelse(is.na(on_2b), 0, 1), ifelse(is.na(on_3b), 0, 1), sep = "")
  ) %>%
  filter(!is.na(bat_speed) & !is.na(swing_length))

# Calculate attack angle
attack_angle_data <- data_clustering %>%
  filter(!is.na(launch_angle) & !is.na(launch_speed)) %>%
  group_by(player_name) %>%
  filter(n() >= 30) %>%
  summarise(
    attack_angle = {
      if (max(launch_angle, na.rm = TRUE) - min(launch_angle, na.rm = TRUE) < 6) {
        mean(launch_angle[order(launch_speed, decreasing = TRUE)][1:15], na.rm = TRUE)
      } else {
        bins <- cut(launch_angle, 
                    breaks = seq(min(launch_angle, na.rm = TRUE), 
                                 max(launch_angle, na.rm = TRUE), 
                                 by = 3), 
                    include.lowest = TRUE)
        if (length(unique(bins[!is.na(bins)])) < 5) {
          mean(launch_angle[order(launch_speed, decreasing = TRUE)][1:15], na.rm = TRUE)
        } else {
          top_20 <- . %>% arrange(desc(launch_speed)) %>% slice_head(prop = 0.2)
          binned_data <- data.frame(launch_angle, launch_speed, bins) %>% 
            group_by(bins) %>% 
            top_20() %>%
            ungroup()
          if (nrow(binned_data) < 5 || sum(!is.na(binned_data$launch_angle)) < 5) {
            mean(launch_angle[order(launch_speed, decreasing = TRUE)][1:15], na.rm = TRUE)
          } else {
            tryCatch({
              parabola_model <- lm(launch_speed ~ launch_angle + I(launch_angle^2), 
                                   data = binned_data)
              peak_angle <- -coef(parabola_model)[2] / (2 * coef(parabola_model)[3])
              top_15_angle <- mean(launch_angle[order(launch_speed, decreasing = TRUE)][1:15], 
                                   na.rm = TRUE)
              # Ensure attack angle is within a realistic range (e.g., -10 to 30 degrees)
              attack_angle <- mean(c(peak_angle, top_15_angle), na.rm = TRUE)
              if (attack_angle < -10 || attack_angle > 30 || is.na(attack_angle)) {
                mean(launch_angle[order(launch_speed, decreasing = TRUE)][1:15], na.rm = TRUE)
              } else {
                attack_angle
              }
            }, error = function(e) {
              mean(launch_angle[order(launch_speed, decreasing = TRUE)][1:15], na.rm = TRUE)
            })
          }
        }
      }
    },
    .groups = "drop"
  ) %>%
  mutate(attack_angle = ifelse(is.na(attack_angle) | !is.finite(attack_angle), 
                               median(attack_angle, na.rm = TRUE), 
                               attack_angle))

# Join attack angle and plate discipline
data_clustering <- data_clustering %>%
  left_join(attack_angle_data, by = "player_name") %>%
  left_join(plate_discipline, by = "player_name")

# Batter-level summaries
batter_summary <- data_clustering %>%
  group_by(player_name, pitch_type, stand, p_throws) %>%
  summarise(
    bat_speed = mean(bat_speed, na.rm = TRUE),
    swing_length = mean(swing_length, na.rm = TRUE),
    attack_angle = mean(attack_angle, na.rm = TRUE),
    spray_angle = mean(spray_angle, na.rm = TRUE),
    Z_Swing = mean(Z_Swing, na.rm = TRUE),
    O_Swing = mean(O_Swing, na.rm = TRUE),
    Swing = mean(Swing, na.rm = TRUE),
    eBA = mean(estimated_ba_using_speedangle, na.rm = TRUE),
    eSLG = mean(estimated_slg_using_speedangle, na.rm = TRUE),
    woba = mean(woba_value, na.rm = TRUE),
    n_pitches = n(),
    .groups = "drop"
  ) %>%
  filter(n_pitches >= 20)

# Pivot to wide format
batter_wide <- batter_summary %>%
  group_by(pitch_type, p_throws) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  pivot_wider(
    names_from = c(pitch_type, p_throws),
    values_from = c(bat_speed, swing_length, attack_angle, spray_angle, Z_Swing, O_Swing, Swing, eBA, eSLG, woba),
    values_fill = NA,
    names_glue = "{pitch_type}_{p_throws}_{.value}"
  ) %>%
  group_by(player_name) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# PCA
pca_vars <- batter_wide %>%
  select(-player_name) %>%
  select(where(is.numeric))

# Remove zero-variance columns
zero_var_cols <- names(pca_vars)[apply(pca_vars, 2, function(x) var(x, na.rm = TRUE) == 0 | all(is.na(x)))]
if (length(zero_var_cols) > 0) {
  message("Removing zero-variance columns: ", paste(zero_var_cols, collapse = ", "))
  pca_vars <- pca_vars %>% select(-all_of(zero_var_cols))
}

# Scale and run PCA
pca_vars_scaled <- scale(pca_vars)
pca_result <- prcomp(pca_vars_scaled, center = TRUE, scale. = TRUE)
pca_scores <- pca_result$x[, 1:4]

# Clustering
batter_pca <- data.frame(
  player_name = batter_wide$player_name,
  stand = batter_summary$stand[match(batter_wide$player_name, batter_summary$player_name)],
  pca_scores
)
rhb_pca <- batter_pca %>% filter(stand == "R")
lhb_pca <- batter_pca %>% filter(stand == "L")
rhb_kmeans <- kmeans(rhb_pca[, grep("PC", names(rhb_pca))], centers = 4, nstart = 25)
lhb_kmeans <- kmeans(lhb_pca[, grep("PC", names(lhb_pca))], centers = 4, nstart = 25)
batter_pca$cluster <- NA
batter_pca$cluster[batter_pca$stand == "R"] <- paste0("RHB-", rhb_kmeans$cluster)
batter_pca$cluster[batter_pca$stand == "L"] <- paste0("LHB-", lhb_kmeans$cluster)

# Platoon summary (Table 1 - for FF pitch type, and Table A2 - full summary)
# Full Platoon Summary (Table A2 in Appendix)
platoon_summary_full <- batter_summary %>%
  left_join(select(batter_pca, player_name, cluster), by = "player_name", relationship = "many-to-one") %>%
  group_by(cluster, pitch_type, p_throws) %>%
  summarise(
    mean_bat_speed = mean(bat_speed, na.rm = TRUE),
    mean_swing_length = mean(swing_length, na.rm = TRUE),
    mean_attack_angle = mean(attack_angle, na.rm = TRUE),
    n_batters = n(),
    .groups = "drop"
  ) %>%
  filter(n_batters >= 5) %>%
  mutate(
    mean_bat_speed = round(mean_bat_speed, 1),
    mean_swing_length = round(mean_swing_length, 1),
    mean_attack_angle = round(mean_attack_angle, 1)
  ) %>%
  select(cluster, pitch_type, p_throws, mean_bat_speed, mean_swing_length, mean_attack_angle) %>%
  rename(
    Cluster = cluster,
    Pitch_Type = pitch_type,
    Pitcher = p_throws,
    "Bat Speed (mph)" = mean_bat_speed,
    "Swing Length (ft)" = mean_swing_length,
    "Attack Angle (deg)" = mean_attack_angle
  )

# Table 1: Platoon Summary for FF pitch type
platoon_summary_ff <- platoon_summary_full %>%
  filter(Pitch_Type == "FF") %>%
  select(Cluster, Pitcher, "Bat Speed (mph)", "Swing Length (ft)", "Attack Angle (deg)")

# Monte Carlo simulation
events <- c("DoublePlay", "Error", "GroundOut", "HBP_CatInt", "HR", "K", "LineOut_InfFly", 
            "Triple", "Walk", "LongSingle", "MediumSingle", "ShortSingle", "ShortDouble", 
            "LongDouble", "LongFly", "MediumFly", "ShortFly")

event_probs <- batter_summary %>%
  left_join(select(batter_pca, player_name, cluster), by = "player_name", relationship = "many-to-one") %>%
  group_by(cluster) %>%
  summarise(
    mean_eBA = mean(eBA, na.rm = TRUE),
    mean_eSLG = mean(eSLG, na.rm = TRUE),
    mean_woba = mean(woba, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    HR = mean_woba * 0.2,
    Walk = mean_woba * 0.15,
    K = (1 - mean_eBA) * 0.3,
    GroundOut = (1 - mean_eBA) * 0.2,
    DoublePlay = (1 - mean_eBA) * 0.05,
    LineOut_InfFly = (1 - mean_eBA) * 0.1,
    LongFly = (1 - mean_eBA) * 0.05,
    MediumFly = (1 - mean_eBA) * 0.05,
    ShortFly = (1 - mean_eBA) * 0.05,
    Error = 0.01,
    HBP_CatInt = 0.015,
    Triple = mean_eSLG * 0.02,
    LongDouble = mean_eSLG * 0.03,
    ShortDouble = mean_eSLG * 0.03,
    LongSingle = mean_eBA * 0.15,
    MediumSingle = mean_eBA * 0.15,
    ShortSingle = mean_eBA * 0.15,
    total_prob = rowSums(across(HR:ShortSingle), na.rm = TRUE),
    across(HR:ShortSingle, ~ .x / total_prob)
  ) %>%
  select(cluster, all_of(events))

simulate_game <- function(lineup_clusters, event_probs, park_factor = 1) {
  runs <- 0
  for (inning in 1:9) {
    outs <- 0
    runners <- c(0, 0, 0)
    while (outs < 3) {
      batter_cluster <- lineup_clusters[(inning + outs) %% length(lineup_clusters) + 1]
      event <- sample(events, 1, prob = event_probs[event_probs$cluster == batter_cluster, events])
      runs <- runs + ifelse(event == "HR", 1 + sum(runners), 0)
      outs <- outs + ifelse(event %in% c("K", "GroundOut"), 1, 0)
    }
  }
  runs * park_factor
}

lineup_clusters <- c("RHB-1", "LHB-2", "RHB-4", "LHB-1", "RHB-3", "LHB-3", "RHB-2", "LHB-4", "RHB-1")
runs_sim <- replicate(10000, simulate_game(lineup_clusters, event_probs, park_factor = 1))
mean_runs <- mean(runs_sim)

# Platoon performance analysis (Tables 2 and 3)
data <- data %>%
  filter(game_type %in% c("R", "F", "D", "L", "W"))

batter_handedness <- data %>%
  group_by(batter) %>%
  summarise(
    stands_R = any(stand == "R", na.rm = TRUE),
    stands_L = any(stand == "L", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    batter_type = case_when(
      stands_R & stands_L ~ "Switch Hitter",
      stands_R ~ "RHB",
      stands_L ~ "LHB",
      TRUE ~ "Unknown"
    )
  )

data <- data %>%
  left_join(select(batter_handedness, batter, batter_type), by = "batter") %>%
  filter(batter_type != "Unknown")

data_pa <- data %>%
  group_by(game_pk, at_bat_number, batter) %>%
  slice(which.max(pitch_number)) %>%
  ungroup()

# Add handedness suffix to player_name in data_pa to match attack_angle_data
data_pa <- data_pa %>%
  mutate(
    player_name = ifelse(stand == "L", paste0(player_name, "-L"), paste0(player_name, "-R")),
    is_hit = events %in% c("single", "double", "triple", "home_run"),
    on_base = events %in% c("single", "double", "triple", "home_run", "walk", "hit_by_pitch"),
    is_at_bat = !events %in% c("walk", "hit_by_pitch", "sac_fly", "sac_bunt", NA),
    total_bases = case_when(
      events == "single" ~ 1,
      events == "double" ~ 2,
      events == "triple" ~ 3,
      events == "home_run" ~ 4,
      TRUE ~ 0
    ),
    is_pa_obp = !events %in% c("sac_bunt", NA)
  )

# Join attack angle to data_pa
data_pa <- data_pa %>%
  left_join(select(attack_angle_data, player_name, attack_angle), 
            by = "player_name")

# Verify columns exist
if (!all(c("bat_speed", "swing_length", "attack_angle") %in% colnames(data_pa))) {
  stop("Required columns (bat_speed, swing_length, attack_angle) not found in data_pa")
}

# Ensure no NA values in critical columns for aggregation
data_pa <- data_pa %>%
  mutate(
    bat_speed = ifelse(is.na(bat_speed), mean(bat_speed, na.rm = TRUE), bat_speed),
    swing_length = ifelse(is.na(swing_length), mean(swing_length, na.rm = TRUE), swing_length),
    attack_angle = ifelse(is.na(attack_angle) | !is.finite(attack_angle), 
                          mean(attack_angle_data$attack_angle, na.rm = TRUE), attack_angle)
  )

# Compute league-wide averages (lgOBP and lgSLG) from data_pa
league_averages <- data_pa %>%
  summarise(
    total_hits = sum(is_hit, na.rm = TRUE),
    total_at_bats = sum(is_at_bat, na.rm = TRUE),
    total_on_base = sum(on_base, na.rm = TRUE),
    total_pa_obp = sum(is_pa_obp, na.rm = TRUE),
    total_bases = sum(total_bases, na.rm = TRUE),
    mean_obp = total_on_base / total_pa_obp,
    mean_slg = total_bases / total_at_bats,
    .groups = "drop"
  )

# Define lgOBP and lgSLG
lgOBP <- league_averages$mean_obp  # League-wide OBP
lgSLG <- league_averages$mean_slg  # League-wide SLG

# Compute performance summary
performance_summary <- data_pa %>%
  group_by(batter_type, p_throws) %>%
  summarise(
    mean_bat_speed = mean(bat_speed, na.rm = TRUE),
    mean_swing_length = mean(swing_length, na.rm = TRUE),
    mean_attack_angle = mean(attack_angle, na.rm = TRUE),
    total_hits = sum(is_hit, na.rm = TRUE),
    total_at_bats = sum(is_at_bat, na.rm = TRUE),
    mean_ba = total_hits / total_at_bats,
    total_on_base = sum(on_base, na.rm = TRUE),
    total_pa_obp = sum(is_pa_obp, na.rm = TRUE),
    mean_obp = total_on_base / total_pa_obp,
    total_bases = sum(total_bases, na.rm = TRUE),
    mean_slg = total_bases / total_at_bats,
    .groups = "drop"
  ) %>%
  mutate(
    mean_ops = mean_obp + mean_slg,
    ops_plus = 100 * (mean_obp / lgOBP + mean_slg / lgSLG - 1),
    mean_bat_speed = round(mean_bat_speed, 1),
    mean_swing_length = round(mean_swing_length, 1),
    mean_attack_angle = round(mean_attack_angle, 1),
    mean_ba = round(mean_ba, 3),
    mean_obp = round(mean_obp, 3),
    mean_slg = round(mean_slg, 3),
    mean_ops = round(mean_ops, 3),
    ops_plus = round(ops_plus, 0)
  )

table_vs_LHP <- performance_summary %>%
  filter(p_throws == "L") %>%
  select(batter_type, mean_bat_speed, mean_swing_length, mean_attack_angle, mean_ba, mean_obp, mean_slg, mean_ops, ops_plus) %>%
  rename(
    Batter_Type = batter_type,
    "Bat Speed (mph)" = mean_bat_speed,
    "Swing Length (ft)" = mean_swing_length,
    "Attack Angle (deg)" = mean_attack_angle,
    Batting_Avg = mean_ba,
    On_Base_Pct = mean_obp,
    Slugging_Pct = mean_slg,
    OPS = mean_ops
  )

table_vs_RHP <- performance_summary %>%
  filter(p_throws == "R") %>%
  select(batter_type, mean_bat_speed, mean_swing_length, mean_attack_angle, mean_ba, mean_obp, mean_slg, mean_ops, ops_plus) %>%
  rename(
    Batter_Type = batter_type,
    "Bat Speed (mph)" = mean_bat_speed,
    "Swing Length (ft)" = mean_swing_length,
    "Attack Angle (deg)" = mean_attack_angle,
    Batting_Avg = mean_ba,
    On_Base_Pct = mean_obp,
    Slugging_Pct = mean_slg,
    OPS = mean_ops
  )

# Generate Table A1: Descriptive statistics using summarytools
selected_data <- data[, c("bat_speed", "swing_length", "launch_angle", "launch_speed", "stand", "p_throws", "pitch_type")]
table_a1 <- dfSummary(selected_data, style = "grid", plain.ascii = FALSE, graph.magnif = 0.75, valid.col = TRUE, tmp.img.dir = "/tmp")

# Visualizations (3 figures for main body, 3 for appendix)
# Figure 1: Bat Speed vs. Launch Speed for Home Runs (main body)
hr_data <- data %>%
  filter(events == "home_run" & !is.na(bat_speed) & !is.na(launch_speed)) 

lm_model <- lm(launch_speed ~ bat_speed, data = hr_data)
intercept <- coef(lm_model)[1]
slope <- coef(lm_model)[2]
ref_bat_speed <- 75
adj_intercept <- intercept + slope * ref_bat_speed
equation <- sprintf("LS = %.1f + %.2f × (BS – %.0f)", adj_intercept, slope, ref_bat_speed)

swing_launch_plot <- ggplot(hr_data, aes(x = bat_speed, y = launch_speed)) +
  geom_point(alpha = 0.5, size = 1, color = "black") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  annotate("text", 
           x = min(hr_data$bat_speed, na.rm = TRUE) + 5, 
           y = max(hr_data$launch_speed, na.rm = TRUE) - 5, 
           label = equation, 
           color = "red", 
           hjust = 0, 
           size = 4) +
  labs(
    title = "Relationship of Swing Speed and Launch Speed\n2024 Home Runs",
    x = "Swing Speed (mph)",
    y = "Launch Speed (mph)",
    caption = "Note: Data includes all home runs with non-missing bat speed and launch speed."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# Figure 2: PCA Scree Plot (main body)
scree_plot <- fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50)) +
  labs(
    title = "PCA Scree Plot",
    caption = "Note: Shows variance explained by each principal component."
  ) +
  theme_minimal(base_size = 12)

# Figure 3: Batter Clusters in PCA Space (main body)
cluster_plot <- fviz_cluster(
  list(data = pca_scores, cluster = batter_pca$cluster),
  geom = "point",
  ellipse.type = "convex",
  palette = "jco",
  pointsize = 1.5,
  alpha = 0.5
) +
  labs(
    title = "Batter Clusters in PCA Space",
    caption = "Note: Clusters based on PCA scores (PC1 and PC2)."
  ) +
  theme_minimal(base_size = 12)

# Figure A1: Bat Speed vs. Swing Length (appendix)
swing_length_plot <- ggplot(data, aes(x = bat_speed, y = swing_length)) +
  geom_point(alpha = 0.3, size = 1.5, color = "forestgreen") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Bat Speed vs. Swing Length",
    x = "Bat Speed (mph)",
    y = "Swing Length (ft)",
    caption = "Note: Includes swing-related pitches with non-missing bat_speed and swing_length."
  ) +
  theme(legend.position = "none")

# Figure A2: Swing Length by Pitch Count (appendix)
swing_length_means <- data %>%
  mutate(count = paste(balls, strikes, sep = "-")) %>%
  filter(count %in% c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2")) %>%
  group_by(count) %>%
  summarise(mean_swing_length = mean(swing_length, na.rm = TRUE), .groups = "drop")

swing_length_count_plot <- ggplot(swing_length_means, aes(x = count, y = mean_swing_length)) +
  geom_point(size = 3, color = "black") +
  labs(
    title = "Swing Length by Count",
    x = "Count",
    y = "Mean Swing Length"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Figure A3: Pitch Type by Pitcher Handedness (appendix)
pitch_hand_plot <- ggplot(data, aes(x = pitch_type, fill = p_throws)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_brewer(palette = "Set2", name = "Pitcher Handedness") +
  labs(
    title = "Pitch Type by Pitcher Handedness",
    x = "Pitch Type",
    y = "Count",
    caption = "Note: Includes all swing-related pitches."
  ) +
  theme(legend.position = "bottom")

# Save plots
ggsave("swing_launch_plot.png", swing_launch_plot, width = 6, height = 4, dpi = 300)
ggsave("scree_plot.png", scree_plot, width = 6, height = 4, dpi = 300)
ggsave("cluster_plot.png", cluster_plot, width = 6, height = 4, dpi = 300)
ggsave("swing_length_plot.png", swing_length_plot, width = 6, height = 4, dpi = 300)
ggsave("swing_length_count_plot.png", swing_length_count_plot, width = 8, height = 5, dpi = 300)
ggsave("pitch_hand_plot.png", pitch_hand_plot, width = 8, height = 5, dpi = 300)

# Print tables for paper
cat("\nTable 1: Platoon Summary by Cluster for Four-Seam Fastballs (FF)\n")
print(platoon_summary_ff)
cat("\nTable 2: Batter Performance vs LHP\n")
print(table_vs_LHP)
cat("\nTable 3: Batter Performance vs RHP\n")
print(table_vs_RHP)
cat("\nTable A1: Descriptive Statistics of Key Variables\n")
print(table_a1, method = "render")
cat("\nTable A2: Full Platoon Summary by Cluster, Pitch Type, and Pitcher Handedness\n")
print(platoon_summary_full)
cat("\nMonte Carlo Simulation: Average Runs per Game\n")
print(mean_runs)