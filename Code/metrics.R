# Load required libraries
library(dplyr)

# Step 1: Read the Statcast data
# Replace the file path with your local path to the downloaded CSV file
data <- read.csv("Final Project/data/statcast_pitch_swing_data_20240402_20241030_with_arm_angle.csv")

# Step 2: Filter for relevant game types (Regular Season and Playoffs)
data <- data %>%
  filter(game_type %in% c("R", "F", "D", "L", "W"))

# Step 3: Classify batters into RHB, LHB, and Switch Hitters
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

# Join batter_type back to the main dataset
data <- data %>%
  left_join(select(batter_handedness, batter, batter_type), by = "batter") %>%
  filter(batter_type != "Unknown")  # Remove any unknown classifications

# Step 4: Aggregate to plate appearance level (take the last pitch of each PA)
data_pa <- data %>%
  group_by(game_pk, at_bat_number, batter) %>%
  slice(which.max(pitch_number)) %>%  # Take the last pitch of each PA
  ungroup()

# Step 5: Calculate plate appearance outcomes for BA, OBP, SLG
data_pa <- data_pa %>%
  mutate(
    # Identify hits
    is_hit = events %in% c("single", "double", "triple", "home_run"),
    # Identify times on base (for OBP)
    on_base = events %in% c("single", "double", "triple", "home_run", "walk", "hit_by_pitch"),
    # Identify at-bats (exclude walks, HBP, sac flies, sac bunts)
    is_at_bat = !events %in% c("walk", "hit_by_pitch", "sac_fly", "sac_bunt", NA),
    # Calculate total bases for SLG
    total_bases = case_when(
      events == "single" ~ 1,
      events == "double" ~ 2,
      events == "triple" ~ 3,
      events == "home_run" ~ 4,
      TRUE ~ 0
    ),
    # Identify plate appearances for OBP denominator
    is_pa_obp = !events %in% c("sac_bunt", NA)  # Exclude sac bunts
  )

# Step 6: Aggregate by batter_type and p_throws to calculate metrics
performance_summary <- data_pa %>%
  group_by(batter_type, p_throws) %>%
  summarise(
    # Batting Average
    total_hits = sum(is_hit, na.rm = TRUE),
    total_at_bats = sum(is_at_bat, na.rm = TRUE),
    mean_ba = total_hits / total_at_bats,
    # On-Base Percentage
    total_on_base = sum(on_base, na.rm = TRUE),
    total_pa_obp = sum(is_pa_obp, na.rm = TRUE),
    mean_obp = total_on_base / total_pa_obp,
    # Slugging Percentage
    total_bases = sum(total_bases, na.rm = TRUE),
    mean_slg = total_bases / total_at_bats,
    .groups = "drop"
  ) %>%
  mutate(
    # OPS
    mean_ops = mean_obp + mean_slg,
    # OPS+ using Baseball Reference formula (lgOBP = 0.312, lgSLG = 0.399)
    ops_plus = 100 * (mean_obp / 0.312 + mean_slg / 0.399 - 1),
    # Round for readability
    mean_ba = round(mean_ba, 3),
    mean_obp = round(mean_obp, 3),
    mean_slg = round(mean_slg, 3),
    mean_ops = round(mean_ops, 3),
    ops_plus = round(ops_plus, 0)
  )

# Step 7: Split into two tables: vs LHP and vs RHP
table_vs_LHP <- performance_summary %>%
  filter(p_throws == "L") %>%
  select(batter_type, mean_ba, mean_obp, mean_slg, mean_ops, ops_plus) %>%
  rename(
    Batter_Type = batter_type,
    Batting_Avg = mean_ba,
    On_Base_Pct = mean_obp,
    Slugging_Pct = mean_slg,
    OPS = mean_ops
  )

table_vs_RHP <- performance_summary %>%
  filter(p_throws == "R") %>%
  select(batter_type, mean_ba, mean_obp, mean_slg, mean_ops, ops_plus) %>%
  rename(
    Batter_Type = batter_type,
    Batting_Avg = mean_ba,
    On_Base_Pct = mean_obp,
    Slugging_Pct = mean_slg,
    OPS = mean_ops
  )

# Step 8: Print the tables
cat("\nBatter Performance vs LHP\n")
print(table_vs_LHP)
cat("\nBatter Performance vs RHP\n")
print(table_vs_RHP)

# Step 9: Validate league-wide averages
league_averages <- data_pa %>%
  summarise(
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
    mean_ba = round(mean_ba, 3),
    mean_obp = round(mean_obp, 3),
    mean_slg = round(mean_slg, 3),
    mean_ops = round(mean_ops, 3)
  )

cat("\nLeague-Wide Averages (for validation)\n")
print(league_averages)