# ============================================================
# How Predictable Is March Madness?
# Connor Correnti, Mike Zeghibe, & Andrew Lynch | Spring 2026
# ============================================================

library(tidyverse)
library(ggplot2)
library(scales)

# ── Load Data ────────────────────────────────────────────────
df <- read_csv("Tournament_Matchups_Updated.csv") %>%
  filter(YEAR != 2026) %>%                          # exclude unplayed games
  mutate(
    ROUND_NAME = case_when(
      `CURRENT ROUND` == 64 ~ "First Round",
      `CURRENT ROUND` == 32 ~ "Round of 32",
      `CURRENT ROUND` == 16 ~ "Sweet 16",
      `CURRENT ROUND` ==  8 ~ "Elite Eight",
      `CURRENT ROUND` ==  4 ~ "Final Four",
      `CURRENT ROUND` ==  2 ~ "Championship"
    ),
    ROUND_NAME = factor(ROUND_NAME, levels = c(
      "First Round","Round of 32","Sweet 16",
      "Elite Eight","Final Four","Championship"))
  )

# ── Shared Theme ─────────────────────────────────────────────
mm_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "#f7f7f7", color = NA),
    panel.background = element_rect(fill = "#f7f7f7", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    axis.text  = element_text(color = "#1a1a1a")
  )

# ============================================================
# FIGURE 1 — Win Percentage by Seed
# ============================================================
seed_stats <- df %>%
  group_by(SEED) %>%
  summarise(win_pct = mean(WIN), .groups = "drop") %>%
  mutate(tier = case_when(
    SEED <= 4  ~ "Seeds 1–4 (Favorites)",
    SEED <= 9  ~ "Seeds 5–9 (Mid-tier)",
    TRUE       ~ "Seeds 10–16 (Underdogs)"
  ),
  tier = factor(tier, levels = c("Seeds 1–4 (Favorites)",
                                  "Seeds 5–9 (Mid-tier)",
                                  "Seeds 10–16 (Underdogs)")))

fig1 <- ggplot(seed_stats, aes(x = SEED, y = win_pct, fill = tier)) +
  geom_col(color = "white", linewidth = 0.5) +
  geom_text(aes(label = percent(win_pct, accuracy = 0.1)),
            vjust = -0.5, size = 3, fontface = "bold") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "#555555", alpha = 0.7) +
  scale_fill_manual(values = c("#1a6b3c", "#e07b00", "#c0392b")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.1)) +
  scale_x_continuous(breaks = 1:16) +
  labs(title = "Win Percentage by Seed (All Tournament Games, 2008–2025)",
       x = "Seed Number", y = "Win Percentage", fill = NULL) +
  mm_theme

ggsave("fig1_win_pct_by_seed.png", fig1, width = 12, height = 6, dpi = 180)

# ============================================================
# FIGURE 2 — Upset Rate by Round
# ============================================================
winners <- df %>% filter(WIN == 1)

upset_by_round <- winners %>%
  group_by(ROUND_NAME) %>%
  summarise(upset_rate = mean(UPSET), n = n(),
            upsets = sum(UPSET), .groups = "drop") %>%
  filter(!is.na(ROUND_NAME))

fig2 <- ggplot(upset_by_round, aes(x = ROUND_NAME, y = upset_rate)) +
  geom_col(fill = "#2c7bb6", color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(percent(upset_rate, accuracy = 0.1),
                               "\n(", upsets, "/", n, ")")),
            vjust = -0.3, size = 3, fontface = "bold") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.6)) +
  labs(title = "Upset Rate by Tournament Round (2008–2025)",
       x = "Tournament Round", y = "Upset Rate") +
  mm_theme +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("fig2_upset_by_round.png", fig2, width = 11, height = 6, dpi = 180)

# ============================================================
# FIGURE 3 — First Round Upset Rate by Seed Matchup
# ============================================================
fr <- df %>% filter(`CURRENT ROUND` == 64)

# Build game-level pairs
games <- fr %>%
  arrange(YEAR, `BY YEAR NO`) %>%
  group_by(YEAR, `CURRENT ROUND`) %>%
  mutate(game_id = ceiling(row_number() / 2)) %>%
  group_by(YEAR, game_id) %>%
  summarise(
    high_seed  = min(SEED),
    low_seed   = max(SEED),
    upset      = first(UPSET),
    .groups = "drop"
  )

matchup_stats <- games %>%
  group_by(high_seed, low_seed) %>%
  summarise(upset_rate = mean(upset), upsets = sum(upset),
            total = n(), .groups = "drop") %>%
  mutate(label = paste0(high_seed, " vs ", low_seed))

fig3 <- ggplot(matchup_stats,
               aes(x = upset_rate, y = reorder(label, -high_seed),
                   fill = upset_rate)) +
  geom_col(color = "white") +
  geom_text(aes(label = paste0(percent(upset_rate, accuracy = 0.1),
                               "  (", upsets, "/", total, ")")),
            hjust = -0.05, size = 3.2, fontface = "bold") +
  scale_fill_gradient2(low = "#1a9850", mid = "#fdae61", high = "#d73027",
                       midpoint = 0.3, guide = "colorbar") +
  scale_x_continuous(labels = percent_format(), limits = c(0, 0.65)) +
  labs(title = "First Round Upset Rate by Seed Matchup (2008–2025)",
       x = "Upset Rate", y = "Seed Matchup", fill = "Upset Rate") +
  mm_theme

ggsave("fig3_upset_heatmap_matchup.png", fig3, width = 10, height = 7, dpi = 180)

# ============================================================
# FIGURE 4 — Annual Upset Rate Over Time
# ============================================================
annual <- winners %>%
  group_by(YEAR) %>%
  summarise(upset_rate = mean(UPSET), upsets = sum(UPSET),
            total = n(), .groups = "drop")

lm_fit <- lm(upset_rate ~ YEAR, data = annual)
annual$trend <- predict(lm_fit)

fig4 <- ggplot(annual, aes(x = YEAR, y = upset_rate)) +
  geom_area(fill = "#2c7bb6", alpha = 0.15) +
  geom_line(color = "#2c7bb6", linewidth = 2) +
  geom_point(color = "#2c7bb6", size = 3) +
  geom_line(aes(y = trend), color = "#c0392b",
            linewidth = 1.8, linetype = "dashed") +
  # Annotate notable years
  annotate("text", x = 2018, y = 0.42,
           label = "UMBC: 1st 16-seed win", size = 2.8, color = "#555") +
  annotate("text", x = 2023, y = 0.38,
           label = "FDU upsets Purdue", size = 2.8, color = "#555") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.55)) +
  scale_x_continuous(breaks = seq(2008, 2025, 2)) +
  labs(title = "Annual Upset Rate Over Time (2008–2025)",
       x = "Year", y = "Upset Rate",
       subtitle = paste0("Trend slope: ",
                         round(coef(lm_fit)[2]*100, 2), "%/yr")) +
  mm_theme

ggsave("fig4_upset_rate_over_time.png", fig4, width = 13, height = 6, dpi = 180)

# ============================================================
# FIGURE 5 — Bracket Chaos: How Deep Each Seed Advances
# ============================================================
team_depth <- df %>%
  group_by(YEAR, `TEAM NO`, SEED) %>%
  summarise(best_round = min(ROUND), .groups = "drop") %>%
  mutate(depth = case_when(
    best_round == 1  ~ "Champion",
    best_round == 2  ~ "Runner-up",
    best_round == 4  ~ "Final Four",
    best_round == 8  ~ "Elite Eight",
    best_round == 16 ~ "Sweet 16",
    best_round == 32 ~ "Round of 32",
    TRUE             ~ "First Round Exit"
  ),
  depth = factor(depth, levels = c("First Round Exit","Round of 32","Sweet 16",
                                    "Elite Eight","Final Four","Runner-up","Champion")))

depth_pct <- team_depth %>%
  count(SEED, depth) %>%
  group_by(SEED) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

fig5 <- ggplot(depth_pct, aes(x = SEED, y = pct, fill = depth)) +
  geom_col(position = "stack", color = "white", linewidth = 0.4) +
  scale_fill_manual(values = c("#d73027","#f46d43","#fdae61","#fee08b",
                                "#66bd63","#1a9850","#006837")) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = 1:16) +
  labs(title = "How Deep Each Seed Advances (2008–2025)",
       x = "Seed Number", y = "% of Tournament Appearances",
       fill = "Deepest Round Reached") +
  mm_theme +
  theme(legend.position = "right")

ggsave("fig5_bracket_chaos.png", fig5, width = 14, height = 7, dpi = 180)

message("All 5 figures saved!")
