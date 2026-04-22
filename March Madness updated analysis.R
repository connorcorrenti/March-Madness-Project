# ============================================================
# How Predictable Is March Madness? — v2
# Connor Correnti & Mike Zeghibe | Spring 2026
# NEW: error bars, seed stat tests, logistic regression,
#      seeding importance by round, seed 1 vs 2 comparison
# ============================================================
install.packages("tidyverse")
install.packages("ggplot2")
installed.packages("scales")

library(tidyverse)
library(ggplot2)
library(scales)

setwd("C:/Users/ccorren2/OneDrive - Providence College/Data science Capstone CSC485/March Madness Projection folder")

# ── Load Data ────────────────────────────────────────────────
df <- read_csv("Tournament_Matchups_Updated.csv") %>%
  filter(YEAR != 2026) %>%
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
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "#555555"),
    axis.text     = element_text(color = "#1a1a1a")
  )


# ============================================================
# FIGURE 1 — Win Percentage by Seed  [NOW WITH ERROR BARS]
# ============================================================
seed_stats <- df %>%
  group_by(SEED) %>%
  summarise(
    n       = n(),
    win_pct = mean(WIN),
    se      = sd(WIN) / sqrt(n),
    ci_lo   = win_pct - 1.96 * se,
    ci_hi   = win_pct + 1.96 * se,
    .groups = "drop"
  ) %>%
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
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0.4, color = "#333333", linewidth = 0.7) +
  geom_text(aes(label = percent(win_pct, accuracy = 0.1)),
            vjust = -1.6, size = 2.8, fontface = "bold") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "#555555", alpha = 0.7) +
  scale_fill_manual(values = c("#1a6b3c", "#e07b00", "#c0392b")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.1)) +
  scale_x_continuous(breaks = 1:16) +
  labs(title = "Win Percentage by Seed (All Tournament Games, 2008–2025)",
       subtitle = "Error bars = 95% confidence intervals",
       x = "Seed Number", y = "Win Percentage", fill = NULL) +
  mm_theme

fig1

ggsave("fig1_win_pct_by_seed.png", fig1, width = 12, height = 6, dpi = 180)


# ============================================================
# FIGURE 2 — Upset Rate by Round  [NOW WITH ERROR BARS]
# ============================================================
winners <- df %>% filter(WIN == 1)

upset_by_round <- winners %>%
  group_by(ROUND_NAME) %>%
  summarise(
    n          = n(),
    upsets     = sum(UPSET),
    upset_rate = mean(UPSET),
    se         = sd(UPSET) / sqrt(n),
    ci_lo      = pmax(upset_rate - 1.96 * se, 0),
    ci_hi      = pmin(upset_rate + 1.96 * se, 1),
    .groups    = "drop"
  ) %>%
  filter(!is.na(ROUND_NAME))

fig2 <- ggplot(upset_by_round, aes(x = ROUND_NAME, y = upset_rate)) +
  geom_col(fill = "#2c7bb6", color = "white", linewidth = 0.5) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0.3, color = "#333333", linewidth = 0.7) +
  geom_text(aes(label = paste0(percent(upset_rate, accuracy = 0.1),
                               "\n(", upsets, "/", n, ")")),
            vjust = -1.5, size = 3, fontface = "bold") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.65)) +
  labs(title = "Upset Rate by Tournament Round (2008–2025)",
       subtitle = "Error bars = 95% confidence intervals",
       x = "Tournament Round", y = "Upset Rate") +
  mm_theme +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

fig2

ggsave("fig2_upset_by_round.png", fig2, width = 11, height = 6, dpi = 180)


# ============================================================
# FIGURE 3 — First Round Upset Heatmap  [NOW WITH ERROR BARS]
# ============================================================
fr <- df %>% filter(`CURRENT ROUND` == 64)

games <- fr %>%
  arrange(YEAR, `BY YEAR NO`) %>%
  group_by(YEAR, `CURRENT ROUND`) %>%
  mutate(game_id = ceiling(row_number() / 2)) %>%
  group_by(YEAR, game_id) %>%
  summarise(
    high_seed = min(SEED),
    low_seed  = max(SEED),
    upset     = first(UPSET),
    .groups   = "drop"
  )

matchup_stats <- games %>%
  group_by(high_seed, low_seed) %>%
  summarise(
    n          = n(),
    upsets     = sum(upset),
    upset_rate = mean(upset),
    se         = sd(upset) / sqrt(n),
    ci_lo      = pmax(upset_rate - 1.96 * se, 0),
    ci_hi      = pmin(upset_rate + 1.96 * se, 1),
    .groups    = "drop"
  ) %>%
  mutate(label = paste0(high_seed, " vs ", low_seed))

fig3 <- ggplot(matchup_stats,
               aes(x = upset_rate, y = reorder(label, -high_seed), fill = upset_rate)) +
  geom_col(color = "white") +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi),
                height = 0.35, color = "#333333", linewidth = 0.7) +
  geom_text(aes(label = paste0(percent(upset_rate, accuracy = 0.1),
                               "  (", upsets, "/", n, ")")),
            hjust = -0.1, size = 3.2, fontface = "bold") +
  scale_fill_gradient2(low = "#1a9850", mid = "#fdae61", high = "#d73027",
                       midpoint = 0.3, guide = "colorbar") +
  scale_x_continuous(labels = percent_format(), limits = c(0, 0.72)) +
  labs(title = "First Round Upset Rate by Seed Matchup (2008–2025)",
       subtitle = "Error bars = 95% confidence intervals",
       x = "Upset Rate", y = "Seed Matchup", fill = "Upset Rate") +
  mm_theme

fig3

ggsave("fig3_upset_heatmap_matchup.png", fig3, width = 10, height = 7, dpi = 180)


# ============================================================
# FIGURE 4 — Annual Upset Rate Over Time  (unchanged)
# ============================================================
annual <- winners %>%
  group_by(YEAR) %>%
  summarise(upset_rate = mean(UPSET), upsets = sum(UPSET),
            total = n(), .groups = "drop")

lm_fit  <- lm(upset_rate ~ YEAR, data = annual)
annual$trend <- predict(lm_fit)

fig4 <- ggplot(annual, aes(x = YEAR, y = upset_rate)) +
  geom_area(fill = "#2c7bb6", alpha = 0.15) +
  geom_line(color = "#2c7bb6", linewidth = 2) +
  geom_point(color = "#2c7bb6", size = 3) +
  geom_line(aes(y = trend), color = "#c0392b",
            linewidth = 1.8, linetype = "dashed") +
  annotate("text", x = 2018, y = 0.42,
           label = "UMBC: 1st 16-seed win", size = 2.8, color = "#555") +
  annotate("text", x = 2023, y = 0.38,
           label = "FDU upsets Purdue", size = 2.8, color = "#555") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.55)) +
  scale_x_continuous(breaks = seq(2008, 2025, 2)) +
  labs(title = "Annual Upset Rate Over Time (2008–2025)",
       x = "Year", y = "Upset Rate",
       subtitle = paste0("Trend slope: ",
                         round(coef(lm_fit)[2] * 100, 2), "%/yr")) +
  mm_theme

fig4

ggsave("fig4_upset_rate_over_time.png", fig4, width = 13, height = 6, dpi = 180)


# ============================================================
# FIGURE 5 — Bracket Chaos  (unchanged)
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

fig5

ggsave("fig5_bracket_chaos.png", fig5, width = 14, height = 7, dpi = 180)


# ============================================================
# NEW FIGURE 6 — Seeds 5–12: Win Rates with Statistical Tests
# Are seeds 5–12 actually different from each other?
# ============================================================

# Stats for each seed 5-12
mid_stats <- df %>%
  filter(SEED %in% 5:12) %>%
  group_by(SEED) %>%
  summarise(
    n       = n(),
    win_pct = mean(WIN),
    se      = sd(WIN) / sqrt(n),
    ci_lo   = win_pct - 1.96 * se,
    ci_hi   = win_pct + 1.96 * se,
    .groups = "drop"
  )

# One-way ANOVA to test if ANY seed differs significantly
anova_result <- aov(WIN ~ factor(SEED), data = df %>% filter(SEED %in% 5:12))
anova_p      <- summary(anova_result)[[1]][["Pr(>F)"]][1]
anova_label  <- paste0("One-way ANOVA: p = ", round(anova_p, 3),
                       ifelse(anova_p < 0.05, " (significant)", " (not significant)"))

fig6 <- ggplot(mid_stats, aes(x = factor(SEED), y = win_pct)) +
  geom_col(fill = "#e07b00", color = "white", linewidth = 0.5) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0.3, color = "#333333", linewidth = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "#c0392b",
             linewidth = 1, alpha = 0.8) +
  annotate("text", x = 7.5, y = 0.53, label = "50% (coin flip line)",
           size = 3, color = "#c0392b", fontface = "italic") +
  geom_text(aes(label = percent(win_pct, accuracy = 0.1)),
            vjust = -1.8, size = 3.2, fontface = "bold") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.75)) +
  labs(
    title    = "Win Rates for Seeds 5–12: Is There a Real Difference?",
    subtitle = paste0(anova_label,
                      "\n95% CI error bars — overlapping bars suggest no statistically significant difference"),
    x = "Seed Number", y = "Win Percentage"
  ) +
  mm_theme

fig6

ggsave("fig6_seeds5to12_anova.png", fig6, width = 10, height = 6, dpi = 180)

# Print pairwise t-test highlights (key matchups)
cat("\n=== Pairwise t-tests for key first-round matchups (seeds 5-12) ===\n")
pairs_to_test <- list(c(5,12), c(6,11), c(7,10), c(8,9))
for (pair in pairs_to_test) {
  a <- pair[1]; b <- pair[2]
  ga <- df$WIN[df$SEED == a]
  gb <- df$WIN[df$SEED == b]
  tt <- t.test(ga, gb)
  cat(sprintf("Seed %d vs Seed %d: diff=%.3f, p=%.3f %s\n",
              a, b,
              mean(ga) - mean(gb),
              tt$p.value,
              ifelse(tt$p.value < 0.05, "** SIGNIFICANT **", "(not significant)")))
}


# ============================================================
# NEW FIGURE 7 — Does Seed Predict Wins? Correlation by Round
# Does seeding stop mattering at a certain point?
# ============================================================

round_corr <- df %>%
  filter(!is.na(ROUND_NAME)) %>%
  group_by(ROUND_NAME) %>%
  summarise(
    n    = n(),
    corr = cor(SEED, WIN, method = "pearson"),
    # Fisher z for CI
    z    = 0.5 * log((1 + corr) / (1 - corr)),
    se_z = 1 / sqrt(n - 3),
    ci_lo = tanh(z - 1.96 * se_z),
    ci_hi = tanh(z + 1.96 * se_z),
    .groups = "drop"
  )

fig7 <- ggplot(round_corr, aes(x = ROUND_NAME, y = corr)) +
  geom_col(aes(fill = corr), color = "white", linewidth = 0.5) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0.3, color = "#333333", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", color = "#333333", linewidth = 0.5) +
  geom_text(aes(label = round(corr, 2)),
            vjust = ifelse(round_corr$corr < 0, 1.5, -1.3),
            size = 3.5, fontface = "bold") +
  scale_fill_gradient2(low = "#c0392b", mid = "#fdae61", high = "#1a6b3c",
                       midpoint = -0.25, guide = "none") +
  scale_y_continuous(limits = c(-0.75, 0.1)) +
  labs(
    title    = "How Important Is Seeding? Correlation Between Seed # and Winning",
    subtitle = "More negative = seed matters more | Near zero = seeding stops predicting outcomes\nError bars = 95% confidence intervals",
    x = "Tournament Round", y = "Pearson Correlation (Seed vs Win)"
  ) +
  mm_theme +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

fig7

ggsave("fig7_seeding_importance_by_round.png", fig7, width = 11, height = 6, dpi = 180)

cat("\n=== Correlation between seed and winning, by round ===\n")
print(round_corr %>% select(ROUND_NAME, n, corr, ci_lo, ci_hi))


# ============================================================
# NEW FIGURE 8 — Seed 1 vs Seed 2: Would Swapping Matter?
# Compare performance at each stage of the tournament
# ============================================================

s1_s2 <- df %>%
  filter(SEED %in% c(1, 2)) %>%
  group_by(SEED, ROUND_NAME) %>%
  summarise(
    n       = n(),
    win_pct = mean(WIN),
    se      = sd(WIN) / sqrt(n),
    ci_lo   = pmax(win_pct - 1.96 * se, 0),
    ci_hi   = pmin(win_pct + 1.96 * se, 1),
    .groups = "drop"
  ) %>%
  filter(!is.na(ROUND_NAME)) %>%
  mutate(seed_label = paste0("Seed ", SEED))

fig8 <- ggplot(s1_s2, aes(x = ROUND_NAME, y = win_pct,
                          fill = seed_label, group = seed_label)) +
  geom_col(position = position_dodge(width = 0.75),
           color = "white", linewidth = 0.5, width = 0.7) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                position = position_dodge(width = 0.75),
                width = 0.25, color = "#333333", linewidth = 0.7) +
  geom_text(aes(label = percent(win_pct, accuracy = 1)),
            position = position_dodge(width = 0.75),
            vjust = -1.6, size = 2.8, fontface = "bold") +
  scale_fill_manual(values = c("Seed 1" = "#1a6b3c", "Seed 2" = "#2c7bb6")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.0)) +
  labs(
    title    = "Seed 1 vs. Seed 2: Win Rate at Each Tournament Stage",
    subtitle = "Would swapping their seeds change outcomes? Gap widens in later rounds.\nError bars = 95% confidence intervals",
    x = "Tournament Round", y = "Win Percentage", fill = NULL
  ) +
  mm_theme +
  theme(axis.text.x = element_text(angle = 15, hjust = 1),
        legend.position = "top")

fig8

ggsave("fig8_seed1_vs_seed2.png", fig8, width = 11, height = 6, dpi = 180)

# Significance test: seed 1 vs seed 2 at Final Four and Championship
cat("\n=== Seed 1 vs Seed 2 t-tests at late rounds ===\n")
for (rval in c(4, 2)) {
  rname <- ifelse(rval == 4, "Final Four", "Championship")
  s1 <- df$WIN[df$SEED == 1 & df$`CURRENT ROUND` == rval]
  s2 <- df$WIN[df$SEED == 2 & df$`CURRENT ROUND` == rval]
  if (length(s1) > 1 & length(s2) > 1) {
    tt <- t.test(s1, s2)
    cat(sprintf("%s — Seed 1: %.1f%%, Seed 2: %.1f%%, p=%.3f %s\n",
                rname,
                mean(s1)*100, mean(s2)*100, tt$p.value,
                ifelse(tt$p.value < 0.05, "** SIGNIFICANT **", "(not significant)")))
  }
}

message("\nAll 8 figures saved!")