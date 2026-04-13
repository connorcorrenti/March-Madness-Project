
# NCAA March Madness Tournament Predictability Analysis

# --- Load Libraries ---
library(tidyverse)
library(scales)

# --- Read & Prepare Data ---
raw <- read.csv("Tournament_Matchups_Updated.csv", stringsAsFactors = FALSE)

# The dataset has TWO rows per game (one per team).
# ROUND = team's seed-side position; CURRENT ROUND = tournament round stage.
# CURRENT ROUND values: 64 (Round of 64), 32 (Round of 32), 16 (Sweet 16),
#                        8 (Elite 8), 4 (Final Four), 2 (Championship)

# Map CURRENT ROUND numbers to readable labels
round_labels <- c(
  "64" = "Round of 64",
  "32" = "Round of 32",
  "16" = "Sweet 16",
  "8"  = "Elite 8",
  "4"  = "Final Four",
  "2"  = "Championship"
)

raw <- raw %>%
  mutate(
    ROUND_LABEL = round_labels[as.character(CURRENT.ROUND)],
    ROUND_LABEL = factor(ROUND_LABEL, levels = round_labels)
  )

# Build a game-level dataset: pair each game's winner and loser.
# Each game = two consecutive rows (sorted by BY.YEAR.NO descending within a year).
# The winning team has WIN == 1.
winners <- raw %>% filter(WIN == 1)
losers  <- raw %>% filter(WIN == 0)

# Pair them: they share the same YEAR and CURRENT.ROUND, matched by position.
# A safer approach: sort both by YEAR desc, BY.YEAR.NO desc, and pair row-by-row.
games <- winners %>%
  arrange(YEAR, CURRENT.ROUND, BY.YEAR.NO) %>%
  mutate(row_id = row_number()) %>%
  rename(
    W_TEAM = TEAM, W_SEED = SEED, W_SCORE = SCORE
  ) %>%
  select(row_id, YEAR, CURRENT.ROUND, ROUND_LABEL, W_TEAM, W_SEED, W_SCORE, UPSET)

losers_sorted <- losers %>%
  arrange(YEAR, CURRENT.ROUND, BY.YEAR.NO) %>%
  mutate(row_id = row_number()) %>%
  rename(
    L_TEAM = TEAM, L_SEED = SEED, L_SCORE = SCORE
  ) %>%
  select(row_id, L_TEAM, L_SEED, L_SCORE)

games <- games %>%
  left_join(losers_sorted, by = "row_id") %>%
  mutate(
    SEED_DIFF = abs(W_SEED - L_SEED),
    HIGHER_SEED = pmin(W_SEED, L_SEED),
    LOWER_SEED  = pmax(W_SEED, L_SEED),
    MATCHUP = paste0(HIGHER_SEED, " vs ", LOWER_SEED)
  )

cat("Dataset loaded:", nrow(games), "games from", min(games$YEAR), "to", max(games$YEAR), "\n\n")


# ============================================================================
# QUESTION 1: Which seed-vs-seed matchups produce the highest upset rates?
# ============================================================================

cat("=" %>% strrep(70), "\n")
cat("Q1: SEED VS. SEED MATCHUP UPSET RATES\n")
cat("=" %>% strrep(70), "\n\n")

matchup_upsets <- games %>%
  group_by(MATCHUP, HIGHER_SEED, LOWER_SEED) %>%
  summarise(
    Total_Games  = n(),
    Upsets       = sum(UPSET),
    Upset_Rate   = mean(UPSET),
    .groups = "drop"
  ) %>%
  filter(Total_Games >= 5) %>%   # Only matchups with enough data

arrange(desc(Upset_Rate))

print(matchup_upsets, n = 20)

# Plot: Top 15 most upset-prone matchups
top_matchups <- matchup_upsets %>% slice_max(Upset_Rate, n = 15)

ggplot(top_matchups, aes(x = reorder(MATCHUP, Upset_Rate), y = Upset_Rate)) +
  geom_col(fill = "#E63946", alpha = 0.85) +
  geom_text(aes(label = paste0(round(Upset_Rate * 100, 1), "%")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(
    title = "Top 15 Most Upset-Prone Seed Matchups",
    subtitle = "Minimum 5 games played | 1985–2024",
    x = "Matchup (Higher Seed vs Lower Seed)",
    y = "Upset Rate"
  ) +
  theme_minimal(base_size = 13)


# ============================================================================
# QUESTION 2: Which rounds are the most unpredictable?
# ============================================================================

cat("\n", "=" %>% strrep(70), "\n")
cat("Q2: UPSET RATES BY TOURNAMENT ROUND\n")
cat("=" %>% strrep(70), "\n\n")

round_upsets <- games %>%
  group_by(ROUND_LABEL) %>%
  summarise(
    Total_Games = n(),
    Upsets      = sum(UPSET),
    Upset_Rate  = mean(UPSET),
    .groups = "drop"
  ) %>%
  arrange(desc(Upset_Rate))

print(round_upsets)

ggplot(round_upsets, aes(x = ROUND_LABEL, y = Upset_Rate)) +
  geom_col(fill = "#457B9D", alpha = 0.85) +
  geom_text(aes(label = paste0(round(Upset_Rate * 100, 1), "%\n(",
                                Upsets, "/", Total_Games, ")")),
            vjust = -0.3, size = 3.5) +
  scale_y_continuous(labels = percent_format(), limits = c(0, max(round_upsets$Upset_Rate) * 1.25)) +
  labs(
    title = "Upset Rate by Tournament Round",
    subtitle = "1985–2024",
    x = "Round",
    y = "Upset Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# ============================================================================
# QUESTION 3: How have upset rates changed over time?
# ============================================================================

cat("\n", "=" %>% strrep(70), "\n")
cat("Q3: UPSET TRENDS OVER TIME\n")
cat("=" %>% strrep(70), "\n\n")

# --- By Year ---
yearly_upsets <- games %>%
  group_by(YEAR) %>%
  summarise(
    Total_Games = n(),
    Upsets      = sum(UPSET),
    Upset_Rate  = mean(UPSET),
    .groups = "drop"
  )

ggplot(yearly_upsets, aes(x = YEAR, y = Upset_Rate)) +
  geom_line(color = "#2A9D8F", linewidth = 0.8) +
  geom_point(color = "#2A9D8F", size = 2) +
  geom_smooth(method = "loess", se = TRUE, color = "#E76F51", linetype = "dashed") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Annual Upset Rate in March Madness (1985–2024)",
    subtitle = "Dashed line = LOESS trend",
    x = "Year",
    y = "Upset Rate"
  ) +
  theme_minimal(base_size = 13)

# --- By Decade ---
decade_upsets <- games %>%
  mutate(Decade = paste0(floor(YEAR / 10) * 10, "s")) %>%
  group_by(Decade) %>%
  summarise(
    Total_Games = n(),
    Upsets      = sum(UPSET),
    Upset_Rate  = mean(UPSET),
    .groups = "drop"
  )

print(decade_upsets)

ggplot(decade_upsets, aes(x = Decade, y = Upset_Rate)) +
  geom_col(fill = "#264653", alpha = 0.85) +
  geom_text(aes(label = paste0(round(Upset_Rate * 100, 1), "%")),
            vjust = -0.5, size = 4) +
  scale_y_continuous(labels = percent_format(), limits = c(0, max(decade_upsets$Upset_Rate) * 1.2)) +
  labs(
    title = "Upset Rate by Decade",
    x = "Decade",
    y = "Upset Rate"
  ) +
  theme_minimal(base_size = 13)


# ============================================================================
# QUESTION 4: Which seeds are most likely to reach later rounds?
# ============================================================================

cat("\n", "=" %>% strrep(70), "\n")
cat("Q4: DEEP TOURNAMENT RUNS BY SEED\n")
cat("=" %>% strrep(70), "\n\n")

# For each seed, count how many times it appeared as a winner in each round.
# Reaching a round = winning that round's game.
seed_advancement <- games %>%
  group_by(W_SEED, ROUND_LABEL) %>%
  summarise(Wins = n(), .groups = "drop") %>%
  rename(Seed = W_SEED)

# Focus on deep runs: Sweet 16, Elite 8, Final Four, Championship
deep_rounds <- c("Sweet 16", "Elite 8", "Final Four", "Championship")

deep_runs <- seed_advancement %>%
  filter(ROUND_LABEL %in% deep_rounds) %>%
  mutate(ROUND_LABEL = factor(ROUND_LABEL, levels = deep_rounds))

ggplot(deep_runs, aes(x = factor(Seed), y = Wins, fill = ROUND_LABEL)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2", name = "Round") +
  labs(
    title = "Deep Tournament Runs by Seed (Sweet 16 through Championship)",
    subtitle = "Number of wins at each stage | 1985–2024",
    x = "Seed",
    y = "Number of Wins"
  ) +
  theme_minimal(base_size = 13)

# Heatmap version: proportion of appearances
seed_round_pct <- games %>%
  group_by(W_SEED, ROUND_LABEL) %>%
  summarise(Wins = n(), .groups = "drop") %>%
  group_by(ROUND_LABEL) %>%
  mutate(Pct = Wins / sum(Wins)) %>%
  ungroup()

ggplot(seed_round_pct, aes(x = ROUND_LABEL, y = factor(W_SEED), fill = Pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(Pct * 100, 0), "%")), size = 3) +
  scale_fill_gradient(low = "#F1FAEE", high = "#E63946", labels = percent_format()) +
  labs(
    title = "Share of Round Wins by Seed",
    subtitle = "What percentage of wins at each stage belong to each seed?",
    x = "Round",
    y = "Seed",
    fill = "% of Wins"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# ============================================================================
# QUESTION 5: Has NIL (post-2021) influenced tournament outcomes?
# ============================================================================

cat("\n", "=" %>% strrep(70), "\n")
cat("Q5: NIL ERA IMPACT ANALYSIS (Post-2021)\n")
cat("=" %>% strrep(70), "\n\n")

# NIL was introduced July 1, 2021 — first tournament affected = 2022
games <- games %>%
  mutate(Era = ifelse(YEAR >= 2022, "NIL Era (2022+)", "Pre-NIL (1985–2021)"))

# Overall upset rate comparison
nil_comparison <- games %>%
  group_by(Era) %>%
  summarise(
    Total_Games = n(),
    Upsets      = sum(UPSET),
    Upset_Rate  = mean(UPSET),
    Avg_Seed_Diff_Winners = mean(W_SEED),
    .groups = "drop"
  )

print(nil_comparison)

ggplot(nil_comparison, aes(x = Era, y = Upset_Rate, fill = Era)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(Upset_Rate * 100, 1), "%\n(",
                                Upsets, " upsets / ", Total_Games, " games)")),
            vjust = -0.3, size = 4) +
  scale_fill_manual(values = c("Pre-NIL (1985–2021)" = "#457B9D",
                                "NIL Era (2022+)" = "#E63946")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, max(nil_comparison$Upset_Rate) * 1.3)) +
  labs(
    title = "Upset Rate: Pre-NIL vs. NIL Era",
    subtitle = "NIL policy effective July 2021; first affected tournament = 2022",
    x = "",
    y = "Upset Rate"
  ) +
  theme_minimal(base_size = 13)

# NIL impact by round
nil_by_round <- games %>%
  group_by(Era, ROUND_LABEL) %>%
  summarise(
    Upset_Rate = mean(UPSET),
    Games = n(),
    .groups = "drop"
  )

ggplot(nil_by_round, aes(x = ROUND_LABEL, y = Upset_Rate, fill = Era)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_fill_manual(values = c("Pre-NIL (1985–2021)" = "#457B9D",
                                "NIL Era (2022+)" = "#E63946")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Upset Rate by Round: Pre-NIL vs. NIL Era",
    x = "Round",
    y = "Upset Rate",
    fill = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Statistical test: is the difference significant?
cat("\nChi-squared test for upset rates Pre-NIL vs. NIL Era:\n")
contingency <- games %>%
  count(Era, UPSET) %>%
  pivot_wider(names_from = UPSET, values_from = n, values_fill = 0)
print(contingency)

chi_result <- chisq.test(matrix(c(contingency[[2]], contingency[[3]]), nrow = 2))
print(chi_result)

cat("\n\nAnalysis complete! Review the plots and tables above.\n")
