# --- Loat the packages ---
library(tidyverse)
library(infer)
library(tibble)
library(ggplot2)
library(readr)
library(lmerTest)
library(performance)
library(broom.mixed)
library(knitr)

# --- Read the data ---
exp_data <- read_csv(url("https://drive.google.com/uc?export=download&id=1estf46syRbfG2K8JfW9myfTq2qDp0Ums"))

# --- Data processing ---
df <- exp_data |>
  select('id', 'treatment', 'hotel1', 'hotel2', 'hotel3') |>
  pivot_longer(
    cols = c('hotel1', 'hotel2', 'hotel3'),
    names_to = 'hotel',
    values_to = 'score'
  ) |>
  mutate(
    scenario = case_when(
      hotel == "hotel1" ~ "negative",
      hotel == "hotel2" ~ "neutral",
      hotel == "hotel3" ~ "positive"
    )
  )

# Check the data
print(df)

# Check data completeness
table(df$id)

# --- Data visualization ---

# Boxplot
ggplot(df, aes(x = scenario, y = score)) +
  geom_boxplot() +
  labs(title = "Hotel Ratings by Scenario")

# Grouped boxplot by treatment 
ggplot(df, aes(x = scenario, y = score, fill = factor(treatment))) +
  geom_boxplot(position = position_dodge()) +
  labs(title = "Hotel Ratings by Scenario", fill = "Treatment") 

# --- Model---

# Set the baseline
df$treatment <- factor(df$treatment)
df$scenario  <- factor(df$scenario,
                       levels = c("neutral", "positive", "negative"))

# Linear Mixed-Effects Model
m1 <- lmer(
  score ~ treatment * scenario + (1 | id),
  data = df
)

# Summary
summary(m1)

# Fixed effect
tidy_m1 <- tidy(m1, effects = "fixed")
print(tidy_m1)

# R-squared 
r2_val <- r2(m1)
print(r2_val)

# --- Bootstrap (Neutral hotel scene) ---

df_neutral <- df %>% filter(scenario == "neutral")

ate_boots <- df_neutral |>
  rep_slice_sample(prop = 1, reps = 1000, replace = TRUE) |>
  group_by(replicate, treatment) |>
  summarize(mean_score = mean(score, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(
    names_from = treatment,
    values_from = mean_score
  ) |>
  mutate(ate = `1` - `0`)

# Calculate 95% Confidence Interval
boot_ci <- quantile(ate_boots$ate, c(0.025, 0.975))
print(boot_ci)

# Create ATE distribution
ggplot(ate_boots, aes(x = ate)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white") +
  geom_vline(xintercept = unlist(boot_ci), linewidth = 1) 






