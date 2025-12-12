# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the data
data <- read.csv("results.csv", stringsAsFactors = FALSE)

# Compute total goals
data <- data %>%
  mutate(total_goals = home_score + away_score,
         neutral = as.factor(neutral))  # Ensure neutral is factor

# 1.1 Main plot: Boxplot of total goals by neutral status
ggplot(data, aes(x = neutral, y = total_goals, fill = neutral)) +
  geom_boxplot() +
  labs(title = "Boxplot of Total Goals Scored by Neutral Venue Status",
       x = "Neutral Venue (TRUE/FALSE)",
       y = "Total Goals") +
  theme_minimal() +
  theme(legend.position = "none")

# Histogram of total goals for neutral and non-neutral
ggplot(data, aes(x = total_goals, fill = neutral)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Histogram of Total Goals by Neutral Venue Status",
       x = "Total Goals",
       y = "Frequency") +
  theme_minimal()

# 2. Analysis: Statistical test (two-sample t-test)
# Split data
neutral_goals <- data %>% filter(neutral == TRUE) %>% pull(total_goals)
non_neutral_goals <- data %>% filter(neutral == FALSE) %>% pull(total_goals)

# Perform t-test (Welch's t-test for unequal variances)
t_test_result <- t.test(neutral_goals, non_neutral_goals)

# Print result
print(t_test_result)
##Optional findings
library(ggplot2)
library(dplyr)
library(gridExtra)
library(patchwork)
# Read data
data <- read.csv("results.csv", stringsAsFactors = FALSE) %>%
  mutate(total_goals = home_score + away_score,
         neutral    = ifelse(neutral, "Neutral", "Home Advantage"),
         year       = as.numeric(substr(date, 1, 4))) %>%
  filter(!is.na(total_goals))

# 1. Violin plot + boxplot combined (best for comparing distributions)
p1 <- ggplot(data, aes(x = neutral, y = total_goals, fill = neutral)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.2, outlier.alpha = 0.4) +
  labs(title = "Distribution of Total Goals: Neutral vs Home Advantage",
       x = "Venue Type",
       y = "Total Goals per Match") +
  theme_minimal(base_size = 13) +
  scale_fill_manual(values = c("Neutral" = "#E69F00", "Home Advantage" = "#56B4E9")) +
  theme(legend.position = "none")

# 2. Density plot (smooth version of histogram)
p2 <- ggplot(data, aes(x = total_goals, color = neutral, fill = neutral)) +
  geom_density(alpha = 0.3) +
  labs(title = "Density of Total Goals by Venue Type",
       x = "Total Goals per Match",
       y = "Density",
       fill = "Venue Type", color = "Venue Type") +
  theme_minimal(base_size = 13) +
  xlim(0, 15)

# 3. Mean total goals over time (decades) – to see if the difference is stable historically
data_decade <- data %>%
  mutate(decade = floor(year / 10) * 10) %>%
  group_by(decade, neutral) %>%
  summarise(mean_goals = mean(total_goals, na.rm = TRUE), .groups = "drop")

p3 <- ggplot(data_decade, aes(x = decade, y = mean_goals, color = neutral, group = neutral)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Evolution of Average Goals per Match by Venue Type (1872–2025)",
       x = "Decade",
       y = "Average Total Goals",
       color = "Venue Type") +
  theme_minimal(base_size = 13)

# 4. Side-by-side bar chart of mean and median with error bars (95% CI)
summary_stats <- data %>%
  group_by(neutral) %>%
  summarise(mean_goals   = mean(total_goals),
            median_goals = median(total_goals),
            sd_goals     = sd(total_goals),
            n            = n(),
            se           = sd_goals / sqrt(n),
            lower        = mean_goals - 1.96 * se,
            upper        = mean_goals + 1.96 * se,
            .groups = "drop")

p4 <- ggplot(summary_stats, aes(x = neutral, y = mean_goals, fill = neutral)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 1) +
  geom_text(aes(label = round(mean_goals, 2)), vjust = -2, size = 5, fontface = "bold") +
  labs(title = "Mean Total Goals with 95% Confidence Interval",
       x = "Venue Type",
       y = "Mean Goals per Match") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")


# 5. Bonus: QQ-plot to check normality in both groups (useful before t-test)
p5 <- data %>%
  ggplot(aes(sample = total_goals, aes(color = neutral)) +
           stat_qq() +
           stat_qq_line() +
           facet_wrap(~neutral) +
           labs(title = "Q-Q Plots: Are Total Goals Normally Distributed?",
                color = "Venue Type") +
           theme_minimal())

# Display all plots (choose any combination you like)
p1
p2
p3
p4
p5


