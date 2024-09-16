install.packages("GGally")
library(tidyverse)
library(ggplot2)
library(cowplot)
library(GGally)
library(patchwork)

# Load and preprocess the dataset
data(iris)
df <- iris %>%
  mutate(Species = as.factor(Species))


# Summary statistics
summary_stats <- df %>%
  group_by(Species) %>%
  summarise(across(everything(), list(mean = mean, sd = sd), .names = "{col}_{fn}"))

print(summary_stats)

# Correlation matrix
correlation_matrix <- df %>%
  select(-Species) %>%
  cor()

# Pair plots with GGally
pair_plot <- ggpairs(df, aes(color = Species)) +
  theme_minimal()

# Advanced Visualizations
# Boxplot with jitter
boxplot_plot <- ggplot(df, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot(outlier.size = 2, outlier.colour = "red") +
  geom_jitter(width = 0.2, alpha = 0.6) +
  labs(title = "Boxplot of Sepal Length by Species", x = "Species", y = "Sepal Length") +
  theme_minimal()

# Density plots
density_plot <- ggplot(df, aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Sepal Length by Species", x = "Sepal Length") +
  theme_minimal()

# Feature engineering: Create a new feature - Sepal.Area
df <- df %>%
  mutate(Sepal.Area = Sepal.Length * Sepal.Width)

# Plot Sepal.Area vs Petal.Length
feature_plot <- ggplot(df, aes(x = Sepal.Area, y = Petal.Length, color = Species)) +
  geom_point() +
  labs(title = "Sepal Area vs Petal Length", x = "Sepal Area", y = "Petal Length") +
  theme_minimal()

# Arrange plots using patchwork
plot_grid(density_plot, boxplot_plot, feature_plot, ncol = 1)

# Save plots to files
ggsave("boxplot_plot.png", plot = boxplot_plot)
ggsave("density_plot.png", plot = density_plot)
ggsave("feature_plot.png", plot = feature_plot)
