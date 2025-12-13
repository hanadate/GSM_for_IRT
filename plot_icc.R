# Load necessary libraries
library(ggplot2)
library(gridExtra) # For side-by-side plotting

# -------------------------------------------------------------------
# 1. Define the Item Response Function (2PL Model)
#    P(theta) = 1 / (1 + exp(-a * (theta - b)))
# -------------------------------------------------------------------
irt_prob <- function(theta, a, b) {
  return(1 / (1 + exp(-a * (theta - b))))
}

# Define the range of Theta (Ability)
theta <- seq(-4, 4, length.out = 200)

# -------------------------------------------------------------------
# 2. Generate Data for Uniform DIF (Same slope 'a', different difficulty 'b')
# -------------------------------------------------------------------
# Reference Group: a = 1.2, b = -0.5
# Focal Group:     a = 1.2, b =  0.5
prob_ref_uni <- irt_prob(theta, a = 1.2, b = -0.5)
prob_foc_uni <- irt_prob(theta, a = 1.2, b = 0.5)

df_uni <- data.frame(
  Theta = rep(theta, 2),
  Probability = c(prob_ref_uni, prob_foc_uni),
  Group = factor(rep(c("Reference", "Focal"), each = length(theta)))
)

# Plot 1: Uniform DIF
p1 <- ggplot(df_uni, aes(x = Theta, y = Probability, linetype = Group)) +
  geom_line(size = 1, color = "black") +  # Black lines
  scale_linetype_manual(values = c("solid", "dashed")) + # Solid and Dashed lines
  labs(
    title = "Uniform DIF",
    subtitle = "Same Slope (a), Different Difficulty (b)",
    x = "Theta (Ability)",
    y = "Probability of Correct Response"
  ) +
  theme_classic() + # Clean theme
  theme(
    legend.position = c(0.85, 0.15), # Place legend inside the plot
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9)
  )

# -------------------------------------------------------------------
# 3. Generate Data for Non-Uniform DIF (Different slope 'a')
# -------------------------------------------------------------------
# Reference Group: a = 0.8, b = 0
# Focal Group:     a = 2.0, b = 0
prob_ref_non <- irt_prob(theta, a = 0.8, b = 0)
prob_foc_non <- irt_prob(theta, a = 2.0, b = 0)

df_non <- data.frame(
  Theta = rep(theta, 2),
  Probability = c(prob_ref_non, prob_foc_non),
  Group = factor(rep(c("Reference", "Focal"), each = length(theta)))
)

# Plot 2: Non-Uniform DIF
p2 <- ggplot(df_non, aes(x = Theta, y = Probability, linetype = Group)) +
  geom_line(size = 1, color = "black") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(
    title = "Non-Uniform DIF",
    subtitle = "Different Slope (a), Crossing Curves",
    x = "Theta (Ability)",
    y = "Probability of Correct Response"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.85, 0.15),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9)
  )

# -------------------------------------------------------------------
# 4. Combine and Display Plots
# -------------------------------------------------------------------
# Arrange the two plots side-by-side
grid.arrange(p1, p2, ncol = 2)


#=================================

# Load necessary libraries
# If you don't have these packages, install them with: install.packages(c("ggplot2", "patchwork"))
library(ggplot2)
library(patchwork)

# Define the Item Response Function (3PL model)
# P(theta) = c + (1 - c) / (1 + exp(-a * (theta - b)))
# 1PL: a is fixed (e.g., 1), c = 0
# 2PL: c = 0
# 3PL: all parameters vary
calc_prob <- function(theta, a, b, c) {
  return(c + (1 - c) / (1 + exp(-a * (theta - b))))
}

# Generate Theta values (Ability)
theta <- seq(-4, 4, length.out = 200)

# -------------------------------------------------------
# 1. 1PL Model (Rasch)
# Parameters: a = 1 (fixed), b = 0, c = 0
# -------------------------------------------------------
data_1pl <- data.frame(
  Theta = theta,
  Probability = calc_prob(theta, a = 1, b = 0, c = 0)
)

plot_1pl <- ggplot(data_1pl, aes(x = Theta, y = Probability)) +
  geom_line(size = 1, color = "black") +
  theme_classic() +
  labs(
    title = "1PL Model (Rasch)",
    subtitle = "a = 1, b = 0, c = 0",
    x = "Ability (Theta)",
    y = "Probability of Correct Response"
  ) +
  scale_y_continuous(limits = c(0, 1))

# -------------------------------------------------------
# 2. 2PL Model
# Parameters: a = 2 (steeper slope), b = 0, c = 0
# -------------------------------------------------------
data_2pl <- data.frame(
  Theta = theta,
  Probability = calc_prob(theta, a = 2, b = 0, c = 0)
)

plot_2pl <- ggplot(data_2pl, aes(x = Theta, y = Probability)) +
  geom_line(size = 1, color = "black") +
  theme_classic() +
  labs(
    title = "2PL Model",
    subtitle = "a = 2, b = 0, c = 0",
    x = "Ability (Theta)",
    y = NULL # Remove y-axis label to avoid clutter in grid
  ) +
  scale_y_continuous(limits = c(0, 1))

# -------------------------------------------------------
# 3. 3PL Model
# Parameters: a = 2, b = 0, c = 0.2 (guessing)
# -------------------------------------------------------
data_3pl <- data.frame(
  Theta = theta,
  Probability = calc_prob(theta, a = 2, b = 0, c = 0.2)
)

plot_3pl <- ggplot(data_3pl, aes(x = Theta, y = Probability)) +
  geom_line(size = 1, color = "black") +
  # Add a dashed line for the asymptote c
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "gray50") + 
  theme_classic() +
  labs(
    title = "3PL Model",
    subtitle = "a = 2, b = 0, c = 0.2",
    x = "Ability (Theta)",
    y = NULL
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  annotate("text", x = 2.5, y = 0.25, label = "Lower Asymptote (c)", size = 3)

# -------------------------------------------------------
# Combine Plots
# -------------------------------------------------------
combined_plot <- plot_1pl + plot_2pl + plot_3pl +
  plot_layout(ncol = 3)

# Display the plot
print(combined_plot)

# Save the plot (optional)
# ggsave("icc_models_bw.png", width = 12, height = 4)


