library(lme4)
library(tidyverse)
library(gridExtra) # For arranging plots

# ==============================================================================
# Simulation Settings
# ==============================================================================

# Number of simulations
n_sim <- 1000

# Number of persons (Reference and Focal groups)
n_ref <- 500
n_focal <- 500
n_total <- n_ref + n_focal

# Number of items (Short test length as requested)
n_items <- 10

# Item Difficulties (b parameters)
# Including extreme values (-3 and 3) as requested
item_difficulties <- seq(-3, 3, length.out = n_items)

# Identify the target item for DIF investigation
# We will target the most difficult item (difficulty = 3)
target_item_index <- 10 
target_item_name <- paste0("Item", target_item_index)

# True DIF size (Set to 0 to investigate Type I error / Bias under Null)
true_dif_size <- 0

# Group difference in latent trait mean (Impact)
mean_diff <- 0 

# ==============================================================================
# Simulation Loop
# ==============================================================================

# Initialize data frame to store estimates and standard errors
results_df <- data.frame(
  estimate = numeric(n_sim),
  std_error = numeric(n_sim)
)

set.seed(12345) # For reproducibility

cat("Starting simulation with nAGQ = 0...\n")

for (i in 1:n_sim) {
  
  if (i %% 100 == 0) cat("Iteration:", i, "/", n_sim, "\n")
  
  # -------------------------------------------------------
  # 1. Data Generation
  # -------------------------------------------------------
  
  # Generate Person Abilities (Theta)
  theta_ref <- rnorm(n_ref, mean = 0, sd = 1)
  theta_focal <- rnorm(n_focal, mean = mean_diff, sd = 1)
  thetas <- c(theta_ref, theta_focal)
  
  # Create Person ID and Group labels (0 = Reference, 1 = Focal)
  person_id <- 1:n_total
  group_vec <- c(rep(0, n_ref), rep(1, n_focal))
  
  # Generate Responses based on 1PL (Rasch) Model
  response_matrix <- matrix(NA, nrow = n_total, ncol = n_items)
  
  for (j in 1:n_items) {
    b <- item_difficulties[j]
    
    # Add DIF effect only for the target item in the focal group
    current_dif <- if (j == target_item_index) true_dif_size else 0
    
    # Calculate linear predictor
    eta <- thetas - b + (current_dif * group_vec)
    
    # Probability
    prob <- 1 / (1 + exp(-eta))
    
    # Bernoulli trial
    response_matrix[, j] <- rbinom(n_total, 1, prob)
  }
  
  # -------------------------------------------------------
  # 2. Data Formatting (Wide to Long)
  # -------------------------------------------------------
  
  df_wide <- data.frame(id = person_id, group = group_vec, response_matrix)
  colnames(df_wide)[3:(2 + n_items)] <- paste0("Item", 1:n_items)
  
  df_long <- df_wide %>%
    pivot_longer(cols = starts_with("Item"), names_to = "item", values_to = "response") %>%
    mutate(
      id = factor(id),
      item = factor(item),
      # Construct the DIF indicator (Covariate)
      dif_flag = ifelse(group == 1 & item == target_item_name, 1, 0)
    )
  
  # -------------------------------------------------------
  # 3. Model Fitting with glmer (nAGQ = 0)
  # -------------------------------------------------------
  
  suppressMessages({
    fit <- try(glmer(response ~ -1 + item + group + dif_flag + (1 | id),
                     data = df_long,
                     family = binomial,
                     nAGQ = 0), silent = TRUE)
  })
  
  if (inherits(fit, "try-error")) {
    results_df[i, ] <- NA 
  } else {
    # Extract coefficient and standard error
    summ <- summary(fit)
    coefs <- summ$coefficients
    
    if ("dif_flag" %in% rownames(coefs)) {
      results_df[i, "estimate"] <- coefs["dif_flag", "Estimate"]
      results_df[i, "std_error"] <- coefs["dif_flag", "Std. Error"]
    } else {
      results_df[i, ] <- NA
    }
  }
}

# Remove any failed convergences
results_clean <- na.omit(results_df)

# ==============================================================================
# Analysis
# ==============================================================================

# --- Analysis 1: Coefficient Bias ---
mean_est <- mean(results_clean$estimate)
bias_est <- mean_est - true_dif_size
mse_est <- mean((results_clean$estimate - true_dif_size)^2)

# --- Analysis 2: Standard Error Bias ---
# The "True" SE is approximated by the Empirical SD of the estimates
empirical_se <- sd(results_clean$estimate)
mean_est_se <- mean(results_clean$std_error)
bias_se <- mean_est_se - empirical_se
# Relative Bias of SE (%)
rel_bias_se <- (bias_se / empirical_se) * 100

cat("\nSimulation Results (nAGQ = 0):\n")
cat("------------------------------------------------\n")
cat("Coefficient Estimates:\n")
cat("  True DIF Value:       ", true_dif_size, "\n")
cat("  Mean Estimate:        ", round(mean_est, 4), "\n")
cat("  Bias (Coef):          ", round(bias_est, 4), "\n")
cat("  MSE:                  ", round(mse_est, 4), "\n")
cat("------------------------------------------------\n")
cat("Standard Error Estimates:\n")
cat("  Empirical SD (True):  ", round(empirical_se, 4), "\n")
cat("  Mean Estimated SE:    ", round(mean_est_se, 4), "\n")
cat("  Bias (SE):            ", round(bias_se, 4), "\n")
cat("  Relative Bias (SE) %: ", round(rel_bias_se, 2), "%\n")
cat("------------------------------------------------\n")

# ==============================================================================
# Plotting
# ==============================================================================

# Common theme for consistency
common_theme <- theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    axis.text = element_text(color = "black")
  )

# --- Plot 1: Distribution of Estimates ---
# We use a dummy data frame for the legend lines to ensure consistency
lines_est <- data.frame(
  xintercept = c(mean_est, true_dif_size),
  Type = c("Mean Estimate", "Reference (True Value)")
)

p1 <- ggplot(results_clean, aes(x = estimate)) +
  geom_histogram(binwidth = 0.05, fill = "white", color = "black") +
  # Use manual color/linetype mapping for legend consistency
  geom_vline(aes(xintercept = mean_est, linetype = "Mean", color = "Mean"), size = 1) +
  geom_vline(aes(xintercept = true_dif_size, linetype = "Reference", color = "Reference"), size = 1) +
  scale_linetype_manual(name = "Legend", 
                        values = c("Mean" = "solid", "Reference" = "dashed"),
                        labels = c("Mean Value", "Reference Value")) +
  scale_color_manual(name = "Legend", 
                     values = c("Mean" = "black", "Reference" = "black"),
                     labels = c("Mean Value", "Reference Value")) +
  labs(title = "Distribution of DIF Coefficients",
       subtitle = paste0("Mean Bias = ", round(bias_est, 3)),
       x = "Estimated Coefficient",
       y = "Frequency") +
  common_theme

# --- Plot 2: Distribution of Standard Errors ---
# Reference here is the Empirical SD
p2 <- ggplot(results_clean, aes(x = std_error)) +
  geom_histogram(binwidth = 0.01, fill = "white", color = "black") +
  geom_vline(aes(xintercept = mean_est_se, linetype = "Mean", color = "Mean"), size = 1) +
  geom_vline(aes(xintercept = empirical_se, linetype = "Reference", color = "Reference"), size = 1) +
  scale_linetype_manual(name = "Legend", 
                        values = c("Mean" = "solid", "Reference" = "dashed"),
                        labels = c("Mean Value", "Reference Value")) +
  scale_color_manual(name = "Legend", 
                     values = c("Mean" = "black", "Reference" = "black"),
                     labels = c("Mean Value", "Reference Value")) +
  labs(title = "Distribution of SE Estimates",
       subtitle = paste0("SE Bias = ", round(bias_se, 3)),
       x = "Estimated Standard Error",
       y = "Frequency") +
  common_theme

# Arrange and Print Plots
# Note: In PQL/nAGQ=0, SEs are often underestimated (Mean SE < Empirical SD)
grid.arrange(p1, p2, ncol = 2)
