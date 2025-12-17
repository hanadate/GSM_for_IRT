library(tidyverse)
library(difR)
library(lme4)
library(foreach)
library(doParallel)
library(rpact)
library(stargazer)
library(psych)
library(ggplot2)

# === Load Data (Ensure these files are in your working directory) ===
# If you do not have the files loaded, the script will stop here.
dat_resp <- read_csv("resp_wide.csv")
dat_resp[is.na(dat_resp)] <- 0
dat_prof <- read_csv("dat_prof.csv")

# sc3: membership of school that study disabilities
dat <- dat_resp %>% 
  mutate(sc3 = dat_prof$sc3) %>% 
  mutate(group=ifelse(sc3==1, "focal", "ref")) %>% 
  select(no, group, starts_with("q")) # Ensure 'no' column is selected for random effects

# ============================
# Simulation / Bootstrapping
# ============================

numCores <- parallel::detectCores()
cl <- makeCluster(max(1, numCores - 4)) # Safety check for cores
registerDoParallel(cl)

niter <- 1000 # Adjust this if it takes too long

res <- foreach(i=1:niter, .combine="rbind", 
               .packages=c("dplyr","tidyr","foreach","lme4")) %dopar% { 
                 set.seed(i)
                 
                 # Shuffle data order (simulate sequential arrival)
                 dat_sim <- dat %>% 
                   dplyr::mutate(stage = sample(rep(1:5, length.out = n())))
                 
                 # Convert to long format
                 dat_longer <- tidyr::pivot_longer(dat_sim, cols=starts_with("q"), names_to="item", values_to="resp")
                 
                 # Define DIF items (Targeting items 15 and 21)
                 items <- unique(dat_longer$item)
                 dif_items <- items[c(15,21)]
                 
                 # Create DIF indicator: 1 if Focal group AND item is a DIF item
                 dif <- with(dat_longer, factor(0+(group=="focal" & item %in% dif_items)))
                 dat_longer_all <- cbind(dat_longer, dif)
                 
                 # Sequential Analysis loop (Stage 1 to 5)
                 stage_res <- foreach(j=1:5, .combine="rbind") %do% {
                   # Filter data for current stage
                   dat_current <- dat_longer_all %>% 
                     dplyr::filter(stage <= j)
                   
                   # Run GLMM
                   # Note: nAGQ=0 is used for speed
                   model <- glmer(resp ~ -1 + item + dif + group + (1 | no),
                                  data=dat_current, family=binomial, nAGQ=0)
                   
                   coefs <- summary(model)$coefficients
                   
                   # Extract the row corresponding to the DIF effect (usually "dif1")
                   dif_row <- which(startsWith(rownames(coefs), "dif"))
                   
                   # Return Estimate, SE, and Z-value
                   data.frame(
                     stage = j,
                     estimate = coefs[dif_row, "Estimate"],
                     se = coefs[dif_row, "Std. Error"],
                     z = coefs[dif_row, "z value"]
                   )
                 }
                 
                 stage_res %>% mutate(nsim = i)
               }

stopCluster(cl)

# Save intermediate results
write_csv(res, "coefs_empirical.csv")

# ============================
# Analysis of Results
# ============================

# Define Critical Values (Boundary definitions)
pocock_5 <- getDesignGroupSequential(kMax=5, typeOfDesign="P", sided=2, alpha=0.05, beta=0.2)$criticalValues
of_5 <- getDesignGroupSequential(kMax=5, typeOfDesign="OF", sided=2, alpha=0.05, beta=0.2)$criticalValues

bounds_df <- data.frame(
  stage = 1:5,
  crit_pocock = pocock_5,
  crit_of = of_5
)

# Merge results with critical values
df_analysis <- res %>%
  left_join(bounds_df, by="stage") %>%
  mutate(
    sig_pocock = abs(z) > crit_pocock,
    sig_of = abs(z) > crit_of
  )

# Function to extract the result at the stopping stage
get_stopping_result <- function(data, method_col) {
  # Find first stage where significance is met
  stop_idx <- which(data[[method_col]])
  
  if(length(stop_idx) > 0) {
    # Stopped early
    idx <- stop_idx[1]
  } else {
    # Did not stop early, take final stage (5)
    idx <- 5 
  }
  return(data[data$stage == idx, ])
}

# Process each simulation iteration to get Fixed vs GSM estimates
comparison_list <- list()

# Split by simulation ID for processing
sim_split <- split(df_analysis, df_analysis$nsim)

comparison_df <- map_dfr(sim_split, function(sub_df) {
  # 1. Fixed Sample (Always Stage 5)
  fixed_res <- sub_df[sub_df$stage == 5, ]
  
  # 2. GSM - Pocock
  pocock_res <- get_stopping_result(sub_df, "sig_pocock")
  
  # 3. GSM - O'Brien-Fleming
  of_res <- get_stopping_result(sub_df, "sig_of")
  
  rbind(
    data.frame(nsim = sub_df$nsim[1], Method = "Fixed Sample", 
               Estimate = fixed_res$estimate, SE = fixed_res$se, Stage = fixed_res$stage),
    data.frame(nsim = sub_df$nsim[1], Method = "GSM (Pocock)", 
               Estimate = pocock_res$estimate, SE = pocock_res$se, Stage = pocock_res$stage),
    data.frame(nsim = sub_df$nsim[1], Method = "GSM (OF)", 
               Estimate = of_res$estimate, SE = of_res$se, Stage = of_res$stage)
  )
})

# ============================
# Visualization
# ============================

# Convert Method to factor for ordering
comparison_df$Method <- factor(comparison_df$Method, 
                               levels = c("Fixed Sample", "GSM (Pocock)", "GSM (OF)"))

# Plot 1: Comparison of DIF Coefficients (Estimates)
p1 <- ggplot(comparison_df, aes(x = Method, y = Estimate)) +
  geom_boxplot(width = 0.5) +
  theme_classic() +
  labs(title = "Comparison of DIF Coefficient Estimates",
       y = "DIF Coefficient (Log-Odds)",
       x = "") +
  theme(axis.text.x = element_text(color="black", size=10),
        axis.text.y = element_text(color="black"))

# Plot 2: Comparison of Standard Errors
p2 <- ggplot(comparison_df, aes(x = Method, y = SE)) +
  geom_boxplot(width = 0.5) +
  theme_classic() +
  labs(title = "Comparison of Standard Errors",
       y = "Standard Error",
       x = "") +
  theme(axis.text.x = element_text(color="black", size=10),
        axis.text.y = element_text(color="black"))

# Print plots
print(p1)
print(p2)

# Calculate summary statistics table
summary_stats <- comparison_df %>%
  group_by(Method) %>%
  summarise(
    Mean_Est = mean(Estimate, na.rm=TRUE),
    SD_Est = sd(Estimate, na.rm=TRUE),
    Mean_SE = mean(SE, na.rm=TRUE)
    # Avg_Stage = mean(Stage)
  )

print(as.data.frame(summary_stats))

# Stargazer output for the summary (Optional)
stargazer(as.matrix(summary_stats), digits=3, title="Summary Statistics by Method")
