library(tidyverse)
library(stargazer)

params_easy <- expand.grid(nitem=c(10,30), # input even number
                      mdif=c(0,0.5,1.0),
                      pdif=c(0.1,0.3),
                      position=c("easy")) %>% 
  mutate(ndif=nitem*pdif,
         param=row_number())
params_middle <- expand.grid(nitem=c(10,30), # input even number
                           mdif=c(0,0.5,1.0),
                           pdif=c(0.1,0.3),
                           position=c("middle")) %>% 
  mutate(ndif=nitem*pdif,
         param=row_number())
params_difficult <- expand.grid(nitem=c(10,30), # input even number
                           mdif=c(0,0.5,1.0),
                           pdif=c(0.1,0.3),
                           position=c("difficult")) %>% 
  mutate(ndif=nitem*pdif,
         param=row_number())


dif_power_easy <- read_csv("dif_power_easy.csv") %>% 
  inner_join(., params_easy, by=c("param"))
dif_power_middle <- read_csv("dif_power_middle.csv") %>% 
  inner_join(., params_middle, by=c("param"))
dif_power_difficult <- read_csv("dif_power_difficult.csv") %>% 
  inner_join(., params_difficult, by=c("param"))

dif_power_all <- 
  bind_rows(dif_power_easy, dif_power_middle, dif_power_difficult) %>% 
  select(position, pdif, mdif, nitem, `250`, `500`, `750`, `1000`)
dif_power_all %>% 
  as.matrix() %>% 
  stargazer()

#=== Type I error rate 
dif_power_all %>% 
  filter(mdif==0)

# ==============================================================================
# Visualization with ggplot2
# ==============================================================================

# 1. Reshape the data from wide format to long format
#    Columns "250", "500", "750", "1000" become the "sample_size" column.
plot_data <- dif_power_all %>%
  pivot_longer(
    cols = c(`250`, `500`, `750`, `1000`),
    names_to = "sample_size",
    values_to = "value"
  ) %>%
  mutate(
    # Convert sample_size to numeric for proper scaling on x-axis
    sample_size = as.numeric(sample_size),
    # Ensure mdif is treated as a factor for discrete coloring
    mdif = as.factor(mdif)
  )

# 2. Create the plot
#    - x-axis: sample_size
#    - y-axis: value (0.128, etc.)
#    - color: mdif
#    - facets: separate panels for position (rows) and nitem + pdif (columns)
ggplot(plot_data, aes(x = sample_size, y = value, color = mdif, group = mdif)) +
  geom_line() +   # Draw lines
  geom_point() +  # Add points for clarity
  facet_grid(position ~ nitem + pdif, labeller = label_both) + # Split plots
  scale_y_continuous(limits = c(0, 1)) + # Optional: Set y-axis limits (0 to 1)
  labs(
    title = "Power Analysis by Condition",
    x = "Sample Size",
    y = "Power",
    color = "mdif"
  ) +
  theme_bw() + # Use a clean white theme
  theme(
    strip.background = element_rect(fill = "lightgray"), # Style the facet labels
    legend.position = "bottom" # Move legend to the bottom
  )
