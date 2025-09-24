# https://ulrich-schroeders.github.io/IRT-sample-size/
# Clear work space and load necessary packages
rm(list = ls())
library(tidyverse)   # General data handling and manipulation
library(mirt)        # Item response theory modeling

# Set the seed for random number generation to ensure reproducibility
set.seed(2024)

# Number of items
n_items <- 30

# Discrimination parameters are randomly drawn from a normal distribution
#   with a mean of 1 and a standard deviation of 0.1 to result in parameters
#   that closely conform to the 1PL while still exhibiting some misfit, that
#   is the discriminations vary slightly around 1.
a_i <- rnorm(n_items, 1, 0.1)

# Difficulty parameters are equally spaced between -2 and 2 to cover the
#   expected difficulty range of the latent proficiency distribution.
b_i <- seq(-2, 2, length = n_items)


# Simulate dichotomous item responses for n respondents to all items
# - 'a' denotes the item discriminations
# - 'b' denotes the item difficulties
# - 'n' denotes the sample size

generate_dich_data <- function(a, b, n) {
  resp <-
    mirt::simdata(a = a, d = -a*b, N = n, itemtype = "dich") %>% 
    as.data.frame()
  
  return(resp)
}

# Induce missingness to the complete simulated data set
# - 'resp' denotes the complete data set

data_link_design <- function(resp) {
  n <- nrow(resp)
  n_items <- ncol(resp)
  
  # Generate an indicator of the administered test version for each respondent
  #   assuming that about half of the sample receives each test version
  resp$version <- sample(c(1, 2), n, replace = TRUE)
  
  # Item responses not included in a test version are set to missing 
  #   depending on the generated indicator of the administered test version.
  resp[resp$version == 1, setdiff(seq(1, n_items), c(seq(1, n_items, 2), 13:18))] <- NA
  resp[resp$version == 2, setdiff(seq(1, n_items), c(seq(2, n_items, 2), 13:18))] <- NA
  resp <- subset(resp, select = -c(version))
  
  return(resp)
}




