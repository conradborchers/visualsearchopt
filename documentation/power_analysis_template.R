library(tidyverse)
library(lme4)

simdat <- readRDS("dat_structure.rds")  # data / design matrix without target variable

# Random effect covariances (in our case obtained from previous study)
theta <- c(0.3, 0.05, -0.2, 0.11, 1.11) 

# Minimal relevant effect sizes, input as ORs in this case
# Intercept, main effects and interaction effect (ratio of odd ratios)
# (2 pairs, ratio of odds ratios)
ORs <- c(1, 1.15, 0.85, 1, 1.15/0.85)
beta <- ORs %>% log()

# simulate() expects all parameters in a list
params <- list(theta, beta)

# Simulations, save each model fit in /simulated-models

i <- dir("./simulated-models", pattern="*.rds") %>% length() # continue where you left off
end <- 1000   # planned number of simulations

while (i <= end) {
  
  # Simulate target variable
  simdat$target <- simulate(
    ~ (1|group:id) + (1|item) + pred1 + pred2*int,
    newparams = params,
    newdata = simdat,
    family = binomial
  )$sim_1
  
  # Fit model 
  m <- glmer(target ~ (1|group:id) + (1|item) + pred1 + pred2*int, 
             data = simdat, family = binomial, nAGQ=0)
  
  # Save output with timestamp
  time <- Sys.time() %>% str_replace_all(pattern=" |:", replacement = "-")
  fn <- paste0("./simulated-models/simulated-model-", time, ".rds")
  
  saveRDS(summary(m), fn)  # only saving summary() is more memory efficient
  
  # Progress
  i <- i+1
  cat("\014", i, "out of", end, "models simulated\n")
  
}