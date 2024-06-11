# Author:  Aidan Burk
# Date:    2024-05-06
# Purpose: Portfolio 3 - Monte Carlo Assignment rmd file
#-------------------------------------------------------------------------------

# Load packages
library("tidyverse")
library("knitr")
library("ggResidpanel")
library("MASS")

# Function to calculate Monte Carlo expectation with standard error
mc_expectation <- function(x) {
  return(
    c(mean = mean(x),
      se   = sd(x)/sqrt(length(x)))
  )
}

# Create data frame from all combinations of n, p, and estimator type
combos <- expand.grid(
  n = c(seq(5, 30, 5)),
  p = c(seq(0.1, 0.9, 0.1)),
  type = c('frequentist', 'Bayesian')
)
# Loop through data frame & calculate MSE, coverage, & standard error variables
# for each combination
for (row in 1:nrow(combos)) {
  ### Frequentist approach
  if(combos$type[row] == 'frequentist') {
    # Set parameters for calculating MSE
    n     <- combos$n[row]
    p     <- combos$p[row]
    x     <- rbinom(1e4, size = n, prob = p) # Simulations
    p_hat <- x / n # Point estimator
    ### Mean squared error (MSE) of the MLE estimator
    combos$mse[row] <- mc_expectation((p_hat - p)^2)[1]
    ### Standard Error of MSE
    combos$mse_se[row] <- mc_expectation((p_hat-p)^2)[2]
    # Set parameters for calculating coverage
    alpha <- .05 # 95% confidence
    z     <- qnorm(1-alpha/2)
    d     <- data.frame(x)
    ci    <- d |> rowwise() |> 
      mutate(p_hat = x / n,
             lcl   = p_hat - z * sqrt(p_hat*(1-p_hat)/n),
             ucl   = p_hat + z * sqrt(p_hat*(1-p_hat)/n),
             Wald  = lcl < p & p < ucl) # Confidence Interval
    ### Coverage of the 95% asymptotic confidence interval
    combos$coverage[row] <- mean(ci$Wald)
    ### Standard Error of the coerage estimator
    combos$coverage_se[row] <- mc_expectation(ci$Wald)[2]
    }
  ### Bayesian approach
  else if (combos$type[row] == 'Bayesian') {
    # Set parameters for calculating MSE
    n     <- combos$n[row]
    p     <- combos$p[row]
    x     <- rbinom(1e4, size = n, prob = p) # Simulations
    p_hat <- (0.5 + x) / (1 + n) # Point estimator
    ### Mean squared error (MSE) of the Bayes estimator
    combos$mse[row] <- mc_expectation((p_hat - p)^2)[1]
    ### Standard error of MSE
    combos$mse_se[row] <- mc_expectation((p_hat - p)^2)[2]
    # Set parameters for calculating coverage
    alpha      <- .05 # 95% confidence
    lowerbound <- qbeta(alpha/2, 0.5 + x, 0.5 + n - x)
    upperbound <- qbeta(1-alpha/2, 0.5 + x, 0.5 + n - x)
    coverage   <- lowerbound < p & p < upperbound
    ### Coverage of a 95% Bayes interval
    combos$coverage[row] <- mean(coverage)
    ### Standard Error of the coverage estimator
    combos$coverage_se[row] <- mc_expectation(coverage)[2]
  }
}


## # Compute both endpoints of a Bayesian credible interval
## alpha <- 0.05 # for a 95% interval
## qbeta(c(alpha/2, 1-alpha/2), 0.5 + y, 0.5 + n - y)

# Create mse faceted scatterplots
mse_plot <- ggplot(combos,
                   aes(x = p,
                       y = mse,
                       color = type)) + 
  geom_point(position = position_jitter(width = 0.015),
             alpha = 0.7) +
  labs(title = 'Scatterplots - Probability of Success (p) by Mean Squared Error (MSE),\nEstimator Type, & Number of Trials (n)',
       x = 'p',
       y = 'MSE',
       color = 'Estimator') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("plum1", "plum4")) +
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  facet_wrap( ~ n, labeller = 'label_both')

mse_plot

# Create MSE standard error faceted scatterplot
mse_se_plot <- ggplot(combos,
                  aes(x = p,
                      y = mse_se,
                      color = type)) + 
  geom_point(position = position_jitter(width = 0.015),
             alpha = 0.7) +
  labs(title = 'Scatterplots - Probability of Success (p) by Monte Carlo Standard Error of MSE,\nEstimator Type, & Number of Trials (n)',
       x = 'p',
       y = 'Standard Error',
       color = 'Estimator') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("plum1", "plum4")) +
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  facet_wrap( ~ n, labeller = 'label_both')

mse_se_plot

# Create coverage faceted scatterplots
coverage_plot <- ggplot(combos,
                   aes(x = p,
                       y = coverage,
                       color = type)) + 
  geom_point(position = position_jitter(width = 0.015),
             alpha = 0.7) +
  labs(title = 'Scatterplots - Probability of Success (p) by Coverage,\nEstimator Type, & Number of Trials (n)',
       x = 'p',
       y = 'Coverage',
       color = 'Estimator') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("plum1", "plum4")) +
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  facet_wrap( ~ n, labeller = 'label_both')

coverage_plot

# Create coverage standard error faceted scatterplot
coverage_se_plot <- ggplot(combos,
                  aes(x = p,
                      y = coverage_se,
                      color = type)) + 
  geom_point(position = position_jitter(width = 0.015),
             alpha = 0.7) +
  labs(title = 'Scatterplots - Probability of Success (p) by Monte Carlo Standard Error of Coverage,\nEstimator Type, & Number of Trials (n)',
       x = 'p',
       y = 'Standard Error',
       color = 'Estimator') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("plum1", "plum4")) +
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) +
  facet_wrap( ~ n, labeller = 'label_both')

coverage_se_plot
