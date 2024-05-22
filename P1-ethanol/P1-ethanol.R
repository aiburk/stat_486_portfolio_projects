# Author:  Aidan Burk
# Date:    2024-04-28
# Purpose: Portfolio 1 - Ethanol Assignment rmd file
#-------------------------------------------------------------------------------

# Load packages
library("tidyverse")
library("knitr")
library("ggResidpanel")

# Read & clean data
gas_mileage <- read.csv('gas_mileage_data.csv') |>
  drop_na(c(mpg, ethanol)) |> # remove rows with NA mpg/ethanol
  mutate(
    ethanol = ifelse(ethanol == 10,
                     yes = '10 %',
                     no  = '0 %') # rewrite 0/10 values in ethanol as Yes/No
  )

s <- gas_mileage |>
  group_by(ethanol) |>
  summarize(Obervations    = n(),
            `Mean mpg` = mean(mpg),
            `Sd mpg`   = sd(mpg),
            .groups = 'drop')

# Create HTML table from summary statistics}
s |>
  rename(Ethanol = ethanol) |>
  knitr::kable(
    caption = "Summary of miles per gallon for gas with no ethanol & gas with 10% ethanol",
    # label   = "summary-statistics",
    align   = c("rlll")) # Column alignment: [r]ight or [l]eft

# Create exploratory scatterplot
ggplot(gas_mileage,
       aes(x = ethanol, 
           y = mpg)) + 
  geom_jitter(height = 0,
              width  = 0.1,
              color = '#101020',
              size = 1) +
  labs(title = 'Scatterplot - Mileage by Type of Gas',
       x = 'Type of gas (Ethanol percentage)',
       y = 'Miles per gallon (mpg)') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Fit regression model of the logarithm of mpg on ethanol
m <- lm(log(mpg) ~ ethanol, 
         data = gas_mileage)

# Create panel of diagnostic plots
resid_panel(m,
            plots = c("resid",
                      "index",
                      "qq",
                      "cookd"))

# Create data frame with unique explanatory variable values
nd <- gas_mileage |>
  select(ethanol) |>
  unique()

# Predict
p <- bind_cols(
  nd,
  predict(m,
          newdata = nd,
          se.fit = TRUE)|>
      as.data.frame() |>
      
      # Manually construct confidence intervals
      mutate(
        lwr = fit - qnorm(0.975) * se.fit,
        upr = fit + qnorm(0.975) * se.fit,
        
        # Exponentiate to get to response scale
        mpg = exp(fit),
        lwr    = exp(lwr),
        upr    = exp(upr)
      ) 
  )

# Plot group estimates
ggplot(p,
       aes(x = ethanol,
           y = mpg,
           group = 1,
           ymin = lwr,
           ymax = upr)) + 
  geom_pointrange(color = '#101020') +
  geom_text(data = p,
            mapping = aes(x = ethanol, label = round(mpg, 2), y = mpg),
            vjust = -.8, hjust = -.2, size = 8/.pt) +
  geom_line() +
  labs(title = 'Estimated Mean Mileage (mpg) with 95% Confidence Intervals',
       subtitle = 'Log-linear Regression Model',
       x = 'Type of gas (Ethanol percentage)',
       y = 'Miles per gallon (mpg)') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

