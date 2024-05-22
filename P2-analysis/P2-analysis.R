# Author:  Aidan Burk
# Date:    2024-05-06
# Purpose: Portfolio 2 - Analysis Assignment rmd file
#-------------------------------------------------------------------------------

# Load packages
library("tidyverse")
library("knitr")
library("ggResidpanel")
library("MASS")

# Read data
birthwt <- MASS::birthwt |>
  dplyr::select(bwt, lwt, smoke) |>
  mutate(
    smoke = ifelse(smoke == 1,
                     yes = 'Yes',
                     no  = 'No')
  )
row.names(birthwt) <- NULL

s <- apply(birthwt[c(1,2)],2,function(x) summary(x)) |> data.frame() |>
  t() |> as.data.frame()
s$Variable <- c('Birth weight', 'Last menstrual period weight')
s <- s[c(7,1:6)]
row.names(s) <- NULL

# Create HTML table from summary statistics}
s |>
  knitr::kable(
    caption = "Summary of numeric variables")

s1 <- birthwt |>
  group_by(smoke) |>
  summarize(Obervations    = n(),
            `Mean bwt` = mean(bwt),
            `Sd bwt`   = sd(bwt),
            .groups = 'drop')

# Create HTML table from summary statistics}
s1 |>
  rename(Smoke = smoke) |>
  knitr::kable(
    caption = "Summary of birth weight for mothers who didn't smoke during pregnancy & mothers who smoked during pregnancy",
    # label   = "summary-statistics",
    align   = c("rlll")) # Column alignment: [r]ight or [l]eft

# Create exploratory scatterplot
g <- ggplot(birthwt,
            aes(x = lwt, 
            y = bwt,
            color = smoke)) + 
  geom_point() +
  labs(title = 'Scatterplot - Birth Weight by Last Menstrual Period Weight\n & Smoking Status During Pregnancy',
       x = 'Mother’s weight at last menstrual period (pounds)',
       y = 'Birth Weight (g)',
       color = 'Smoking status') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("plum1", "plum4"))

g

# Fit regression model of the logarithm of mpg on ethanol
m <- lm(bwt ~ lwt + smoke, 
         data = birthwt)

# Obtain unique values of Diet
nd <- expand.grid(
  smoke = unique(birthwt$smoke),
  lwt = range(birthwt$lwt)
) 

# Construct obtain predictions
p <- nd |>
  mutate(
    bwt = predict(m, newdata = nd)
  )

# Plot
g +
  geom_line(data = p) 


# Create panel of diagnostic plots
resid_panel(m,
            plots = c("resid",
                      "index",
                      "qq",
                      "cookd"))

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
        upr = fit + qnorm(0.975) * se.fit
      ) 
  )

# Plot group estimates
ggplot(p,
       aes(x = lwt,
           y = fit,
           group = smoke,
           color = smoke,
           ymin = lwr,
           ymax = upr)) + 
  geom_pointrange() +
  geom_text(data = p,
            mapping = aes(x = lwt, label = round(fit, 2), y = fit),
            vjust = .8, hjust = -.2, size = 8/.pt) +
  geom_line() +
  labs(title = 'Estimated Mean Birth Weight (g) with 95% Confidence Intervals',
       subtitle = 'Multiple Regression Model',
       x = 'Mother’s weight at last menstrual period (pounds)',
       y = 'Birth Weight (g)',
       color = 'Smoking status') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("plum1", "plum4"))

