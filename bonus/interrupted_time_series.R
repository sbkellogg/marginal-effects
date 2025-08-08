## -----------------------------------------------------------------------------
library(marginaleffects)
library(tinyplot)
tinytheme("ipsum")
set.seed(48103)

# number of days
n <- 365

# intervention at day
intervention <- 200

# time index from 1 to 365
time <- c(1:n)

# treatment variable: 0 before the intervention and 1 after
treatment <- c(rep(0, intervention), rep(1, n - intervention))

# outcome equation
outcome <- 
  10 +  # pre-intervention intercept
  15 * time +  # pre-intervention slope (trend)
  20 * treatment +  # post-intervention intercept (shift of 10)
  5 * treatment * time + # steeper slope after the intervention
  rnorm(n, mean = 0, sd = 100) # noise

dat <- data.frame(outcome, time, treatment)


## -----------------------------------------------------------------------------
library(tinyplot)
tinyplot(outcome ~ time | treatment, type = "p", palette = "okabeito",
    data = transform(dat, treatment = factor(treatment)))


## -----------------------------------------------------------------------------
mod <- lm(outcome ~ time * treatment, data = dat)
summary(mod)


## -----------------------------------------------------------------------------
p0 <- predictions(mod, newdata = datagrid(time = 199, treatment = 0))
p1 <- predictions(mod, newdata = datagrid(time = 200, treatment = 1))

p0

p1


## -----------------------------------------------------------------------------
comparisons(mod, variables = "treatment", newdata = datagrid(time = 200))


## -----------------------------------------------------------------------------
avg_slopes(mod, variables = "time", by = "treatment")


## -----------------------------------------------------------------------------
library(ggplot2)
p <- predictions(mod, variables = c("time", "treatment"))
p <- subset(p, time > intervention | treatment == 0)
ggplot(p, aes(x = time, y = estimate, color = factor(treatment))) +
  geom_line() +
  labs(title = "Predicted outcome over time",
       x = "Time",
       y = "Outcome")

