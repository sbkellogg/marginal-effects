## -----------------------------------------------------------------------------
#| cache: false
#| echo: false
#| warning: false
source("code/load.R")


## -----------------------------------------------------------------------------
library(marginaleffects)
library(ggplot2)
library(patchwork)
dat = get_dataset("interaction_01")
head(dat)


## -----------------------------------------------------------------------------
mod = glm(Y ~ X + M + X:M, data = dat, family = binomial)


## -----------------------------------------------------------------------------
mod = glm(Y ~ X * M, data = dat, family = binomial)


## -----------------------------------------------------------------------------
summary(mod)


## -----------------------------------------------------------------------------
avg_predictions(mod, by = c("X", "M"))

## -----------------------------------------------------------------------------
#| echo: false
p = avg_predictions(mod, by = c("X", "M"))
prmin = sprintf("%.0f%%", min(p$estimate) * 100)
prmax = sprintf("%.0f%%", max(p$estimate) * 100)


## -----------------------------------------------------------------------------
#| label: fig-interactions_predictions_bin_cat
#| fig-cap: Average predicted probability that Y=1 for different values of M and X.
plot_predictions(mod, by = c("M", "X"))


## -----------------------------------------------------------------------------
avg_comparisons(mod, variables = "X")

## -----------------------------------------------------------------------------
#| echo: false
p = avg_comparisons(mod, variables = "X")
pp = sprintf("%.3f", p$estimate[1])
ppt = sprintf("%.1f", p$estimate[1] * 100)


## -----------------------------------------------------------------------------
avg_comparisons(mod, variables = "X", by = "M")

## -----------------------------------------------------------------------------
#| echo: false
cmp = avg_comparisons(mod, variables = "X", by = "M")
cmpA = sprintf("%.0f", cmp$estimate[cmp$M == "a"] * 100)
cmpB = sprintf("%.0f", cmp$estimate[cmp$M == "b"] * 100)
cmpC = sprintf("%.0f", cmp$estimate[cmp$M == "c"] * 100)


## -----------------------------------------------------------------------------
avg_comparisons(mod,
  variables = "X",
  by = "M",
  hypothesis = "b3 - b1 = 0")

## -----------------------------------------------------------------------------
#| echo: false
d = avg_comparisons(mod, variables = "X", by = "M")
b3 = sprintf("%.4f", d$estimate[3])
b1 = sprintf("%.4f", d$estimate[1])
b3b1 = sprintf("%.4f", d$estimate[3] - d$estimate[1])


## -----------------------------------------------------------------------------
dat = get_dataset("interaction_02")
head(dat)


## -----------------------------------------------------------------------------
mod = glm(Y ~ X * M, data = dat, family = binomial)
summary(mod)


## -----------------------------------------------------------------------------
predictions(mod, newdata = datagrid(X = c(0, 1), M = fivenum))

## -----------------------------------------------------------------------------
#| echo: false
p = predictions(mod, newdata = datagrid(X = c(0, 1), M = fivenum))
prmin = sprintf("%.3f", min(p$estimate))
prmax = sprintf("%.3f", max(p$estimate))


## -----------------------------------------------------------------------------
#| label: fig-interactions-plot_predictions_num_bin
#| fig-cap: Predicted probability that Y=1, for different values of X and M
plot_predictions(mod, condition = c("M", "X"))


## -----------------------------------------------------------------------------
avg_comparisons(mod, variables = "X")

## -----------------------------------------------------------------------------
#| echo: false
cmp = avg_comparisons(mod, variables = "X")
cmp = sprintf("%.1f", abs(cmp$estimate) * 100)


## -----------------------------------------------------------------------------
comparisons(mod, variables = "X", newdata = datagrid(M = range))

## -----------------------------------------------------------------------------
#| echo: false
cmp = comparisons(mod, variables = "X", newdata = datagrid(M = range))
cmp0 = sprintf("%.3f", cmp$estimate[cmp$M == min(cmp$M)])
cmp1 = sprintf("%.3f", cmp$estimate[cmp$M == max(cmp$M)])


## -----------------------------------------------------------------------------
comparisons(mod, 
  hypothesis = "b2 - b1 = 0",
  variables = "X",
  newdata = datagrid(M = range))


## -----------------------------------------------------------------------------
dat = get_dataset("interaction_03")
head(dat)

mod = glm(Y ~ X * M, data = dat, family = binomial)
summary(mod)


## -----------------------------------------------------------------------------
p = predictions(mod, newdata = datagrid(
  X = c(-2, 2),
  M = c(-1, 0, 1)))
p


## -----------------------------------------------------------------------------
#| fig-cap: Predicted value of Y for different values of X and M
#| label: fig-interactions_plot_predictions_num_num
plot_predictions(mod, condition = c("X", "M"))


## -----------------------------------------------------------------------------
s = avg_slopes(mod, variables = "X")
s


## -----------------------------------------------------------------------------
slopes(mod, variables = "X", newdata = datagrid(M = fivenum))


## -----------------------------------------------------------------------------
#| fig-cap: Slope of $Y$ with respect to $X$, for different values of the moderator $M$.
#| label: fig-interactions_plot_slopes_num_num
plot_slopes(mod, variables = "X", condition = "M") +
    geom_hline(yintercept = 0, linetype = "dotted")


## -----------------------------------------------------------------------------
slopes(mod,
  variables = "X",
  newdata = datagrid(M = range))


## -----------------------------------------------------------------------------
slopes(mod,
  variables = "X",
  newdata = datagrid(M = range),
  hypothesis = "b2 - b1 = 0")


## -----------------------------------------------------------------------------
dat = get_dataset("interaction_04")
head(dat)


## -----------------------------------------------------------------------------
mod = glm(Y ~ X * M1 * M2, data = dat, family = binomial)
summary(mod)


## -----------------------------------------------------------------------------
#| label: fig-interactions_plot_predictions_multiple
#| fig-cap: Average predicted outcomes for different combinations of $X$, $M_1$, and $M_2$.
#| out-width: 100%
#| fig-width: 7.14
#| fig-asp: .4
plot_predictions(mod, by = c("X", "M1", "M2"))


## -----------------------------------------------------------------------------
#| echo: false
p = avg_predictions(mod, newdata = datagrid(
  X = 0, M1 = 0, M2 = 0
))


## -----------------------------------------------------------------------------
avg_predictions(mod, newdata = datagrid(
  X = 0, M1 = 0, M2 = 0
))


## -----------------------------------------------------------------------------
#| eval: false
# avg_predictions(mod, by = c("X", "M1", "M2"))


## -----------------------------------------------------------------------------
avg_comparisons(mod, variables = "X")

## -----------------------------------------------------------------------------
#| echo: false
cmp = avg_comparisons(mod, variables = "X")$estimate
cmp = sprintf("%.1f", cmp * 100)


## -----------------------------------------------------------------------------
avg_comparisons(mod, variables = "X", by = "M1")

## -----------------------------------------------------------------------------
#| echo: false
cmp = avg_comparisons(mod, variables = "X", by = "M1")
cmp = sprintf("%.4f", cmp$estimate)


## -----------------------------------------------------------------------------
avg_comparisons(mod, 
  variables = "X",
  by = "M1",
  hypothesis = "b2 - b1 = 0")


## -----------------------------------------------------------------------------
avg_comparisons(mod, 
    variables = "X", 
    by = c("M2", "M1"))

## -----------------------------------------------------------------------------
#| echo: false
cmp = avg_comparisons(mod, 
    variables = "X", 
    by = c("M2", "M1"))
cmp00 = sprintf("%.1f", cmp$estimate[cmp$M1 == 0 & cmp$M2 == 0] * 100)
cmp01 = sprintf("%.1f", cmp$estimate[cmp$M1 == 0 & cmp$M2 == 1] * 100)


## -----------------------------------------------------------------------------
avg_comparisons(mod, 
    hypothesis = "b2 - b1 = 0",
    variables = "X", 
    by = c("M2", "M1"))


## -----------------------------------------------------------------------------
avg_comparisons(mod, 
    hypothesis = "b4 - b3 = 0",
    variables = "X", 
    by = c("M2", "M1"))


## -----------------------------------------------------------------------------
avg_comparisons(mod, 
    hypothesis = "(b2 - b1) - (b4 - b3) = 0",
    variables = "X", 
    by = c("M2", "M1"))

## -----------------------------------------------------------------------------
#| echo: false
pv = avg_comparisons(mod, 
    hypothesis = "(b2 - b1) - (b4 - b3) = 0",
    variables = "X", 
    by = c("M2", "M1"))
pv = sprintf("%.4f", pv$p.value)


## -----------------------------------------------------------------------------
#| label: fig-interactions_polynomials
#| fig-cap: "Modelling a curvilinear relationship with linear or polynomial regression."
#| out-width: 100%
#| fig-width: 7.14
#| fig-asp: .4
library(patchwork)
dat = get_dataset("polynomial_01")

mod_linear = lm(Y ~ X, data = dat)
p1 = plot_predictions(mod_linear, condition = "X", points = .05) + 
  ggtitle("Linear")

mod_cubic = lm(Y ~ X + I(X^2) + I(X^3), data = dat)
p2 = plot_predictions(mod_cubic, condition = "X", points = .05) + 
  ggtitle("Cubic")

p1 + p2


## -----------------------------------------------------------------------------
slopes(mod_cubic, variables = "X", newdata = datagrid(X = c(-2, 0, 2)))


## -----------------------------------------------------------------------------
#| label: fig-interactions_polynomials_interaction
#| out-width: 100%
#| fig-width: 7.14
#| fig-asp: .4
dat = get_dataset("polynomial_02")

mod_cubic = lm(Y ~ X + I(X^2) + I(X^3), data = dat)
p1 = plot_predictions(mod_cubic, condition = "X",
  points = .05)

mod_cubic_interaction = lm(Y ~ M * (X + I(X^2) + I(X^3)), data = dat)
p2 = plot_predictions(mod_cubic_interaction, condition = c("X", "M"),
  points = .1)

p1 + p2


## -----------------------------------------------------------------------------
s = slopes(mod_cubic_interaction,
  variables = "X",
  newdata = datagrid(M = c(0, 1), X = fivenum))
s

