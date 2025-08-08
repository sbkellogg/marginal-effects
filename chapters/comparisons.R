## -----------------------------------------------------------------------------
#| cache: false
#| echo: false
#| warning: false
source("code/load.R")


## -----------------------------------------------------------------------------

library(marginaleffects)
dat = get_dataset("thornton")
mod = glm(outcome ~ incentive * (agecat + distance), 
    data = dat, family = binomial)
summary(mod)


## -----------------------------------------------------------------------------
grid = data.frame(distance = 2, agecat = "18 to 35", incentive = 1)
grid


## -----------------------------------------------------------------------------
#| eval: false
# # Counterfactual grids of predictor values
# g_treatment = transform(grid, incentive = 1)
# g_control = transform(grid, incentive = 0)
# 
# # Counterfactual predictions
# p_treatment = predictions(mod, newdata = g_treatment)$estimate
# p_control = predictions(mod, newdata = g_control)$estimate
# 
# # Counterfactual comparison
# p_treatment - p_control

## -----------------------------------------------------------------------------
#| echo: false
#| warning: false
cmp = comparisons(mod, variables = "incentive", newdata = grid)$estimate[1]
cmp


## -----------------------------------------------------------------------------
#| warning: false
comparisons(mod, variables = "incentive", newdata = grid)


## -----------------------------------------------------------------------------
comparisons(mod,
  variables = "incentive",
  comparison = "ratio",
  hypothesis = 1,
  newdata = grid)


## -----------------------------------------------------------------------------
comparisons(mod,
  variables = "incentive",
  comparison = "lift",
  newdata = grid)


## -----------------------------------------------------------------------------
lnor = function(hi, lo) {
  log((mean(hi) / (1 - mean(hi))) / (mean(lo) / (1 - mean(lo))))
}
comparisons(mod,
  variables = "incentive",
  comparison = lnor)


## -----------------------------------------------------------------------------
comparisons(mod, variables = "incentive", newdata = grid)


## -----------------------------------------------------------------------------
comparisons(mod, variables = list("incentive" = c(1, 0)), newdata = grid)

## -----------------------------------------------------------------------------
#| echo: false
cmp = comparisons(mod, variables = list("incentive" = c(1, 0)), newdata = grid)
cmp = sprintf("%.0f", cmp$estimate[1] * 100)


## -----------------------------------------------------------------------------
#| warning: false
comparisons(mod, variables = "agecat", newdata = grid)

## -----------------------------------------------------------------------------
#| echo: false
cmp = comparisons(mod, variables = "agecat", newdata = grid)


## -----------------------------------------------------------------------------
#| eval: false
# # Specific comparison
# comparisons(mod,
#   variables = list("agecat" = c("18 to 35", ">35")),
#   newdata = grid)
# 
# # Sequential comparisons
# comparisons(mod,
#   variables = list("agecat" = "sequential"),
#   newdata = grid)


## -----------------------------------------------------------------------------
comparisons(mod, variables = "distance", newdata = grid)

## -----------------------------------------------------------------------------
#| echo: false
cmp = comparisons(mod, variables = "distance", newdata = grid)


## -----------------------------------------------------------------------------
grid


## -----------------------------------------------------------------------------
#| eval: false
# # Increase of 5 units
# comparisons(mod, variables = list("distance" = 5), newdata = grid)
# 
# # Increase of 1 standard deviation
# comparisons(mod, variables = list("distance" = "sd"), newdata = grid)
# 
# # Change between specific values
# comparisons(mod, variables = list("distance" = c(0, 3)), newdata = grid)
# 
# # Change across the interquartile range
# comparisons(mod, variables = list("distance" = "iqr"), newdata = grid)
# 
# # Change across the full range
# comparisons(mod, variables = list("distance" = "minmax"), newdata = grid)


## -----------------------------------------------------------------------------
cmp = comparisons(mod,
  variables = c("incentive", "distance"), 
  cross = TRUE,
  newdata = grid)
cmp


## -----------------------------------------------------------------------------
comparisons(mod, variables = "incentive")


## -----------------------------------------------------------------------------
cmp = comparisons(mod)
nrow(cmp)

## -----------------------------------------------------------------------------
cmp


## -----------------------------------------------------------------------------
#| label: fig-plot_comparisons_unit
#| fig-cap: Distribution of unit-level risk differences associated with changes in each of the predictors.
#| warning: false
#| message: false
#| fig-asp: .3
#| out-width: 100%
#| fig-width: 7.14
library(ggplot2)

ggplot(cmp, aes(x = estimate)) +
  geom_histogram(bins = 30) +
  facet_grid(. ~ term + contrast, scales = "free") +
  labs(x = "Estimated change in predicted outcome", y = "Count")


## -----------------------------------------------------------------------------
#| echo: false
mmin = sprintf("%.1f", min(cmp$estimate[cmp$term=="incentive"]))
mmax = sprintf("%.1f", max(cmp$estimate[cmp$term=="incentive"]))


## -----------------------------------------------------------------------------
#| eval: false
# comparisons(mod,
#   variables = "incentive",
#   newdata = datagrid(agecat = unique, distance = mean))

## -----------------------------------------------------------------------------
#| echo: false
cmp = comparisons(mod,
  variables = "incentive",
  newdata = datagrid(
    agecat = unique,
    distance = mean))
cmp$incentive = NULL
mid = sprintf("%.1f", 100 * cmp$estimate[cmp$agecat=="18 to 35"])
old = sprintf("%.1f", 100 * cmp$estimate[cmp$agecat==">35"])
cmp


## -----------------------------------------------------------------------------
#| eval: false
# comparisons(mod, variables = "incentive", newdata = "mean")

## -----------------------------------------------------------------------------
#| echo: false
cmp = comparisons(mod, variables = "incentive", newdata = "mean")
cmp$incentive = NULL
cmp


## -----------------------------------------------------------------------------
#| eval: false
# cmp = comparisons(mod, variables = "incentive", newdata = "balanced")
# as.data.frame(cmp)


## -----------------------------------------------------------------------------
#| echo: false
cmp = comparisons(mod, variables = "incentive", newdata = "balanced")
cmp$incentive = NULL
as.data.frame(cmp)


## -----------------------------------------------------------------------------
avg_comparisons(mod, variables = "incentive")

## -----------------------------------------------------------------------------
#| echo: false
cmp = avg_comparisons(mod, variables = "incentive")


## -----------------------------------------------------------------------------
cmp = comparisons(mod, variables = "incentive")
mean(cmp$estimate)


## -----------------------------------------------------------------------------
avg_comparisons(mod, variables = "incentive", by = "agecat")

## -----------------------------------------------------------------------------
#| echo: false
cmp = avg_comparisons(mod, variables = "incentive", by = "agecat")
cmp$estimate = sprintf("%.1f", 100 * cmp$estimate)


## -----------------------------------------------------------------------------
avg_comparisons(mod,
  variables = "incentive",
  newdata = subset(incentive == 1)
)


## -----------------------------------------------------------------------------
penguins = get_dataset("penguins", "palmerpenguins") 

aggregate(
  cbind(body_mass_g, flipper_length_mm) ~ species, 
  FUN = mean,
  data = penguins)


## -----------------------------------------------------------------------------
fit = lm(flipper_length_mm ~ body_mass_g * species, data = penguins)

avg_predictions(fit, by = "species")


## -----------------------------------------------------------------------------
# Adelie
penguins |>
  transform(species = "Adelie") |>
  predict(fit, newdata = _) |>
  mean(na.rm = TRUE)

# Chinstrap
penguins |>
  transform(species = "Chinstrap") |>
  predict(fit, newdata = _) |>
  mean(na.rm = TRUE)

# Gentoo
penguins |>
  transform(species = "Gentoo") |>
  predict(fit, newdata = _) |>
  mean(na.rm = TRUE)


## -----------------------------------------------------------------------------
p = avg_predictions(fit, variables = "species", by = "species")
p


## -----------------------------------------------------------------------------
diff(p$estimate)

avg_comparisons(fit, variables = list(species = "sequential"))


## -----------------------------------------------------------------------------
#| echo: false
options(tinytable_theme_placement_latex_float = NULL)

## -----------------------------------------------------------------------------
#| label: tbl-comparisons_uncertainty
#| tbl-cap: Alternative ways to compute uncertainty about counterfactual comparisons.
#| warning: false
library(modelsummary)

models = list(
  "Heteroskedasticity" = avg_comparisons(mod, vcov = "HC3"),
  "Clustered" = avg_comparisons(mod, vcov = ~village),
  "Bootstrap" = avg_comparisons(mod) |> inferences("boot")
)

modelsummary(models,
  statistic = "conf.int", fmt = 4, gof_map = "nobs",
  shape = term + contrast ~ model
)

## -----------------------------------------------------------------------------
#| echo: false
options(tinytable_theme_placement_latex_float = "H")


## -----------------------------------------------------------------------------
cmp = avg_comparisons(mod,
  variables = "incentive",
  by = "agecat")
cmp

## -----------------------------------------------------------------------------
#| echo: false
cmpstr = sprintf("%.2f", cmp$estimate)
equal = sprintf("%.2f", cmp$estimate[3] - cmp$estimate[1])


## -----------------------------------------------------------------------------
avg_comparisons(mod,
  hypothesis = "b1 - b3 = 0",
  variables = "incentive",
  by = "agecat")


## -----------------------------------------------------------------------------
avg_comparisons(mod, variables = "incentive", by = "agecat")


## -----------------------------------------------------------------------------
#| label: fig-plot_comparisons_marginal
#| fig-cap: Marginal counterfactual comparisons for a change in incentive, by age category.
library(ggplot2) 

plot_comparisons(mod, variables = "incentive", by = "agecat") +
  labs(x = "Age", y = "Average risk difference")


## -----------------------------------------------------------------------------
library(patchwork)

p1 = plot_comparisons(mod,
  variables = "incentive",
  condition = "distance") +
  labs(y = "Conditional risk difference")

p2 = plot_comparisons(mod, 
  variables = "incentive",
  condition = c("distance", "agecat")) +
  labs(y = "Conditional risk difference")

p1 + p2


## -----------------------------------------------------------------------------
#| echo: false
#| tbl-cap: Main arguments of the `comparisons()`, `avg_comparisons()`, and `plot_comparisons()` functions.
#| label: tbl-comparisons-arguments
library(tinytable)
args = c(
  "`model`" = "Fitted model used to make counterfactual comparisons.",
  "`variables`" = "Focal predictor whose association with or effect on the outcome we are interested in.",
  "`newdata`" = "Grid of predictor values.",
  "`comparison`" = "How the counterfactual predictions are compared: difference, ratio, lift, etc.",
  "`vcov`" = "Standard errors: Classical, robust, clustered, etc.",
  "`conf_level`" = "Size of the confidence intervals.",
  "`type`" = "Type of predictions to compare: response, link, etc.",
  "`cross`" = "Estimate the effect of changing multiple variables at once (cross-contrast).",
  "`by`" = "Grouping variable for average predictons.",
  "`wts`" = "Weights used to compute average comparisons.",
  "`transform`" = "Post-hoc transformations.",
  "`hypothesis`" = "Compare different comparisons to one another, conduct linear or non-linear hypothesis tests, or specify null hypotheses.",
  "`equivalence`" = "Equivalence tests",
  "`df`" = "Degrees of freedom for hypothesis tests.",
  "`numderiv`" = "Algorithm used to compute numerical derivatives.",
  "`...`" = "Additional arguments are passed to the `predict()` method supplied by the modelling package."
)
df = data.frame(
  "Argument" = names(args),
  " " = args,
  check.names = FALSE)
options(tinytable_theme_placement_latex_float = NULL)
tt(df, width = c(1, 5)) |> format_tt(j = 1:2, markdown = TRUE)
options(tinytable_theme_placement_latex_float = "H")

