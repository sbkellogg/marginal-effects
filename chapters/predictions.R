## -----------------------------------------------------------------------------
#| cache: false
#| echo: false
#| warning: false
source("code/load.R")


## -----------------------------------------------------------------------------
library(marginaleffects)
dat = get_dataset("thornton")
mod = glm(outcome ~ incentive + agecat, data = dat, family = binomial)


## -----------------------------------------------------------------------------
b = coef(mod)
b

## -----------------------------------------------------------------------------
#| echo: false
bs = sprintf("%.3f", coef(mod))


## -----------------------------------------------------------------------------
#| eval: false
# linpred_treatment_younger = b[1] + b[2] * 1 + b[3] * 1 + b[4] * 0
# linpred_treatment_younger

## -----------------------------------------------------------------------------
#| echo: false
linpred_treatment_younger = b[1] + b[2] * 1 + b[3] * 1 + b[4] * 0
unname(linpred_treatment_younger)

## -----------------------------------------------------------------------------
#| eval: false
# linpred_control_older = b[1] + b[2] * 0 + b[3] * 0 + b[4] * 1
# linpred_control_older

## -----------------------------------------------------------------------------
#| echo: false
linpred_control_older = b[1] + b[2] * 0 + b[3] * 0 + b[4] * 1
unname(linpred_control_older)


## -----------------------------------------------------------------------------
#| eval: false
# g = \(x) 1 / (1 + exp(-x))
# g(linpred_treatment_younger)

## -----------------------------------------------------------------------------
#| echo: false
g = \(x) unname(1 / (1 + exp(-x)))
g(linpred_treatment_younger)

## -----------------------------------------------------------------------------
#| eval: false
# g(linpred_control_older)

## -----------------------------------------------------------------------------
#| echo: false
g(linpred_control_older)


## -----------------------------------------------------------------------------
#| echo: false
a = sprintf("%.0f%%", g(linpred_treatment_younger) * 100)
b = sprintf("%.0f%%", g(linpred_control_older) * 100)


## -----------------------------------------------------------------------------
grid = data.frame(agecat = c("18 to 35", ">35"), incentive = c(1, 0))
grid


## -----------------------------------------------------------------------------
predictions(mod, newdata = grid, type = "link")


## -----------------------------------------------------------------------------
predictions(mod, newdata = grid)


## -----------------------------------------------------------------------------
mod = glm(outcome ~ incentive + agecat + distance,
  data = dat, family = binomial)


## -----------------------------------------------------------------------------
p = predictions(mod)
p


## -----------------------------------------------------------------------------
dim(p)


## -----------------------------------------------------------------------------
colnames(p)


## -----------------------------------------------------------------------------
p[1:4, "estimate"]


## -----------------------------------------------------------------------------
datagrid(agecat = "18 to 35", incentive = c(0, 1), model = mod)


## -----------------------------------------------------------------------------
predictions(mod,
  newdata = datagrid(agecat = "18 to 35", incentive = c(0, 1))
)

## -----------------------------------------------------------------------------
#| echo: false
p = predictions(mod,
  newdata = datagrid(agecat = "18 to 35", incentive = c(0, 1))
)


## -----------------------------------------------------------------------------
predictions(mod, 
  newdata = datagrid(distance = 2, agecat = unique, incentive = max)
)


## -----------------------------------------------------------------------------
p = predictions(mod, newdata = "mean")
p


## -----------------------------------------------------------------------------
#| eval: false
# predictions(mod, newdata = datagrid(
#   agecat = unique, incentive = unique, distance = mean
# ))
# 
# predictions(mod, newdata = "balanced")

## -----------------------------------------------------------------------------
#| echo: false
predictions(mod, newdata = datagrid(
  agecat = unique, incentive = unique, distance = mean
))


## -----------------------------------------------------------------------------
#| echo: false
datfull = marginaleffects:::get_modeldata(mod)


## -----------------------------------------------------------------------------
p = predictions(mod, variables = list(incentive = 0:1))
dim(p)


## -----------------------------------------------------------------------------
#| label: fig-logit_counterfactual
#| fig-cap: Predicted probabilities for counterfactual values of incentive. 
#| out-width: 50%
#| fig-width: 6
library(ggplot2)

p = data.frame(
  Control = p[p$incentive == 0, "estimate"],
  Treatment = p[p$incentive == 1, "estimate"])

ggplot(p, aes(Control, Treatment)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point() +
  labs(x = "Pr(outcome=1) when incentive = 0", 
       y = "Pr(outcome=1) when incentive = 1") +
  xlim(0, 1) + ylim(0, 1) + coord_equal()


## -----------------------------------------------------------------------------
p = predictions(mod)
mean(p$estimate)


## -----------------------------------------------------------------------------
avg_predictions(mod)


## -----------------------------------------------------------------------------
avg_predictions(mod, by = "agecat")

## -----------------------------------------------------------------------------
#| echo: false
p = avg_predictions(mod, by = "agecat")


## -----------------------------------------------------------------------------
avg_predictions(mod, newdata = "balanced", by = "agecat")

## -----------------------------------------------------------------------------
#| echo: false
p = avg_predictions(mod, newdata = "balanced", by = "agecat")


## -----------------------------------------------------------------------------
table(dat$incentive)


## -----------------------------------------------------------------------------
avg_predictions(mod, by = "incentive")


## -----------------------------------------------------------------------------
avg_predictions(
    mod,
    variables = list(incentive = c(0, 1)),
    by = "incentive")


## -----------------------------------------------------------------------------
p0 = predictions(mod, newdata = transform(dat, incentive = 0))
mean(p0$estimate)
p1 = predictions(mod, newdata = transform(dat, incentive = 1))
mean(p1$estimate)


## -----------------------------------------------------------------------------
avg_predictions(mod,
  by = "incentive",
  vcov = "HC3",
  conf_level = .9)


## -----------------------------------------------------------------------------
avg_predictions(mod,
  by = "incentive",
  vcov = ~ village,
  conf_level = .9)


## -----------------------------------------------------------------------------
avg_predictions(mod,
  by = "incentive",
  conf_level = .9) |>
  inferences(method = "boot", R = 1000)


## -----------------------------------------------------------------------------
p = avg_predictions(mod, by = "agecat")
p


## -----------------------------------------------------------------------------
p$estimate[3] - p$estimate[2]


## -----------------------------------------------------------------------------
p = avg_predictions(mod, by = "agecat",
  hypothesis = "b3 - b2 = 0")
p


## -----------------------------------------------------------------------------
avg_predictions(mod,
  by = "agecat",
  hypothesis = difference ~ sequential)


## -----------------------------------------------------------------------------
avg_predictions(mod,
  by = "agecat",
  hypothesis = difference ~ reference)


## -----------------------------------------------------------------------------
avg_predictions(mod,
    by = c("incentive", "agecat"))


## -----------------------------------------------------------------------------
avg_predictions(mod,
    by = c("incentive", "agecat"),
    hypothesis = difference ~ sequential | incentive)

## -----------------------------------------------------------------------------
#| echo: false
p = avg_predictions(mod,
    by = c("incentive", "agecat"),
    hypothesis = difference ~ sequential | incentive)


## -----------------------------------------------------------------------------
avg_predictions(mod, by = "agecat")


## -----------------------------------------------------------------------------
avg_predictions(mod,
    by = "agecat",
    hypothesis = "b3 - b1 = 0")


## -----------------------------------------------------------------------------
avg_predictions(mod,
    by = "agecat",
    hypothesis = "b3 - b1 = 0",
    equivalence = c(-0.1, 0.1))


## -----------------------------------------------------------------------------
#| label: fig-plot_predictions_unit
#| fig-cap: Distribution of unit-level predictions (fitted values), by treatment group.
#| warning: false
#| out-width: 100%
#| fig-width: 7.14
#| fig-asp: .3
library(patchwork)

p = predictions(mod)

# Histogram
p1 = ggplot(p) +
  geom_histogram(aes(estimate, fill = factor(incentive))) +
  labs(x = "Pr(outcome = 1)", y = "Count", fill = "Incentive") +
  scale_fill_grey()

# Empirical Cumulative Distribution Function
p2 = ggplot(p) +
  stat_ecdf(aes(estimate, colour = factor(incentive))) +
  labs(x = "Pr(outcome = 1)", 
       y = "Cumulative Probability", 
       colour = "Incentive") +
  scale_colour_grey()

# Combined plots
p1 + p2


## -----------------------------------------------------------------------------
avg_predictions(mod, by = "incentive")


## -----------------------------------------------------------------------------
#| label: fig-plot_predictions_marginal
#| fig-cap: Marginal predicted probabilities that `outcome` equals 1.
#| out-width: 100%
#| fig-width: 7.14
#| fig-asp: .4
p1 = plot_predictions(mod, by = "incentive")
p2 = plot_predictions(mod, by = c("incentive", "agecat"))
p1 + p2


## -----------------------------------------------------------------------------
#| eval: false
# plot_predictions(mod, by = "incentive", newdata = "balanced")


## -----------------------------------------------------------------------------
#| eval: false
# p1 = plot_predictions(mod,
#   condition = "distance"
# )
# p2 = plot_predictions(mod,
#   condition = c("distance", "incentive")
# )
# p3 = plot_predictions(mod,
#   condition = c("distance", "incentive", "agecat")
# )
# (p1 + p2) / p3

## -----------------------------------------------------------------------------
#| label: fig-plot_predictions_conditional
#| fig-cap: Predicted probability that `outcome` equals 1, conditional on incentive, age category, and distance. Other variables are held at their means or modes. 
#| echo: false
#| out-width: 100%
#| fig-width: 7.14
library(patchwork)
p1 = plot_predictions(mod, condition = "distance")
p2 = plot_predictions(mod, condition = c("distance", "incentive"))
p3 = plot_predictions(mod, condition = c("distance", "incentive", "agecat"))
p1 = p1 + ggtitle("p1")
p2 = p2 + ggtitle("p2")
p3 = p3 + ggtitle("p3")
(p1 + p2) / p3


## -----------------------------------------------------------------------------
plot_predictions(mod, condition = list(
    "distance", "agecat" = ">35", "incentive" = 0
))


## -----------------------------------------------------------------------------
#| warning: false
plot_predictions(mod, condition = "distance", rug = TRUE) +
    theme_grey() +
    ylim(c(.65, .81)) + xlim(c(2, 4.5))


## -----------------------------------------------------------------------------
plot_predictions(mod, by = "incentive", draw = FALSE)


## -----------------------------------------------------------------------------
#| echo: false
#| tbl-cap: Main arguments of the `predictions()`, `avg_predictions()`, and `plot_predictions()` functions.
#| label: tbl-predictions-arguments
library(tinytable)
args = c(
  "`model`" = "Fitted model used to make predictions.",
  "`newdata`" = "Grid of predictor values.",
  "`variables`" = "Make predictions on a counterfactual grid.",
  "`vcov`" = "Standard errors: Classical, robust, clustered, etc.",
  "`conf_level`" = "Size of the confidence intervals.",
  "`type`" = "Type of predictions: response, link, etc.",
  "`by`" = "Grouping variable for average predictons.",
  "`byfun`" = "Custom aggregation functions.",
  "`wts`" = "Weights used to compute average predictions.",
  "`transform`" = "Post-hoc transformations.",
  "`hypothesis`" = "Compare predictions to one another, conduct linear or non-linear hypothesis tests, or specify null hypotheses.",
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

