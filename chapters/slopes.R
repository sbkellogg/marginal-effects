## -----------------------------------------------------------------------------
#| cache: false
#| echo: false
#| warning: false
source("code/load.R")


## -----------------------------------------------------------------------------
#| eval: false
#| echo: false
# X = seq(-5, 5, length.out = 1000)
# Y = rbinom(N,  1, plogis(X))
# mod = glm(Y ~ .3 * X, family = binomial)
# s = slopes(mod)[c(250, 500, 750), ]
# s$intercept = s$predicted - s$estimate * s$X
# s$x = s$X - 1
# s$xend = s$X + 1
# s$y = s$predicted - 1 * s$estimate
# s$yend = s$predicted + 1 * s$estimate
# plot_predictions(mod, condition = "X", vcov = FALSE) +
#     geom_segment(data = s,
#       aes(x = x, xend = xend, y = y, yend = yend),
#       color = "orange", linetype = "dotted")


## -----------------------------------------------------------------------------
#| echo: false
#| out-width: 100%
#| fig-width: 7.14
#| fig-cap: Three functions and their derivatives.
#| label: fig-slopes_3functions
library(ggplot2)
library(patchwork)
p1 = ggplot() + geom_function(fun = function(x) -1 + 0.5 * x) + geom_line() + xlim(-5, 5) +
    labs(y = "Y", x = "X") +
    ggtitle(expression(Y==-1 + 0.5 * X))
p2 = ggplot() + geom_function(fun = function(x) x^2) + geom_line() + xlim(-5, 5) +
    labs(y = "Y", x = "X") +
    ggtitle(expression(Y==X^{2})) 
#p2 = ggplot() + geom_function(fun = function(x) -x^2) + geom_line() + xlim(-5, 5) +
#    labs(y = "Y", x = "X") +
#    ggtitle(expression(Y==-X^{2}))
p3 = ggplot() + geom_function(fun = function(x) cos(x)) + geom_line() + xlim(-5, 5) +
    labs(y = "Y", x = "X") +
    ggtitle(expression(Y==cos(X)))
p4 = ggplot() + geom_function(fun = function(x) 0.5) + geom_line() + 
    xlim(-5, 5) + geom_hline(yintercept = 0, linetype = 3) + ylim(-1, 1) +
    labs(y = expression(partialdiff * Y / partialdiff * X), x = "X",
         title = expression(frac(partialdiff * Y, partialdiff * X) == 0.5))
p5 = ggplot() + geom_function(fun = function(x) 2 * x) + geom_line() + 
    xlim(-5, 5) + geom_hline(yintercept = 0, linetype = 3) +
    labs(y = expression(partialdiff * Y / partialdiff * X), x = "X",
         title = expression(frac(partialdiff * Y, partialdiff * X) == 2*X))
#p5 = ggplot() + geom_function(fun = function(x) -2 * x) + geom_line() + 
#    xlim(-5, 5) + geom_hline(yintercept = 0, linetype = 3) +
#    labs(y = expression(partialdiff * Y / partialdiff * X), x = "X",
#         title = expression(frac(partialdiff * Y, partialdiff * X) == -2*X))
p6 = ggplot() + geom_function(fun = function(x) -sin(x)) + geom_line() + 
    xlim(-5, 5) + geom_hline(yintercept = 0, linetype = 3) +
    labs(y = expression(partialdiff * Y / partialdiff * X), x = "X",
         title = expression(frac(partialdiff * Y, partialdiff * X) == -sin(X)))
(p1 + p2 + p3) / (p4 + p5 + p6)


## -----------------------------------------------------------------------------
library(marginaleffects)
library(patchwork)
set.seed(48103)
N = 1e6
X = rnorm(N, sd = 2)
p = plogis(-1 + 0.5 * X)
Y = rbinom(N, 1, p)
dat = data.frame(Y, X)


## -----------------------------------------------------------------------------
#| fig-cap: In a logistic model, the strength of association between the response and the predictor (i.e., the slope) depends on the baseline value of the predictor, and on the other covariates in the model.
#| fig-asp: 1
mod = glm(Y ~ X, data = dat, family = binomial)
b = coef(mod)
b


## -----------------------------------------------------------------------------
#| fig-asp: 1
#| fig-cap: Logistic function and its derivative.
#| label: fig-slopes_logit
library(patchwork)
p_function = plot_predictions(mod, condition = "X")
p_derivative = plot_slopes(mod, variables = "X", condition = "X")
p_function / p_derivative


## -----------------------------------------------------------------------------
#| eval: false
# data.frame(
#   "dY/dX|X=-5" = b[2] * dlogis(b[1] + b[2] * -5),
#   "dY/dX|X=0" = b[2] * dlogis(b[1] + b[2] * 0),
#   "dY/dX|X=10" = b[2] * dlogis(b[1] + b[2] * 10)
# )

## -----------------------------------------------------------------------------
#| echo: false
d = data.frame(
  "dY/dX|X=-5" = b[2] * dlogis(b[1] + b[2] * -5),
  "dY/dX|X=0" = b[2] * dlogis(b[1] + b[2] * 0),
  "dY/dX|X=10" = b[2] * dlogis(b[1] + b[2] * 10),
  check.names = FALSE
)
print(d, row.names = FALSE)


## -----------------------------------------------------------------------------
slopes(mod, 
  variables = "X",
  newdata = datagrid(X = c(-5, 0, 10)))


## -----------------------------------------------------------------------------
dat = get_dataset("thornton")
mod = glm(outcome ~ incentive * distance * I(distance^2),
  data = dat, family = binomial)
summary(mod)


## -----------------------------------------------------------------------------
#| eval: false
# slopes(mod, variables = "distance")


## -----------------------------------------------------------------------------
library(marginaleffects)
slopes(mod,
  variables = "distance",
  newdata = datagrid(incentive = 1, distance = 1))

## -----------------------------------------------------------------------------
#| echo: false
library(marginaleffects)
s1 = slopes(mod,
  variables = "distance",
  newdata = datagrid(incentive = 1, distance = 1))
s2 = slopes(mod,
  variables = "distance",
  newdata = "mean")


## -----------------------------------------------------------------------------
slopes(mod, variables = "distance", newdata = "mean")


## -----------------------------------------------------------------------------
slopes(mod, variables = "distance")


## -----------------------------------------------------------------------------
avg_slopes(mod, variables = "distance")


## -----------------------------------------------------------------------------
avg_slopes(mod, variables = "distance", by = "incentive")


## -----------------------------------------------------------------------------
avg_slopes(mod, variables = "distance", vcov = ~village)

avg_slopes(mod, variables = "distance") |> inferences(method = "boot")


## -----------------------------------------------------------------------------
avg_slopes(mod,
  variables = "distance",
  by = "incentive")


## -----------------------------------------------------------------------------
avg_slopes(mod,
  variables = "distance",
  by = "incentive",
  hypothesis = "b1 - b2 = 0")

## -----------------------------------------------------------------------------
#| echo: false
h = avg_slopes(mod, variables = "distance", by = "incentive", hypothesis = "b1 - b2 = 0")


## -----------------------------------------------------------------------------
#| fig-cap: Model-based predictions (top) and slopes (bottom).
#| label: fig-slopes_vs_predictions
#| fig-asp: 1
library(ggplot2)
library(patchwork)

p1 = plot_predictions(mod, condition = "distance") +
    labs(y = "Predicted Pr(Outcome=1)")

p2 = plot_slopes(mod, variables = "distance", condition = "distance") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(y = "dY/dX")

p1 / p2


## -----------------------------------------------------------------------------
plot_slopes(mod,
  variables = "distance",
  condition = c("distance", "incentive"))


## -----------------------------------------------------------------------------
plot_slopes(mod,
  variables = "distance",
  by = "incentive")


## -----------------------------------------------------------------------------
#| echo: false
#| tbl-cap: Main arguments of the `slopes()`, `avg_slopes()`, and `plot_slopes()` functions.
#| label: tbl-slopes-arguments
library(tinytable)
args = c(
  "`model`" = "Fitted model used to make counterfactual slopes.",
  "`variables`" = "Focal predictor whose association with or effect on the outcome we are interested in.",
  "`newdata`" = "Grid of predictor values.",
  "`slope`" = "Choice between derivative or (semi-)elasticity.",
  "`vcov`" = "Standard errors: Classical, robust, clustered, etc.",
  "`conf_level`" = "Size of the confidence intervals.",
  "`type`" = "Type of predictions to compare: response, link, etc.",
  "`by`" = "Grouping variable for average predictons.",
  "`wts`" = "Weights used to compute average slopes.",
  "`transform`" = "Post-hoc transformations.",
  "`hypothesis`" = "Compare different slopes to one another, conduct linear or non-linear hypothesis tests, or specify null hypotheses.",
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

