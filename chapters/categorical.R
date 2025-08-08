## -----------------------------------------------------------------------------
#| cache: false
#| echo: false
#| warning: false
source("code/load.R")


## -----------------------------------------------------------------------------
#| label: fig-categorical_barplot
#| fig-cap: Distribution of the self-reported number of extra-marital affairs during 12 months preceding the survey (Fair, 1978).
library(MASS)
library(ggplot2)
library(marginaleffects)
dat = get_dataset("affairs")
barplot(table(dat$affairs), ylab = "N", xlab = "Affairs")


## -----------------------------------------------------------------------------
mod = polr(
  affairs ~ children + yearsmarried + gender,
  method = "probit", data = dat, Hess = TRUE)
summary(mod)


## -----------------------------------------------------------------------------
p = predictions(mod, newdata = datagrid(
  children = "yes",
  yearsmarried = 10,
  gender = "woman"))
p


## -----------------------------------------------------------------------------
p = avg_predictions(mod)
p


## -----------------------------------------------------------------------------
colnames(p)
p$group


## -----------------------------------------------------------------------------
p = avg_predictions(mod, by = "children")
head(p, 2)


## -----------------------------------------------------------------------------
#| label: fig-categorical_plot_predictions
#| fig-cap: Average predicted probabilities for each outcome level, for the subpopulations with and without children.
plot_predictions(mod, by = c("group", "children")) +
  labs(x = "Number of affairs", y = "Average predicted probability")


## -----------------------------------------------------------------------------
hyp = function(x) {
  x$term = ifelse(x$group == "0", "0", ">0")
  aggregate(estimate ~ term + children, FUN = sum, data = x)
}
p = avg_predictions(mod, by = "children", hypothesis = hyp)
p


## -----------------------------------------------------------------------------
#| eval: false
# library(tidyverse)
# hyp = function(x) {
#   x %>%
#     mutate(term = if_else(group == "0", "0", ">0")) %>%
#     summarize(estimate = sum(estimate), .by = c("term", "children"))
# }
# avg_predictions(mod, by = "children", hypothesis = hyp)


## -----------------------------------------------------------------------------
avg_comparisons(mod, variables = list(yearsmarried = 5))

## -----------------------------------------------------------------------------
cmp = avg_comparisons(mod, variables = list(yearsmarried = 5))


## -----------------------------------------------------------------------------
#| echo: false
cmp = sprintf("%.1f", abs(cmp$estimate) * 100)


## -----------------------------------------------------------------------------
#| warning: false
avg_comparisons(mod, variables = "gender")

