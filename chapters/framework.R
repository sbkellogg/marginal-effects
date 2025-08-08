## -----------------------------------------------------------------------------
#| cache: false
#| warning: false
#| echo: false
source("code/load.R")
options(tinytable_tt_digits = 4)


## -----------------------------------------------------------------------------
library(marginaleffects)
library(tinytable)

set.seed(48103)
N = 10
dat = data.frame(
  Num = rnorm(N),
  Bin = rbinom(N, size = 1, prob = 0.5),
  Cat = sample(c("A", "B", "C"), size = N, replace = TRUE)
)


## -----------------------------------------------------------------------------
dat |> tt()


## -----------------------------------------------------------------------------
datagrid(Bin = c(0, 1), newdata = dat) |> tt()


## -----------------------------------------------------------------------------
datagrid(Num = range, Bin = mean, Cat = unique, newdata = dat) |> tt()


## -----------------------------------------------------------------------------
datagrid(grid_type = "mean_or_mode", newdata = dat) |> tt()


## -----------------------------------------------------------------------------
datagrid(grid_type = "balanced", newdata = dat) |> tt()


## -----------------------------------------------------------------------------
g = datagrid(
  Bin = c(0, 1),
  grid_type = "counterfactual",
  newdata = dat
)
nrow(g)


## -----------------------------------------------------------------------------
subset(g, rowidcf %in% 1:3) |> tt()

