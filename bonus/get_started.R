## .table {
##   font-size: .8em;
## }
## .table td, .table th {
##   white-space: nowrap;
## }

## ----include = FALSE----------------------------------------------------------
options(width = 10000)
okabeito <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7', '#999999', '#000000')
options(ggplot2.discrete.colour = okabeito)
options(ggplot2.discrete.fill = okabeito)
url <- "https://raw.githubusercontent.com/vincentarelbundock/marginaleffects/main/data-raw/supported_models.csv"
dat <- read.csv(url)
n_support <- nrow(dat)

options(marginaleffects_print_style = "tinytable")
options("tinytable_theme_placement_latex_float" = "H")


## -----------------------------------------------------------------------------
#| eval: false
# install.packages("marginaleffects")


## pip install marginaleffects

## -----------------------------------------------------------------------------
library(marginaleffects)

mod <- lm(mpg ~ hp * wt * am, data = mtcars)


## import polars as pl
## import numpy as np
## import statsmodels.formula.api as smf
## from marginaleffects import *
## 
## mtcars = pl.read_csv("https://vincentarelbundock.github.io/Rdatasets/csv/datasets/mtcars.csv")
## 
## mod = smf.ols("mpg ~ hp * wt * am", data = mtcars).fit()

## -----------------------------------------------------------------------------
pre <- predictions(mod)

nrow(mtcars)

nrow(pre)

pre


## pre = predictions(mod)
## 
## mtcars.shape
## 
## pre.shape
## 
## print(pre)

## -----------------------------------------------------------------------------
cmp <- comparisons(mod)

nrow(cmp)

cmp


## cmp = comparisons(mod)
## 
## cmp.shape
## 
## print(cmp)

## -----------------------------------------------------------------------------
comparisons(mod, variables = list(hp = c(120, 100)))


## cmp = comparisons(mod, variables = {"hp": [120, 100]})
## print(cmp)

## -----------------------------------------------------------------------------
comparisons(mod, variables = list(hp = "sd"))


## cmp = comparisons(mod, variables = {"hp": "sd"})
## print(cmp)

## -----------------------------------------------------------------------------
comparisons(
  mod,
  variables = list(hp = 50),
  comparison = "ratioavg")


## cmp = comparisons(
##   mod,
##   variables = {"hp": 50},
##   comparison = "ratioavg")
## print(cmp)

## -----------------------------------------------------------------------------
mfx <- slopes(mod)

nrow(mfx)

mfx


## mfx = slopes(mod)
## 
## mfx.shape
## 
## print(mfx)

## -----------------------------------------------------------------------------
predictions(mod, newdata = "mean")

predictions(mod, newdata = "median")


## p = predictions(mod, newdata = "mean")
## print(p)
## 
## p = predictions(mod, newdata = "median")
## print(p)

## -----------------------------------------------------------------------------
predictions(
  mod,
  newdata = datagrid(
    am = c(0, 1),
    wt = range))


## p = predictions(
##   mod,
##   newdata = datagrid(
##     am = [0, 1],
##     wt = [mtcars["wt"].min(), mtcars["wt"].max()]))
## print(p)

## -----------------------------------------------------------------------------
slopes(
  mod,
  variables = "wt",
  newdata = datagrid(am = 0:1))


## s = slopes(
##   mod,
##   variables = "wt",
##   newdata = datagrid(mod, am = [0, 1]))
## print(s)

## -----------------------------------------------------------------------------
plot_predictions(mod, condition = list("hp", "wt" = "threenum", "am"))


## cond = {
##   "hp": None,
##   "wt": [mtcars["wt"].mean() - mtcars["wt"].std(),
##          mtcars["wt"].mean(),
##          mtcars["wt"].mean() + mtcars["wt"].std()],
##   "am": None
## }
## plot_predictions(mod, condition = cond)

## -----------------------------------------------------------------------------
plot_slopes(mod, variables = "am", condition = list("hp", "wt" = "minmax"))


## plot_slopes(mod,
##   variables = "am",
##   condition = {"hp": None, "wt": [mtcars["wt"].min(), mtcars["wt"].max()]}
## )

## -----------------------------------------------------------------------------
avg_predictions(mod)


## p = avg_predictions(mod)
## print(p)

## -----------------------------------------------------------------------------
mean(predict(mod))


## np.mean(mod.predict())

## -----------------------------------------------------------------------------
avg_comparisons(mod, by = "am")


## cmp = avg_comparisons(mod, by = "am")
## print(cmp)

## -----------------------------------------------------------------------------
dat <- mtcars
dat$am <- as.logical(dat$am)
dat$cyl <- as.factor(dat$cyl)
mod_cat <- lm(mpg ~ am + cyl + hp, data = dat)


## dat = pl.read_csv("https://vincentarelbundock.github.io/Rdatasets/csv/datasets/mtcars.csv") \
##   .with_columns(pl.col("am").cast(pl.Boolean),
##                 pl.col("cyl").cast(pl.Utf8))
## mod_cat = smf.ols('mpg ~ am + cyl + hp', data=dat.to_pandas()).fit()

## -----------------------------------------------------------------------------
avg_predictions(
  mod_cat,
  newdata = "balanced",
  by = "am")


## predictions(
##   mod_cat,
##   newdata = datagrid(grid_type = "balanced"),
##   by = "am")
## print(p)

## -----------------------------------------------------------------------------
avg_predictions(mod, by = "am", vcov = "HC3")


## -----------------------------------------------------------------------------
avg_predictions(mod, by = "am") |>
  inferences(method = "boot", R = 500)


## -----------------------------------------------------------------------------
mod <- lm(mpg ~ qsec * drat, data = mtcars)
coef(mod)


## mod = smf.ols('mpg ~ qsec * drat', data=mtcars).fit()
## print(mod.params)

## -----------------------------------------------------------------------------
hypotheses(mod, "drat = 2 * qsec")


## h = hypotheses(mod, "drat = 2 * qsec")
## print(h)

## -----------------------------------------------------------------------------
hypotheses(mod, "b3 = 2 * b2")


## h = hypotheses(mod, "b2 = 2 * b1")
## print(h)

## -----------------------------------------------------------------------------
slopes(
  mod,
  variables = "drat",
  newdata = datagrid(qsec = range))


## s = slopes(
##   mod,
##   variables = "drat",
##   newdata = datagrid(qsec = [mtcars["qsec"].min(), mtcars["qsec"].max()]))
## print(s)

## -----------------------------------------------------------------------------
slopes(
  mod,
  hypothesis = "b1 = b2",
  variables = "drat",
  newdata = datagrid(qsec = range))


## s = slopes(
##   mod,
##   hypothesis = "b0 = b1",
##   variables = "drat",
##   newdata = datagrid(qsec = [mtcars["qsec"].min(), mtcars["qsec"].max()]))
## print(s)

## -----------------------------------------------------------------------------
avg_slopes(mod)

avg_slopes(mod, hypothesis = "drat = qsec")


## s = avg_slopes(mod)
## print(s)
## 
## s = avg_slopes(mod, hypothesis = "drat = qsec")
## print(s)

## -----------------------------------------------------------------------------
avg_slopes(mod, equivalence = c(-2, 2))


## s = avg_slopes(mod, equivalence = [-2., 2.])
## print(s)
