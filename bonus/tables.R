## ----include = FALSE----------------------------------------------------------
options(width = 1000)
## this vignette is in .Rbuildignore because lme4 is not available on old CRAN
## test machines.

knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 9,
  fig.asp = .4,
  out.width = "100%",
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)


## -----------------------------------------------------------------------------
library(modelsummary)
library(marginaleffects)

mod <- glm(am ~ wt + drat, family = binomial, data = mtcars)
mfx <- avg_slopes(mod)

modelsummary(mfx)


## -----------------------------------------------------------------------------
modelplot(mfx)


## We can display several models or marginal effects side by side:
## 
## ```{r}
## models <- list(
##     glm(am ~ drat, family = binomial, data = mtcars),
##     glm(am ~ wt + drat, family = binomial, data = mtcars))
## models[[1]] <- slopes(models[[1]])
## models[[2]] <- slopes(models[[2]], vcov = "HC3")
## modelsummary(models)
## ```

## -----------------------------------------------------------------------------
dat <- mtcars
dat$gear <- as.factor(dat$gear)
mod <- glm(vs ~ gear + mpg, data = dat, family = binomial)

cmp <- avg_comparisons(mod)
get_estimates(cmp)


## -----------------------------------------------------------------------------
modelsummary(cmp, shape = term + contrast ~ model)


## -----------------------------------------------------------------------------
mod <- lm(mpg ~ factor(cyl) + factor(gear), data = mtcars)
cmp <- avg_comparisons(
  mod,
  variables = c("gear", "cyl"),
  cross = TRUE)
get_estimates(cmp)


## -----------------------------------------------------------------------------
modelsummary(
  cmp,
  shape = contrast_gear + contrast_cyl ~ model)

