## ----include = FALSE----------------------------------------------------------
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
library(marginaleffects)
mod <- lm(mpg ~ hp + wt, data = mtcars)

avg_slopes(mod)

avg_slopes(mod, slope = "eyex")

avg_slopes(mod, slope = "eydx")

avg_slopes(mod, slope = "dyex")

