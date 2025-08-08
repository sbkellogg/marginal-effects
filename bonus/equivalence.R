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
library(marginaleffects)
mod <- lm(mpg ~ hp + factor(gear), data = mtcars)


## import polars as pl
## import statsmodels.formula.api as smf
## from marginaleffects import *
## 
## mtcars = pl.read_csv("https://vincentarelbundock.github.io/Rdatasets/csv/datasets/mtcars.csv")
## mtcars = mtcars.cast({"gear" : pl.Utf8, "hp" : pl.Float64})
## 
## mod = smf.ols("mpg ~ hp + gear", data = mtcars.to_pandas()).fit()

## -----------------------------------------------------------------------------
p <- predictions(mod, newdata = "median")
p


## p = predictions(mod, newdata = "median")
## print(p)

## ----include = FALSE----------------------------------------------------------
options(width = 120)
hyp <- hypotheses(p, equivalence = c(17, 18))


## -----------------------------------------------------------------------------
hypotheses(p, equivalence = c(17, 18))


## # Not implemented yet

## -----------------------------------------------------------------------------
coef(mod)[4]


## mod.params['gear[T.5]']

## -----------------------------------------------------------------------------
hypotheses(mod, equivalence = c(5, 7))[4, ]


## h = pl.DataFrame(hypotheses(mod, equivalence = [5., 7.]))[2,:]
## print(h)

## ----include = FALSE----------------------------------------------------------
hyp <- hypotheses(mod, equivalence = c(5, 7))[4, "p.value.equiv"]


## -----------------------------------------------------------------------------
avg_slopes(mod, variables = "hp", equivalence = c(-.1, .1))


## s = pl.DataFrame(avg_slopes(mod, variables = "hp", equivalence = [-.1, .1]))
## print(s)

## ----include = FALSE----------------------------------------------------------
hyp <- avg_slopes(mod, variables = "hp", equivalence = c(-.1, .1))$p.value.equiv


## -----------------------------------------------------------------------------
int <- lm(mpg ~ hp * factor(gear), data = mtcars)


## inter = smf.ols("mpg ~ hp * gear", data = mtcars.to_pandas()).fit()

## -----------------------------------------------------------------------------
avg_comparisons(int, variables = "hp", by = "gear")


## print(avg_comparisons(inter, variables = "hp", by = "gear"))

## -----------------------------------------------------------------------------
avg_comparisons(int, variables = "hp", by = "gear",
    hypothesis = "pairwise")


## print(avg_comparisons(inter, variables = "hp", by = "gear",
##     hypothesis = "pairwise"))

## -----------------------------------------------------------------------------
avg_comparisons(int, variables = "hp", by = "gear",
    hypothesis = "pairwise",
    equivalence = c(-.1, .1))


## c = pl.DataFrame(
##     avg_comparisons(inter, variables = "hp", by = "gear",
##         hypothesis = "pairwise",
##         equivalence = [-.1, .1])
##     )
## print(c)

## -----------------------------------------------------------------------------
library(emmeans)

mod <- lm(log(conc) ~ source + factor(percent), data = pigs)

## {emmeans}
emmeans(mod, specs = "source") |>
    pairs() |>
    test(df = Inf,
         null = 0,
         delta = log(1.25),
         side = "equivalence",
         adjust = "none")

## {marginaleffects}
predictions(
    mod,
    newdata = "balanced",
    by = "source",
    hypothesis = "pairwise",
    equivalence = c(-log(1.25), log(1.25))
)


## -----------------------------------------------------------------------------
library(equivalence)

set.seed(1024)

## simulate data data
N <- 20
dat <- data.frame(
    y = rnorm(N),
    x = sample(c(rep(0, N / 2), rep(1, N / 2)), N))

## fit model
mod <- lm(y ~ x, data = dat)

## test with the {equivalence} package
e <- tost(
    x = dat$y[dat$x == 0],
    y = dat$y[dat$x == 1],
    epsilon = 10)
e

## test with {marginaleffects} package
h <- hypotheses(mod, equivalence = c(-10, 10), df = e$parameter)[2, ]
h

# identical p values
h$p.value.equiv |> as.vector()

e$tost.p.value |> as.vector()

