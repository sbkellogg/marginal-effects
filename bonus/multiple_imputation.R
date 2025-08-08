## ----include = FALSE----------------------------------------------------------
options(width = 1000)
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
set.seed(1024)

dat <- iris
dat$Sepal.Length[sample(seq_len(nrow(iris)), 40)] <- NA
dat$Sepal.Width[sample(seq_len(nrow(iris)), 40)] <- NA
dat$Species[sample(seq_len(nrow(iris)), 40)] <- NA


## -----------------------------------------------------------------------------
library(mice)

dat_mice <- mice(dat, m = 20, printFlag = FALSE, .Random.seed = 1024)


## -----------------------------------------------------------------------------
mod_mice <- with(dat_mice, lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species))


## -----------------------------------------------------------------------------
mfx_mice <- avg_slopes(mod_mice, by = "Species")
mfx_mice


## ----include = FALSE----------------------------------------------------------
## no startup messages
library(Amelia)

## ----message = FALSE, warning = FALSE-----------------------------------------
library(Amelia)

dat_amelia <- amelia(dat, m = 20, noms = "Species", p2s = 0)


## -----------------------------------------------------------------------------
mod_amelia <- with(dat_amelia, lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species))


## -----------------------------------------------------------------------------
mfx_amelia <- avg_slopes(mod_amelia, by = "Species")
mfx_amelia


## ----include = FALSE----------------------------------------------------------
## no startup messages
library(miceadds)
library(missRanger)


## ----message = FALSE, warning = FALSE-----------------------------------------
library(miceadds)
library(missRanger)

## convert lists of imputed datasets to `mids` objects
dat_missRanger <- replicate(20, missRanger(dat, verbose = 0), simplify = FALSE)
mids_missRanger <- datlist2mids(dat_missRanger)

## fit models
mod_missRanger <- with(mids_missRanger, lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species))

## `missRanger` slopes
mfx_missRanger <- avg_slopes(mod_missRanger, by = "Species")
mfx_missRanger


## -----------------------------------------------------------------------------
library(modelsummary)

## listwise deletion slopes
mod_lwd <- lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species, data = dat)
mfx_lwd <- avg_slopes(mod_lwd, by = "Species")

## regression table
models <- list(
    "LWD" = mfx_lwd,
    "mice" = mfx_mice,
    "Amelia" = mfx_amelia,
    "missRanger" = mfx_missRanger)
modelsummary(models, shape = term : contrast + Species ~ model)


## -----------------------------------------------------------------------------
set.seed(1024)
library(mice)
library(fixest)
library(marginaleffects)

dat <- mtcars

## insert missing values
dat$hp[sample(seq_len(nrow(mtcars)), 10)] <- NA
dat$mpg[sample(seq_len(nrow(mtcars)), 10)] <- NA
dat$gear[sample(seq_len(nrow(mtcars)), 10)] <- NA

## multiple imputation
dat <- mice(dat, m = 5, method = "sample", printFlag = FALSE)
dat <- complete(dat, action = "all")

## fit models
mod <- lapply(dat, \(x) 
    feglm(am ~ mpg * cyl + hp,
        weight = ~gear,
        family = binomial,
        data = x))

## slopes without weights
lapply(seq_along(mod), \(i) 
    avg_slopes(mod[[i]], newdata = dat[[i]])) |>
    mice::pool()

## slopes with weights
lapply(seq_along(mod), \(i) 
    avg_slopes(mod[[i]], newdata = dat[[i]], wts = "gear")) |>
    mice::pool()

