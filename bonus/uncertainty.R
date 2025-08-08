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


## ----message = FALSE----------------------------------------------------------
library(marginaleffects)
library(patchwork)
library(lme4)

dat <- mtcars
dat$cyl <- factor(dat$cyl)
dat$am <- as.logical(dat$am)
mod <- lmer(mpg ~ hp + am + (1 | cyl), data = dat)


## -----------------------------------------------------------------------------
slopes(mod, newdata = "mean")


## -----------------------------------------------------------------------------
slopes(mod,
                newdata = "mean",
                vcov = "kenward-roger")


## -----------------------------------------------------------------------------
plot_predictions(mod, condition = "hp", vcov = "satterthwaite")


## -----------------------------------------------------------------------------
dat <- get_dataset("penguins", "palmerpenguins")
dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
mod <- glm(large_penguin ~ bill_length_mm * flipper_length_mm + species, data = dat, family = binomial)


## -----------------------------------------------------------------------------
avg_slopes(mod, variables = "bill_length_mm")

options(marginaleffects_numDeriv = list(method = "Richardson"))
avg_slopes(mod, variables = "bill_length_mm")

options(marginaleffects_numDeriv = list(method = "simple", method.args = list(eps = 1e-3)))
avg_slopes(mod, variables = "bill_length_mm")

options(marginaleffects_numDeriv = list(method = "simple", method.args = list(eps = 1e-5)))
avg_slopes(mod, variables = "bill_length_mm")

options(marginaleffects_numDeriv = list(method = "simple", method.args = list(eps = 1e-7)))
avg_slopes(mod, variables = "bill_length_mm")


## -----------------------------------------------------------------------------
eps1 <- 1e-5 # slope
eps2 <- 1e-7 # delta method

s <- slopes(mod, newdata = head(dat, 3), variables = "bill_length_mm", eps = eps1)
print(s[, 1:5], digits = 6)


## -----------------------------------------------------------------------------
linkinv <- mod$family$linkinv

## increment the variable of interest by h
dat_hi <- transform(dat, bill_length_mm = bill_length_mm + eps1)

## model matrices: first 3 rows
mm_lo <- insight::get_modelmatrix(mod, data = dat)[1:3,]
mm_hi <- insight::get_modelmatrix(mod, data = dat_hi)[1:3,]

## predictions
p_lo <- linkinv(mm_lo %*% coef(mod))
p_hi <- linkinv(mm_hi %*% coef(mod))

## slopes
(p_hi - p_lo) / eps1


## -----------------------------------------------------------------------------
b_lo <- b_hi <- coef(mod)
b_hi[1] <- b_hi[1] + eps2

dydx_lo <- (linkinv(mm_hi %*% b_lo) - linkinv(mm_lo %*% b_lo)) / eps1
dydx_hi <- (linkinv(mm_hi %*% b_hi) - linkinv(mm_lo %*% b_hi)) / eps1
(dydx_hi - dydx_lo) / eps2


## -----------------------------------------------------------------------------
J <- attr(s, "jacobian")
J


## -----------------------------------------------------------------------------
sqrt(diag(J %*% vcov(mod) %*% t(J)))


## -----------------------------------------------------------------------------
print(s[, 1:5], digits = 7)


## -----------------------------------------------------------------------------
options(marginaleffects_numDeriv = NULL)


## -----------------------------------------------------------------------------
#| eval: false
# library(margins)
# margins(mod, variables = "bill_length_mm", data = head(dat, 3), unit_ses = TRUE)$SE_dydx_bill_length_mm
# 
# margins(mod, variables = "bill_length_mm", data = head(dat, 3), eps = 1e-4, unit_ses = TRUE)$SE_dydx_bill_length_mm
# 
# margins(mod, variables = "bill_length_mm", data = head(dat, 3), eps = 1e-5, unit_ses = TRUE)$SE_dydx_bill_length_mm


## -----------------------------------------------------------------------------
library(marginaleffects)
mod <- lm(mpg ~ hp * qsec, mtcars)

## On average CIs around unit-level estimates are:
pre1 <- predictions(mod)
as.numeric(mean(pre1$conf.high - pre1$conf.low))

## The CI of the average estimate is:
pre2 <- tidy(predictions(mod))
as.numeric(pre2$conf.high - pre2$conf.low)

## On average CIs around unit-level estimates are:
cmp1 <- comparisons(mod, variables = "hp")
mean(cmp1$conf.high - cmp1$conf.low)

## The CI of the average estimate is:
cmp2 <- comparisons(mod, variables = "hp", comparison = "differenceavg")
cmp2$conf.high - cmp2$conf.low

