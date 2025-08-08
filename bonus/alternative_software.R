## ----include = FALSE----------------------------------------------------------
options(width = 10000)
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 6,
  fig.asp = .4,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)


## ----eval = FALSE-------------------------------------------------------------
# model <- lm(y ~ x * z, data)
# 
# comparisons(
#   model,
#   variables = list(x = 1), # what is the effect of 1-unit change in x?
#   newdata = datagrid(z = -1:1) # when z is held at values -1, 0, or 1
# )


## -----------------------------------------------------------------------------
library(emmeans)
library(marginaleffects)

mod <- glm(vs ~ hp + factor(cyl), data = mtcars, family = binomial)


## -----------------------------------------------------------------------------
emm <- emmeans(mod, specs = "cyl")
contrast(emm, method = "revpairwise", adjust = "none", df = Inf)

comparisons(mod,
            type = "link",
            newdata = "mean",
            variables = list(cyl = "pairwise"))


## -----------------------------------------------------------------------------
emm <- emmeans(mod, specs = "cyl", regrid = "response")
contrast(emm, method = "trt.vs.ctrl1", adjust = "none", df = Inf, ratios = FALSE)

comparisons(mod, newdata = "mean")


## -----------------------------------------------------------------------------
library(dplyr)
library(lme4)
library(emmeans)

dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/lme4/VerbAgg.csv")
dat$woman <- as.numeric(dat$Gender == "F")

mod <- glmer(
    woman ~ btype * resp + situ + (1 + Anger | item),
    family = binomial,
    data = dat)

emmeans(mod, specs = "btype", by = "resp") |>
    contrast(method = "revpairwise", adjust = "none")


## -----------------------------------------------------------------------------
nd <- datagrid(
    model = mod,
    resp = dat$resp,
    situ = dat$situ,
    btype = dat$btype)
nrow(nd)


## -----------------------------------------------------------------------------
cmp <- comparisons(mod,
    variables = list("btype" = "pairwise"),
    newdata = nd,
    type = "link")
nrow(cmp)


## -----------------------------------------------------------------------------
avg_comparisons(mod,
    by = "resp",
    variables = list("btype" = "pairwise"),
    newdata = nd,
    type = "link")


## -----------------------------------------------------------------------------
mod <- glm(vs ~ hp + factor(cyl), data = mtcars, family = binomial)

emtrends(mod, ~hp, "hp", regrid = "response", at = list(cyl = 4))

slopes(mod, newdata = datagrid(cyl = 4))


## -----------------------------------------------------------------------------
emtrends(mod, ~hp, "hp", at = list(cyl = 4))

slopes(mod, type = "link", newdata = datagrid(cyl = 4))


## ----eval = FALSE-------------------------------------------------------------
# ## Example of examining a continuous x categorical interaction using emmeans and marginaleffects
# ## Authors: Cameron Patrick and Vincent Arel-Bundock
# 
# library(tidyverse)
# library(emmeans)
# library(marginaleffects)
# 
# ## use the mtcars data, set up am as a factor
# data(mtcars)
# mc <- mtcars |> mutate(am = factor(am))
# 
# ## fit a linear model to mpg with wt x am interaction
# m <- lm(mpg ~ wt*am, data = mc)
# summary(m)
# 
# ## 1. means for each level of am at mean wt.
# emmeans(m, "am")
# predictions(m, newdata = datagrid(am = 0:1))
# 
# ## 2. means for each level of am at wt = 2.5, 3, 3.5.
# emmeans(m, c("am", "wt"), at = list(wt = c(2.5, 3, 3.5)))
# predictions(m, newdata = datagrid(am = 0:1, wt = c(2.5, 3, 3.5))
# 
# ## 3. means for wt = 2.5, 3, 3.5, averaged over levels of am (implicitly!).
# emmeans(m, "wt", at = list(wt = c(2.5, 3, 3.5)))
# 
# ## same thing, but the averaging is more explicit, using the `by` argument
# predictions(
#   m,
#   newdata = datagrid(am = 0:1, wt = c(2.5, 3, 3.5)),
#   by = "wt")
# 
# ## 4. graphical version of 2.
# emmip(m, am ~ wt, at = list(wt = c(2.5, 3, 3.5)), CIs = TRUE)
# plot_predictions(m, condition = c("wt", "am"))
# 
# ## 5. compare levels of am at specific values of wt.
# ## this is a bit ugly because the emmeans defaults for pairs() are silly.
# ## infer = TRUE: enable confidence intervals.
# ## adjust = "none": begone, Tukey.
# ## reverse = TRUE: contrasts as (later level) - (earlier level)
# pairs(emmeans(m, "am", by = "wt", at = list(wt = c(2.5, 3, 3.5))),
#       infer = TRUE, adjust = "none", reverse = TRUE)
# 
# comparisons(
#   m,
#   variables = "am",
#   newdata = datagrid(wt = c(2.5, 3, 3.5)))
# 
# ## 6. plot of pairswise comparisons
# plot(pairs(emmeans(m, "am", by = "wt", at = list(wt = c(2.5, 3, 3.5))),
#       infer = TRUE, adjust = "none", reverse = TRUE))
# 
# ## Since `wt` is numeric, the default is to plot it as a continuous variable on
# ## the x-axis.  But not that this is the **exact same info** as in the emmeans plot.
# plot_comparisons(m, variables = "am", condition = "wt")
# 
# ## You of course customize everything, set draw=FALSE, and feed the raw data to feed to ggplot2
# p <- plot_comparisons(
#   m,
#   variables = "am",
#   condition = list(wt = c(2.5, 3, 3.5)),
#   draw = FALSE)
# 
# ggplot(p, aes(y = wt, x = comparison, xmin = conf.low, xmax = conf.high)) +
#   geom_pointrange()
# 
# ## 7. slope of wt for each level of am
# emtrends(m, "am", "wt")
# slopes(m, newdata = datagrid(am = 0:1))


## -----------------------------------------------------------------------------
library(margins)
library(marginaleffects)

mod <- lm(mpg ~ cyl + hp + wt, data = mtcars)

mar <- margins(mod)
summary(mar)

mfx <- slopes(mod)


## -----------------------------------------------------------------------------
head(data.frame(mar))

head(mfx)
nd <- data.frame(cyl = 4, hp = 110, wt = 3)


## -----------------------------------------------------------------------------
mar <- margins(mod, data = data.frame(prediction::mean_or_mode(mtcars)), unit_ses = TRUE)
data.frame(mar)

slopes(mod, newdata = "mean")


## -----------------------------------------------------------------------------
dat <- mtcars
dat$cyl <- factor(dat$cyl)
mod <- lm(mpg ~ cyl * hp + wt, data = mtcars)

mar <- margins(mod, at = list(cyl = c(4, 6, 8)))
summary(mar)

avg_slopes(
    mod,
    by = "cyl",
    newdata = datagrid(cyl = c(4, 6, 8)), grid_type = "counterfactual")


## -----------------------------------------------------------------------------
prediction::prediction(mod) |> head()

marginaleffects::predictions(mod) |> head()


## -----------------------------------------------------------------------------
library("marginaleffects")
mod <- lm(mpg ~ cyl + hp + wt, data = mtcars)
avg_slopes(mod)


## -----------------------------------------------------------------------------
mod <- lm(mpg ~ as.factor(cyl) * hp + wt, data = mtcars)

avg_slopes(
    mod,
    variables = "hp",
    by = "cyl",
    newdata = datagrid(cyl = c(4, 6, 8), grid_type = "counterfactual"))


## -----------------------------------------------------------------------------
mfx <- slopes(
    mod,
    variables = "hp",
    newdata = datagrid(cyl = c(4, 6, 8), grid_type = "counterfactual"))
aggregate(estimate ~ term + cyl, data = mfx, FUN = mean)


## -----------------------------------------------------------------------------
J <- attr(mfx, "jacobian")
J_mean <- aggregate(J, by = list(mfx$cyl), FUN = mean)
J_mean <- as.matrix(J_mean[, 2:ncol(J_mean)])
sqrt(diag(J_mean %*% vcov(mod) %*% t(J_mean)))


## -----------------------------------------------------------------------------
mod <- lm(mpg ~ as.factor(cyl) * hp + wt, data = mtcars)

predictions(
    mod,
    by = "cyl",
    newdata = datagrid(cyl = c(4, 6, 8), grid_type = "counterfactual"))

predictions(
    mod,
    newdata = datagrid(cyl = c(4, 6, 8), grid_type = "counterfactual")) |>
    group_by(cyl) |>
    summarize(AAP = mean(estimate))


## ----message = FALSE, warning = FALSE-----------------------------------------
library(brms)
library(brmsmargins)
library(marginaleffects)
library(data.table)
library(withr)
setDTthreads(5)
h <- 1e-4

void <- capture.output(
    bayes.logistic <- brm(
      vs ~ am + mpg, data = mtcars,
      family = "bernoulli", seed = 1234,
      silent = 2, refresh = 0,
      backend = "cmdstanr",
      chains = 4L, cores = 4L)
)


## -----------------------------------------------------------------------------
d1 <- d2 <- mtcars
d2$mpg <- d2$mpg + h
p1 <- posterior_epred(bayes.logistic, newdata = d1)
p2 <- posterior_epred(bayes.logistic, newdata = d2)
m <- (p2 - p1) / h
quantile(rowMeans(m), c(.5, .025, .975))


## -----------------------------------------------------------------------------
bm <- brmsmargins(
  bayes.logistic,
  add = data.frame(mpg = c(0, 0 + h)),
  contrasts = cbind("AME MPG" = c(-1 / h, 1 / h)),
  CI = 0.95,
  CIType = "ETI")
data.frame(bm$ContrastSummary)


## -----------------------------------------------------------------------------
avg_slopes(bayes.logistic) 


## ----message = FALSE, warning = TRUE------------------------------------------
d <- withr::with_seed(
  seed = 12345, code = {
    nGroups <- 100
    nObs <- 20
    theta.location <- matrix(rnorm(nGroups * 2), nrow = nGroups, ncol = 2)
    theta.location[, 1] <- theta.location[, 1] - mean(theta.location[, 1])
    theta.location[, 2] <- theta.location[, 2] - mean(theta.location[, 2])
    theta.location[, 1] <- theta.location[, 1] / sd(theta.location[, 1])
    theta.location[, 2] <- theta.location[, 2] / sd(theta.location[, 2])
    theta.location <- theta.location %*% chol(matrix(c(1.5, -.25, -.25, .5^2), 2))
    theta.location[, 1] <- theta.location[, 1] - 2.5
    theta.location[, 2] <- theta.location[, 2] + 1
    d <- data.table(
      x = rep(rep(0:1, each = nObs / 2), times = nGroups))
    d[, ID := rep(seq_len(nGroups), each = nObs)]

    for (i in seq_len(nGroups)) {
      d[ID == i, y := rbinom(
        n = nObs,
        size = 1,
        prob = plogis(theta.location[i, 1] + theta.location[i, 2] * x))
        ]
    }
    copy(d)
  })

void <- capture.output(
    mlogit <- brms::brm(
      y ~ 1 + x + (1 + x | ID), family = "bernoulli",
      data = d, seed = 1234,
      backend = "cmdstanr",
      silent = 2, refresh = 0,
      chains = 4L, cores = 4L)
)


## -----------------------------------------------------------------------------
bm <- brmsmargins(
  mlogit,
  add = data.frame(x = c(0, h)),
  contrasts = cbind("AME x" = c(-1 / h, 1 / h)),
  effects = "includeRE",
  CI = .95,
  CIType = "ETI")
data.frame(bm$ContrastSummary)

avg_slopes(mlogit)


## -----------------------------------------------------------------------------
bm <- brmsmargins(
  mlogit,
  add = data.frame(x = c(0, h)),
  contrasts = cbind("AME x" = c(-1 / h, 1 / h)),
  effects = "fixedonly",
  CI = .95,
  CIType = "ETI")
data.frame(bm$ContrastSummary)

avg_slopes(mlogit, re_formula = NA)


## ----message = FALSE, warning = FALSE-----------------------------------------
d <- withr::with_seed(
  seed = 12345, code = {
    nObs <- 1000L
    d <- data.table(
      grp = rep(0:1, each = nObs / 2L),
      x = rnorm(nObs, mean = 0, sd = 0.25))
    d[, y := rnorm(nObs,
                   mean = x + grp,
                   sd = exp(1 + x + grp))]
    copy(d)
  })

void <- capture.output(
    ls.fe <- brm(bf(
      y ~ 1 + x + grp,
      sigma ~ 1 + x + grp),
      family = "gaussian",
      data = d, seed = 1234,
      silent = 2, refresh = 0,
      backend = "cmdstanr",
      chains = 4L, cores = 4L)
)


## -----------------------------------------------------------------------------
bm <- brmsmargins(
  ls.fe,
  add = data.frame(x = c(0, h)),
  contrasts = cbind("AME x" = c(-1 / h, 1 / h)),
  CI = 0.95, CIType = "ETI",
  effects = "fixedonly")
data.frame(bm$ContrastSummary)

avg_slopes(ls.fe, re_formula = NA)


## -----------------------------------------------------------------------------
bm <- brmsmargins(
  ls.fe,
  at = data.frame(grp = c(0, 1)),
  contrasts = cbind("AME grp" = c(-1, 1)),
  CI = 0.95, CIType = "ETI", dpar = "sigma",
  effects = "fixedonly")
data.frame(bm$ContrastSummary)


## -----------------------------------------------------------------------------
avg_comparisons(
  ls.fe,
  variables = list(grp = 0:1),
  dpar = "sigma")


## -----------------------------------------------------------------------------
bm <- brmsmargins(
  ls.fe,
  add = data.frame(x = c(0, h)),
  contrasts = cbind("AME x" = c(-1 / h, 1 / h)),
  CI = 0.95, CIType = "ETI", dpar = "sigma",
  effects = "fixedonly")
data.frame(bm$ContrastSummary)

avg_slopes(ls.fe, dpar = "sigma", re_formula = NA)


## ----message = FALSE, warning = FALSE-----------------------------------------
#| eval: false
# library("mlr3verse")
# library("fmeffects")
# data("bikes", package = "fmeffects")
# task <- as_task_regr(x = bikes, id = "bikes", target = "count")
# forest <- lrn("regr.ranger")$train(task)


## -----------------------------------------------------------------------------
#| eval: false
# avg_comparisons(forest, variables = list(temp = 1), newdata = bikes)


## -----------------------------------------------------------------------------
#| eval: false
# mfx <- fme(model = forest, data = bikes, features = list("temp" = 1))
# print(mfx)


## -----------------------------------------------------------------------------
#| eval: false
# FUN <- function(x) data.frame(lo = x, hi = "misty")
# 
# avg_comparisons(
#   forest,
#   newdata = bikes,
#   variables = list(weather = FUN)
# )

