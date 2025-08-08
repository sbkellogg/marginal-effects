## ----include = FALSE----------------------------------------------------------
options(width = 1000)
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 6,
  fig.asp = .4,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)
library("tidyverse")
library("kableExtra")


## -----------------------------------------------------------------------------
library(marginaleffects)

dat <- mtcars
dat$cyl <- as.factor(dat$cyl)
dat$am <- as.logical(dat$am)
mod <- lm(mpg ~ hp + cyl + am, data = dat)


## -----------------------------------------------------------------------------
p <- predictions(
    mod,
    newdata = datagrid(am = unique, cyl = unique))


## ----echo = FALSE, message = FALSE, warning = FALSE---------------------------
library(tidyverse)
library(kableExtra)
pred <- p |>
    select(cyl, am, estimate) |>
    pivot_wider(names_from = "am", values_from = "estimate") |>
    rowwise() |>
    mutate(`Marginal means by cyl` = mean(c(`TRUE`, `FALSE`)))
row <- data.frame(x = "Marginal means by am",
                  y = mean(pred[["TRUE"]]),
                  z = mean(pred[["FALSE"]]))
colnames(row) <- colnames(pred)[1:3]
pred <- bind_rows(pred, row)
for (i in 2:ncol(pred)) {
    pred[[i]] <- sprintf("%.1f", pred[[i]])
}
pred[pred == "NA"] <- ""
kbl(pred) |> 
    kable_styling() |>
    add_header_above(c(" " = 1, "am" = 2, " " = 1))


## -----------------------------------------------------------------------------
predictions(
    mod,
    by = "am",
    newdata = datagrid(am = unique, cyl = unique))


## -----------------------------------------------------------------------------
predictions(
    mod,
    by = "cyl",
    newdata = "balanced")


## -----------------------------------------------------------------------------
predictions(
    mod,
    by = c("am", "cyl"),
    newdata = "balanced")


## -----------------------------------------------------------------------------
library(emmeans)
emmeans(mod, specs = ~cyl)
emmeans(mod, specs = ~cyl + am)


## -----------------------------------------------------------------------------
library(ggplot2)

mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)

p <- predictions(mod,
    by = "hp",
    newdata = datagrid(
        model = mod,
        hp = seq(100, 120, length.out = 10),
        cyl = mtcars$cyl))

ggplot(p) +
    geom_ribbon(aes(hp, ymin = conf.low, ymax = conf.high), alpha = .2) +
    geom_line(aes(hp, estimate))

