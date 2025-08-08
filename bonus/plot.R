## ----include = FALSE----------------------------------------------------------
## this vignette is in .Rbuildignore

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
## libraries
library(ggplot2)
library(patchwork) # combine plots with the + and / signs
library(marginaleffects)

## visual theme
theme_set(theme_minimal())
okabeito <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7', '#999999', '#000000')
options(ggplot2.discrete.fill = okabeito)
options(ggplot2.discrete.colour = okabeito)
options(width = 1000)

## download data
dat <- get_dataset("penguins", "palmerpenguins")

mod <- lm(body_mass_g ~ flipper_length_mm * species * bill_length_mm + island, data = dat)


## import statsmodels.formula.api as smf
## from marginaleffects import *
## from plotnine import *
## import polars as pl
## 
## # visual theme
## theme_set(theme_minimal())
## 
## dat = pl.read_csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
## 
## mod = smf.ols(
##   "body_mass_g ~ flipper_length_mm * species * bill_length_mm + island",
##   data = dat.to_pandas()).fit()

## -----------------------------------------------------------------------------
pre <- predictions(mod, newdata = datagrid(flipper_length_mm = c(172, 231), species = unique))
pre


## pre = predictions(
##   mod,
##   newdata = datagrid(
##     flipper_length_mm = [172, 231],
##     species = dat["species"].unique(),
##     model = mod)
## )
## pre

## -----------------------------------------------------------------------------
plot_predictions(mod, condition = c("flipper_length_mm", "species"))


## plot_predictions(mod, condition = ["flipper_length_mm", "species"])

## -----------------------------------------------------------------------------
plot_predictions(
    mod,
    condition = list(
        "flipper_length_mm" = 180:220,
        "bill_length_mm" = "threenum",
        "species" = unique))


## plot_predictions(
##     mod,
##     condition = {
##         "flipper_length_mm": list(range(180, 221)),
##         "bill_length_mm": "threenum",
##         "species": dat["species"].unique(),
##         }
## )

## -----------------------------------------------------------------------------
predictions(mod, by = "species")


## -----------------------------------------------------------------------------
plot_predictions(mod, by = "species")


## -----------------------------------------------------------------------------
predictions(mod, by = c("species", "island"))


## -----------------------------------------------------------------------------
plot_predictions(mod, by = c("species", "island"))


## -----------------------------------------------------------------------------
comparisons(mod,
  variables = "flipper_length_mm",
  newdata = datagrid(flipper_length_mm = c(172, 231), species = unique))

plot_comparisons(mod,
  variables = "flipper_length_mm",
  condition = c("bill_length_mm", "species"))


## -----------------------------------------------------------------------------
plot_comparisons(mod,
  variables = list("flipper_length_mm" = "sd"),
  condition = c("bill_length_mm", "species")) +

plot_comparisons(mod,
  variables = list("flipper_length_mm" = 10),
  condition = c("bill_length_mm", "species"))


## -----------------------------------------------------------------------------
plot_comparisons(mod,
  variables = "species",
  condition = "bill_length_mm",
  comparison = "ratio")


## -----------------------------------------------------------------------------
plot_comparisons(mod,
  variables = "flipper_length_mm",
  by = "species") +

plot_comparisons(mod,
  variables = "flipper_length_mm",
  by = c("species", "island"))


## -----------------------------------------------------------------------------
plot_comparisons(mod,
  variables = c("flipper_length_mm", "bill_length_mm"),
  by = c("species", "island"))


## -----------------------------------------------------------------------------
## conditional
plot_slopes(mod,
  variables = "bill_length_mm",
  slope = "eyex",
  condition = c("species", "island"))

## marginal
plot_slopes(mod,
  variables = "bill_length_mm",
  slope = "eyex",
  by = c("species", "island"))


## -----------------------------------------------------------------------------
mod2 <- lm(mpg ~ wt * qsec * factor(gear), data = mtcars)

plot_slopes(mod2, variables = "qsec", condition = c("wt", "gear"))


## ----fig.asp = .5, warning = FALSE--------------------------------------------
plot_slopes(mod,
  variables = "bill_length_mm", condition = "flipper_length_mm") +
  ylim(c(-150, 200)) +

## clustered standard errors
plot_slopes(mod,
  vcov = ~island,
  variables = "bill_length_mm", condition = "flipper_length_mm") +
  ylim(c(-150, 200)) +

## alpha level
plot_slopes(mod,
  conf_level = .8,
  variables = "bill_length_mm", condition = "flipper_length_mm") +
  ylim(c(-150, 200))


## ----fig.asp = .7-------------------------------------------------------------
library(ggrepel)

mt <- mtcars
mt$label <- row.names(mt)

mod <- lm(mpg ~ hp * factor(cyl), data = mt)

plot_predictions(mod, condition = c("hp", "cyl"), points = .5, rug = TRUE, vcov = FALSE) +
    geom_text_repel(aes(x = hp, y = mpg, label = label),
                    data = subset(mt, hp > 250),
                    nudge_y = 2) +
    theme_classic()


## ----warning = FALSE----------------------------------------------------------
library(ggdist)
library(ggplot2)

dat <- get_dataset("Titanic", "Stat2Data")

mod <- glm(Survived ~ Age * SexCode * PClass, data = dat, family = binomial)

plot_predictions(mod, condition = c("Age", "PClass")) +
    geom_dots(
        alpha = .8,
        scale = .3,
        pch = 18,
        data = dat, aes(
        x = Age,
        y = Survived,
        side = ifelse(Survived == 1, "bottom", "top")))


## -----------------------------------------------------------------------------
p <- plot_predictions(mod, condition = c("Age", "PClass"), draw = FALSE)
head(p)


## -----------------------------------------------------------------------------
library(ggdist)
library(distributional)
plot_slopes(mod, variables = "SexCode", condition = c("Age", "PClass"), type = "link", draw = FALSE) |>
  ggplot() +
  stat_lineribbon(aes(
    x = Age,
    ydist = dist_normal(mu = estimate, sigma = std.error),
    fill = PClass),
    alpha = 1 / 4)


## -----------------------------------------------------------------------------
dat <- get_dataset("Titanic", "Stat2Data")
mod <- glm(Survived ~ Age * PClass, data = dat, family = binomial)

plot_predictions(mod, condition = c("Age", "PClass")) +
    geom_smooth(data = dat, aes(Age, Survived), method = "lm", se = FALSE, color = "black") +
    geom_smooth(data = dat, aes(Age, Survived), se = FALSE, color = "black")


## -----------------------------------------------------------------------------
library(MASS)
mod <- polr(factor(gear) ~ mpg + hp, data = mtcars)

predictions(mod)


## ----message = FALSE----------------------------------------------------------
plot_predictions(mod, condition = c("mpg", "group"), type = "probs", vcov = FALSE)

plot_predictions(mod, condition = "mpg", type = "probs", vcov = FALSE) +
  facet_wrap(~ group)


## ----error = TRUE-------------------------------------------------------------
try({
mod <- lm(mpg ~ hp * wt * factor(cyl), data = mtcars)
p <- predictions(mod)
plot(p)
})


## -----------------------------------------------------------------------------
p <- avg_predictions(mod, by = "cyl")
p

p$estimate

p$std.error

p$conf.low


## -----------------------------------------------------------------------------
plot_predictions(mod, by = "cyl")

plot(p$cyl, p$estimate)

ggplot(p, aes(x = cyl, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange()

