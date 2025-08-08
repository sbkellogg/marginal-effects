## ----include = FALSE----------------------------------------------------------
options(width = 1000)
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = FALSE,
  fig.width = 9,
  fig.asp = .4,
  out.width = "100%",
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

library(ggplot2)

theme_clean <- function() {
  theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(size = rel(1), hjust = 0),
          strip.background = element_blank(),
          legend.position = "bottom")
}
ggplot2::theme_set(theme_clean())


source("https://raw.githubusercontent.com/vincentarelbundock/marginaleffects/5c38b0b7673d5e992aa80022d1001905c49cc755/inst/tinytest/helpers.R")


## ----eval = FALSE-------------------------------------------------------------
# predictions(mod, type = "prediction", ndraws = 10, re_formula = NA)


## ----eval = FALSE-------------------------------------------------------------
# ?brms::posterior_epred
# ?brms::posterior_linpred
# ?brms::posterior_predict


## ----eval = FALSE-------------------------------------------------------------
# library(brms)
# library(posterior)
# library(marginaleffects)
# mod <- brm(survived ~ woman * age + passengerClass,
#            family = bernoulli(link = "logit"),
#            data = dat)

## ----eval = TRUE, include = FALSE---------------------------------------------
library(brms)
library(posterior)
library(marginaleffects)
mod <- marginaleffects:::modelarchive_model("brms_titanic_int")


## -----------------------------------------------------------------------------
#| eval: true
pred <- avg_predictions(mod, by = "passengerClass")


## -----------------------------------------------------------------------------
#| eval: true
draws <- get_draws(pred, "rvar")

# expected value of the posterior distribution
E(draws$rvar)

# quantiles of the posterior distribution
quantile2(draws$rvar, c(0.05, 0.5, 0.95))

# mass of the posterior above 0.5
Pr(draws$rvar > 0.5)


## ----message = FALSE----------------------------------------------------------
library(marginaleffects)
library(brms)
library(ggplot2)
library(ggdist)

dat <- get_dataset("Titanic", "Stat2Data")
dat$survived <- ifelse(dat$survived == "yes", 1, 0)
dat$woman <- ifelse(dat$sex == "female", 1, 0)


## ----eval = FALSE-------------------------------------------------------------
# mod <- brm(survived ~ woman * age + passengerClass,
#            family = bernoulli(link = "logit"),
#            data = dat)


## ----include = FALSE----------------------------------------------------------
mod <- marginaleffects:::modelarchive_model("brms_titanic_int")


## -----------------------------------------------------------------------------
predictions(mod)


## -----------------------------------------------------------------------------
plot_predictions(mod, condition = "age")


## -----------------------------------------------------------------------------
pred <- predictions(mod,
                    newdata = datagrid(woman = 0:1,
                                       passengerClass = c("1st", "2nd", "3rd")))
pred


## -----------------------------------------------------------------------------
pred <- get_draws(pred)
head(pred)


## -----------------------------------------------------------------------------
ggplot(pred, aes(x = draw, fill = factor(woman))) +
    geom_density() +
    facet_grid(~ passengerClass, labeller = label_both) +
    labs(x = "Predicted probability of survival", y = "", fill = "Woman")


## -----------------------------------------------------------------------------
mfx <- slopes(mod)
mfx


## -----------------------------------------------------------------------------
slopes(
    mod,
    newdata = datagrid(
        woman = 1,
        passengerClass = "1st"))


## -----------------------------------------------------------------------------
plot_slopes(mod, variables = "woman", condition = "age")


## -----------------------------------------------------------------------------
draws <- get_draws(mfx)

dim(draws)

head(draws)


## -----------------------------------------------------------------------------
mfx <- slopes(mod,
    variables = "age",
    newdata = datagrid(woman = 0:1)) |>
    get_draws()

ggplot(mfx, aes(x = draw, fill = factor(woman))) +
    stat_halfeye(slab_alpha = .5) +
    labs(x = "Marginal Effect of Age on Survival",
         y = "Posterior density",
         fill = "Woman")


## ----message = FALSE----------------------------------------------------------
library(brms)
library(ggdist)
library(patchwork)
library(marginaleffects)

vdem_2015 <- read.csv("https://github.com/vincentarelbundock/marginaleffects/raw/main/data-raw/vdem_2015.csv")

head(vdem_2015)


## ----eval = FALSE-------------------------------------------------------------
# mod <- brm(
#   bf(media_index ~ party_autonomy + civil_liberties + (1 | region),
#      phi ~ (1 | region)),
#   data = vdem_2015,
#   family = Beta(),
#   control = list(adapt_delta = 0.9))


## ----include = FALSE----------------------------------------------------------
mod <- marginaleffects:::modelarchive_model("brms_vdem")


## -----------------------------------------------------------------------------
nd = datagrid(model = mod,
              party_autonomy = c(TRUE, FALSE),
              civil_liberties = .5,
              region = "Middle East and North Africa")
p1 <- predictions(mod, type = "response", newdata = nd) |>
    get_draws() |>
    transform(type = "Response")
p2 <- predictions(mod, type = "prediction", newdata = nd) |>
    get_draws() |>
    transform(type = "Prediction")
pred <- rbind(p1, p2)


## -----------------------------------------------------------------------------
ggplot(pred, aes(x = draw, fill = party_autonomy)) +
    stat_halfeye(alpha = .5) +
    facet_wrap(~ type) +
    labs(x = "Media index (predicted)", 
         y = "Posterior density",
         fill = "Party autonomy")


## -----------------------------------------------------------------------------
mfx <- slopes(mod,
                       newdata = datagrid(civil_liberties = .5,
                                          region = "Middle East and North Africa"))
mfx


## -----------------------------------------------------------------------------
mfx <- get_draws(mfx)

ggplot(mfx, aes(x = draw, y = term)) +
  stat_halfeye() +
  labs(x = "Marginal effect", y = "")


## ----fig.asp = .8-------------------------------------------------------------
plot_slopes(mod,
         variables = "civil_liberties",
         condition = "party_autonomy")


## ----fig.asp = .6-------------------------------------------------------------
pred <- predictions(mod,
                    newdata = datagrid(party_autonomy = FALSE,
                                       region = "Middle East and North Africa",
                                       civil_liberties = seq(0, 1, by = 0.05))) |>
        get_draws()

ggplot(pred, aes(x = civil_liberties, y = draw)) +
    stat_lineribbon() +
    scale_fill_brewer(palette = "Reds") +
    labs(x = "Civil liberties",
         y = "Media index (predicted)",
         fill = "")


## -----------------------------------------------------------------------------
mfx <- slopes(mod,
    newdata = datagrid(
        civil_liberties = c(.2, .5, .8),
        party_autonomy = FALSE,
        region = "Middle East and North Africa"),
    variables = "civil_liberties")
mfx


## -----------------------------------------------------------------------------
mfx <- get_draws(mfx)

ggplot(mfx, aes(x = draw, fill = factor(civil_liberties))) +
    stat_halfeye(slab_alpha = .5) +
    labs(x = "Marginal effect of Civil Liberties on Media Index",
         y = "Posterior density",
         fill = "Civil liberties")


## -----------------------------------------------------------------------------
mfx <- slopes(
    mod,
    newdata = datagrid(
        civil_liberties = c(.2, .5, .8),
        party_autonomy = FALSE,
        region = "Middle East and North Africa"),
    variables = "civil_liberties",
    re_formula = NA) |>
    get_draws()

ggplot(mfx, aes(x = draw, fill = factor(civil_liberties))) +
    stat_halfeye(slab_alpha = .5) +
    labs(x = "Marginal effect of Civil Liberties on Media Index",
         y = "Posterior density",
         fill = "Civil liberties")


## ----eval = FALSE, include = FALSE--------------------------------------------
# library(emmeans)
# emtrends(mod,
#          ~ civil_liberties,
#          var = "civil_liberties",
#          at = list(party_autonomy = FALSE,
#                    civil_liberties = c(.2, .5, .8),
#                    region = "Middle East and North Africa"),
#          transform = "response")


## ----fig.width = 9------------------------------------------------------------
pred <- predictions(
    mod,
    re_formula = NA,
    newdata = datagrid(party_autonomy = c(TRUE, FALSE))) |>
    get_draws()

mfx <- slopes(
    mod,
    re_formula = NA,
    variables = "party_autonomy") |>
    get_draws()

plot1 <- ggplot(pred, aes(x = draw, fill = party_autonomy)) +
         stat_halfeye(slab_alpha = .5) +
         labs(x = "Media index (Predicted)",
              y = "Posterior density",
              fill = "Party autonomy")

plot2 <- ggplot(mfx, aes(x = draw)) +
         stat_halfeye(slab_alpha = .5)  +
         labs(x = "Contrast: Party autonomy TRUE - FALSE",
              y = "",
              fill = "Party autonomy")

## combine plots using the `patchwork` package
plot1 + plot2


## ----out.width = "100%", fig.width = 9----------------------------------------
pred <- predictions(mod,
                    newdata = datagrid(region = vdem_2015$region,
                                       party_autonomy = FALSE, 
                                       civil_liberties = seq(0, 1, length.out = 100))) |> 
        get_draws()

ggplot(pred, aes(x = civil_liberties, y = draw)) +
    stat_lineribbon() +
    scale_fill_brewer(palette = "Reds") +
    facet_wrap(~ region) +
    labs(x = "Civil liberties",
         y = "Media index (predicted)",
         fill = "")


## ----out.width = "100%", fig.width = 9----------------------------------------
pred <- predictions(mod,
                    newdata = datagrid(region = vdem_2015$region,
                                       civil_liberties = c(.2, .8),
                                      party_autonomy = FALSE)) |>
        get_draws()

ggplot(pred, aes(x = draw, fill = factor(civil_liberties))) +
    stat_halfeye(slab_alpha = .5) +
    facet_wrap(~ region) +
    labs(x = "Media index (predicted)",
         y = "Posterior density",
         fill = "Civil liberties")


## ----fig.asp = .9-------------------------------------------------------------
pred <- predictions(mod,
                    newdata = datagrid(region = vdem_2015$region,
                                       party_autonomy = c(TRUE, FALSE),
                                       civil_liberties = .5)) |>
        get_draws()

ggplot(pred, aes(x = draw, y = region , fill = party_autonomy)) +
    stat_halfeye(slab_alpha = .5) +
    labs(x = "Media index (predicted)",
         y = "",
         fill = "Party autonomy")


## ----fig.asp = .9-------------------------------------------------------------
mfx <- slopes(
    mod,
    variables = "party_autonomy",
    newdata = datagrid(
        region = vdem_2015$region,
        civil_liberties = .5)) |>
    get_draws()

ggplot(mfx, aes(x = draw, y = region , fill = party_autonomy)) +
    stat_halfeye(slab_alpha = .5) +
    labs(x = "Media index (predicted)",
         y = "",
         fill = "Party autonomy")


## -----------------------------------------------------------------------------
dat <- data.frame(civil_liberties = .5,
                  party_autonomy = FALSE,
                  region = "New Region")

mfx <- slopes(
    mod,
    variables = "party_autonomy",
    allow_new_levels = TRUE,
    newdata = dat)

draws <- get_draws(mfx)

ggplot(draws, aes(x = draw)) +
     stat_halfeye() +
     labs(x = "Marginal effect of party autonomy in a generic world region", y = "")


## ----eval = FALSE-------------------------------------------------------------
# dat <- get_dataset("EmplUK", "plm")
# dat$x <- as.numeric(dat$output > median(dat$output))
# dat$y <- as.numeric(dat$emp > median(dat$emp))
# mod <- brm(y ~ x + (1 | firm), data = dat, backend = "cmdstanr", family = "bernoulli")


## ----include = FALSE----------------------------------------------------------
dat <- get_dataset("EmplUK", "plm")
dat$x <- as.numeric(dat$output > median(dat$output))
dat$y <- as.numeric(dat$emp > median(dat$emp))
mod <- marginaleffects:::modelarchive_model("brms_logit_re")


## -----------------------------------------------------------------------------
p <- predictions(mod, newdata = datagrid(x = 0, firm = unique))
head(p)


## -----------------------------------------------------------------------------
avg_predictions(mod, newdata = datagrid(x = 0, firm = unique))

predictions(mod, newdata = datagrid(x = 0:1, firm = unique), by = "x")


## -----------------------------------------------------------------------------
predictions(
    mod,
    newdata = datagrid(x = 0:1, firm = -1:-100),
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian",
    by = "x")


## -----------------------------------------------------------------------------
avg_comparisons(
    mod,
    newdata = datagrid(firm = -1:-100),
    allow_new_levels = TRUE,
    sample_new_levels = "gaussian")


## -----------------------------------------------------------------------------
library(brmsmargins)
bm <- brmsmargins(
  k = 100,
  object = mod,
  at = data.frame(x = c(0, 1)),
  CI = .95,
  CIType = "ETI",
  contrasts = cbind("AME x" = c(-1, 1)),
  effects = "integrateoutRE")
bm$ContrastSummary |> data.frame()


## ----eval = FALSE-------------------------------------------------------------
# dat <- get_dataset("Heating", "Ecdat")
# mod <- brm(depvar ~ ic.gc + oc.gc,
#            data = dat,
#            family = categorical(link = "logit"))


## ----include = FALSE----------------------------------------------------------
dat <- "https://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Heating.csv"
dat <- read.csv(dat)
mod <- marginaleffects:::modelarchive_model("brms_heating")


## -----------------------------------------------------------------------------
pred <- predictions(mod)

head(pred)


## -----------------------------------------------------------------------------
draws <- get_draws(pred)

ggplot(draws, aes(x = draw, fill = group)) +
    geom_density(alpha = .2, color = "white") +
    labs(x = "Predicted probability",
         y = "Density",
         fill = "Heating system")


## -----------------------------------------------------------------------------
plot_predictions(mod, condition = "oc.gc") +
    facet_wrap(~ group) +
    labs(y = "Predicted probability")


## -----------------------------------------------------------------------------
avg_slopes(mod)


## ----eval = FALSE-------------------------------------------------------------
# library(gapminder)
# library(brms)
# library(dplyr)
# library(ggplot2)
# library(ggdist)
# library(cmdstanr)
# library(patchwork)
# library(marginaleffects)
# 
# set.seed(1024)
# 
# CHAINS <- 4
# ITER <- 2000
# WARMUP <- 1000
# BAYES_SEED <- 1234
# 
# gapminder <- gapminder::gapminder |>
#   filter(continent != "Oceania") |>
#   # Make a bunch of GDP values 0
#   mutate(prob_zero = ifelse(lifeExp < 50, 0.3, 0.02),
#          will_be_zero = rbinom(n(), 1, prob = prob_zero),
#          gdpPercap = ifelse(will_be_zero, 0, gdpPercap)) |>
#   select(-prob_zero, -will_be_zero) |>
#   # Make a logged version of GDP per capita
#   mutate(log_gdpPercap = log1p(gdpPercap)) |>
#   mutate(is_zero = gdpPercap == 0)
# 
# mod <- brm(
#   bf(gdpPercap ~ lifeExp + year + (1 + lifeExp + year | continent),
#      hu ~ lifeExp),
#   data = gapminder,
#   backend = "cmdstanr",
#   family = hurdle_lognormal(),
#   cores = 2,
#   chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
#   silent = 2)


## ----include = FALSE----------------------------------------------------------
url <- "https://github.com/vincentarelbundock/modelarchive/raw/main/data/brms_hurdle_heiss.rds"
tmp <- tempfile()
download.file(url, tmp)
mod <- readRDS(tmp)
gapminder <- mod$data


## -----------------------------------------------------------------------------
predictions(mod) |> head()


## -----------------------------------------------------------------------------
predictions(mod, dpar = "hu") |> head()


## -----------------------------------------------------------------------------
predictions(mod, type = "link", dpar = "hu") |> head()


## -----------------------------------------------------------------------------
plot_predictions(
    mod,
    condition = "lifeExp") +
    labs(y = "mu") +
plot_predictions(
    mod,
    dpar = "hu",
    condition = "lifeExp") +
    labs(y = "hu")


## -----------------------------------------------------------------------------
plot_predictions(
    mod,
    re_formula = NULL,
    condition = c("lifeExp", "continent"))


## -----------------------------------------------------------------------------
predictions(
    mod,
    re_formula = NULL,
    newdata = datagrid(model = mod,
                       continent = gapminder$continent,
                       year = c(1952, 2007),
                       lifeExp = seq(30, 80, 1))) |>
    get_draws() |>
    ggplot(aes(lifeExp, draw, fill = continent, color = continent)) +
    stat_lineribbon(alpha = .25) +
    facet_grid(year ~ continent)


## -----------------------------------------------------------------------------
avg_comparisons(mod)


## -----------------------------------------------------------------------------
avg_comparisons(mod, variables = list(lifeExp = "sd"))


## -----------------------------------------------------------------------------
avg_comparisons(
    mod,
    variables = list(lifeExp = c(50, 60), year = "minmax"),
    cross = TRUE)


## -----------------------------------------------------------------------------
avg_comparisons(mod) |>
    get_draws() |>
    ggplot(aes(estimate, term)) +
    stat_dotsinterval() +
    labs(x = "Posterior distribution of average contrasts", y = "")


## -----------------------------------------------------------------------------
avg_slopes(mod)

avg_slopes(mod, type = "link")

avg_slopes(mod, dpar = "hu")

avg_slopes(mod, dpar = "hu", type = "link")


## -----------------------------------------------------------------------------
plot_slopes(
    mod,
    variables = "lifeExp",
    condition = "lifeExp") +
    labs(y = "mu") +

plot_slopes(
    mod,
    dpar = "hu",
    variables = "lifeExp",
    condition = "lifeExp") +
    labs(y = "hu")


## -----------------------------------------------------------------------------
comparisons(
    mod,
    type = "link",
    variables = "lifeExp",
    newdata = datagrid(lifeExp = c(40, 70), continent = gapminder$continent)) |>
    get_draws() |>
    ggplot(aes(draw, continent, fill = continent)) +
    stat_dotsinterval() +
    facet_grid(lifeExp ~ .) +
    labs(x = "Effect of a 1 unit change in Life Expectancy")


## -----------------------------------------------------------------------------
library(insight)
library(marginaleffects)

mod <- insight::download_model("brms_1")

options(marginaleffects_posterior_interval = "hdi")
options(marginaleffects_posterior_center = mean)
avg_comparisons(mod)

options(marginaleffects_posterior_interval = "eti")
options(marginaleffects_posterior_center = stats::median)
avg_comparisons(mod)


## ----results = "hide"---------------------------------------------------------
library(brms)
library(ggdist)
library(ggplot2)
library(marginaleffects)
mod <- brm(am ~ mpg + hp, data = mtcars, family = bernoulli)


## -----------------------------------------------------------------------------
avg_comparisons(mod) |>
  get_draws(shape = "rvar") |>
  ggplot(aes(y = term, xdist = rvar)) + 
  stat_slabinterval()


## ----include=FALSE------------------------------------------------------------
mod <- marginaleffects:::modelarchive_model("brms_numeric2")
dat <- mtcars


## -----------------------------------------------------------------------------
#| eval = FALSE
# mod <- brm(am ~ mpg + hp, data = mtcars, family = bernoulli(),
#            seed = 1024, silent = 2, chains = 4, iter = 1000)


## -----------------------------------------------------------------------------
avg_comparisons(mod)

comparisons(mod, comparison = "differenceavg")


## -----------------------------------------------------------------------------
comparisons(
    mod,
    comparison = "differenceavg",
    hypothesis = "b2 - b1 = 0.2")


## -----------------------------------------------------------------------------
avg_comparisons(mod, comparison = "differenceavg") |>
    get_draws(shape = "DxP") |>
    brms::hypothesis("b2 - b1 > .2")


## ----results = "hide", echo=FALSE---------------------------------------------
data(iris)
library(brms)
mod <- brm(bf(
    Sepal.Length ~ Sepal.Width * Petal.Length,
    sigma ~ Sepal.Width * Petal.Length), 
    family = gaussian(), data = iris,
    backend = "cmdstanr")


## ----eval = FALSE-------------------------------------------------------------
# data(iris)
# library(brms)
# mod <- brm(bf(
#     Sepal.Length ~ Sepal.Width * Petal.Length,
#     sigma ~ Sepal.Width * Petal.Length),
#     family = gaussian(), data = iris,
#     backend = "cmdstanr")


## -----------------------------------------------------------------------------
avg_predictions(mod)

avg_predictions(mod, dpar = "sigma")

avg_slopes(mod, dpar = "sigma")


## ----eval = FALSE-------------------------------------------------------------
# library(marginaleffects)
# data("ChickWeight")
# 
# mod = brm(data = ChickWeight,
#           weight ~ Time * Diet + (Time|Chick),
#           seed = 123,
#           backend = "cmdstanr")
# 
# # NA
# comparisons(mod,
#     variables = "Time",
#     by = "Diet",
#     re_formula = NA)
# 
# d0 <- ChickWeight
# d1 <- transform(d0, Time = Time + 1)
# p0 <- posterior_epred(mod, newdata = d0, re_formula = NA)
# p1 <- posterior_epred(mod, newdata = d1, re_formula = NA)
# p <- p1 - p0
# cmp <- apply(p, 1, function(x) tapply(x, ChickWeight$Diet, mean))
# apply(cmp, 1, quantile, prob = .025)
# 
# # NULL
# comparisons(mod,
#     variables = "Time",
#     by = "Diet",
#     re_formula = NULL)
# 
# d0 <- ChickWeight
# d1 <- transform(d0, Time = Time + 1)
# p0 <- posterior_epred(mod, newdata = d0, re_formula = NULL)
# p1 <- posterior_epred(mod, newdata = d1, re_formula = NULL)
# p <- p1 - p0
# cmp <- apply(p, 1, function(x) tapply(x, ChickWeight$Diet, mean))
# apply(cmp, 1, quantile, prob = .025)

