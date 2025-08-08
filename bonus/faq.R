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
url <- "https://raw.githubusercontent.com/vincentarelbundock/marginaleffects/main/data-raw/supported_models.csv"
dat <- read.csv(url)
n_support <- nrow(dat)


## -----------------------------------------------------------------------------
library(marginaleffects)
mod <- glm(am ~ vs + scale(drat), family = binomial, mtcars)

avg_comparisons(mod,
    variables = "vs",
    comparison = "lnoravg",
    type = "response")


## -----------------------------------------------------------------------------
fun <- function(hi, lo) log((mean(hi) / (1 - mean(hi))) / (mean(lo) / (1 - mean(lo))))

comparisons(mod,
    variables = "vs",
    comparison = fun,
    type = "response")


## -----------------------------------------------------------------------------
dat0 <- transform(mtcars, vs = 0)
dat1 <- transform(mtcars, vs = 1)

p0 <- predictions(mod, newdata = dat0, type = "response")
p1 <- predictions(mod, newdata = dat1, type = "response")

fun(p1$estimate, p0$estimate)


## -----------------------------------------------------------------------------
p <- avg_predictions(mod, by = "vs", type = "response")
fun(p$estimate[2], p$estimate[1])


## -----------------------------------------------------------------------------
dat0 <- subset(mtcars, vs == 0)
dat1 <- subset(mtcars, vs == 1)

p0 <- predictions(mod, newdata = dat0, type = "response")
p1 <- predictions(mod, newdata = dat1, type = "response")

fun(p1$estimate, p0$estimate)


## -----------------------------------------------------------------------------
#| error: true
try({
library(lubridate)
library(marginaleffects)
set.seed(123)

N <- 100
Date <- ymd("2024-01-01") + days(sample(0:30, N, replace = TRUE))
Days <- as.integer(Date)
Response <- pi * as.integer(Date) + rnorm(N)

mod <- lm(Response ~ Date)
predict(mod) |> head()
avg_slopes(mod)

mod <- lm(Response ~ Days)
predict(mod) |> head()
avg_slopes(mod)
})

