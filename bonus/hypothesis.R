## ----include = FALSE----------------------------------------------------------
options(width = 1000)
# this vignette is in .Rbuildignore because lme4 is not available on old CRAN
# test machines.

knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 9,
  fig.asp = .4,
  out.width = "100%",
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)
options(marginaleffects_print_style = "tinytable")
options("tinytable_theme_placement_latex_float" = "H")

## .table {
##   font-size: .8em;
## }
## .table td, .table th {
##   white-space: nowrap;
## }

## -----------------------------------------------------------------------------
library(marginaleffects)
mod <- glm(am ~ hp + drat, data = mtcars, family = binomial)


## -----------------------------------------------------------------------------
summary(mod)


## -----------------------------------------------------------------------------
hypotheses(mod, hypothesis = 6)


## -----------------------------------------------------------------------------
predictions(mod, newdata = "mean", type = "response")


## -----------------------------------------------------------------------------
predictions(mod, newdata = "mean", hypothesis = .5, type = "response")


## -----------------------------------------------------------------------------
predictions(mod, newdata = "mean", hypothesis = 0, type = "invlink(link)")
predictions(mod, newdata = "mean", hypothesis = 0, type = "link")


## -----------------------------------------------------------------------------
avg_comparisons(
    mod,
    variables = "hp",
    comparison = "ratio",
    hypothesis = 1) |>
    print(digits = 5)


## -----------------------------------------------------------------------------
library(marginaleffects)
mod <- lm(mpg ~ hp + wt + factor(cyl), data = mtcars)


## -----------------------------------------------------------------------------
hypotheses(mod)


## -----------------------------------------------------------------------------
hypotheses(mod, "hp = wt")


## -----------------------------------------------------------------------------
hypotheses(mod, "exp(hp + wt) = 0.1")


## -----------------------------------------------------------------------------
hypotheses(mod, "hp = wt", vcov = "HC3")


## -----------------------------------------------------------------------------
hypotheses(mod, "b2 = b3")


## -----------------------------------------------------------------------------
hypotheses(mod, hypothesis = "b* / b3 = 1")


## -----------------------------------------------------------------------------
hypotheses(mod, "`factor(cyl)6` = `factor(cyl)8`")


## -----------------------------------------------------------------------------
mod <- lm(mpg ~ am + vs, data = mtcars)

p <- predictions(
    mod,
    newdata = datagrid(am = 0:1, vs = 0:1))
p


## -----------------------------------------------------------------------------
hypotheses(p, hypothesis = "b1 = b2")


## -----------------------------------------------------------------------------
predictions(
    mod,
    hypothesis = "b1 = b2",
    newdata = datagrid(am = 0:1, vs = 0:1))

p$estimate[1] - p$estimate[2]


## -----------------------------------------------------------------------------
predictions(
    mod,
    hypothesis = "b1 + b2 = 30",
    newdata = datagrid(am = 0:1, vs = 0:1))

p$estimate[1] + p$estimate[2] - 30

predictions(
    mod,
    hypothesis = "(b2 - b1) / (b3 - b2) = 0",
    newdata = datagrid(am = 0:1, vs = 0:1))


## -----------------------------------------------------------------------------
mod <- lm(mpg ~ am + vs, data = mtcars)

cmp <- avg_comparisons(mod)
cmp


## -----------------------------------------------------------------------------
avg_comparisons(mod, hypothesis = "am = vs")


## -----------------------------------------------------------------------------
avg_comparisons(mod, hypothesis = "exp(am) - 2 * vs = -400")


## -----------------------------------------------------------------------------
#| cache: true
set.seed(1234)

avg_comparisons(mod, hypothesis = "exp(am) - 2 * vs = -400") |>
  inferences(method = "boot")


## -----------------------------------------------------------------------------
mod <- lm(mpg ~ qsec * hp, data = mtcars)

avg_slopes(mod, hypothesis = "10 * hp - qsec = 0")


## -----------------------------------------------------------------------------
dat <- within(mtcars, {
    gear = factor(gear)
    cyl = factor(cyl)
})
dat <- sort_by(dat, ~ gear + cyl + am)
mod <- lm(mpg ~ am * wt * factor(cyl), data = dat)

avg_predictions(mod, by = "gear")


## -----------------------------------------------------------------------------
avg_predictions(mod, 
    hypothesis = ~ sequential,
    by = "gear")

avg_predictions(mod, 
    hypothesis = ~ meandev,
    by = "gear")

avg_predictions(mod, 
    hypothesis = ratio ~ sequential,
    by = "gear")


## -----------------------------------------------------------------------------
avg_predictions(mod, 
    by = c("am", "gear")
)

avg_predictions(mod, 
    hypothesis = ~ sequential | am,
    by = c("am", "gear")
)


## -----------------------------------------------------------------------------
avg_comparisons(mod, 
    variables = "cyl",
    by = "am")

avg_comparisons(mod, 
    variables = "cyl",
    by = "am", 
    hypothesis = ~ sequential | contrast)

avg_comparisons(mod, 
    variables = "cyl",
    by = "am", 
    hypothesis = ~ sequential | am)


## -----------------------------------------------------------------------------
avg_slopes(mod,
    variables = "wt",
    by = "cyl",
    hypothesis = ~ I(mean(x[2:3])))


## -----------------------------------------------------------------------------
less_vs_8 <- function(x) {
  c("{8} - {4, 6}" = x[3] - mean(x[1:2]),
    "{8} / {4, 6}" = x[3] / mean(x[1:2]))
}

avg_slopes(mod,
    variables = "wt",
    by = "cyl",
    hypothesis = ~ I(less_vs_8(x)))


## -----------------------------------------------------------------------------
avg_slopes(mod,
    variables = "wt",
    by = c("cyl", "am"),
    hypothesis = ~ I(less_vs_8(x)) | am)


## -----------------------------------------------------------------------------
library("MASS")

mod <- polr(gear ~ cyl + hp, dat)

avg_comparisons(mod, variables = "cyl")


## -----------------------------------------------------------------------------
avg_comparisons(mod, 
    variables = "cyl",
    hypothesis = ~ sequential | group)


## -----------------------------------------------------------------------------
avg_comparisons(mod, 
    variables = "cyl",
    hypothesis = ~ sequential | contrast)


## -----------------------------------------------------------------------------
mean_rank <- function(x) {
  weighted.mean(c(3, 4, 5), w = x)
}
ranks <- avg_predictions(mod,
           variables = "cyl",
           hypothesis = ~ I(mean_rank(x)) | cyl)
ranks


## -----------------------------------------------------------------------------
hypotheses(ranks, ratio ~ pairwise)


## -----------------------------------------------------------------------------
dat <- transform(mtcars, gear = factor(gear), cyl = factor(cyl))

mod <- lm(wt ~ mpg * hp * cyl, data = dat)

hyp <- function(x) {
    data.frame(
        hypothesis = "Avg(Ŷ) = 2",
        estimate = mean(x$estimate) - 2
    )
}
predictions(mod, hypothesis = hyp)


## -----------------------------------------------------------------------------
#| messages: false
library(MASS)
library(dplyr)

mod <- polr(gear ~ cyl + hp, dat)

avg_predictions(mod)


## -----------------------------------------------------------------------------
fun <- function(x) {
    out <- x |> 
        mutate(term = ifelse(group %in% 3:4, "3 & 4", "5")) |>
        summarize(estimate = mean(estimate), .by = term)
    return(out)
}
avg_predictions(mod, hypothesis = fun)


## -----------------------------------------------------------------------------
fun <- function(x) {
    out <- x |> 
        mutate(term = ifelse(group %in% 3:4, "3 & 4", "5")) |>
        summarize(estimate = mean(estimate), .by = term) |>
        summarize(estimate = diff(estimate), term = "5 - (3 & 4)")
    return(out)
}
avg_predictions(mod, hypothesis = fun)


## -----------------------------------------------------------------------------
avg_comparisons(mod, variables = "hp")


## -----------------------------------------------------------------------------
fun <- function(x) {
    x |> 
    mutate(estimate = (estimate - lag(estimate)),
           group = sprintf("%s - %s", group, lag(group))) |>
    filter(!is.na(estimate))
}
avg_comparisons(mod, variables = "hp", hypothesis = fun)


## -----------------------------------------------------------------------------
avg_comparisons(mod, variables = "hp", by = "cyl")


## -----------------------------------------------------------------------------
fun <- function(x) {
    x |> 
    mutate(estimate = (estimate - lag(estimate)),
           group = sprintf("%s - %s", group, lag(group)), 
           .by = "cyl") |>
    filter(!is.na(estimate))
}
avg_comparisons(mod, variables = "hp", by = "cyl", hypothesis = fun)


## ## `specify_hypothesis()`
## 
## Supplying a function to the `hypothesis` argument is a powerful strategy, but it can sometimes be a bit tedious, because there is a lot of "boilerplate" code to write. `specify_hypothesis()` is an experimental function which handles a lot of the annoying work automatically for us: label creation, group-wise estimates, etc.
## 
## To begin, we estimate a simple model and make predictions on a grid wih 6 rows:
## 
## ```{r}
## dat <- transform(mtcars, gear = factor(gear))
## mod <- lm(hp ~ mpg * am * factor(cyl), data = dat)
## 
## nd <- datagrid(am = 0:1, cyl = sort(unique(dat$cyl)), model = mod)
## 
## predictions(mod, newdata = nd)
## ```
## 
## By default, `specify_hypothesis()` computes a `reference` hypothesis, that is, the difference between each row and the first row in the output (`rowid[1]`):
## 
## ```{r}
## hyp <- specify_hypothesis()
## predictions(mod, newdata = nd, hypothesis = hyp)
## ```
## 
## Alternatively, we can specify a `"sequential"` hypothesis to compare each row to its previous one:
## 
## 
## ```{r}
## hyp <- specify_hypothesis(hypothesis = ~sequential)
## predictions(mod, newdata = nd, hypothesis = hyp)
## ```
## 
## A more powerful customization option is to supply one's own comparison function, along with a function to create labels. Compare each row to the global mean:
## 
## ```{r}
## hyp <- specify_hypothesis(
##     hypothesis = \(x) x - mean(x),
##     label = \(x) sprintf("%s - ȳ", x),
##     label_columns = "rowid"
## )
## predictions(mod, newdata = nd, hypothesis = hyp)
## ```
## 
## The ratio of each row to the first:
## 
## ```{r}
## hyp <- specify_hypothesis(
##     hypothesis =  \(x) x / x[1],
##     label = \(x) sprintf("(%s) / (%s)", x, x[1])
## )
## predictions(mod, newdata = nd, hypothesis = hyp)
## ```
## 
## We can use the `by` argument to specify subgroups in which to apply the function. For example, we may want to compute the ratio of each row to the reference (first) row of each subgroup of `am`. Notice that the denomiator `rowid` changes depending on the `am` subgroup:
## 
## ```{r}
## hyp <- specify_hypothesis(
##     by = "am",
##     hypothesis =  \(x) x / x[1],
##     label = \(x) sprintf("(%s) / (%s)", x, x[1])
## )
## predictions(mod, newdata = nd, hypothesis = hyp)
## ```
## 
## ###  `term`, `contrast`, and `group`
## 
## By default, `specify_hypothesis()` will always apply comparison functions within subgroups of the `term`, `contrast`, and `group` columns. For example:
## 
## ```{r}
## library(MASS)
## dat <- transform(mtcars, gear = factor(gear), cyl = factor(cyl))
## mod <- polr(gear ~ cyl + hp, dat, Hess = TRUE)
## 
## avg_predictions(mod, by = "am")
## 
## hyp <- specify_hypothesis()
## avg_predictions(mod, by = "am", hypothesis = hyp)
## ```
## 
## To avoid applying the function within `group` and compare group levels to one another, we can specify `by="rowid"`:
## 
## ```{r}
## avg_predictions(mod)
## 
## hyp <- specify_hypothesis(
##     by = "rowid",
##     hypothesis = ~sequential,
##     label_columns = "group"
## )
## avg_predictions(mod, hypothesis = hyp)
## ```
## 

## -----------------------------------------------------------------------------
mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)

fun <- function(x) {
    b <- coef(x)
    out <- data.frame(
        term = "hp + mpg = 2",
        estimate = b["hp"] + b["mpg"] - 2,
        row.names = NULL
    )
    return(out)
}

hypotheses(mod, hypothesis = fun)


## -----------------------------------------------------------------------------
fun <- function(x) {
    p <- predict(x, newdata = mtcars)
    out <- data.frame(term = "pred[2] = pred[3]", estimate = p[2] - p[3])
    return(out)
}
hypotheses(mod, hypothesis = fun)


## -----------------------------------------------------------------------------
#| messages: false
library(MASS)
library(dplyr)

dat <- transform(mtcars, 
    gear = factor(gear),
    cyl = factor(cyl))
mod <- polr(gear ~ cyl + hp, dat)

predictions(mod)


## -----------------------------------------------------------------------------
fun <- function(x) {
    predictions(x, vcov = FALSE) |>
        # label the new categories of outcome levels
        mutate(group = ifelse(group %in% c("3", "4"), "3 & 4", "5")) |>
        # sum of probabilities at the individual level
        summarize(estimate = sum(estimate), .by = c("rowid", "cyl", "group")) |>
        # average probabilities for each value of `cyl`
        summarize(estimate = mean(estimate), .by = c("cyl", "group")) |>
        # the `FUN` argument requires a `term` column
        rename(term = cyl)
}

hypotheses(mod, hypothesis = fun)


## -----------------------------------------------------------------------------
library(marginaleffects)
library(emmeans)
library(nnet)

dat <- mtcars
dat$carb <- factor(dat$carb)
dat$cyl <- factor(dat$cyl)
dat$am <- as.logical(dat$am)
dat <- sort_by(dat, ~carb)

mod <- lm(mpg ~ carb + cyl, dat)

mm <- predictions(mod,
    by = "carb",
    newdata = "balanced")
mm


## -----------------------------------------------------------------------------
21.66232 - 21.34058 


## -----------------------------------------------------------------------------
21.66232 + -(21.34058)


## -----------------------------------------------------------------------------
sum(c(21.66232, 21.34058) * c(1, -1))


## -----------------------------------------------------------------------------
c(21.66232, 21.34058) %*% c(1, -1)


## -----------------------------------------------------------------------------
lc <- c(1, -1, 0, 0, 0, 0)
predictions(mod,
    by = "carb",
    newdata = "balanced",
    hypothesis = lc)


## -----------------------------------------------------------------------------
lc <- c(0, -2, 1, 1, -1, 1)
predictions(mod,
    by = "carb",
    newdata = "balanced",
    hypothesis = lc)


## -----------------------------------------------------------------------------
library(emmeans)
em <- emmeans(mod, "carb")
lc <- data.frame(custom_contrast = c(0, -2, 1, 1, -1, 1))
contrast(em, method = lc)


## -----------------------------------------------------------------------------
lc <- matrix(c(
    -2, 1, 1, 0, -1, 1,
    1, -1, 0, 0, 0, 0
    ), ncol = 2)
colnames(lc) <- c("Contrast A", "Contrast B")
lc

predictions(mod,
    by = "carb",
    newdata = "balanced",
    hypothesis = lc)


## -----------------------------------------------------------------------------
# simulated means and medians
draw <- function(i) { 
  x <- rnorm(n = 10000, mean = 0, sd = 1)
  out <- data.frame(median = median(x), mean =  mean(x))
  return(out)
}
sims <- do.call("rbind", lapply(1:25, draw))

# average mean and average median 
coeftable <- data.frame(
  term = c("median", "mean"),
  estimate = c(mean(sims$median), mean(sims$mean))
)

# variance-covariance
vcov <- cov(sims)

# is the median equal to the mean?
hypotheses(
  coeftable,
  vcov = vcov,
  hypothesis = "median = mean"
)


## -----------------------------------------------------------------------------
model <- lm(mpg ~ as.factor(cyl) * hp, data = mtcars)
coef(model)


## -----------------------------------------------------------------------------
hypotheses(model, joint = c("as.factor(cyl)6:hp", "as.factor(cyl)8:hp"))


## -----------------------------------------------------------------------------
# ## joint hypotheses: regular expression
hypotheses(model, joint = "cyl")

# joint hypotheses: integer indices
hypotheses(model, joint = 2:3)

# joint hypotheses: different null hypotheses
hypotheses(model, joint = 2:3, hypothesis = 1)
hypotheses(model, joint = 2:3, hypothesis = 1:2)

# joint hypotheses: marginaleffects object
cmp <- avg_comparisons(model)
hypotheses(cmp, joint = "cyl")


## -----------------------------------------------------------------------------
# fit model
mod <- lm(mpg ~ factor(carb), mtcars)

# hypothesis matrix for linear combinations
H <- matrix(0, nrow = length(coef(mod)), ncol = 2)
H[2:3, 1] <- H[4:6, 2] <- 1

# test individual linear combinations
hyp <- hypotheses(mod, hypothesis = H)
hyp

# test joint hypotheses
#hypotheses(hyp, joint = TRUE, hypothesis = c(-10, -20))


## -----------------------------------------------------------------------------
fit <- lm(mpg ~ factor(carb) - 1, data = mtcars)

hypotheses(fit, ~reference)

hypotheses(fit, ~reference) |> hypotheses(joint = TRUE)


## ----message = FALSE----------------------------------------------------------
library(data.table)

N <- 1000
did <- data.table(
    id = 1:N,
    pre = rnorm(N),
    trt = sample(0:1, N, replace = TRUE))
did$post <- did$pre + did$trt * 0.3 + rnorm(N)
did <- melt(
    did,
    value.name = "y",
    variable.name = "time",
    id.vars = c("id", "trt"))
head(did)


## -----------------------------------------------------------------------------
did_model <- lm(y ~ time * trt, data = did)

comparisons(
    did_model,
    newdata = datagrid(trt = 0:1),
    variables = "time")


## -----------------------------------------------------------------------------
comparisons(
    did_model,
    variables = "time",
    newdata = datagrid(trt = 0:1),
    hypothesis = ~pairwise)

