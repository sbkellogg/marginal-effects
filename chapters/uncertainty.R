## -----------------------------------------------------------------------------
#| cache: false
#| echo: false
#| warning: false
source("code/load.R")


## -----------------------------------------------------------------------------
set.seed(48103)
X = rnorm(100)
Z = rnorm(100)
Y = 1 * X + 0.5 * Z + rnorm(100)
dat = data.frame(Y, X, Z)
mod = lm(Y ~ X + Z, data = dat)
summary(mod)


## -----------------------------------------------------------------------------
#| eval: false
# b2 = coef(mod)[2]
# b2

## -----------------------------------------------------------------------------
#| echo: false
b2 = coef(mod)[2]
b2 = unname(b2)
b2


## -----------------------------------------------------------------------------
v = vcov(mod)[2, 2]
v


## -----------------------------------------------------------------------------
log(b2)


## -----------------------------------------------------------------------------
#| eval: false
# sqrt(v / b2^2)

## -----------------------------------------------------------------------------
#| echo: false
unname(sqrt(v / b2^2))


## -----------------------------------------------------------------------------
library(marginaleffects)
hypotheses(mod, "log(b2) = 0")


## -----------------------------------------------------------------------------
#| eval: false
# b = coef(mod)
# b[2] - b[3]

## -----------------------------------------------------------------------------
#| echo: false
b = coef(mod)
unname(b[2] - b[3])


## -----------------------------------------------------------------------------
V = vcov(mod)
V


## -----------------------------------------------------------------------------
#| eval: false
# J = matrix(c(0, 1, -1), ncol = 1)
# variance = t(J) %*% V %*% J
# sqrt(variance)

## -----------------------------------------------------------------------------
#| echo: false
J = matrix(c(0, 1, -1), ncol = 1)
V = vcov(mod)
se = drop(sqrt(t(J) %*% V %*% J))
se


## -----------------------------------------------------------------------------
hypotheses(mod, hypothesis = "b2 - b3 = 0")


## -----------------------------------------------------------------------------
library(sandwich)
library(marginaleffects)
dat = get_dataset("thornton")
dat = na.omit(dat[, c("incentive", "outcome")])
mod = lm(outcome ~ incentive, data = dat)
summary(mod)


## -----------------------------------------------------------------------------
X = cbind(1, dat$incentive)
Y = dat$outcome
solve(crossprod(X, X)) %*% crossprod(X, Y)


## -----------------------------------------------------------------------------
e = Y - predict(mod)
var_e = sum((e - mean(e))^2) / df.residual(mod)
bread = solve(crossprod(X))
meat = var_e * crossprod(X)
V = bread %*% meat %*% bread
V


## -----------------------------------------------------------------------------
vcov(mod)


## -----------------------------------------------------------------------------
sqrt(diag(V))


## -----------------------------------------------------------------------------
meat = t(X) %*% diag(e^2) %*% X
bread %*% meat %*% bread


## -----------------------------------------------------------------------------
vcovHC(mod, type = "HC0")


## -----------------------------------------------------------------------------
avg_comparisons(mod, vcov = "HC0")


## -----------------------------------------------------------------------------
#| eval: false
# V = sandwich::vcovHC(mod)
# avg_comparisons(mod, vcov = V)         # heteroskedasticity-consistent
# avg_comparisons(mod, vcov = ~ village) # clustered by village


## -----------------------------------------------------------------------------
dat = get_dataset("thornton")
mod = lm(outcome ~ agecat - 1, data = dat)
coef(mod)


## -----------------------------------------------------------------------------
#| eval: false
# statistic = \(model) coef(model)[3] - coef(model)[2]
# statistic(mod)

## -----------------------------------------------------------------------------
#| echo: false
statistic = \(model) coef(model)[3] - coef(model)[2]
statistic(mod) |> unname()


## -----------------------------------------------------------------------------
sample_fit_compute = function() {
  # Step 1: Sample
  index = sample(1:nrow(dat), size = nrow(dat), replace = TRUE)
  resample = dat[index, ]

  # Step 2: Fit
  mod_resample = lm(outcome ~ agecat - 1, data = resample)

  # Step 3: Compute
  statistic(mod_resample)
}

# Step 4: Repeat
set.seed(48103)
boot_dist = replicate(1000, sample_fit_compute())

# Step 5: Quantiles
ci = quantile(boot_dist, prob = c(0.025, 0.975))
ci


## -----------------------------------------------------------------------------
#| warning: false
hypotheses(mod, "b3 - b2 = 0") |>
  inferences(method = "boot", R = 1000)


## -----------------------------------------------------------------------------
avg_comparisons(mod) |> inferences(method = "boot")


## -----------------------------------------------------------------------------
#| cache: true
# Step 1: Simulate coefficients
library(mvtnorm)
set.seed(48103)
coef_sim = rmvnorm(1000, coef(mod), vcov(mod))

# Step 2: Compute the quantity of interest
statistics = coef_sim[, 3] - coef_sim[, 2]

# Step 3: Build confidence intervals as quantiles
ci = quantile(statistics, probs = c(0.025, 0.975))
ci


## -----------------------------------------------------------------------------
hypotheses(mod, "b3 - b2 = 0") |>
  inferences(method = "simulation", R = 1000)

avg_comparisons(mod, variables = "agecat") |>
  inferences(method = "simulation", R = 1000)


## -----------------------------------------------------------------------------
#| cache: true
simulation = function(...) {
  # Draw random training and test sets
  Y_train = rnorm(25, mean = 3.141593)
  Y_test = rnorm(25, mean = 3.141593)

  # Fit an intercept-only linear model
  m = lm(Y_train ~ 1)

  # Predictions with 90% confidence interval
  p = predictions(m, conf_level = .90)

  # Does the confidence interval cover the true mean?
  coverage_mean = mean(p$conf.low < 3.141593 & p$conf.high > 3.141593)

  # Does the confidence interval cover out of sample observations?
  coverage_test = mean(p$conf.low < Y_test & p$conf.high > Y_test)

  out = c(
    "Mean coverage" = coverage_mean,
    "Out-of-sample coverage" = coverage_test)
  return(out)
}


## -----------------------------------------------------------------------------
set.seed(48103)
coverage = rowMeans(replicate(1000, simulation()))
coverage


## -----------------------------------------------------------------------------
library(marginaleffects)
dat = get_dataset("military")
head(dat)


## -----------------------------------------------------------------------------
set.seed(48103)
idx = sample(
  c("train", "calibration", "test"),
  size = nrow(dat),
  replace = TRUE)
dat = split(dat, idx)


## -----------------------------------------------------------------------------
mod = lm(rank ~ grade + branch + gender + race, data = dat$train)
summary(mod)


## -----------------------------------------------------------------------------
#| label: fig-uncertainty_abs_resid
#| fig-cap: "Distribution of absolute residuals in the calibration set, with the 95th quantile identified by a dashed vertical line."
#| fig-asp: .8
# Predictions in the calibration set
yhat_calib = predict(mod, newdata = dat$calib)

# Absolute residuals in the calibration set
resid_calib = abs(dat$calib$rank - yhat_calib)

# Plot
hist(resid_calib, breaks = 50, main = NULL, xlab = "|e|")
abline(v = quantile(resid_calib, 0.95), lty = 2)


## -----------------------------------------------------------------------------
# Predictions in the test set
yhat_test = predict(mod, newdata = dat$test)

# Measure the width of prediction intervals
q95 = quantile(resid_calib, probs = 0.95)
d = min(resid_calib[resid_calib > q95])

# Build prediction intervals
conformal_lo = yhat_test - d
conformal_hi = yhat_test + d


## -----------------------------------------------------------------------------
covered = dat$test$rank > conformal_lo & dat$test$rank < conformal_hi
mean(covered)


## -----------------------------------------------------------------------------
p = predictions(mod) |>
  inferences(
    method = "conformal_split",
    conformal_calibration = dat$calib,
    conformal_test = dat$test)

covered = p$rank > p$pred.low & p$rank < p$pred.high
mean(covered)


## -----------------------------------------------------------------------------
library("ggplot2")

# simulate data
N = 1000
X = rnorm(N * 2)
sim = data.frame(
    X = X,
    Y = X + X^2 + X^3 + rnorm(N * 2))
sim_train = sim[1:N,]
sim_test = sim[(N + 1):nrow(sim),]

# fit misspecified model
m = lm(Y ~ X, data = sim_train)

# conformal predictions
p = predictions(m) |>
  inferences(
    R = 5,
    method = "conformal_cv+",
    conformal_test = sim_test)

# plot both the confidence and prediction intervals
ggplot(p, aes(X, Y)) +
  geom_point(alpha = .05) +
  geom_ribbon(aes(X, ymin = pred.low, ymax = pred.high), alpha = .2) +
  geom_ribbon(aes(X, ymin = conf.low, ymax = conf.high), alpha = .4)


## -----------------------------------------------------------------------------
# prediction interval coverage
with(p, mean(Y <= pred.high & Y >= pred.low))

# confidence interval coverage
with(p, mean(Y <= conf.high & Y >= conf.low))


## -----------------------------------------------------------------------------
library(nnet)
mod = multinom(branch ~ gender * race, data = dat$train, trace = FALSE)


## -----------------------------------------------------------------------------
set.seed(48103)
p = predictions(mod, conf_level = .8) |>
    inferences(
        R = 5,
        method = "conformal_cv+",
        conformal_score = "softmax",
        conformal_test = dat$test)


## -----------------------------------------------------------------------------
#| echo: false
# sanity checks to match text
k = predictions(mod, newdata = tail(dat$test, 1))
k
stopifnot(k$group[k$estimate == max(k$estimate)] == "army")
stopifnot(tail(dat$test$branch, 1) == "air force")
stopifnot("air force" %in% tail(p$pred.set, 1)[[1]])

## -----------------------------------------------------------------------------
#| eval: false
# predictions(mod, newdata = tail(dat$test, 1))


## -----------------------------------------------------------------------------
tail(dat$test$branch, 1)


## -----------------------------------------------------------------------------
tail(p, 1)


## -----------------------------------------------------------------------------
mean(unlist(Map(\(o, p) o %in% p, p$branch, p$pred.set)))

