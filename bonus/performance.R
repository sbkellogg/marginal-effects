## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 9,
  fig.asp = .4,
  out.width = "100%",
  warning = FALSE,
  eval = FALSE,
  message = FALSE,
  comment = "#>"
)


## ----eval = FALSE-------------------------------------------------------------
# library(marginaleffects)
# 
# ## simulate data and fit a large model
# N <- 1e5
# dat <- data.frame(matrix(rnorm(N * 26), ncol = 26))
# mod <- lm(X1 ~ ., dat)
# 
# results <- bench::mark(
#     # marginal effects at the mean; no standard error
#     slopes(mod, vcov = FALSE, newdata = "mean"),
#     # marginal effects at the mean
#     slopes(mod, newdata = "mean"),
#     # 1 variable; no standard error
#     slopes(mod, vcov = FALSE, variables = "X3"),
#     # 1 variable
#     slopes(mod, variables = "X3"),
#     # 26 variables; no standard error
#     slopes(mod, vcov = FALSE),
#     # 26 variables
#     slopes(mod),
#     iterations = 1, check = FALSE)
# 
# results[, c(1, 3, 5)]
# # expression                                        median mem_alloc
# # "slopes(mod, vcov = FALSE, newdata = \"mean\")" 194.98ms  306.19MB
# # "slopes(mod, newdata = \"mean\")"               345.38ms  311.45MB
# # "slopes(mod, vcov = FALSE, variables = \"X3\")" 197.51ms   649.6MB
# # "slopes(mod, variables = \"X3\")"               742.05ms    1.27GB
# # "slopes(mod, vcov = FALSE)"                        4.09s   13.87GB
# # "slopes(mod)"                                     15.33s   26.83GB


## -----------------------------------------------------------------------------
#| eval: false
# library(mgcv)
# library(tictoc)
# library(future)
# library(nycflights13)
# library(marginaleffects)
# data("flights")
# packageVersion("marginaleffects")
# 
# cores <- 8
# plan(multicore, workers = cores, number_of_workers = 8)
# 
# flights <- flights |>
#     transform(date = as.Date(paste(year, month, day, sep = "/"))) |>
#     transform(date.num = as.numeric(date - min(date))) |>
#     transform(wday = as.POSIXlt(date)$wday) |>
#     transform(time = as.POSIXct(paste(hour, minute, sep = ":"), format = "%H:%M")) |>
#     transform(time.dt = difftime(time, as.POSIXct('00:00', format = '%H:%M'), units = 'min')) |>
#     transform(time.num = as.numeric(time.dt)) |>
#     transform(dep_delay = ifelse(dep_delay < 0, 0, dep_delay)) |>
#     transform(dep_delay = ifelse(is.na(dep_delay), 0, dep_delay)) |>
#     transform(carrier = factor(carrier)) |>
#     transform(dest = factor(dest)) |>
#     transform(origin = factor(origin))
# 
# model <- bam(dep_delay ~ s(date.num, bs = "cr") +
#                   s(wday, bs = "cc", k = 3) +
#                   s(time.num, bs = "cr") +
#                   s(carrier, bs = "re") +
#                   origin +
#                   s(distance, bs = "cr") +
#                   s(dest, bs = "re"),
#               data = flights,
#               family = poisson,
#               discrete = TRUE,
#               nthreads = cores)


## -----------------------------------------------------------------------------
#| eval: false
# length(coef(model))


## -----------------------------------------------------------------------------
#| eval: false
# tic()
# p1 <- predictions(model, vcov = FALSE)
# toc()


## -----------------------------------------------------------------------------
#| eval: false
# options("marginaleffects_parallel" = TRUE)
# 
# tic()
# p1 <- predictions(model)
# toc()


## -----------------------------------------------------------------------------
#| eval: false
# options("marginaleffects_parallel" = FALSE)
# 
# tic()
# p2 <- predictions(model)
# toc()


## -----------------------------------------------------------------------------
#| eval: false
# cor(p1$estimate, p2$estimate)
# 
# cor(p1$std.error, p2$std.error)
# 
# head(p1)
# 
# head(p2)


## ----eval = FALSE-------------------------------------------------------------
#| eval: false
# library(margins)
# 
# N <- 1e3
# dat <- data.frame(
#     y = sample(0:1, N, replace = TRUE),
#     x1 = rnorm(N),
#     x2 = rnorm(N),
#     x3 = rnorm(N),
#     x4 = factor(sample(letters[1:5], N, replace = TRUE)))
# mod <- glm(y ~ x1 + x2 + x3 + x4, data = dat, family = binomial)


## ----eval = FALSE-------------------------------------------------------------
#| eval: false
# results <- bench::mark(
#     slopes(mod, vcov = FALSE),
#     margins(mod, unit_ses = FALSE),
#     check = FALSE, relative = TRUE)
# results[, c(1, 3, 5)]
# 
# # expression                     median mem_alloc
# # <bch:expr>                      <dbl>     <dbl>
# # slopes(mod, vcov = FALSE)        1         1
# # margins(mod, unit_ses = FALSE)   3.21      2.83


## ----eval = FALSE-------------------------------------------------------------
#| eval: false
# results <- bench::mark(
#     slopes(mod, vcov = TRUE),
#     margins(mod, unit_ses = TRUE),
#     check = FALSE, relative = TRUE, iterations = 1)
# results[, c(1, 3, 5)]
# # expression                    median mem_alloc
# #  <bch:expr>                     <dbl>     <dbl>
# #  slopes(mod, vcov = TRUE)          1        1
# #  margins(mod, unit_ses = TRUE)  1161.      32.9

