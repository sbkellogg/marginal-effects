## -----------------------------------------------------------------------------
#| message: false
#| warning: false
library(dplyr)
library(mlogit)
library(marginaleffects)

# Fishing mode‑choice data from mlogit
 data(Fishing)
 Fish <- dfidx(Fishing, varying = 2:9, shape = "wide", choice = "mode")

 m <- mlogit(mode ~ price + catch | income, data = Fish)


## -----------------------------------------------------------------------------
# Prepare data for the two scenarios
lo <- transform(as.data.frame(Fish), term = "lo")
lo$id1 <- Fish$idx$id1
lo$id2 <- Fish$idx$id2
lo$idx <- NULL

hi <- transform(lo,
    price = ifelse(id2 == "beach", price + 100, price),
    term = "hi")

dat <- rbind(lo, hi)
dat$term <- factor(dat$term, levels = c("lo", "hi"))


## -----------------------------------------------------------------------------
#| warning: false
predictions(m,
    newdata = dat,
    by = c("group", "term"))


## -----------------------------------------------------------------------------
#| warning: false
h <- \(x) aggregate(estimate ~ term + group, data = x, FUN = mean)
predictions(m, newdata = dat, hypothesis = h)


## -----------------------------------------------------------------------------
#| eval: false
# library(dplyr)
# h <- function(x) {
#   x |> summarize(estimate = mean(estimate), .by = c("term", "group"))
# }
# predictions(m, newdata = dat, hypothesis = h)


## -----------------------------------------------------------------------------
#| warning: false
h <- function(x) {
    x |>
        aggregate(estimate ~ term + group, data = _, FUN = mean) |>
        aggregate(estimate ~ group, data = _, FUN = diff) |>
        setNames(c("term", "estimate"))
}
predictions(m, newdata = dat, hypothesis = h)


## -----------------------------------------------------------------------------
#| eval: false
# h <- function(x) {
#   x |>
#     summarize(estimate = mean(estimate), .by = c("term", "group")) |>
#     summarize(estimate = diff(estimate), .by = "group") |>
#     rename(term = group)
# }
# predictions(m, newdata = dat, hypothesis = h)

