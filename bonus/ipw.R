## ----include = FALSE----------------------------------------------------------
options(width = 10000)


## -----------------------------------------------------------------------------
library(marginaleffects)
data("lalonde", package = "MatchIt")
head(lalonde)


## -----------------------------------------------------------------------------
m <- glm(treat ~ age + educ + race + re74, data = lalonde, family = binomial)


## -----------------------------------------------------------------------------
dat <- predictions(m, newdata = lalonde)
dat$wts <- ifelse(dat$treat == 1, 1 / dat$estimate, 1 / (1 - dat$estimate))


## -----------------------------------------------------------------------------
mod <- lm(re78 ~ treat * (age + educ + race + re74), data = dat, weights = wts)


## -----------------------------------------------------------------------------
#| warning: false
avg_comparisons(mod,
    variables = "treat",
    wts = "wts",
    vcov = "HC3")


## -----------------------------------------------------------------------------
ht <- \(hi, lo, w, newdata) {
    (sum(hi * w) / nrow(newdata)) - (sum(lo * w) / nrow(newdata))
}

comparisons(mod,
    comparison = ht,
    variables = "treat",
    wts = "wts",
    vcov = "HC3")

