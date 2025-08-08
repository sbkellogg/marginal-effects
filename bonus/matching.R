## -----------------------------------------------------------------------------
library("MatchIt")
library("marginaleffects")
data("lalonde", package = "MatchIt")

head(lalonde)


## -----------------------------------------------------------------------------
dat <- matchit(
    treat ~ age + educ + race + married + nodegree + re74 + re75, 
    data = lalonde, distance = "mahalanobis",
    replace = FALSE)
dat <- match.data(dat)


## -----------------------------------------------------------------------------
fit <- lm(
    re78 ~ treat * (age + educ + race + married + nodegree),
    data = dat,
    weights = weights)


## ----warning=FALSE------------------------------------------------------------
avg_comparisons(
    fit,
    variables = "treat",
    newdata = subset(dat, treat == 1),
    vcov = ~subclass,
    wts = "weights")

