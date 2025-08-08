## -----------------------------------------------------------------------------
#| include: false
library(marginaleffects)
library(partykit)


## -----------------------------------------------------------------------------
library(marginaleffects)
library(partykit)
data(bikes, package = "fmeffects")

mod <- glm(
    count ~ season * weekday + weather * temp,
    data = bikes, family = quasipoisson)


## -----------------------------------------------------------------------------
#| warnings: false
cmp <- comparisons(mod, variables = list(temp = 5))
cmp


## -----------------------------------------------------------------------------
avg_comparisons(mod, variables = list(temp = 5))


## -----------------------------------------------------------------------------
tree <- ctree(
    estimate ~ weekday + season,
    data = cmp,
    control = ctree_control(maxdepth = 2)
)


## -----------------------------------------------------------------------------
plot(tree)


## -----------------------------------------------------------------------------
dat <- transform(bikes, nodeid = predict(tree, type = "node"))
comparisons(mod,
    variables = list(temp = 5),
    newdata = dat,
    by = "nodeid")


## -----------------------------------------------------------------------------
print(tree)

