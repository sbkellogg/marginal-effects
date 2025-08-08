## -----------------------------------------------------------------------------
#| cache: false
#| echo: false
#| warning: false
source("code/load.R")


## -----------------------------------------------------------------------------
#| warning: false
#| tbl-cap: Six rows of the Thornton (2008) dataset.
library(marginaleffects)
library(tinytable)
dat = get_dataset("thornton")
tt(head(dat))


## -----------------------------------------------------------------------------
mod = lm(outcome ~ agecat - 1, data = dat)
coef(mod)


## -----------------------------------------------------------------------------
aggregate(outcome ~ agecat, FUN = mean, data = dat)


## -----------------------------------------------------------------------------
summary(mod)


## -----------------------------------------------------------------------------
#| echo: false
b = coef(mod)[1]
se = summary(mod)$coefficients[1, 2]
t1 = b / se
bstr = sprintf("%.5f", b)
sestr = sprintf("%.5f", se)
t1str = sprintf("%.2f", t1)


## -----------------------------------------------------------------------------
hypotheses(mod, hypothesis = 0.5)


## -----------------------------------------------------------------------------
# First coefficient
b = coef(mod)[1]

# The standard error is the square root of the diagonal element of the
# variance-covariance matrix
se = sqrt(diag(vcov(mod)))[1]

# The Z statistic for Wald test with null hypothesis of b = 0.5
z = (b - .5) / se

# The p-value is the area under the curve, in the tails of 
# the normal distribution beyond |Z|
pnorm(-abs(z)) * 2


## -----------------------------------------------------------------------------
#| include: false
h = hypotheses(mod, hypothesis = "b3 - b1 = 0")
b = sprintf("%.3f", h$estimate)
p = sprintf("%.3f", h$p.value)

## -----------------------------------------------------------------------------
hypotheses(mod, hypothesis = "b3 - b1 = 0")


## -----------------------------------------------------------------------------
hypotheses(mod, hypothesis = "b3 / b1 = 1")


## -----------------------------------------------------------------------------
#| eval: false
# hypotheses(mod, hypothesis = "b2^2 * exp(b1) = 0")
# hypotheses(mod, hypothesis = "b1 - (b2 * b3) = 2")


## -----------------------------------------------------------------------------
hypotheses(mod, hypothesis = difference ~ reference)


## -----------------------------------------------------------------------------
hypotheses(mod, hypothesis = ratio ~ sequential)


## -----------------------------------------------------------------------------
hypotheses(mod, multcomp = "holm")


## -----------------------------------------------------------------------------
coef(mod)
hypotheses(mod, hypothesis = "b3 - b2 = 0")


## -----------------------------------------------------------------------------
#| eval: false
# hypotheses(mod,
#   hypothesis = "b3 - b2 = 0",
#   equivalence = c(-0.05, 0.05))

## -----------------------------------------------------------------------------
#| echo: false
h = hypotheses(mod, 
  hypothesis = "b3 - b2 = 0", 
  equivalence = c(-0.05, 0.05))
h[, c("hypothesis", "estimate", "std.error", "p.value.nonsup", "p.value.noninf", "p.value.equiv")]

