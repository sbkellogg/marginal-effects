## -----------------------------------------------------------------------------
#| include: false
options(marginaleffects_print_type = FALSE)
options(marginaleffects_print_column_names = FALSE)
options(width = 300)

## -----------------------------------------------------------------------------
#| warning: false
#| message: false
library(marginaleffects)

# model-fitting packages
library(survival)
library(splines)

# plotting packages and theme
library(tinyplot)
tinytheme("ipsum")

# parallel processing for bootstrap confidence intervals
library(future.apply)
plan(multisession)
options(marginaleffects_parallel_inferences = TRUE)
options(marginaleffects_parallel_packages = c("survival", "splines"))

head(rotterdam)


## -----------------------------------------------------------------------------
model <- coxph(
    Surv(dtime, death) ~ hormon + grade + ns(age, df = 2),
    data = rotterdam
)

summary(model)


## -----------------------------------------------------------------------------
p <- avg_predictions(model,
    type = "survival",
    by = c("dtime", "hormon"),
    vcov = "rsample",
    newdata = datagrid(
        hormon = 0:1,
        dtime = seq(36, 7043, length.out = 25),
        grid_type = "counterfactual"
    )
)
 
tail(p)


## -----------------------------------------------------------------------------
last <- p[p$dtime == max(p$dtime),]
last


## -----------------------------------------------------------------------------
#| warning: false
#| message: false
with(p, {
    tinyplot(
        x = dtime,
        y = estimate,
        ymin = conf.low,
        ymax = conf.high,
        by = hormon,
        type = "ribbon",
        ylab = "Adjusted Survival Probability"
    )
})


## -----------------------------------------------------------------------------
p <- avg_predictions(model,
  hypothesis = difference ~ reference | dtime,
  type = "survival",
  by = c("dtime", "hormon"),
  vcov = "rsample",
  newdata = datagrid(
    hormon = 0:1,
    dtime = c(1000, 4000, 7000),
    grid_type = "counterfactual"
  )
)
p


## -----------------------------------------------------------------------------
p <- avg_predictions(model,
  hypothesis = ratio ~ reference | dtime,
  type = "survival",
  by = c("dtime", "hormon"),
  vcov = "rsample",
  newdata = datagrid(
    hormon = 0:1,
    dtime = c(1000, 4000, 7000),
    grid_type = "counterfactual"
  )
)
p


## -----------------------------------------------------------------------------
model <- coxph(
  Surv(dtime, death) ~ hormon * factor(grade) + ns(age, df=2), 
  data=rotterdam
)

summary(model)


## -----------------------------------------------------------------------------
p <- avg_predictions(model,
  type = "survival",
  by = c("dtime", "hormon", "grade"),
  vcov = "rsample",
  newdata = datagrid(
    hormon = unique,
    grade = unique,
    dtime = c(1000, 4000, 7000),
    grid_type = "counterfactual"
  )
)
p


## -----------------------------------------------------------------------------
p[c(7, 1),]

diff(p$estimate[c(7, 1)])


## -----------------------------------------------------------------------------
#| warning: false
#| message: false
avg_predictions(model,
  hypothesis = "b1 - b7 = 0",
  type = "survival",
  by = c("dtime", "hormon", "grade"),
  vcov = "rsample",
  newdata = datagrid(
    hormon = unique,
    grade = unique,
    dtime = c(1000, 4000, 7000),
    grid_type = "counterfactual"
  )
)


## ::: {.callout-note}
## If we were instead interested in the **effect modification** of the effect of `hormon` by `grade`, we could use almost the same code. We would only have to remove the `grade` specification from the `datagrid()` call (see @VanderWeele2009 for an explanation of the distinction).
## :::

## -----------------------------------------------------------------------------
#| warning: false
#| message: false
p <- predictions(model,
  type = "survival",
  by = c("dtime", "hormon", "grade"),
  vcov = "rsample",
  newdata = datagrid(
    hormon = unique,
    grade = unique,
    dtime = seq(36, 7043, length.out = 25),
    grid_type = "counterfactual"
  )
)

with(p, {
    tinyplot(
        estimate ~ dtime | hormon + grade,
        ymin = conf.low,
        ymax = conf.high,
        type = "ribbon",
        ylab = "Adjusted Survival Probability"
    )
})


## -----------------------------------------------------------------------------
cmp <- avg_comparisons(model,
    variables = "hormon",
    type = "survival",
    vcov = "rsample",
    newdata = datagrid(
        dtime = 5000,
        grid_type = "counterfactual"
    )
)
cmp


## -----------------------------------------------------------------------------
avg_comparisons(model,
    comparison = "ratio",
    variables = "hormon",
    type = "survival",
    vcov = "rsample",
    newdata = datagrid(
        dtime = 5000,
        grid_type = "counterfactual"
    )
)


## -----------------------------------------------------------------------------
cmp <- avg_comparisons(model,
    variables = "hormon",
    type = "survival",
    by = "grade",
    vcov = "rsample",
    newdata = datagrid(
        dtime = 5000,
        grid_type = "counterfactual"
    )
)
cmp


## -----------------------------------------------------------------------------
cmp <- avg_comparisons(model,
    hypothesis = difference ~ reference,
    variables = "hormon",
    type = "survival",
    by = "grade",
    vcov = "rsample",
    newdata = datagrid(
        dtime = 5000,
        grid_type = "counterfactual"
    )
)
cmp


## For any subject with covariates $x$, the marginal survivor estimate is
## 
## $$
## \hat S(t\mid x)=\exp\!\bigl[-\hat H_{0}(t)\,\exp\{x^{\top}\hat\beta\}\bigr],
## $$
## 
## Its variance decomposes as
## 
## $$
## \text{Var}[\hat S]=
## \underbrace{\Bigl(\partial S/\partial\beta\Bigr)^{\!\top}
## \widehat{\text{Var}}(\hat\beta)\,
## \Bigl(\partial S/\partial\beta\Bigr)}_{\text{coefficient part}}
## \;+\;
## \underbrace{\Bigl(\partial S/\partial H_{0}\Bigr)^{\!\top}
## \widehat{\text{Var}}\bigl[\hat H_{0}(t)\bigr]\,
## \Bigl(\partial S/\partial H_{0}\Bigr)}_{\text{baseline part}}
## \;+\;2\,\text{Cov}\bigl(\hat\beta,\hat H_{0}\bigr),
## $$
## 
## with a baseline term that can be evaluated with different strategies.
