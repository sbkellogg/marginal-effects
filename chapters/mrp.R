## -----------------------------------------------------------------------------
#| cache: false
#| echo: false
#| warning: false
#| message: false
source("code/load.R")
options(tinytable_tt_digits = 4)
library(marginaleffects)
library(brms)
library(ggplot2)
library(ggdist)


## -----------------------------------------------------------------------------
#| message: false
#| warning: false
library(marginaleffects)
survey = get_dataset("ces_survey")
head(survey)


## -----------------------------------------------------------------------------
#| warning: false
library(glmmTMB)
library(marginaleffects)

mod = glmmTMB(
  defund ~ age + education + military + gender + (1 + gender | state),
  family = binomial,
  data = survey)
summary(mod)


## -----------------------------------------------------------------------------
#| warning: false
p = predictions(mod, newdata = datagrid(
  state = c("CA", "AL"),
  gender = "Man",
  military = 0,
  education = "4 year",
  age = "50-59"
))
p


## -----------------------------------------------------------------------------
#| warning: false
avg_comparisons(mod, variables = "age")

## -----------------------------------------------------------------------------
#| warning: false
#| echo: false
cmp = avg_comparisons(mod, variables = "age")$estimate
cmp = sprintf("%.0f", abs(cmp * 100))


## -----------------------------------------------------------------------------
#| eval: false
# defund ~ age + education + military + gender + (1 + gender | state)


## -----------------------------------------------------------------------------
library(brms)
library(ggdist)
library(ggplot2)

priors_vague = c(
  prior(normal(0, 1e6), class = "b"),
  prior(normal(0, 1e6), class = "Intercept")
)


## -----------------------------------------------------------------------------
priors_informative = c(
  prior(normal(0, 0.2), class = "b"),
  prior(normal(0, 0.2), class = "Intercept")
)


## -----------------------------------------------------------------------------
#| echo: true
#| eval: false
# model_vague = brm(
#   defund ~ age + education + military + gender + (1 + gender | state),
#   family = bernoulli,
#   prior = priors_vague,
#   sample_prior = "only",
#   data = survey)
# 
# model_informative = brm(
#   defund ~ age + education + military + gender + (1 + gender | state),
#   family = bernoulli,
#   prior = priors_informative,
#   sample_prior = "only",
#   data = survey)

## -----------------------------------------------------------------------------
#| echo: false
model_vague = brm(
  defund ~ age + education + military + gender + (1 + gender | state),
  family = bernoulli,
  prior = priors_vague,
  sample_prior = "only",
  data = survey,
  refresh = 0, silent = 2)

model_informative = brm(
  defund ~ age + education + military + gender + (1 + gender | state),
  family = bernoulli,
  prior = priors_informative,
  sample_prior = "only",
  data = survey,
  refresh = 0, silent = 2)


## -----------------------------------------------------------------------------
fixef(model_vague)


## -----------------------------------------------------------------------------
p_vague = avg_predictions(model_vague, by = "gender")
p_vague


## -----------------------------------------------------------------------------
p_informative = avg_predictions(model_informative, by = "gender")
p_informative


## -----------------------------------------------------------------------------
#| label: fig-mrp_prior_predictions
#| fig-cap: "Average predictions with vague and informative priors (no data)."
#| out-width: 100%
#| fig.asp: .3
#| fig-width: 8.571429
draws = rbind(
  get_draws(p_vague, shape = "rvar"),
  get_draws(p_informative, shape = "rvar")
)
draws$Priors = c("Vague", "Vague", "Informative", "Informative")

ggplot(draws, aes(xdist = rvar, fill = Priors)) +
  stat_slabinterval(alpha = .5) +
  facet_wrap(~ gender) +
  scale_fill_grey() +
  labs(x = "Average predicted probability (prior-only)", y = "Density")


## -----------------------------------------------------------------------------
avg_comparisons(model_vague, variables = "military")

avg_comparisons(model_informative, variables = "military")


## -----------------------------------------------------------------------------
#| echo: true
#| eval: false
#| warning: false
# model = brm(
#     defund ~ age + education + military + gender + (1 + gender | state),
#     family = bernoulli,
#     prior = priors_vague,
#     data = survey)

## -----------------------------------------------------------------------------
#| echo: false
#| warning: false
model = brm(
    defund ~ age + education + military + gender + (1 + gender | state),
    family = bernoulli,
    silent = 2, refresh = 0,
    prior = priors_vague,
    data = survey)


## -----------------------------------------------------------------------------
p = avg_predictions(model, by = "military")
p


## -----------------------------------------------------------------------------
cmp = avg_comparisons(model, variables = list(age = c("18-29", "70+")))
cmp


## -----------------------------------------------------------------------------
cmp = comparisons(model,
    variables = "gender", 
    newdata = datagrid(state = unique))


## -----------------------------------------------------------------------------
#| out-width: 100%
#| fig-width: 10
#| fig-asp: .3
#| label: fig-bayes_gender_state
#| fig-cap: "Average risk difference for a hypothetical individual with typical characteristics living in different states."
cmp = sort_by(cmp, ~estimate) |>
    transform(state = factor(state, levels = state))

ggplot(cmp,
       aes(x = state, y = estimate,
           ymin = conf.low, ymax = conf.high)) +
    geom_hline(yintercept = 0, linetype = 3) +
    geom_pointrange() + 
    labs(x = "", y = "Average risk difference") +
    theme(axis.text.x = element_text(angle = 90, vjust = .5))


## -----------------------------------------------------------------------------
draws = avg_predictions(model) |> get_draws(shape = "long")
draws[1:5, 1:2]


## -----------------------------------------------------------------------------
mean(draws$draw > .4)


## -----------------------------------------------------------------------------
sort(tapply(survey$state, survey$state, length))


## -----------------------------------------------------------------------------
demographics = get_dataset("ces_demographics")
head(demographics)


## -----------------------------------------------------------------------------
with(demographics, sum(percent[state == "AL"]))


## -----------------------------------------------------------------------------
p = predictions(model, newdata = demographics)


## -----------------------------------------------------------------------------
p = avg_predictions(model, 
  newdata = demographics,
  wts = "percent",
  by = "state")
head(p)


## -----------------------------------------------------------------------------
#| fig-cap: Estimated average probability of supporting cuts to police budgets, for each US state. Estimates obtained via Bayesian multi-level regression and poststratification.
#| fig.asp: 1.7
#| label: fig-mrp_ridge
library(ggdist)
library(ggplot2)

p = p |>
  get_draws(shape = "rvar") |>
  sort_by(~ estimate) |>
  transform(state = factor(state, levels = state))

ggplot(p, aes(y = state, xdist = rvar)) + 
  stat_slab(height = 2, color = "white") +
  labs(x = "Posterior density", y = NULL) +
  xlim(c(.3, .5))

