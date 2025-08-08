## -----------------------------------------------------------------------------
#| include: false
## this vignette is in .Rbuildignore

knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 9,
  fig.asp = .4,
  out.width = "100%",
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)
library(ggplot2)
theme_set(theme_minimal())


## ----message = FALSE, warning = FALSE-----------------------------------------
library(marginaleffects)
library(modelsummary)
library(ggplot2)
library(rms)

load(url(
"https://github.com/vincentarelbundock/modelarchive/raw/main/data-raw/gusto.rda"
))

gusto <- subset(gusto, tx %in% c("tPA", "SK"))
gusto$tx <- factor(gusto$tx, levels = c("tPA", "SK"))

mod <- glm(
    day30 ~ tx + rcs(age, 4) + Killip + pmin(sysbp, 120) + lsp(pulse, 50) +
    pmi + miloc + sex, family = "binomial",
    data = gusto)


## ----warning = FALSE----------------------------------------------------------
modelsummary(mod, exponentiate = TRUE, coef_omit = "^(?!txSK)") 


## -----------------------------------------------------------------------------
comparisons(
    mod,
    variables = "tx")


## -----------------------------------------------------------------------------
avg_comparisons(mod, variables = "tx")


## -----------------------------------------------------------------------------
avg_comparisons(
    mod,
    variables = "tx",
    comparison = "lnratioavg",
    transform = exp)


## -----------------------------------------------------------------------------
avg_comparisons(
    mod,
    variables = "tx",
    comparison = "lnoravg",
    transform = "exp")


## -----------------------------------------------------------------------------
cmp <- comparisons(mod, variables = "tx")
cmp


## ----fig.asp = 1--------------------------------------------------------------
ggplot(cmp, aes(predicted_hi, predicted_lo)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 3) +
  coord_fixed() +
  labs(x = "SK", y = "tPA")


## -----------------------------------------------------------------------------
ggplot(cmp, aes(estimate)) + stat_ecdf()


## -----------------------------------------------------------------------------
ggplot(cmp, aes(estimate)) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = mean(cmp$estimate), color = "orange") +
  geom_vline(xintercept = median(cmp$estimate), color = "darkgreen") +
  labs(x = "SK - TPA", title = "Distribution of unit-level contrasts")


## -----------------------------------------------------------------------------
d  <- gusto

d$tx = "SK"
predicted_hi <- predict(mod, newdata = d, type = "response")

d$tx = "tPA"
predicted_lo <- predict(mod, newdata = d, type = "response")

comparison <- predicted_hi - predicted_lo


## -----------------------------------------------------------------------------
nrow(gusto)


## -----------------------------------------------------------------------------
nrow(cmp)

