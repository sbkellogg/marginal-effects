## ----include = FALSE----------------------------------------------------------
## this vignette is in .Rbuildignore because lme4 is not available on old CRAN
## test machines.

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

theme_clean <- function() {
  theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(size = rel(1), hjust = 0),
          strip.background = element_blank(),
          legend.position = "bottom")
}
ggplot2::theme_set(theme_clean())


## -----------------------------------------------------------------------------
library(marginaleffects)
library(itsadug)
library(mgcv)

simdat$Subject <- as.factor(simdat$Subject)

dim(simdat)
head(simdat)


## -----------------------------------------------------------------------------
model <- bam(Y ~ Group + s(Time, by = Group) + s(Subject, bs = "re"),
             data = simdat)

summary(model)


## -----------------------------------------------------------------------------
pred <- predictions(model)
dim(pred)
head(pred)


## -----------------------------------------------------------------------------
plot_predictions(model, condition = "Time")


## -----------------------------------------------------------------------------
mfx <- slopes(model, variables = "Time")
head(mfx)


## -----------------------------------------------------------------------------
plot_slopes(model, variables = "Time", condition = "Time")


## -----------------------------------------------------------------------------
predictions(model, newdata = "mean", exclude = "s(Subject)")

