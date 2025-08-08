## ----include = FALSE----------------------------------------------------------
options(width = 1000)
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


## -----------------------------------------------------------------------------
library(marginaleffects)
dat <- transform(mtcars, cyl = factor(cyl))
mod <- lm(mpg ~ cyl, dat)
hyp <- hypotheses(mod, "cyl6 = cyl8")
hyp


## -----------------------------------------------------------------------------
#| include: false
attr(hyp$p.value, "label") <- attr(hyp$p.value, "jacobian") <- NULL

## -----------------------------------------------------------------------------
-log2(hyp$p.value)


## ----echo=FALSE, fig.asp=1, fig.width = 5-------------------------------------
# Create a sequence of p values from 0 to 1
p_values <- seq(0, 1, length.out = 100)

# Compute the S values using -log2(p)
S_values <- -log2(p_values)

# Create the plot
plot(p_values, S_values, type = "l",
     main = "Deterministic relationship between p and S",
     xlab = "p",
     ylab = "S")

text(x = 0.15, y = 6, labels = "Surprising")
text(x = 0.85, y = .75, labels = "Unsurprising")

