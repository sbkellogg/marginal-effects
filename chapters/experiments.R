## -----------------------------------------------------------------------------
#| cache: false
#| echo: false
#| warning: false
source("code/load.R")
knitr::opts_chunk$set(
    warning = FALSE,
    message = FALSE
)
options(tinytable_tt_digits = 4)


## -----------------------------------------------------------------------------
library(marginaleffects)
dat = get_dataset("thornton")
mod = lm(outcome ~ incentive, data = dat)
coef(mod)


## -----------------------------------------------------------------------------
avg_comparisons(mod, variables = "incentive", vcov = "HC2")


## -----------------------------------------------------------------------------
mod = lm(outcome ~ incentive * (age + distance + hiv2004), data = dat)


## -----------------------------------------------------------------------------
coef(mod)


## -----------------------------------------------------------------------------
avg_comparisons(mod, 
    variables = "incentive",
    vcov = "HC2")

## -----------------------------------------------------------------------------
#| echo: false
cmp = avg_comparisons(mod, 
    variables = "incentive",
    vcov = "HC2")$estimate


## -----------------------------------------------------------------------------
library(marginaleffects)
dat = get_dataset("factorial_01")
mod = lm(Y ~ Ta + Tb + Ta:Tb, data = dat)
coef(mod)

## -----------------------------------------------------------------------------
#| echo: false
b = sprintf("%.3f", coef(mod))


## -----------------------------------------------------------------------------
plot_predictions(mod, by = c("Ta", "Tb"))


## -----------------------------------------------------------------------------
avg_comparisons(mod,
  variables = "Ta",
  newdata = subset(Tb == 0))


## -----------------------------------------------------------------------------
avg_comparisons(mod, 
  variables = c("Ta", "Tb"),
  cross = TRUE)


## -----------------------------------------------------------------------------
#| label: fig-factorial_comparisons
#| echo: false
#| fig-asp: 1.7
#| out.width: "100%"
#| fig.cap: "Six contrasts of interest in a 2-by-2 experimental design. The x-axis shows values of the $T_a$ treatment, and the 0/1 symbols on the plotting surface indicate values of the $T_b$ treatment."
library(ggplot2)
library(patchwork)
pd = avg_predictions(mod, by = c("Ta", "Tb"))
pd$Ta = factor(pd$Ta)
pd$Tb = factor(pd$Tb)
p = ggplot(pd, aes(Ta, estimate, shape = Tb)) +
    geom_point(size = 3) +
    theme(legend.position = "none", plot.title = element_text(size=10)) +
    scale_shape_manual(values = c("0", "1")) +
    labs(y = "Y") +
    ylim(10, 35)
p1 = p +  annotate("segment",
    x = 1, xend = 2, y = pd$estimate[1], yend = pd$estimate[3],
    arrow = arrow(type = "closed", length = unit(0.04, "npc"))) +
    ggtitle('avg_comparisons(mod,\n  variables = "Ta",\n  newdata = subset(Tb == 0))')
p2 = p +  annotate("segment",
    x = 1, xend = 1, y = pd$estimate[1], yend = pd$estimate[2],
    arrow = arrow(type = "closed", length = unit(0.04, "npc"))) +
    ggtitle('avg_comparisons(mod,\n  variables = "Tb",\n  newdata = subset(Ta == 0))')
p3 = p +  annotate("segment",
    x = 1, xend = 2, y = pd$estimate[2], yend = pd$estimate[4],
    arrow = arrow(type = "closed", length = unit(0.04, "npc"))) +
    ggtitle('avg_comparisons(mod,\n  variables = "Ta",\n  newdata = subset(Tb == 1))')
p4 = p +  annotate("segment",
    x = 2, xend = 2, y = pd$estimate[3], yend = pd$estimate[4],
    arrow = arrow(type = "closed", length = unit(0.04, "npc"))) +
    ggtitle('avg_comparisons(mod,\n  variables = "Tb",\n  newdata = subset(Ta == 1))')
p5 = p +  annotate("segment",
    x = 1, xend = 2, y = pd$estimate[1], yend = pd$estimate[4],
    arrow = arrow(type = "closed", length = unit(0.04, "npc"))) +
    ggtitle('avg_comparisons(mod,\n  variables = c("Ta", "Tb"),\n  cross = TRUE)')
p6 = p +  annotate("segment",
    x = 1, xend = 2, y = pd$estimate[2], yend = pd$estimate[3],
    arrow = arrow(type = "closed", length = unit(0.04, "npc"))) +
    ggtitle('avg_comparisons(mod,\n  variables = list(\n    Ta = c(0, 1),\n    Tb = c(1, 0)),\n  cross = TRUE)')
(p1 + p2) / (p3 + p4) / (p5 + p6)


## -----------------------------------------------------------------------------
cmp <- avg_comparisons(mod, variables = "Ta", by = "Tb")
cmp


## -----------------------------------------------------------------------------
avg_comparisons(mod,
  variables = "Ta",
  by = "Tb",
  hypothesis = "b2 - b1 = 0")

