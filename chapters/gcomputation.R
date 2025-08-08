## -----------------------------------------------------------------------------
#| cache: false
#| echo: false
#| warning: false
source("code/load.R")
options(marginaleffects_print_digits = 6)


## \begin{tikzpicture}
##     \node (Z) at (1, -1) {Z};
##     \node (L) at (2, 0) {L};
##     \node (D) at (3, 0) {D};
##     \node (Y) at (4, 0)  {Y};
##     \draw[->] (Z) -- (L);
##     \draw[->, bend right] (Z) to (Y);
##     \draw[->] (L) -- (D);
##     \draw[->] (D) -- (Y);
## \end{tikzpicture}

## -----------------------------------------------------------------------------
library(marginaleffects)
dat = get_dataset("lottery")
dat = subset(dat, win_big == 1 | win == 0)

head(dat, n = 2)


## -----------------------------------------------------------------------------
mod = lm(
  earnings_post_avg ~ win_big * (
    tickets + man + work + age + education + college + year +
    earnings_pre_1 + earnings_pre_2 + earnings_pre_3),
  data = dat)

summary(mod)


## -----------------------------------------------------------------------------
d0 = transform(dat, win_big = 0)
d1 = transform(dat, win_big = 1)


## -----------------------------------------------------------------------------
p0 = predictions(mod, newdata = d0)
p1 = predictions(mod, newdata = d1)


## -----------------------------------------------------------------------------
dat[6, "win_big"]


## -----------------------------------------------------------------------------
p0[6, "estimate"]


## -----------------------------------------------------------------------------
p1[6, "estimate"]


## -----------------------------------------------------------------------------
mean(p0$estimate)


## -----------------------------------------------------------------------------
mean(p1$estimate)


## -----------------------------------------------------------------------------
avg_predictions(mod,
  variables = "win_big",
  by = "win_big")


## -----------------------------------------------------------------------------
cmp = avg_comparisons(mod,
  variables = "win_big",
  newdata = dat)
cmp


## -----------------------------------------------------------------------------
avg_comparisons(mod, 
  variables = "win_big", 
  newdata = subset(win_big == 1))


## -----------------------------------------------------------------------------
avg_comparisons(mod, 
  variables = "win_big", 
  newdata = subset(win_big == 0))


## -----------------------------------------------------------------------------
avg_comparisons(mod, variables = "win_big", by = "work")

## -----------------------------------------------------------------------------
#| echo: false
cmp = avg_comparisons(mod, variables = "win_big", by = "work")

