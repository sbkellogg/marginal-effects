## -----------------------------------------------------------------------------
#| cache: false
#| echo: false
#| warning: false
#| message: false
source("code/load.R")
options(tinytable_tt_digits = 4)


## -----------------------------------------------------------------------------
#| echo: false
suppressPackageStartupMessages(library(tidymodels))

## -----------------------------------------------------------------------------
#| messages: FALSE
library(tidymodels)
library(marginaleffects)
set.seed(48103)
airbnb = get_dataset("airbnb")
airbnb[1:5, 1:6]


## -----------------------------------------------------------------------------
airbnb_split = initial_split(airbnb)
train = training(airbnb_split)
test = testing(airbnb_split)


## -----------------------------------------------------------------------------
xgb = boost_tree(mode = "regression", engine = "xgboost")

mod = recipe(airbnb, price ~ .) |>
  step_dummy(all_nominal_predictors()) |>
  workflow(spec = xgb) |>
  fit(train)


## -----------------------------------------------------------------------------
#| label: fig-ml_predictions
#| fig.cap: "XGBoost predictions of rental prices against observed prices."
#| warning: false
#| out.width: 50%
#| fig.width: 6.29
p = predictions(mod, newdata = test)

ggplot(p, aes(x = price, y = estimate)) +
  geom_point(alpha = .2) +
  geom_abline(linetype = 3) +
  labs(x = "Observed Price", y = "Predicted Price") +
  xlim(0, 500) + ylim(0, 500) +
  coord_equal()


## -----------------------------------------------------------------------------
avg_predictions(mod,
  by = "unit_type",
  newdata = test)


## -----------------------------------------------------------------------------
#| fig.cap: "Relationship between predicted price, number of bedrooms, and type of rental unit in London."
#| label: fig-ml_pdp1
plot_predictions(mod, 
  by = c("bedrooms", "unit_type"), 
  newdata = airbnb) +
  labs(x = "# Bedrooms", y = "Predicted Price", linetype = "")


## -----------------------------------------------------------------------------
set.seed(48103)
airbnb_subset = airbnb[sample(1:nrow(airbnb), 10000), ]

grid = datagrid(
  bedrooms = unique,
  unit_type = unique,
  newdata = airbnb_subset,
  grid_type = "counterfactual")

plot_predictions(mod,
  newdata = grid,
  by = c("bedrooms", "unit_type")) +
  labs(x = "# Bedrooms", y = "Predicted Price", linetype = "")


## -----------------------------------------------------------------------------
#| eval: false
#| message: false
#| warning: false
# library("DALEXtra")
# pdp_rf = explain_tidymodels(
#     mod,
#     data = airbnb,
#     y = airbnb$price,
#     label = "XGBoost",
#     verbose = FALSE)
# pdp_rf = model_profile(pdp_rf,
#     N = 10000,
#     variables = "bedrooms",
#     groups = "unit_type")
# 
# png("~/Downloads/dalextra.png")
# plot(pdp_rf)
# dev.off()


## -----------------------------------------------------------------------------
avg_comparisons(mod,
  variables = list(bedrooms = 2),
  newdata = airbnb)

## -----------------------------------------------------------------------------
#| echo: false
cmp = avg_comparisons(mod,
  variables = list(bedrooms = 2),
  newdata = airbnb)


## -----------------------------------------------------------------------------
avg_comparisons(mod,
  variables = c("bedrooms", "Wireless Internet"),
  cross = TRUE,
  newdata = airbnb)

## -----------------------------------------------------------------------------
#| echo: false
cmp = avg_comparisons(mod,
  variables = c("bedrooms", "Wireless Internet"),
  cross = TRUE,
  newdata = airbnb)

