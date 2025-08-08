## -----------------------------------------------------------------------------
#| echo: false
#| tbl-cap: Main functions of the `marginaleffects` package.
#| label: tbl-marginaleffects_functions
library(tinytable)
df = data.frame(
  Goal = c("Predictions", "", "", "Comparisons", "", "", "Slopes", "", "", "Grids", "Hypotheses and Equivalence", "Bayes, Bootstrap, Simulation", ""),
  Function = c("`predictions()`", "`avg_predictions()`", "`plot_predictions()`", "`comparisons()`", "`avg_comparisons()`", "`plot_comparisons()`", 
               "`slopes()`", "`avg_slopes()`", "`plot_slopes()`", "`datagrid()`", "`hypotheses()`", "`get_draws()`", "`inferences()`"),
  stringsAsFactors = FALSE
)
tt(df) |> format_tt(j = 2, markdown = TRUE)


## -----------------------------------------------------------------------------
library(marginaleffects)
dat = get_dataset("Titanic", "Stat2Data")
dat[1:5, c("Name", "Survived", "Age")]


## -----------------------------------------------------------------------------
#| eval: false
# get_dataset(search = "Titanic")


## -----------------------------------------------------------------------------
#| eval: false
# get_dataset("Titanic", "Stat2Data", docs = TRUE)

