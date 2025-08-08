# load packages ---------------------------------------------------------------
library(tidyverse)
library(marginaleffects)


# Load the dataset -----------------------------------------------------------
batting = get_dataset("Batting", package = "Lahman") |>
  select(playerID, yearID, teamID, lgID, AB, HR) |>
  filter(yearID >= 2000, yearID <= 2010, AB >= 100)
