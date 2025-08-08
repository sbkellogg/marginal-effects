## -----------------------------------------------------------------------------
#| echo: false
#| tbl-cap: A forced-choice task in a conjoint experiment.
library(tinytable)
tmp <- data.frame(
  "Attributes" = c("Language Skills", "Job"),
  `Profile 1` = c("Fluent in English", "Construction worker"),
  `Profile 2` = c("Broken English", "Nurse"),
  check.names = FALSE
)
tt(tmp)


## -----------------------------------------------------------------------------
library(marginaleffects)
dat <- get_dataset("immigration")
dat[dat$respondent == 4, ]


## -----------------------------------------------------------------------------
mod <- lm(choice ~ job * language, data = dat)


## -----------------------------------------------------------------------------
avg_predictions(mod, 
  newdata = "balanced",
  by = "language", 
  vcov = ~respondent)


## -----------------------------------------------------------------------------
avg_predictions(mod, 
  hypothesis = "b1 = b4",
  newdata = "balanced",
  by = "language", 
  vcov = ~respondent)

## -----------------------------------------------------------------------------
#| echo: false
cmp <- avg_predictions(mod, 
  newdata = "balanced",
  by = "language", 
  vcov = ~respondent,
  hypothesis = "b1 = b4")$estimate


## -----------------------------------------------------------------------------
avg_comparisons(mod, vcov = ~respondent, newdata = "balanced")


## -----------------------------------------------------------------------------
#| include: false
#| echo: false
#| eval: false
# library(cjoint)
# amce_results <- amce(
#   choice ~ language * job,
#   data = dat,
#   cluster = TRUE,
#   respondent.id = "respondent")
# summary(amce_results)$amce


## -----------------------------------------------------------------------------
dat <- by(dat, ~ respondent + task, \(x) transform(x,
  language.alt = rev(language),
  job.alt = rev(job)
))
dat <- do.call(rbind, dat)


## -----------------------------------------------------------------------------
subset(dat, respondent == 4 & task == 1)


## -----------------------------------------------------------------------------
mod <- lm(choice ~ language * language.alt + job * job.alt, data = dat)


## -----------------------------------------------------------------------------
avg_predictions(mod, 
  by = c("language", "language.alt"), 
  newdata = subset(language.alt == "fluent"),
  vcov = ~respondent)


## -----------------------------------------------------------------------------
#| warning: false
#| message: false
#| echo: false
#| eval: false
# library(afcp)
# library(cjoint)
# amce_results <- amce(
#   choice ~ language * job,
#   data = dat,
#   cluster = TRUE,
#   respondent.id = "respondent")
# afcp_results <- afcp(
#   amce_results,
#   respondent.id = "respondent",
#   task.id = "task",
#   profile.id = "profile",
#   attribute = "language")
# afcp_results$afcp


## -----------------------------------------------------------------------------
avg_predictions(mod, 
  by = c("language", "language.alt"), 
  hypothesis = "b4 - b2 = 0",
  newdata = subset(language.alt == "fluent"),
  vcov = ~respondent)

## -----------------------------------------------------------------------------
#| echo: false
p <- avg_predictions(mod, 
  by = c("language", "language.alt"), 
  hypothesis = "b4 - b2 = 0",
  newdata = subset(language.alt == "fluent"),
  vcov = ~respondent)

