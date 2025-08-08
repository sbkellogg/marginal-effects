## -----------------------------------------------------------------------------
library(future)
library(splines)
library(marginaleffects)
options(marginaleffects_parallel_inferences = TRUE)
options(marginaleffects_parallel_packages = c("marginaleffects", "splines"))
plan(multisession)


## -----------------------------------------------------------------------------
#| include: false
options(width = 200)


## -----------------------------------------------------------------------------
mod <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
avg_comparisons(mod)


## -----------------------------------------------------------------------------
avg_comparisons(mod) |> inferences(method = "rsample")


## -----------------------------------------------------------------------------
avg_predictions(mod, by = "Species") |> inferences(method = "rsample")

hypotheses(mod, hypothesis = ratio ~ sequential) |> inferences(method = "rsample")


## -----------------------------------------------------------------------------
library(marginaleffects)

lalonde <- get_dataset("lalonde")

# Define custom estimator function
# input: data frame
# output: marginaleffects object
estimator <- function(data) {
    # Step 1: Estimate propensity scores
    fit1 <- glm(treat ~ age + educ + race, family = binomial, data = data)
    ps <- predict(fit1, type = "response") 
    
    # Step 2: Fit weighted outcome model
    m <- lm(re78 ~ treat * (re75 + age + educ + race),
        data = data, weight = ps
    )
    
    # Step 3: Compute average treatment effect by G-computation
    avg_comparisons(m, variables = "treat", wts = ps, vcov = FALSE)
}

# Test the estimator on the full dataset
estimator(lalonde)

# Bootstrap the entire procedure
cmp <- estimator(lalonde) |>
    inferences(method = "rsample", estimator = estimator)


## -----------------------------------------------------------------------------
cmp


## -----------------------------------------------------------------------------
d <- get_draws(cmp)$draw
hist(d, 
     main = "Bootstrap Distribution of Average Treatment Effect",
     xlab = "")

