## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  eval = FALSE,
  collapse = TRUE,
  fig.width = 6,
  fig.asp = .4,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)


## ----results = "hide", message=FALSE------------------------------------------
library(reticulate)
library(marginaleffects)

model <- '
## Model code adapted from the NumPyro documtation under Apache License:
## https://num.pyro.ai/en/latest/tutorials/bayesian_hierarchical_linear_regression.html

import pandas as pd
import numpy as np
import numpyro
from numpyro.infer import SVI, Predictive, MCMC,NUTS, autoguide, TraceMeanField_ELBO
import numpyro.distributions as dist
from numpyro.infer.initialization import init_to_median, init_to_uniform,init_to_sample
from jax import random
from sklearn.preprocessing import LabelEncoder
import pickle

def load_df():
    train = pd.read_csv("https://raw.githubusercontent.com/vincentarelbundock/modelarchive/main/data-raw/osic_pulmonary_fibrosis.csv")
    return train


def model(data, predict = False):
    FVC_obs = data["FVC"].values  if predict == False else None
    patient_encoder = LabelEncoder()
    Age_obs = data["Age"].values
    patient_code = patient_encoder.fit_transform(data["Patient"].values)
    μ_α = numpyro.sample("μ_α", dist.Normal(0.0, 500.0))
    σ_α = numpyro.sample("σ_α", dist.HalfNormal(100.0))

    age = numpyro.sample("age", dist.Normal(0.0, 500.0))

    n_patients = len(np.unique(patient_code))

    with numpyro.plate("plate_i", n_patients):
        α = numpyro.sample("α", dist.Normal(μ_α, σ_α))

    σ = numpyro.sample("σ", dist.HalfNormal(100.0))
    FVC_est = α[patient_code] + age * Age_obs

    with numpyro.plate("data", len(patient_code)):
        numpyro.sample("obs", dist.Normal(FVC_est, σ), obs=FVC_obs)


def fit_mcmc_model(train_df, samples = 1000):
    numpyro.set_host_device_count(4)
    rng_key = random.PRNGKey(0)
    mcmc = MCMC(
        NUTS(model),
        num_samples=samples,
        num_warmup=1000,
        progress_bar=True,
        num_chains = 4
        )
    
    mcmc.run(rng_key, train_df)

    get_draws = mcmc.get_samples()

    with open("mcmc_get_draws.pickle", "wb") as handle:
        pickle.dump(get_draws, handle, protocol=pickle.HIGHEST_PROTOCOL)

def predict_mcmc(data):

    with open("mcmc_get_draws.pickle", "rb") as handle:
        get_draws = pickle.load(handle)

    predictive = Predictive(model = model,posterior_samples=get_draws)
    samples = predictive(random.PRNGKey(1), data, predict = True)
    y_pred = samples["obs"]
    # transpose so that each column is a draw and each row is an observation
    y_pred = np.transpose(np.array(y_pred))

    return y_pred 
'

## save python script to temp file
tmp <- tempfile()
cat(model, file = tmp)

## load functions
source_python(tmp)

## download data
df <- load_df()

## fit model
fit_mcmc_model(df)


## -----------------------------------------------------------------------------
mod <- data.frame()
class(mod) <- "custom"

options("marginaleffects_model_classes" = "custom")


## -----------------------------------------------------------------------------
get_predict.custom <- function(model, newdata, ...) {
    pred <- predict_mcmc(newdata)
    out <- data.frame(
        rowid = seq_len(nrow(newdata)),
        predicted = apply(pred, 1, stats::median)
    )
    attr(out, "get_draws") <- pred
    return(out)
}


## ----warning = FALSE----------------------------------------------------------
## predictions on the original dataset
predictions(mod, newdata = df) |> head()

## predictions for user-defined predictor values
predictions(mod, newdata = datagrid(newdata = df, Age = c(60, 70)))

predictions(mod, newdata = datagrid(newdata = df, Age = range))

## average predictions by group
predictions(mod, newdata = df, by = "Sex")

## contrasts (average)
avg_comparisons(mod, variables = "Age", newdata = df)

avg_comparisons(mod, variables = list("Age" = "sd"), newdata = df)

## slope (elasticity)
avg_slopes(mod, variables = "Age", slope = "eyex", newdata = df)

