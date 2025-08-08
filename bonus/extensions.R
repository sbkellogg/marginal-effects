## ----include = FALSE----------------------------------------------------------
options(width = 1000)
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 6,
  fig.asp = .4,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)


## -----------------------------------------------------------------------------
lm_manual <- function(f, data, ...) {
    # design matrix
    X <- model.matrix(f, data = data)
    # response matrix
    Y <- data[[as.character(f[2])]]
    # coefficients
    b <- solve(crossprod(X)) %*% crossprod(X, Y)
    Yhat <- X %*% b
    # variance-covariance matrix
    e <- Y - Yhat
    df <- nrow(X) - ncol(X)
    s2 <- sum(e^2) / df
    V <- s2 * solve(crossprod(X))
    # model object
    out <- list(
        d = data,
        f = f,
        X = X,
        Y = Y,
        V = V,
        b = b)
    # class name: lm_manual
    class(out) <- c("lm_manual", "list")
    return(out)
}


## -----------------------------------------------------------------------------
model <- lm_manual(mpg ~ hp + drat, data = mtcars)
model$b

model_lm <- lm(mpg ~ hp + drat, data = mtcars)
coef(model_lm)


## -----------------------------------------------------------------------------
library(marginaleffects)

options("marginaleffects_model_classes" = "lm_manual")


## -----------------------------------------------------------------------------
get_coef.lm_manual <- function(model, ...) {
    b <- model$b
    b <- setNames(as.vector(b), row.names(b))
    return(b)
}

set_coef.lm_manual <- function(model, coefs, ...) {
    out <- model
    out$b <- coefs
    return(out)
}

get_vcov.lm_manual <- function(model, ...) {
    return(model$V)
}

get_predict.lm_manual <- function(model, newdata, ...) {
    newX <- model.matrix(model$f, data = newdata)
    Yhat <- newX %*% model$b
    out <- data.frame(
        rowid = seq_len(nrow(Yhat)),
        estimate = as.vector(Yhat))
    return(out)
}


## -----------------------------------------------------------------------------
get_coef(model)

get_vcov(model)

get_predict(model, newdata = head(mtcars))


## -----------------------------------------------------------------------------
avg_slopes(model, newdata = mtcars, variables = c("hp", "drat"))

predictions(model, newdata = mtcars) |> head()


## -----------------------------------------------------------------------------
library(mclogit)
library(data.table)

model <- mblogit(
    factor(gear) ~ am + mpg,
    data = mtcars,
    trace = FALSE)


## -----------------------------------------------------------------------------
options("marginaleffects_model_classes" = "customclass")

model_custom <- model

class(model_custom) <- c("customclass", class(model))


## -----------------------------------------------------------------------------
get_predict.customclass <- function(model, newdata, ...) {
    out <- predict(model, newdata = newdata, type = "link")
    out <- cbind(0, out)
    colnames(out)[1] <- dimnames(model$D)[[1]][[1]]
    out <- out - rowMeans(out)
    out <- as.data.frame(out)
    out$rowid <- seq_len(nrow(out))
    out <- data.table(out)
    out <- melt(
        out,
        id.vars = "rowid",
        value.name = "estimate",
        variable.name = "group")
}


## -----------------------------------------------------------------------------
avg_predictions(model)

avg_predictions(model_custom)

