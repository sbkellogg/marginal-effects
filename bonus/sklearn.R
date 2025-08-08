## from marginaleffects import *
## from statsmodels.formula.api import ols
## import polars.selectors as cs
## from sklearn.pipeline import make_pipeline
## from sklearn.model_selection import train_test_split
## from sklearn.preprocessing import OneHotEncoder, FunctionTransformer
## from sklearn.linear_model import LinearRegression
## from sklearn.compose import make_column_transformer
## from xgboost import XGBRegressor

## military = get_dataset("military")
## 
## mod_sk = fit_sklearn(
##     "rank ~ officer + hisp + branch",
##     data=military,
##     engine=LinearRegression(),
## )

## avg_predictions(mod_sk, by="branch")

## mod_sm = ols(
##     "rank ~ officer + hisp + branch",
##     data=military.to_pandas()).fit()
## avg_predictions(mod_sm, by="branch")

## airbnb = get_dataset("airbnb")
## 
## train, test = train_test_split(airbnb)

## catvar = airbnb.select(~cs.numeric()).columns
## preprocessor = make_column_transformer(
##     (OneHotEncoder(), catvar),
##     remainder=FunctionTransformer(lambda x: x.to_numpy()),
## )
## pipeline = make_pipeline(preprocessor, XGBRegressor())

## def selector(data):
##     y = data.select(cs.by_name("price", require_all=False))
##     X = data.select(~cs.by_name("price", require_all=False))
##     return y, X
## 
## mod = fit_sklearn(selector, data=train, engine=pipeline)
## 
## avg_predictions(mod, newdata=test, by="unit_type")

## avg_comparisons(mod, variables={"bedrooms": 2}, newdata=test)
