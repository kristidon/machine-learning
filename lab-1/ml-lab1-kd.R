
# wrangling, etc. 
library(tidyverse)
# modeling
library(tidymodels)

d <- read_csv("data/ngsschat-processed-data.csv")
d

# predicting one of outcomes TS/SB or not

#spot into training & testing sets; using 70% to train - use less data for testing
# split randomly; can also sample within strata for smaller groups or particular interests
train_test_split <- initial_split(d, prop = .70)
# can use strata option to account for groups and ensure representation 
# train_test_split <- initial_split(d, prop = .70, strata=)
data_train <- training(train_test_split)
# do not use data for training at all; keep separate until end 
data_test <- testing(train_test_split)

# code is DV; predicting it by ., which means all other variables in dataset beyond that one; and to use the training data
my_rec <- recipe(code ~ ., data = data_train)

# specify model
my_mod <-
  logistic_reg() %>% # type of model is logistic regression
  set_engine("glm") %>% # use r's glm to estimate model
  set_mode("classification") # predict categories, not number

# specify workflow
my_wf <-
  workflow() %>%
  add_model(my_mod) %>% 
  add_recipe(my_rec)

# taking workflow, fit model using training data
fitted_model <- fit(my_wf, data = data_train)
# warning okay - means it may or may not be an issue

# take trained model to predict test data. 
final_fit <- last_fit(fitted_model, train_test_split)

# evaluate accuracy
final_fit %>%
  collect_metrics()
# .878 - about 87% of the time the model predicted the same as the human coded data 

final_fit |>
  collect_predictions()
# model didn't predicted same as humans in 1 or 2 but did in 3, etc. # times predicted the same / number of rows

# less than 20% of overall data needed for this portion - to train, really to estimate a relative predictive accuracy of how the model fits in the rest of the data 

