library(plumber)
library(tidyverse)
library(tidymodels)

golf_dat <- readr::read_csv("golf_dat.csv")

golf_dat_factor <-
  golf_dat |>
  dplyr::mutate(across(material:thickness, ~ as.factor(.x)))

set.seed(1234)

golf_split <- initial_split(golf_dat_factor)
golf_train <- training(golf_split)
golf_test <- testing(golf_split)

golf_rec <-
  recipe(iron ~ ., data = golf_train) |>
  step_dummy(all_factor_predictors()) |> 
  prep(training = golf_train)

#* @apiTitle 5-iron golf club flight distance prediction

#* Predict the distance of a ball based on its characteristics
#* @param material material of the ball
#* @param dimples number of dimples
#* @param diameter diameter of the ball
#* @param thickness thickness of the ball
#* @get /iron

function(material, dimples, diameter, thickness) {
  
  new_dat <- tibble(material, dimples, diameter, thickness)

  golf_linear <- linear_reg()

  golf_wflow <-
    workflow() |>
    add_model(golf_linear) |>
    add_recipe(golf_rec)
  
  golf_fit <-
    golf_wflow |>
    fit(data = golf_train)
  
  iron <-
    predict(golf_fit, new_dat)$.pred
  
}