source("../src/eda.R")
library(tidymodels)
library(ranger)

set.seed(107)

mtry <- 2
trees <- 100
min_n <- 40

model_recipe_best <- recipe(Diabetes_binary ~ BMI + Age + GenHlth + count_common + count_rare, data = data_fixed_m) |>
  step_mutate(
    GenHlth = factor(GenHlth, ordered = TRUE),
    Age = factor(Age, ordered = TRUE)
  )

# model_recipe_best |> prep() |> bake(new_data = data_fixed_m)

RF_spec_best <- rand_forest(
  mtry = mtry,
  trees = trees,
  min_n = min_n) |>
  set_engine("ranger") |>
  set_mode("classification")

RF_wkf_best <- workflow() |>
  add_recipe(model_recipe_best) |>
  add_model(RF_spec_best)

RF_best_fit <- RF_wkf_best |>
    fit(data = data_fixed_m)


# predict
predictions <- predict(RF_best_fit, new_data = data_fixed_m)
predictions_bound <- bind_cols(data_fixed_m, predictions) |>
  mutate(Diabetes_binary = as.factor(Diabetes_binary),
         .pred_class = as.factor(.pred_class)) |>
    select(Diabetes_binary, .pred_class)
conf_matrix <- conf_mat(predictions_bound, truth = Diabetes_binary, estimate = .pred_class)
true_neg <- conf_matrix$table[1, 1]
true_pos <- conf_matrix$table[2, 2]
false_pos <- conf_matrix$table[2, 1]
false_neg <- conf_matrix$table[1, 2]
accuracy <- (true_pos + true_neg) / sum(conf_matrix$table)
precision <- true_pos / (true_pos + false_pos)

#* Predicts for Diabetes given the following parameters
#* @param HighBP High Blood Pressure 0 = No, 1 = Yes
#* @param HighChol High Cholesterol 0 = No, 1 = Yes
#* @param PhysActivity Physical Activity last 30 days 0 = No, 1 = Yes
#* @param DiffWalk Difficulty Walking 0 = No, 1 = Yes
#* @param Smoker Smoker (5 packs lifetime) 0 = No, 1 = Yes
#* @param CholCheck Cholesterol Check (last 5 years) 0 = No, 1 = Yes
#* @param Stroke Stroke 0 = No, 1 = Yes
#* @param HeartDiseaseorAttack Heart Disease or Attack 0 = No, 1 = Yes
#* @param HvyAlcoholConsump Heavy Alcohol Consumption 14/wk men, 7/wk women 0 = No, 1 = Yes
#* @param GenHlth General Health 1 = Excellent, 2 = Very Good, 3 = Good, 4 = Fair, 5 = Poor
#* @param Age Age 1 = 18-24, 2 = 25-29, 3 = 30-34, 4 = 35-39, 5 = 40-44, 6 = 45-49, 7 = 50-54, 8 = 55-59, 9 = 60-64, 10 = 65-69, 11 = 70-74, 12 = 75-79, 13 = 80+
#* @param BMI Body Mass Index
#* @get /pred
function(HighBP=0, HighChol=0, PhysActivity=1, DiffWalk=0, Smoker=0, CholCheck=1, Stroke=0, HeartDiseaseorAttack=0, HvyAlcoholConsump=0, GenHlth=2, Age=9, BMI=28){''
  new_data <- tibble(
    count_common = as.numeric(HighBP) + as.numeric(HighChol) + as.numeric(Smoker) + as.numeric(PhysActivity) + as.numeric(DiffWalk),
    count_rare = as.numeric(CholCheck) + as.numeric(Stroke) + as.numeric(HeartDiseaseorAttack) + as.numeric(HvyAlcoholConsump),
    HighBP = as.factor(HighBP),
    HighChol = as.factor(HighChol),
    PhysActivity = as.factor(PhysActivity),
    DiffWalk = as.factor(DiffWalk),
    Smoker = as.factor(Smoker),
    CholCheck = as.factor(CholCheck),
    Stroke = as.factor(Stroke),
    HeartDiseaseorAttack = as.factor(HeartDiseaseorAttack),
    HvyAlcoholConsump = as.factor(HvyAlcoholConsump),
    GenHlth = factor(GenHlth, levels = c(1, 2, 3, 4, 5), labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")),
    Age = factor(Age, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")),
    BMI = as.numeric(BMI)
  )
  predicts <- predict(RF_best_fit, new_data = new_data)
  predicts$.pred_class
}

# 3 example calls
library(httr)

# default
response <- GET("http://127.0.0.1:8000/pred",
                query = list(
                  HighBP = 0,
                  HighChol = 0,
                  PhysActivity = 1,
                  DiffWalk = 0,
                  Smoker = 0,
                  CholCheck = 1,
                  Stroke = 0,
                  HeartDiseaseorAttack = 0,
                  HvyAlcoholConsump = 0,
                  GenHlth = 2,
                  Age = 9,
                  BMI = 28
                ))

content(response)

# mostly having issues, also gets yes prediction
response <- GET("http://127.0.0.1:8000/pred",
                query = list(
                  HighBP = 1,
                  HighChol = 1,
                  PhysActivity = 0,
                  DiffWalk = 1,
                  Smoker = 1,
                  CholCheck = 1,
                  Stroke = 1,
                  HeartDiseaseorAttack = 1,
                  HvyAlcoholConsump = 1,
                  GenHlth = 5,
                  Age = 11,
                  BMI = 40
                ))

content(response)

# random other call
response <- GET("http://127.0.0.1:8000/pred",
                query = list(
                  HighBP = 1,
                  HighChol = 0,
                  PhysActivity = 1,
                  DiffWalk = 1,
                  Smoker = 0,
                  CholCheck = 1,
                  Stroke = 1,
                  HeartDiseaseorAttack = 0,
                  HvyAlcoholConsump = 0,
                  GenHlth = 1,
                  Age = 5,
                  BMI = 20
                ))

content(response)

#* Get info of author name and repository html
#* @get /info
function(){
  c("Steven Jones", "https://stevejones92.github.io/Project3/pages/EDA.html")
}


#* Get confusion matrix of best model prediction results
#* @serializer png
#* @get /confusion
function(){
  print(
    ggplot(data.frame(
      Prediction = c("Pos", "Pos", "Neg", "Neg"),
      Actual = c("Pos", "Neg", "Pos", "Neg"),
      Count = c(true_pos, false_pos, false_neg, true_neg)
    ), aes(x = Actual, y = Prediction)) +
      geom_tile(fill = "white", color = "black") + 
      geom_text(aes(label = Count), color = "black") +
      scale_x_discrete(limits = c("Pos", "Neg"))
  )
}

