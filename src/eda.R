library(dplyr)
library(ggplot2)
library(tidymodels)
library(parsnip)

# load data
data <- read.csv("data/diabetes_binary_health_indicators_BRFSS2015.csv")

# data summaries from looking at the kaggle descriptions
# Diabetes_binary - thing to predict - 0 = no diabetes, 1 = prediabetes or diabetes
# HighBP - high blood pressure - 0 = no, 1 = yes
# HighChol - high cholesterol - 0 = no, 1 = yes
# CholCheck - cholesterol checked - 0 = no, 1 = yes (within 5 years)
# BMI - body mass index
# Smoker - 5 packs (100 cigarettes lifetime) - 0 = no, 1 = yes
# Stroke - ever told had stroke - 0 = no, 1 = yes
# HeartDiseaseorAttack - CHD or MI - 0 = no, 1 = yes
# PhysActivity - last 30 days - 0 = no, 1 = yes
# Fruits - 1 or more fruits/day - 0 = no, 1 = yes
# Veggies - 1 or more veggies/day - 0 = no, 1 = yes
# HvyAlcoholConsump - 14/wk men or 7/wk women - 0 = no, 1 = yes
# AnyHealthcare - 0 = no, 1 = yes
# NodocbcCost - needed to see a doctor but couldn't past 12 month - 0 = no, 1 = yes
# GenHlth - general health - 1 = excellent, 2 = very good, 3 = good, 4 = fair, 5 = poor
# MentHlth - days thinking about in last 30 days - 0-30
# PhysHlth - days thinking about in last 30 days - 0-30
# DiffWalk - walking or stairs difficulty - 0 = no, 1 = yes
# Sex - 0 = female, 1 = male
# Age - grouped by 5 years - 0-13 - first is 18-24 and last is 80+
# Education = 1-6, from never attended to college graduate
# Income - 1-8, from less than $10,000 to $75,000+

# no NAs
sum(is.na(data))

n <- nrow(data) # 253680 items
table(data$Diabetes_binary) / n # 14% have diabetes, would get 86% simply by guessing no on all


helper <- function(category) {
  # finds the proportions of the category for yes/no categories
  # finds the proportion of each combination
  # finds the percentages of diabetes based on the category
  prop <- table(data[[category]]) / n
  ct <- table(ifelse(data$Diabetes_binary == 1, "Diabetes", "NoDiabetes" ),
              ifelse(data[[category]] == 1, category, paste0("No", category))) / n
  explained <- t(t(ct) / ( ct[1,] + ct[2,]))
  max(explained[,1]) * prop[2] + max(explained[,2]) * prop[1]
  print(prop)
  print(ct)
  print(explained)
}

helper("HighBP")
helper("HighChol")
helper("CholCheck")
helper("Smoker")
helper("Stroke")
helper("HeartDiseaseorAttack")
helper("PhysActivity")
helper("Fruits")
helper("Veggies")
helper("HvyAlcoholConsump")
helper("AnyHealthcare")
helper("NoDocbcCost")
helper("DiffWalk")
helper("Sex")

table(f=data$Fruits, bp=data$HighBP, d=data$Diabetes_binary)

# leftover BMI, GenHlth, MentHlth, PhysHlth, Age, Education, Income
cor(data$MentHlth, data$Diabetes_binary)
cor(data$PhysHlth, data$Diabetes_binary)
cor(data$GenHlth, data$Diabetes_binary)
cor(data$BMI, data$Diabetes_binary)
cor(data$Age, data$Diabetes_binary)
cor(data$Education, data$Diabetes_binary)
cor(data$Income, data$Diabetes_binary)

ggplot(data, aes(x = Age, fill = factor(Diabetes_binary))) +
  geom_bar(position = "fill")
ggplot(data, aes(x = Income, fill = factor(Diabetes_binary))) +
  geom_bar(position = "fill")
ggplot(data, aes(x = Education, fill = factor(Diabetes_binary))) +
  geom_bar(position = "fill")
ggplot(data, aes(x = GenHlth, fill = factor(Diabetes_binary))) +
  geom_bar(position = "fill")
ggplot(data, aes(x = MentHlth, fill = factor(Diabetes_binary))) +
  geom_bar(position = "fill")
ggplot(data, aes(x = PhysHlth, fill = factor(Diabetes_binary))) +
  geom_bar(position = "fill")
ggplot(data, aes(x = BMI, fill = factor(Diabetes_binary))) +
    geom_histogram(position = "fill")
hist(data$BMI, breaks = 20)

# summing across the common items that have an effect
# summing across the rare items that have an effect
data_fixed <- data |>
  mutate(
    count_common = rowSums(across(c(HighBP, HighChol, PhysActivity, DiffWalk, Smoker))),
    count_rare = rowSums(across(c(CholCheck, Stroke, HeartDiseaseorAttack, HvyAlcoholConsump)))
  ) |>
  select(-c(Fruits, Veggies, AnyHealthcare, NoDocbcCost, Income, Education, PhysHlth, MentHlth, Sex))

table(data_fixed$Diabetes_binary, data_fixed$count_common)
table(data_fixed$Diabetes_binary, data_fixed$count_rare)

str(data_fixed)
data_fixed <- data_fixed |>
  mutate(
    HighBP = factor(HighBP, levels = c(0, 1), labels = c("No", "Yes")),
    HighChol = factor(HighChol, levels = c(0, 1), labels = c("No", "Yes")),
    CholCheck = factor(CholCheck, levels = c(0, 1), labels = c("No", "Yes")),
    Smoker = factor(Smoker, levels = c(0, 1), labels = c("No", "Yes")),
    Stroke = factor(Stroke, levels = c(0, 1), labels = c("No", "Yes")),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = c(0, 1), labels = c("No", "Yes")),
    PhysActivity = factor(PhysActivity, levels = c(0, 1), labels = c("No", "Yes")),
    HvyAlcoholConsump = factor(HvyAlcoholConsump, levels = c(0, 1), labels = c("No", "Yes")),
    DiffWalk = factor(DiffWalk, levels = c(0, 1), labels = c("No", "Yes")),
    Diabetes_binary = factor(Diabetes_binary, levels = c(0, 1), labels = c("No", "Yes"))
  )
str(data_fixed)

set.seed(11)
data_split <- initial_split(data_fixed, prop = 0.75, strata = Diabetes_binary)
data_train <- training(data_split)
data_test <- testing(data_split)
data_10_fold <- vfold_cv(data_train, v = 10, strata = Diabetes_binary)

REG_TREE_spec <- decision_tree(tree_depth = tune(), min_n = 5, cost_complexity = tune()) |>
  set_engine("rpart") |>
  set_mode ("classification")

model_recipe <- recipe(Diabetes_binary ~ BMI + Age + GenHlth + count_common + count_rare, data = data_train) |>
  #step_rm(HighBP, HighChol, CholCheck, Smoker, Stroke, HeartDiseaseorAttack, PhysActivity, HvyAlcoholConsump, DiffWalk) |>
  step_normalize(all_numeric())

model_recipe |> prep() |> bake(new_data = data_train)

REG_TREE_wkf <- workflow() |>
  add_recipe(model_recipe) |>
  add_model(REG_TREE_spec)

REG_TREE_grid <- REG_TREE_wkf |>
  tune_grid(resamples=data_10_fold, grid=grid_regular(cost_complexity(range(-5, -1)), tree_depth(range(3L, 8L)), levels=c(4, 3)))

REG_TREE_grid |>
  collect_metrics() |>
  filter(.metric == "accuracy") |>
  ggplot(aes(x=tree_depth, y=mean, color=.metric)) +
  geom_line()

REG_TREE_grid |>
  collect_metrics() |>
  filter(.metric == "accuracy") |>
  ggplot(aes(x=cost_complexity, y=mean, color=.metric)) +
  geom_line()

best_accuracy <- REG_TREE_grid |>
    select_best(metric = "accuracy")
best_accuracy

REG_TREE_final_fit <- REG_TREE_wkf |>
    finalize_workflow(best_accuracy) |>
    last_fit(data_split, metrics = metric_set(accuracy, mn_log_loss))

tree_final_model <- extract_workflow(REG_TREE_final_fit)
tree_final_model |>
  extract_fit_engine() |>
  rpart.plot::rpart.plot(roundint = FALSE)
REG_TREE_final_fit |>
  collect_metrics()