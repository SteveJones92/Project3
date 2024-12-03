library(tidymodels)
library(parsnip)

set.seed(11)
data_split <- initial_split(data_fixed_m, prop = 0.10, strata = Diabetes_binary)
data_train <- training(data_split)
data_test <- testing(data_split)
data_5_fold <- vfold_cv(data_train, v=5, strata = Diabetes_binary)

model_recipe <- recipe(Diabetes_binary ~ BMI + Age + GenHlth + count_common + count_rare, data = data_train) |>
  step_mutate(
    GenHlth = factor(GenHlth, ordered = TRUE),
    Age = factor(Age, ordered = TRUE)
  )

model_recipe |> prep() |> bake(new_data = data_train)

class_tree_spec <- decision_tree(tree_depth = tune(), min_n = 5, cost_complexity = tune()) |>
  set_engine("rpart") |>
  set_mode ("classification")

class_tree_wkf <- workflow() |>
  add_recipe(model_recipe) |>
  add_model(class_tree_spec)

class_tree_grid <- class_tree_wkf |>
  tune_grid(
    resamples=data_5_fold,
    grid=grid_regular(
      cost_complexity(range(-4, -1)),
      tree_depth(range(5L, 15L)),
      levels=c(5, 5)),
    metrics = metric_set(mn_log_loss, accuracy)
  )
class_tree_grid |>
  collect_metrics() |>
  print(n=Inf)

class_tree_grid |>
  collect_metrics() |>
  filter(.metric == "mn_log_loss") |>
  ggplot(aes(x=tree_depth, y=mean, color=as.factor(cost_complexity))) +
  geom_line()

class_tree_grid |>
  collect_metrics() |>
  filter(.metric == "mn_log_loss") |>
  ggplot(aes(x=cost_complexity, y=mean, color=as.factor(tree_depth))) +
  geom_line()

best_log_loss <- class_tree_grid |>
    select_best(metric = "mn_log_loss")
best_log_loss

class_tree_final_fit <- class_tree_wkf |>
    finalize_workflow(best_log_loss) |>
    last_fit(data_split, metrics = metric_set(mn_log_loss, accuracy))

tree_final_model <- extract_workflow(class_tree_final_fit)
tree_final_model |>
  extract_fit_engine() |>
  rpart.plot::rpart.plot(roundint = FALSE)
class_tree_final_fit |>
  collect_metrics()


# -----------------------
# Random Forest
RF_spec <- rand_forest(
  mtry = tune(),
  trees = tune()) |>  # default is 500
  set_engine("ranger", importance="impurity") |>
  set_mode("classification")

RF_wkf <- workflow() |>
  add_recipe(model_recipe) |>
  add_model(RF_spec)

RF_grid <- RF_wkf |>
  tune_grid(
    resamples = data_5_fold,
    grid = grid_regular(
      mtry(range(1L, 4L)),
      trees(range(100L, 2000L)),
      levels=c(4, 5)
    ),
    metrics = metric_set(mn_log_loss, accuracy),
    control = control_grid(verbose = TRUE)
  )

RF_grid |>
  collect_metrics() |>
  filter(.metric == "mn_log_loss") |>
  ggplot(aes(x=mtry, y=mean, color=as.factor(trees))) +
  geom_line()

RF_grid |>
  collect_metrics() |>
  filter(.metric == "mn_log_loss") |>
  ggplot(aes(x=trees, y=mean, color=as.factor(mtry))) +
  geom_line()

best_log_loss <- RF_grid |>
    select_best(metric = "mn_log_loss")
best_log_loss

RF_final_fit <- RF_wkf |>
    finalize_workflow(best_log_loss) |>
    last_fit(data_split, metrics = metric_set(mn_log_loss, accuracy))

RF_final_model <- extract_fit_engine(RF_final_fit)
importance_values <- RF_final_model$variable.importance
importance_df <- data.frame(
  term = names(importance_values),
  value = importance_values
) |>
  as_tibble() |>
  arrange(desc(value))

importance_df |>
  mutate(term = factor(term, levels = term)) |>
  ggplot(aes(x=term, y=value)) +
  geom_bar(stat = "identity") +
  coord_flip()

RF_final_fit |>
    collect_metrics()