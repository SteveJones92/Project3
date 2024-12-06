library(tidymodels)
library(parsnip)

set.seed(107)
data_split <- initial_split(data_fixed_m, prop = 0.1, strata = Diabetes_binary)
data_train <- training(data_split)
data_test <- testing(data_split)
data_5_fold <- vfold_cv(data_train, v=5, strata = Diabetes_binary)

model_recipe <- recipe(Diabetes_binary ~ BMI + Age + GenHlth + count_common + count_rare, data = data_train) |>
  step_mutate(
    GenHlth = factor(GenHlth, ordered = TRUE),
    Age = factor(Age, ordered = TRUE)
  )

model_recipe |> prep() |> bake(new_data = data_train)

class_tree_spec <- decision_tree(tree_depth = tune(), min_n = tune(), cost_complexity = tune()) |>
  set_engine("rpart") |>
  set_mode ("classification")

class_tree_wkf <- workflow() |>
  add_recipe(model_recipe) |>
  add_model(class_tree_spec)

class_tree_grid <- class_tree_wkf |>
  tune_grid(
    resamples=data_5_fold,
    grid=grid_regular(
        tree_depth(range(6L, 20L)),
        cost_complexity(range(-10, -1)),
        min_n(range(2L, 40L)),
      levels=c(5, 10, 3)),
    metrics = metric_set(mn_log_loss, accuracy),
    control = control_grid(verbose = TRUE)
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
  mtry = tune(),  # default is 1
  trees = tune(),  # default is 500
  min_n = tune()) |>  # default is 2
  set_engine("ranger", importance="impurity") |>
  set_mode("classification")

RF_wkf <- workflow() |>
  add_recipe(model_recipe) |>
  add_model(RF_spec)

RF_grid <- RF_wkf |>
  tune_grid(
    resamples = data_5_fold,
    grid = grid_regular(
      mtry(range(1L, 2L)),
      trees(range(1000L, 3000L)),
      min_n(range(2L, 40L)),
      levels=c(2, 3, 4)
    ),
    metrics = metric_set(mn_log_loss, accuracy),
    control = control_grid(verbose = TRUE)
  )

RF_grid |>
  collect_metrics() |>
  filter(.metric == "mn_log_loss") |>
  ggplot(aes(x=mtry, y=mean, color=as.factor(trees))) +
  facet_wrap(~min_n) +
  geom_line()

RF_grid |>
  collect_metrics() |>
  filter(.metric == "mn_log_loss") |>
  ggplot(aes(x=trees, y=mean, color=as.factor(mtry))) +
  facet_wrap(~min_n) +
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