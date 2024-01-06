library(keras)
library(parsnip)
library(caret)
library(yardstick)
library(modelsummary)
library(MLmetrics)
library(e1071) ## SVM
library(vip)
library(stats)
library(rsample)
library(kableExtra)

# taking small samples

set.seed(3047)

NAtoMean$normalized_weights <- NAtoMean$interview_weight / 
  sum(NAtoMean$interview_weight) * nrow(NAtoMean)

resampled_data <- NAtoMean %>% 
  slice_sample(weight_by = normalized_weights, n= nrow(NAtoMean), replace= TRUE)

CAD2 <- filter(resampled_data, had_coronary_heart_disease == 2)
CAD1 <- filter(resampled_data, had_coronary_heart_disease == 1)
CAD2sample <- sample_n(CAD2, 200)

CAD <- rbind(CAD1, CAD2sample) %>% distinct()

# convert to factor
CAD$had_coronary_heart_disease <- as.factor(CAD$had_coronary_heart_disease)


#### USING SMALL SAMPLE OF DATA ####
split <- initial_split(CAD, prop = 0.75)

train <- training(split)
test <- testing(split)


features <- c("age_at_screening_yrs", "ldl_cholesterol_mg_d_l", "had_discomfort_in_chest",
              "mean_systolic", "had_heart_attack", "had_thyroid_problem",
              "told_to_reduce_calories", "had_angina", "country_of_birth",
              "told_to_reduce_salt", "told_to_increase_exercise", "had_copd_emphysema_ch_b")

selected_columns <- c("age_at_screening_yrs", "ldl_cholesterol_mg_d_l", "had_discomfort_in_chest",
                      "mean_systolic", "had_heart_attack", "had_thyroid_problem",
                      "told_to_reduce_calories", "had_angina", "country_of_birth",
                      "told_to_reduce_salt", "told_to_increase_exercise", "had_copd_emphysema_ch_b",
                      "had_coronary_heart_disease")

test <- test %>% select(all_of(selected_columns))


formula <- as.formula(paste("had_coronary_heart_disease ~", paste(features, collapse = " + ")))

#### XGBOOST TREE ######

boost_small_fit <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification") |>
  fit(formula, data = train)


results <- test

results$boost_pred <- predict(boost_small_fit, new_data = test)$.pred_class


confusionMatrix(results$boost_pred, 
                as.factor(results$had_coronary_heart_disease),
                mode = "everything")


f1_boost <- F1_Score(y_pred = results$boost_pred, y_true = test$had_coronary_heart_disease,
                           positive = 1)

boost_accuracy <- mean(results$boost_pred == test$had_coronary_heart_disease)

vip(boost_small_fit)

##### RANDOM FOREST ######

forest_small_fit <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification") |>
  fit(formula,
      data = train)

results$forest_pred <- predict(forest_small_fit, new_data = test)$.pred_class

f1_rand_forest <- F1_Score(y_pred = results$forest_pred, y_true = test$had_coronary_heart_disease,
                           positive = 1)

rand_forest_accuracy <- mean(results$forest_pred == test$had_coronary_heart_disease)

confusionMatrix(results$forest_pred, 
                as.factor(results$had_coronary_heart_disease),
                mode = "everything")


#### Support Vector Machines ####

svm_model <- svm(formula, data = train, 
                 kernel = "radial", cost = 1)

svm_predictions <- predict(svm_model, test)
svm_predictions <- as.numeric(svm_predictions)

svm_accuracy <- mean(svm_predictions == as.numeric(test$had_coronary_heart_disease))
svm_f1 <- F1_Score(y_pred = svm_predictions,
                   y_true = test$had_coronary_heart_disease)



#### K-Nearest Neighbor #####

knn_model <- train(formula, data = train, 
                   method = "knn", preProcess = c("center", "scale"))

knn_predictions <- predict(knn_model, test)

knn_accuracy <- mean(knn_predictions == test$had_coronary_heart_disease)

knn_f1 <- F1_Score(y_pred = knn_predictions, y_true = test$had_coronary_heart_disease)


#### Logistic Regression ####
logreg_clean <- train
logreg_clean$had_coronary_heart_disease <- logreg_clean$had_coronary_heart_disease %>%
  as.numeric()

logreg_clean <- logreg_clean %>%
  mutate(had_coronary_heart_disease = ifelse(had_coronary_heart_disease == 2, 0, had_coronary_heart_disease))
logreg_model <- glm(formula, 
                    data = logreg_clean, family = binomial)

logreg_predictions <- predict(logreg_model, test, type = "response")
threshold <- 0.5

logreg_binary_pred <- ifelse(logreg_predictions > threshold, 1, 2)

logreg_accuracy <- mean(logreg_binary_pred == test$had_coronary_heart_disease)


logreg_f1 <- F1_Score(y_pred = logreg_binary_pred, y_true = test$had_coronary_heart_disease)


##### nerual network #####
train_data <- select(train, age_at_screening_yrs,
                     ldl_cholesterol_mg_d_l,had_discomfort_in_chest,
                     mean_systolic, had_heart_attack,
                     had_thyroid_problem, told_to_reduce_calories,
                     had_angina, country_of_birth, told_to_reduce_salt,
                     told_to_increase_exercise, had_copd_emphysema_ch_b)

rows <- nrow(train_data)
cols <- ncol(train_data)

model <- keras_model_sequential()

model %>%
  layer_dense(units = 512, activation = 'relu', 
              input_shape = c(cols)) %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')

learning_rate <- 0.0001

model %>% compile(
  optimizer = optimizer_adam(learning_rate = learning_rate),
  loss = 'binary_crossentropy',
  metrics = list('accuracy')
)

train_data <- unlist(train_data)
dim(train_data) <- c(rows, cols)
train_data <- train_data %>% scale() %>% normalize()

train_label <- train$had_coronary_heart_disease
train_label <- ifelse(train_label == 2, 0, train_label)
train_label <- train_label %>% as.matrix() 


model %>% fit(
  train_data,
  train_label,
  epochs = 50,
  batch_size = 64,
  validation_split = 0.2
  
)


y_results <- train_label

test_data <- test %>% select(
                   features) %>%
  scale() %>% normalize()

probability_scores <- predict(model, test_data)

threshold <- 0.5

nn_predicted_classes <- ifelse(probability_scores > threshold, 1, 0)

test_labels <- test$had_coronary_heart_disease
clean_test_labels <- ifelse(test_labels == 2, 0, test_labels)

nn_accuracy <- mean(clean_test_labels == nn_predicted_classes)


factor_test_labels <- as.factor(clean_test_labels)
factor_predicted_classes <- as.factor(nn_predicted_classes)
f1_score_nn <- F1_Score(factor_predicted_classes, factor_test_labels)


nn_conf_matrix <- table(factor_test_labels, factor_predicted_classes)

nn_conf_matrix_df <- as.data.frame(nn_conf_matrix)

colnames(nn_conf_matrix_df) <- c("True_Class","Predicted_Class", "Frequency")

ggplot(nn_conf_matrix_df, aes(x = True_Class, y = Predicted_Class, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), vjust = 1) +
  labs(title = "Confusion Matrix",
       x = "True Class",
       y = "Predicted Class",
       fill = "Frequency")






