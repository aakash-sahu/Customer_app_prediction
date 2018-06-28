## Performing model validation on test partition..

library(ggplot2)
library(tidyr)

# Bundle the models into a single list object
ml_models <- list(
  "Logistic Regression" = logistic_regression,
  "Ridge Regression" = ridge_regression,
  "Decision Tree" = decision_tree,
  "Random Forest" = random_forest,
  "Gradient Boosted Trees" = gradient_bt,
  "Naive Bayes" = naive_bayes
)

### Summary of logistic regression

summary(logistic_regression)

### Feature importance of random forest

ml_tree_feature_importance(random_forest)

ml_tree_feature_importance(gradient_bt)

# Function for confusion matrix

confusion_matrix <- function(model_in,df_in = partitions$test, threshold = 0.5){
  
  levels <- c("Y","N")
  #cat("Threshold: ",threshold)

  new_df <- sdf_predict(model_in, df_in) %>%
  mutate(prediction_status = ifelse(probability_Y >= !!threshold, 'Y', 'N')) %>%
  select(Loan_Status,prediction_status) %>% collect() %>%
    mutate(Loan_Status = factor(Loan_Status, levels = !!levels),
        prediction_status = factor(prediction_status, levels = !!levels))
  
  confusion_matrix <- table(new_df$prediction_status, new_df$Loan_Status)
  #cat('\nConfusion matrix:\n')
  #print(confusion_matrix)
  
}

## Show confusion matrix...
lapply(ml_models, confusion_matrix)

# Function for scoring models
score_test_data <- function(model, data=partitions$test, threshold = 0.5){
  pred <- sdf_predict(model, data) %>% 
          mutate(Model_Prediction= if_else(probability_Y >= !!threshold, 1.0, 0.0))
  select(pred,Loan_Status_idx,probability_Y,Model_Prediction)
}

# Score all the models
ml_score <- lapply(ml_models, score_test_data, data = partitions$training, threshold = 0.6 )


# Function for calculating accuracy
calc_metric <- function(data_in, metric){
  #metric = c(metric)
  data_in %>% 
    #mutate(probability_Y = if_else(probability_Y >= !!threshold, 1.0, 0.0)) %>%
    ml_classification_eval("Model_Prediction", "Loan_Status_idx", metric_name = metric)
}

### Calculate AUC, accuracy, Weighted_Precision,Weighted_Recall,and F1_score
Performance_Metrics <- data.frame(
  Model = names(ml_score),
  AUC = sapply(ml_score, ml_binary_classification_eval, "Loan_Status_idx", "probability_Y"),
  Accuracy = sapply(ml_score, calc_metric, "accuracy"),
  Weighted_Precision = sapply(ml_score,calc_metric, "weightedPrecision"),
  Weighted_Recall = sapply(ml_score,calc_metric, "weightedRecall"),
  F1_score = sapply(ml_score,calc_metric, "f1"),  
  row.names = NULL, stringsAsFactors = FALSE)

### Metrics for each model
Performance_Metrics

## Plot results for AUC and accuracy
gather(Performance_Metrics, Metric, Value, AUC, Accuracy,Weighted_Precision,Weighted_Recall,F1_score)%>%
filter(Metric == 'AUC' | Metric == 'Accuracy')%>%
  ggplot(aes(reorder(Model, Value), Value, fill = Metric)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() +
  xlab("") +
  ylab("") +
  ggtitle("Performance Metrics") + scale_fill_brewer(palette="Set3")


## Decided to save and use ridge regression model

#ml_save(ridge_regression, path = "/<path>/Saved_model/")
