## Training classification models

data_final <- data_mod

### Defining Loan Status index as dependent variable
response_var <- "Loan_Status_idx"

### Using all transformed and independent variables as features
feature_vars <- c("Total_income_log", "LoanAmount_log", "Credit_History","Gender","Married","Dependents",
                 "Education","Self_Employed","Loan_Amount_Term",
                 "Property_Area")

formula = formula(paste0(response_var,"~" , paste0(feature_vars,collapse = "+")))

### Partitioning between training and test set....
partitions <- data_final %>%
  sdf_partition(training = 0.75,
                test = 0.25 
                ,seed = 123
               )

### 1. Training Logistic regression with all variables...
logistic_regression <- ml_generalized_linear_regression(partitions$training,
                                                        formula,
                                                        family = "binomial",
                                                       prediction_col = "probability_Y")



### 2. Training Ridge regression - Logistic regression with regularization....
lambda = 0.1
ridge_regression <- ml_generalized_linear_regression(partitions$training,
                                                     formula,
                                                     reg_param = lambda,
                                                     family = "binomial",
                                                     prediction_col = "probability_Y")


### 3. Training Decision Tree....
formula_t = formula(paste0("Loan_Status","~" , paste0(feature_vars,collapse = "+")))

decision_tree <- ml_decision_tree(partitions$training,formula_t, type = "classification")

### 4. Training Random Forest...
random_forest <- ml_random_forest(partitions$training,
                                    formula_t,
                                    type = "classification",
                                  )



### 5. Training Gradient Boosted Tree...

gradient_bt <- ml_gradient_boosted_trees(partitions$training,formula_t, type = "classification")

### 6. Training Naive Bayes...
naive_bayes <- ml_naive_bayes(partitions$training,formula_t)
