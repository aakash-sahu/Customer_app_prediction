## Predicting on new data
model<- ml_load(sc, path = "/<path>/Saved_model/")

### Loan new data
data_new <- tbl(sc, 'schema.loan_data_new_table name') %>% compute(name = "data_new")

glimpse(data_new)

### Check for missing data
missing_data <- data_new %>% 
  mutate_all(is.na) %>%
  mutate_all(as.numeric) %>%
  summarise_all(sum) %>% as.data.frame()

print(missing_data)

### Impute missing values and perform transformations...
data_new <- data_new %>%
  mutate(Gender = ifelse(is.na(Gender), 'Male', Gender),
         Married = ifelse(is.na(Married), 'Yes', Married),
         Dependents = ifelse(is.na(Dependents),'0',Dependents),
         Self_Employed = ifelse(is.na(Self_Employed), 'Yes', Self_Employed),
         Loan_Amount_Term = ifelse(is.na(Loan_Amount_Term),360,Loan_Amount_Term),
         Credit_History = ifelse(is.na(Credit_History), 1,Credit_History)
        )


loan_amount_med <- summarize(data_new,median = percentile(LoanAmount, 0.5)
         ) %>% pull()

data_new <- data_new %>% mutate(LoanAmount=ifelse(is.na(LoanAmount),!!loan_amount_med,LoanAmount)) %>%
  mutate(LoanAmount_log = log(LoanAmount),
        Total_income = ApplicantIncome + CoapplicantIncome) %>%
  mutate(Total_income_log = log(Total_income))

## Perform prediction on new data using saved model
prediction <- ml_transform(model, data_new) %>% 
mutate(Loan_status_prediction = if_else(probability_Y >= 0.5, "Y", "N")) %>%
select(Loan_ID, Loan_status_prediction)

prediction %>% as.data.frame()


### Count of Yes and No
prediction %>% count(Loan_status_prediction) %>% as.data.frame
