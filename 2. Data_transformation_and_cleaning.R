data_mod <- data

### Replacing missing loan amount with median values
loan_amount_median <- summarize(data_mod,median = percentile(LoanAmount, 0.5)
         ) %>% pull()



data_mod <- data_mod %>% mutate(LoanAmount=ifelse(is.na(LoanAmount),!!loan_amount_median,LoanAmount))

### Creating total income feature and taking log of Loan Amount and Total income
data_mod <- data_mod %>%
    mutate(LoanAmount_log = log(LoanAmount),
           Total_income = ApplicantIncome + CoapplicantIncome) %>%
    mutate(Total_income_log = log(Total_income))

glimpse(data_mod)

data_mod %>% dbplot_histogram(LoanAmount_log, binwidth = 0.1)

dbplot_histogram(data_mod, Total_income, binwidth = 1000)

dbplot_histogram(data_mod, Total_income_log, binwidth = 0.1)

### Imputing all missing variables with most common value for that feature


data_mod <- data_mod %>%
  mutate(Gender = ifelse(is.na(Gender), 'Male', Gender),
         Married = ifelse(is.na(Married), 'Yes', Married),
         Dependents = ifelse(is.na(Dependents),'0',Dependents),
         Self_Employed = ifelse(is.na(Self_Employed), 'Yes', Self_Employed),
         Loan_Amount_Term = ifelse(is.na(Loan_Amount_Term),360,Loan_Amount_Term),
         Credit_History = ifelse(is.na(Credit_History), 1,Credit_History)
        )

### Description of prepared data
sdf_describe(data_mod) %>% as.data.frame

#* Missing values count - Sanity check
data_mod %>% 
  mutate_all(is.na) %>%
  mutate_all(as.numeric) %>%
  summarise_all(sum) %>%
  collect() %>% as.data.frame()

#Indexing Loan Status as 1 for 'Y' and 0 for 'N'.....
data_mod<- data_mod %>% mutate(Loan_Status_idx = as.numeric(if_else(Loan_Status == "Y", 1.0,0.0)))
