##Loading required libraries

library(sparklyr)
library(dplyr)

library(ggplot2)
library(dbplot)

## Configuring and opening Spark Connection
conf <- spark_config()

conf$spark.executor.memory <- "8G"
conf$spark.driver.memory <- "6G"
conf$spark.memory.fraction <- 0.8
conf$spark.executor.cores <- 4
conf$spark.executor.instances <- 2

sc <- spark_connect(master = "yarn-client", app_name = "aakash_cdsw",
config = conf)

### Load and describe historical data

#data <- spark_read_csv(sc, name ="train", path = "/user/p624274/Loan_prediction/train.csv")

data <- tbl(sc, 'schema_name.table_name') %>% compute(name = "historical_data")

#Dimensions
sdf_dim(data)

#Glimpse
glimpse(data)

### Basic statistics
sdf_describe(data) %>% as.data.frame

### Count of missing values
missing_count <- data %>% 
  mutate_all(is.na) %>%
  mutate_all(as.numeric) %>%
  summarise_all(sum) %>% 
  as.data.frame
missing_count


## Plots for exploratory data analysis


### Continuous variables - histograms, raster
dbplot_histogram(data, LoanAmount, binwidth = 20) +
labs(title = "Loan Amount")

dbplot_histogram(data, ApplicantIncome, binwidth = 1000)+
labs(title = "Applicant Income")



data %>% mutate(total_income =ApplicantIncome +CoapplicantIncome) %>%
dbplot_raster(total_income,LoanAmount)+
labs(title = "Raster Plot for Total Income and Loan Amount")

data %>% filter (ApplicantIncome == 0)%>% count() %>% pull()

data %>% filter(CoApplicantIncome == 0) %>% count()%>% pull()

data %>% filter (LoanAmount == 0) %>% count()

### Continuous vs Categorical variables - Boxplots
data %>% dbplot_boxplot(Loan_Status, ApplicantIncome) +
labs(title = "Box Plot for Loan Status - Applicant Income")

data %>% dbplot_boxplot(Loan_Status,LoanAmount)+
labs(title = "Box Plot for Loan Status - LoanAmount")

data %>% mutate(total_income =ApplicantIncome +CoapplicantIncome)%>% 
dbplot_boxplot(Loan_Status,total_income)+
labs(title = "Box Plot for Total Income and Loan Status")

data %>% dbplot_boxplot(Dependents,LoanAmount)+
labs(title = "Box Plot for Dependents and Loan Amount")

data %>% dbplot_boxplot(Gender,LoanAmount)+
labs(title = "Box Plot for Gender and Loan Amount")


### Categorical variables - bar charts, and tables
count(data,Credit_History)

dbplot_bar(data,Loan_Status)+
labs(title = "Loan Status")

dbplot_bar(data,Credit_History)+
labs(title = "Credit History")

#Evaluating impact of Credit History on Loan Status
data %>% count(Credit_History,Loan_Status) %>% na.omit() %>% as.data.frame()

data %>% count(Credit_History,Loan_Status) %>% 
na.omit() %>%
collect() %>%
ggplot() +
geom_col(aes(x = Credit_History,y = n,fill = Loan_Status)) +
ylab("Count") + xlab("Credit History")+
labs(title = "Credit History and Loan Status")

data %>% count(Credit_History,Loan_Status) %>% mutate(prop = n/sum(n)) %>% 
na.omit() %>%
collect() %>%
ggplot() +
geom_col(aes(x = Credit_History,y = prop,fill = Loan_Status)) +
ylab("Proportion") + xlab("Credit History")+
labs(title = "Credit History and Loan Status - proportion")


#Evaluating impact of Education on Loan Approval
count(data, Education,Loan_Status)%>% na.omit()%>% mutate(prop = n/sum(n)) %>% as.data.frame

data %>% count(Education,Loan_Status) %>%
na.omit() %>%
collect() %>%
ggplot() +
geom_col(aes(x = Education,y = n,fill = Loan_Status)) +
ylab("Count") + xlab("Education")+
labs(title = "Education and Loan Status")

data %>% count(Education,Loan_Status) %>% mutate(prop = n/sum(n)) %>% 
na.omit() %>%
collect() %>%
ggplot() +
geom_col(aes(x = Education,y = prop,fill = Loan_Status)) +
ylab("Proportion") + xlab("Education")+
labs(title = "Education and Loan Status - proportion")


data %>% count(Self_Employed,Loan_Status) %>% mutate(prop = n/sum(n)) %>% 
na.omit() %>%
collect() %>%
ggplot() +
geom_col(aes(x = Self_Employed,y = prop,fill = Loan_Status)) +
ylab("Proportion") + xlab("Self_Employed") +
labs(title = "Employment and Loan Status")



### Analyze for missing values

#* Missing count
missing_count

# Analysis of NULLs in all variables

data %>% count(Credit_History) %>% na.omit() %>% mutate(prop = n/sum(n, na.rm = TRUE)) %>% as.data.frame

data %>% count(Gender) %>% na.omit() %>% mutate(prop = n/sum(n, na.rm = TRUE))%>% as.data.frame %>% as.data.frame

data %>% count(Married) %>% na.omit() %>% mutate(prop = n/sum(n, na.rm = TRUE)) %>% as.data.frame

data %>% count(Dependents) %>% na.omit() %>% mutate(prop = n/sum(n, na.rm = TRUE))%>% as.data.frame

data %>% count(Self_Employed) %>% na.omit() %>% mutate(prop = n/sum(n, na.rm = TRUE))%>% as.data.frame

#data %>% count(Loan_Amount_Term) %>% na.omit() %>% mutate(prop = n/sum(n, na.rm = TRUE))%>% as.data.frame

db_compute_count(data,Dependents)


# Mean and median loan amount and applicant income grouped by Credit History
data %>% group_by(Credit_History)%>% summarize(mean_loan_amount = mean(LoanAmount), 
                                               median_loan_amount = percentile(LoanAmount,0.5),
                                               mean_app_income = mean(ApplicantIncome),
                                               median_app_income = percentile(ApplicantIncome,0.5)
                                              ) %>% as.data.frame()
