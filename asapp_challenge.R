# Loading libraries
library(tidyverse)
library(ggplot2)


# Importing data
credit <- read_fwf(file="data/ds-credit.tsv", 
                   fwf_cols(customer_id = c(1,3), 
                                 checking_account_balance = c(5,9), 
                                 debts_paid = c(13,20),
                                 savings_account_balance= c(21,25),
                                 current_open_loan_applications= c(29)))

result <- read_fwf("data/ds-result.tsv",
                     fwf_cols(customer_id = c(1,3), 
                              was_loan_approved = c(5)))

borrower <- read_fwf(file="data/ds-borrower.csv", 
                   fwf_cols(customer_id = c(1,3), 
                            years_current_employer = c(5,7), 
                            years_current_residence = c(9),
                            age = c(13,14),
                            rent_own_home = c(17,21),
                            type_current_employer = c(25,31),
                            number_dependants_including_self = c(33)))

app <- read_delim("data/ds-app.tsv", col_names=F, delim=" ")



# Checking
glimpse(app)


# Naming
names(app) <- c("customer_id","loan_payoff_period_in_months","loan_reason","requested_amount","interest_rate", "co_applicant")

# Stripping whitespaces in app dataset
app$loan_reason <- str_trim(app$loan_reason)
app$requested_amount <- str_trim(app$requested_amount)
app$interest_rate <- str_trim(app$interest_rate)
app$co_applicant <- str_trim(app$co_applicant)

###########################################
# Having the correct type of features

##########
# APP
glimpse(app)
app$loan_payoff_period_in_months <- as.numeric(app$loan_payoff_period_in_months)
app$loan_reason <- as.factor(app$loan_reason)
app$requested_amount <- as.numeric(app$requested_amount)
app$interest_rate <- as.numeric(app$interest_rate)
app$co_applicant <- as.factor(app$co_applicant)


##########
# CREDIT
glimpse(credit)
credit$checking_account_balance <- as.factor(credit$checking_account_balance)
credit$debts_paid <- as.factor(credit$debts_paid)
credit$savings_account_balance <- as.factor(credit$savings_account_balance)


##########
# BORROWER
glimpse(borrower)
borrower$years_current_employer <- as.factor(borrower$years_current_employer)
borrower$rent_own_home <- as.factor(borrower$rent_own_home)
borrower$type_current_employer <- as.factor(borrower$type_current_employer)

##########
# RESULT
glimpse(result)
result$was_loan_approved <- as.factor(result$was_loan_approved)



#############################################################
# Now that I have everything clean, let's check some data

#############
# APP
app %>%
  ggplot(aes(x=loan_payoff_period_in_months, y=interest_rate, color = as.factor(loan_reason))) +
  geom_point()

app %>%
  ggplot(aes(x=requested_amount, y=interest_rate, color = as.factor(loan_reason))) +
  geom_point()

app %>%
  ggplot(aes(x=requested_amount, y=interest_rate, color = as.factor(co_applicant))) +
  geom_point()

app %>%
  group_by(loan_reason) %>%
    summarise(avg_period = mean(loan_payoff_period_in_months),
              avg_amount = mean(requested_amount),
              avg_rate = mean(interest_rate),
              med_period = median(loan_payoff_period_in_months),
              med_amount = median(requested_amount),
              med_rate = median(interest_rate))
# Why "others" is more money at more time but with less rate??



################
# CREDIT
glimpse(credit)

table(credit$checking_account_balance, credit$debts_paid)
table(credit$checking_account_balance, credit$savings_account_balance)

credit %>%
  ggplot(aes(x=checking_account_balance, y=current_open_loan_applications)) +
  geom_boxplot()

credit %>%
  ggplot(aes(x=debts_paid, y=current_open_loan_applications)) +
  geom_boxplot()
# delayed debt is asking for more loans => ponzi

credit %>%
  ggplot(aes(x=savings_account_balance, y=current_open_loan_applications)) +
  geom_boxplot()
# If you have more money in account you ask for less loans



################
# BORROWER
glimpse(borrower)

borrower %>%
  ggplot(aes(x=rent_own_home, y=age)) +
  geom_boxplot()
# what is "free"?
# at least good to see there is no problem with ages :)

borrower %>%
  ggplot(aes(x=type_current_employer, y=years_current_residence)) +
  geom_boxplot()
# why none above 4 years in residence?

table(borrower$years_current_employer, borrower$type_current_employer)

borrower %>%
  ggplot(aes(x=age, y=number_dependants_including_self, color=rent_own_home)) +
  geom_point()



############################
# Joining all

asapp <- left_join(borrower, app, by="customer_id") %>%
            left_join(.,credit, by="customer_id") %>%
            left_join(.,result,by="customer_id")

table(asapp$was_loan_approved)
# Have to take out 2 rows with "1" in objective
# Gonna lose additional 90 rows with no "approved" flag

asapp2 <- asapp %>% filter(was_loan_approved != "1")

asapp2$was_loan_approved <- droplevels(asapp2$was_loan_approved)

glimpse(asapp2)


########################
# Discovery

asapp2 %>%
  ggplot(aes(x=requested_amount,y=interest_rate,color=was_loan_approved)) +
  geom_jitter(height = 0.5, alpha=0.4)
# low amounts more approved => loan_reason correlated


asapp2 %>%
  ggplot(aes(x=was_loan_approved, y=age)) + 
  geom_boxplot()

asapp2 %>%
  ggplot(aes(x=was_loan_approved, y=loan_payoff_period_in_months)) + 
  geom_boxplot()
# short time loans get more approved => perceived low risk?

asapp2 %>%
  ggplot(aes(x=was_loan_approved, y=requested_amount)) + 
  geom_boxplot()
# small loans get more approved => perceived low risk?

asapp2 %>%
  ggplot(aes(x=was_loan_approved, y=requested_amount)) + 
  geom_boxplot()
# small loans get more approved => perceived low risk?


prop.table(table(asapp2$loan_reason, asapp2$was_loan_approved),1)
# goods more approved
prop.table(table(asapp2$rent_own_home, asapp2$was_loan_approved),1)
# owned home more approved => perceived low risk?
prop.table(table(asapp2$co_applicant, asapp2$was_loan_approved),1)
# obviously, if guarantee => more approved.
# what is "co-app"?
prop.table(table(asapp2$savings_account_balance, asapp2$was_loan_approved),1)
# weird that "none" gets high approval...
prop.table(table(asapp2$checking_account_balance, asapp2$was_loan_approved),1)
# more money, more approved. The question is what happens to "none"...
prop.table(table(asapp2$type_current_employer, asapp2$was_loan_approved),1)
# unskill more approved??? (128 cases)
prop.table(table(asapp2$debts_paid, asapp2$was_loan_approved),1)
# delayed more approved??
prop.table(table(asapp2$years_current_employer, asapp2$was_loan_approved),1)
# high correlation to stability in employment vs approval (logical)

# so far all relations are about perceived low risk
# some weird stuff: 
  # none saving gets good approval
  # delayed payment gets good approval
  # unskill employment gets good approval
prop.table(table(asapp2$type_current_employer, asapp2$loan_reason),1)
# most unskill go for "good" kind of loan, which is for less money, which is perceived as less risky

asapp2 %>%
  ggplot(aes(x=loan_reason, y=requested_amount)) + 
  geom_boxplot()

###########################
# Check NA's concentration
library(naniar)
vis_miss(asapp2)
# 1/9th of app data is missing
# 1/8th of credit



# 2 ways of dealing with these
# 1) ommiting antthing with missing value (losing around 25% of data...)
# 2) imputing a value ("no_info" for factors, median for numeric?)


###############################
# Imputing values
library(Hmisc)
# Numeric => mean
asapp2$loan_payoff_period_in_months <- with(asapp2, impute(loan_payoff_period_in_months, mean))
asapp2$requested_amount <- with(asapp2, impute(requested_amount, mean))
asapp2$interest_rate <- with(asapp2, impute(interest_rate, mean))
asapp2$current_open_loan_applications <- with(asapp2, impute(current_open_loan_applications, mean))
# Factor => "no_info"
asapp2$loan_reason <- fct_explicit_na(asapp2$loan_reason, na_level = "no_info")
asapp2$co_applicant <- fct_explicit_na(asapp2$co_applicant, na_level = "no_info")
asapp2$checking_account_balance <- fct_explicit_na(asapp2$checking_account_balance, na_level = "no_info")
asapp2$debts_paid <- fct_explicit_na(asapp2$debts_paid, na_level = "no_info")
asapp2$savings_account_balance <- fct_explicit_na(asapp2$savings_account_balance, na_level = "no_info")




# Keeping the imputed and no NA's features
asapp2_imputed <- asapp2 %>%
                    select(-c(imputed_loan_payoff_period_in_months, customer_id))
vis_miss(asapp2_imputed)

asapp2_imputed$was_loan_approved <- droplevels(asapp2_imputed$was_loan_approved)


########################
# Classify
library(caret)
set.seed(31416)

# 1) Quick Random Forest for feature selection

# Training and testing (75-25)
inTrain <- createDataPartition(y = asapp2_imputed$was_loan_approved, p = .75,  list = FALSE)
train_rf <- asapp2_imputed[inTrain,]
test_rf <- asapp2_imputed[-inTrain,]

# cross-validarion 10 folds to control
fit_control <- trainControl(method = "repeatedcv", number = 10, repeats=3, classProbs = T)

rf_fit <- train(was_loan_approved ~ .,
                data = train_rf,
                method = "rf", 
                metric = "Accuracy", # using Accuracy since objective is quite leveled
                trControl=fit_control)


# Predicting with the model and comparing
predict_rf <- predict(rf_fit, newdata = test_rf, type="prob")

library(pROC)
result_roc <- roc(test_rf$was_loan_approved, predict_rf$N) # Draw ROC curve.
plot(result_roc, print.thres="best", print.thres.best.method="closest.topleft")
# AUC: RandomForest is 0.7455 for N (rf)
# AUC: Tree is 0.7066 for N (rpart2)
# AUC: Boosted Logistic Regression is 0.315 for N  (LogitBoost)




######################
# Omitting NA's
asapp3 <- inner_join(app, credit, by="customer_id") %>%
            inner_join(.,borrower, by="customer_id") %>%
              inner_join(.,result,by="customer_id")

vis_miss(asapp3)

table(asapp$was_loan_approved)
# Have to take out 2 rows with "1" in objective
# Gonna lose additional 90 rows with no "approved" flag

asapp4 <- asapp3 %>% 
            filter(was_loan_approved != "1") %>%
              select(-c(customer_id))
asapp4$was_loan_approved <- droplevels(asapp4$was_loan_approved)

# 1) Quick Random Forest for feature selection

# Training and testing (75-25)
inTrain_nona <- createDataPartition(y = asapp4$was_loan_approved, p = .75,  list = FALSE)
train_rf_nona <- asapp4[inTrain_nona,]
test_rf_nona <- asapp4[-inTrain_nona,]

# cross-validarion 10 folds to control
fit_control <- trainControl(method = "repeatedcv", number = 10, repeats=3, classProbs = T)

rf_fit_nona <- train(was_loan_approved ~ .,
                data = train_rf_nona,
                method = "rf", 
                metric = "Accuracy", # using Accuracy since objective is quite leveled
                trControl=fit_control)


# Predicting with the model and comparing
predict_rf_nona <- predict(rf_fit_nona, newdata = test_rf_nona, type="prob")

# RandomForest: 58.65% balanced accuracy
# Rpart: 57.48% balanced accuracy
confusionMatrix(data = predict_rf_nona, test_rf_nona$was_loan_approved)

library(pROC)
result_roc_nona <- roc(test_rf_nona$was_loan_approved, predict_rf_nona$N) # Draw ROC curve.
plot(result_roc_nona, print.thres="best", print.thres.best.method="closest.topleft")


# AUC: RandomForest is 0.7925
# AUC: Tree is 0.7235 for N



######################
# Predicting Default

# the approved-not_approved flag is replicating the risk structure that the company already has
# if "debts_paid" is actually the real objective, I would drop "was_loan_approved" and crete a new set of rules based on paid-not_paid

new_asapp <- asapp2_imputed %>%
              filter(debts_paid != "no_info") %>%
                select(-c(was_loan_approved))


new_asapp$debts_paid <- droplevels(new_asapp$debts_paid)

glimpse(new_asapp)

# Base_line
table(asapp2$was_loan_approved, asapp2$debts_paid)
#If approved: 44.84% delayed payment
#If not approved: 25.13% delayed payment (of previous loans, I imagine then cannot used this...)



