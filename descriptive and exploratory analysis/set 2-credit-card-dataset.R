# load package
library(tidyverse)
library(psych) # for different summary statistics
# load data
credit_card_data <- read_csv("credit_card.csv")

# statistical summary for dataset
# summary
summary(credit_card_data)
#structure
str(credit_card_data)
# describe the data using housing
describeBy(credit_card_data, credit_card_data$housing)

# check number of true missing values
table(is.na(credit_card_data))

# find which variables has missing values # check the summary of the data
summary(credit_card_data)
# variables age and income has missing values
# replace age with the median
age_median<- median(credit_card_data$age, na.rm = T)
credit_card_data$age[is.na(credit_card_data$age)] <- age_median
# replace income NA with its mean
income_mean <- mean(credit_card_data$income, na.rm = T)
credit_card_data$income[is.na(credit_card_data$income)] <- income_mean

# check for missing values
table(is.na(credit_card_data))


# factoring variables
credit_card_data<- credit_card_data %>% 
  mutate(
    outcome = factor(outcome),
    housing = factor(housing),
    credit_report = factor(credit_report)
  )

# confirm if these variables were converted into factors
class(credit_card_data$outcome)
class(credit_card_data$housing)
class(credit_card_data$credit_report)

# changing the levels of factor variables
credit_card_data<- credit_card_data %>% 
  mutate(
    outcome = fct_recode(
      outcome,
      Accept = "Accepted",
      Decline = "Dacline"
    ),
    housing = fct_recode(
      housing,
      Owner = "Own",
      Rental = "Rent",
      Rental = "Renting"
      
    ),
    credit_report = fct_recode(
      credit_report,
      "More than five" = "More than",
      "Up to five" = "one",
      "Up to five" = "Up to",
      "Up to five" = "Up to fiver"
    )
  )
# filter out age lower than 21
min_age <- 21
max_age <- 85
credit_card_data<-credit_card_data %>% 
  filter(age>= min_age & age<= max_age)

# a barplot for credit_report 
credit_card_data %>% 
  ggplot(aes(credit_report))+
  geom_bar()+
  labs(title = "A bar plot of numbers of credit report in the record", x = "Credit Report")
# count the credit reports
credit_card_data %>% 
  count(credit_report, sort = T)

# density plot for housing and income
credit_card_data %>% 
  ggplot(aes(income, fill = housing))+
  geom_density(alpha = 0.5)

# count the number of different housing
credit_card_data %>% 
  count(housing, sort = TRUE)

# a plot for different age group together with income and outcome
credit_card_data %>% 
  ggplot(aes(x = age, y = income, color = outcome))+
  geom_point()

# bar plot to show how outcome corelate with credit report
credit_card_data %>% 
  ggplot(aes(outcome, ..count..))+
  geom_bar(aes(fill = credit_report), position = "dodge")+labs(
    title = "Bar plot showing the relationship between outcome and credit reports"
  )

