
# load the packages and dataset
library(tidyverse) # for data manipulation, visualization

# load the data
housing_data <- read_csv("housing.csv")

# view data
head(housing_data)
tail(housing_data)

# check the summary of the data
summary(housing_data)
str(housing_data)

# check the structure of the data
str(housing_data)

# check for missing values
table(is.na(housing_data))
# check for missing values in each variables
tab_housing_data <- list()
for (var in seq_along(housing_data)) {
  tab_housing_data[[var]] <- table(is.na(housing_data[[var]]))
  
}
tab_housing_data

# replace all NAs with means and mode of respective variables
size_mean <- mean(housing_data$size, na.rm = TRUE)
bedrooms_median <- median(housing_data$bedrooms, na.rm = TRUE)
# replace with mean
housing_data$size[is.na(housing_data$size)] <- size_mean
# replace with mode
housing_data$bedrooms[is.na(housing_data$bedrooms)] <-
  bedrooms_median
# check if na has been removed
table(is.na(housing_data))

# convert into factors
housing_data <- housing_data %>%
  mutate(
    age = factor(age),
    bedrooms = factor(bedrooms),
    style = factor(style)
  )

# creating new and realistic levels in our data

housing_data <- housing_data %>%
  mutate(
    age = fct_recode(
      age,
      More_than_20_years = "More than 20",
      More_than_20_years = "More than 200 years",
      More_than_20_years = "than 20 years",
      More_than_20_years = "More than 20 years",
      More_than_20_years = "Between 11 and 20 years",
      Between_11_and_20_years = "Between 6 and 10 years",
      Between_11_and_20_years = "Up to 5",
      Between_11_and_20_years = "Up to 5 years"
    ),
    style = fct_recode(
      style,
      Others = "Other",
      Town_House = "Town House",
      Town_House = "Town-House",
      Town_House = "TownHouse",
      Traditional = "Tradition"
    )
    
  )

# histogram on price variable
housing_data %>%
  ggplot(aes(x = price)) +
  geom_histogram()

# scatter plot for price and sizes
housing_data %>%
  ggplot(aes(x = size, y = price)) +
  geom_point()

# density plot of different house styles together with price
housing_data %>%
  ggplot(aes(price, fill = style)) +
  geom_density(aes(y = ..count..), alpha = 0.5)

# bar plot for price against size colored using age
housing_data %>% 
  ggplot(aes(x = size, y = price, color = age))+
  geom_point()+
  labs(
    title = "A plot for House Price in dollars against House size in square feet",
    x = "House Size(square feet)",
    y = "House Price(US dollars)"
  )

# table that shows the distribution of this price according to age
housing_data %>% 
  group_by(age) %>% 
  count(age,sort = T)

# create a histogram facet of style variable on price
housing_data %>% 
  ggplot(aes(price))+
  geom_histogram()+
  labs(
    title = "Faceted histograms of different house style on house price",
    x = "House prices"
  )+
  facet_wrap(~style)

# plot for price and the numbers of bedrooms
housing_data %>% 
  ggplot(aes(price))+
  geom_histogram()+
  labs(
    title = "A histogram plot for the house price and number of bedrooms",
    x = "House Price(dollars)"
  )+
  facet_wrap(~bedrooms)