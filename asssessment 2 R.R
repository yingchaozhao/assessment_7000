library(tidyverse)
library(skimr)
library(knitr)
library(ggplot2)
library(tidymodels)
library(dplyr)

thanksgiving_meals <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-11-20/thanksgiving_meals.csv")

###Q1 Display the first 10 rows of the dataset using `kable()` function

knitr::kable(thanksgiving_meals[1:10])

###Q2 Using `skim()` display the summary of variables.Think about the task to predict a family income based on their menu: what variables may be useful? Are all of them correct type? 
  
skim(thanksgiving_meals) %>%
  summary() %>%
  kable()

#There are too many columns in this data set, not every column is useful to predict a family income.And not every type of variables are useful to predict a community type or us_region either. A lot of data types showing too much repeating and missing value. It results to these data might be invalid. I build another new variable (level-thanksgiving) to show which columns are useful in this database. 

level_thanksgiving <- thanksgiving_meals %>%
  select(main_dish,main_prep,stuffing,cranberry,family_income,community_type,us_region)
kable(level_thanksgiving)

###Q3 Use `fct_reorder` and `parse_number` functions to create a factor variable `family_income`

FI_thanksgiving <- thanksgiving_meals %>%
  mutate(family_income = fct_reorder(family_income, parse_number(family_income)))
skim(FI_thanksgiving)

###Q4 What is the number of people who celebrate? 

thanksgiving_meals %>%
  count(celebrate) %>%
  kable()

###Q5 What are categories and insights for each main dish served and the method it is prepared?

popular_thanksgiving <- thanksgiving_meals %>%
  count(main_dish, main_prep, sort = TRUE) %>%
  filter(main_dish != "NA") 
kable(popular_thanksgiving)

#From this chart we can see that 422 people choose Turkey as their main_dish and baked method as the main_prep. This is the most popular main_dish and main_prep. 

###Q6 Create 3 different data viz showing insights for main dish served and the method. Provide your own legend and use themes.Write 2-3 sentences with your explanation of each insight.

#First one

popular_thanksgiving %>%
  drop_na(main_dish) %>%
  drop_na(main_prep) %>%
  ggplot(aes(main_prep,main_dish))+geom_count(aes(color = ..n..))

#According to the chart we can see, roasted is the most popular method for cooking all main_dish. People can use various of methods to cook Turkey. And only use one method to cook Turducken which is roasted. 

# Second one

popular_thanksgiving %>%
  drop_na(main_dish) %>%
  ggplot(aes(main_dish)) + 
  geom_bar(colour= "red",fill="gray")

#From the chart we can see, few people like Turducken to be the main_dish to celebrate the thanksgiving day. Turkey is their first choice. 

# Third one

thanksgiving_meals %>%
  ggplot(aes(main_dish, cranberry, color = main_dish, fill= main_dish)) +
  geom_jitter()

#Most people choose cranberries when they use Turkey to be their main_dish. When people choose Turduncken to be their main_dish to celebrate Thanksgiving day, quite few of them choose cranberries.  

###Q7 How many use cranberry sauce? How many use gravy? 

thanksgiving_meals %>%
  filter(cranberry!='None') %>%
  count() %>%
  kable()

thanksgiving_meals %>%
  filter(gravy == 'Yes') %>%
  count() %>%
  kable()

###Q8-9 What is the distribution of those who celebrate across income ranges. Create a data viz.Write 2-3 sentences with your explanation of each insight.

thanksgiving1 <- FI_thanksgiving [!(FI_thanksgiving$celebrate=="No"| FI_thanksgiving$family_income=="Prefer not to answer"),]

summary(thanksgiving1)

thanksgiving1 %>%
  drop_na(family_income) %>%
  drop_na(celebrate) %>%
  group_by(family_income) %>%
  summarize(celebrate = n()) %>%
  ggplot(aes(family_income, celebrate))+geom_col()

#According to this data set analysis, it is a positively skewed distribution. The data shows mode<median<mean. In this statistics, a right-skewed distribution can indicate most of families who celebrate the thanksgiving day are on the left. From the family_income range we can see that lower and middle family_income families, which prefer to celebrate thanksgiving day. With the number goes up, there is a significantly decreased trend.  


###10 Use the following code to create a new data set.

side_pie_dessert <- thanksgiving_meals %>%
  select(id, starts_with("side"), starts_with("pie"), starts_with("dessert")) %>%
  select(-side15, -pie13, -dessert12) %>%
  gather(type, value, -id) %>%
  filter(!is.na(value),!value %in% c("None", "Other (please specify)")) %>%
  mutate(type = str_remove(type, "\\d+"))

#Select id first. The rest of them start with "side, pie and dessert". Then, select side15, pie13, dessert12 and remove them. Gather type, value and id, after that rank id with descending order. Filter the invalid value and add a new column named "type". And remove type's("\\d+") which means match 2 or more digits.  

###Q11-12 Install package `widyr` and use `pairwise_cor()` function https://www.rdocumentation.org/packages/widyr/versions/0.1.3/topics/pairwise_cor.Write 2-3 sentences with your explanation of what it does. 

install.packages("widyr")
library(widyr)

side_pie_dessert %>%
  pairwise_cor(value, id, sort = TRUE)

#Explains the correlation between different types of items.

###Q13 Use `lm()` or randomForest() function to build a model that predict a family income based on data in the dataset. 

install.packages("randomForest")
library(randomForest)

#rf1

rf1 <- randomForest(family_income ~  celebrate , data = FI_thanksgiving, na.action = na.omit)
print(rf1)

#From the first data set we can see, when I use 'celebrate' column to predict a family_income, it shows that the estimate of error rate is 82.05%. That is to say, this result is inaccurate forecast. 

#rf2

rf2 <- randomForest(family_income ~  celebrate + community_type + age + us_region, data = FI_thanksgiving, na.action = na.omit)
print(rf2)

#According to the second data set, when I use 'community', 'age', 'living region' to predict family income which has an estimate of error rate 84%. This result can be considered inaccurate as well. 

#rf3

rf3 <- randomForest(family_income ~ celebrate + travel + work_black_friday + community_type + us_region, data = FI_thanksgiving, na.action = na.omit)
print(rf3)

#According to the third data set, when I try to use 'celebrate', 'travel', 'work_black_friday', 'community_type', 'us_region' to predict family income which has an estimate of error rate 89.39%. This result can not be used to predict family income. These conditions are irrelevant to family income. This is an inexact forecast.









