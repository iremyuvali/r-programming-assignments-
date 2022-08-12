# irem yuvali | 2071555071

library(tidyverse)

# It's generated according to my student ID

set.seed(2017555071)


# Given covid data is saved as a csv file. This file is imported as covid_data. 

covid_data <- read_csv2("D:/covid-data-2020.csv")

covid_data


# Random 1000 data from the covid_data is chosen as a sample.

my_sample <- sample_n(covid_data, size=1000, replace=TRUE)

View(my_sample)


# Five number statistics (min q1 med q3 max) are calculated.

five_num_sta <- my_sample %>% group_by(location, month)

five_num_sta <- five_num_sta %>% summarise(
  min_q = quantile(new_cases, na.rm=TRUE, probs=0.0),
  q1 = quantile(new_cases, na.rm=TRUE, probs=0.25),
  q2 = quantile(new_cases, na.rm=TRUE, probs=0.50),
  q3 = quantile(new_cases, na.rm=TRUE, probs=0.75),
  max_q = quantile(new_cases, na.rm=TRUE, probs=1.0)
)

View(five_num_sta)


# the highest daily cases and deaths are calculated.

highest_values <- my_sample %>% group_by(location)

highest_values <- highest_values %>% summarise(
  max_cases = new_cases,
  max_deaths = new_deaths
)

highest_values <- arrange(highest_values, desc(max_cases))
highest_values <- arrange(highest_values, desc(max_deaths))

View(highest_values)


# mean daily cases are calculated. 

mean_cases <- my_sample %>% group_by(location, month)

mean_cases <- mean_cases %>% summarise(
  means = mean(new_cases, na.rm=TRUE)
)

highest_cases <- arrange(mean_cases, desc(means))

View(highest_cases)


# plotting of monthly cases 3 countries


three_country<- filter(my_sample, location =="Italy" | location =="Spain"|  location =="Germany") 

ggplot(data=three_country, mapping=aes(x=month, y=new_cases)) +
  geom_point(mapping = aes(color=location, size=new_cases)) +
  geom_smooth(se=FALSE)


