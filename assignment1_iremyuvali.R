# iremyuvali | 2017555071 

library(dplyr)
library(tidyverse)

starwars

# question 1: ss : star ships without null. Counts and names of the characters having at least one starship.

ss<-filter(starwars, starships!="character(0)")
ss
ss$name
count(ss)


# question 2: eye_c : eye color frequencies from most to least 

eye_c <- starwars %>% group_by(eye_color)

eye_c <- eye_c %>% summarise(
  freq_col=n()
)

View(arrange(eye_c, desc(freq_col)))


# question 3: oldest_three : three oldest species according to the mean age.

spec <- starwars %>% group_by(species)

spec <- spec %>% summarise(
  mean_age=mean(birth_year, na.rm=TRUE)
)

oldest_three <- arrange(spec, desc(mean_age))
oldest_three [1:3,]


# question 4: my_char : a new data set consists of my data. 

my_char <- add_row(starwars, name="iremy",
                    height=155,
                    mass=45.0,
                    hair_color="brown",
                    skin_color="white",
                    eye_color="brown",
                    birth_year=23.0,
                    sex="female",
                    gender="feminine",
                    homeworld="Bestine IV",
                    species="Human",
                    films=list("A New Hope"),
                    vehicles=list("Snowspeeder"),
                    starships=list("Millennium Falcon")
                    )
View(my_char)

# question 5: BMI values of "my_char data set" are calculated and categorized. 


my_char <- mutate(my_char, BMI=mass/((height/100)*(height/100)))


my_char <- mutate(my_char, h_status=cut(BMI,breaks=c(-Inf,18.5,25.0,30.0,+Inf),
                                      labels=c("Underweight","Healthy","Overweight","Obese")))

select(my_char,h_status)

View(my_char)


# question 6: It's plotted according to the BMI<100 condition.

no_filter <- my_char

my_char <- filter(my_char,birth_year<100)

ggplot(data=my_char, mapping=aes(x=BMI, y=birth_year)) +
  geom_point(mapping = aes(color=h_status))
  

# question 7: It's plotted according to the relationship between BMI and birth_year

my_char <- no_filter

ggplot(data=my_char, mapping=aes(x=birth_year, y=BMI)) +
  geom_point(mapping = aes(color=h_status)) +
  geom_line() +
  geom_smooth(se=FALSE)


## the version of the graph above that filtered by BMI<100 and birth_year<100.

my_char <- filter(my_char,birth_year<100,BMI<100)

ggplot(data=my_char, mapping=aes(x=birth_year, y=BMI)) +
  geom_point(mapping = aes(color=h_status)) +
  geom_line() +
  geom_smooth(se=FALSE)

