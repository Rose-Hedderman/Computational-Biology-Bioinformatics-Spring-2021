# Question 1
library(tidyverse)
txhousing2 <- read_csv("http://www.nathanielwoodward.com/txhousing2.csv")
# 193 variables

# Question 2
two <- txhousing2 %>% 
  pivot_longer(cols = !city , names_to = "name", values_to = "value")
two
# Question 3
three <- two %>%
  separate(name, into = c("variable", "stat", "year"))
three

# Question 4
four <- three %>%
  arrange(desc(year)) %>%
  pivot_wider(names_from = stat, values_from = value)
?pivot_wider()
four

# Question 5
five <- four %>%
  filter(city %in% c("Houston", "San Antonio", "Dallas", "Austin", "Fort Worth", "El Paso" ), variable == "median")
five
# add graph
ggplot(five, aes(city,mean , color = city)) +
  geom_point() + 
  geom_line()

# Question 6
library(readxl)
datasets <- read_excel("datasets.xlsx")

# Question 7
seven <- datasets %>%
  pivot_longer(cols = !Species , names_to = "type", values_to = "values") %>%
  separate(type, into = c("type", "measure"))
seven

# Question 8
mtcars <- read_excel("datasets.xlsx", sheet = "mtcars")
head(mtcars)

# Question 9
mtcars <- read_excel("datasets.xlsx", sheet = "mtcars",  na = ".")
mean(mtcars$mpg, na.rm=T)

# Question 10
mydata <- read_csv("HW,Lab,DB
          22.5,10,4
          21.5,10,4")
write_csv(mydata,"Lab4.csv")