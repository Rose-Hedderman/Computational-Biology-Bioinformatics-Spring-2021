library(dplyr)
library(ggplot)
data(mtcars)
fit <- glm(am ~ wt, data = mtcars, family = "binomial")
summary(fit)

# Question 2
predict(fit, newdata = data.frame(wt = 3), type = "response")
ggplot(mtcars, aes(x = wt, y = am)) +
  geom_point() + 
  geom_smooth(method="glm", method.args = list(family = "binomial")) +
  xlab("Weight (in 1,000 lbs)") +
  ylab("Probability of Manual Transmission")

titanic <- read.csv("https://raw.githubusercontent.com/laylaguyot/datasets/main//TitanicSurvival.csv")
titanic <-titanic %>% 
  as.data.frame %>%
  mutate(y = ifelse(survived == "yes", 1, 0)) %>%
  rename(class = passengerClass)

full_fit <- glm(y ~ class + age + sex, data = titanic, family = "binomial")
summary(full_fit)

titanic$prob <- predict(full_fit, newdata = titanic, type = "response")

titanic$predicted <- ifelse(titanic$prob > .5, 1, 0)

table(actual = titanic$y, prediction = titanic$predicted)

# sensitivity
mean(titanic[titanic$y == 1,]$prob > .5, na.rm = TRUE)
# specificity
mean(titanic[titanic$y == 0,]$prob <= .5, na.rm = TRUE)

install.packages("plotROC")
library(plotROC)
ggplot(titanic) + 
  geom_roc(aes(d = y, m = prob), cutoffs.at = list(0.3, 0.5, 0.6, 0.8)) 

