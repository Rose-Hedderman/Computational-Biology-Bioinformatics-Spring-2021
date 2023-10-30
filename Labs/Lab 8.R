# numeric by categorical
PlantGrowth
summary(aov(PlantGrowth$weight ~ PlantGrowth$group, data = PlantGrowth)) 
library(dplyr)
library(tidyverse)
# Question 2
options(pillar.sigfig = 6) 
PlantGrowth %>%
  group_by(group) %>%
  summarize(mean = mean(weight))

5.032 - 4.66100
5.032 - 5.526
5.526 - 4.66100

# Question 3
pairwise.t.test(PlantGrowth$weight,PlantGrowth$group, p.adj="none")
bon = 0.05/3
bon

# Question 4
mymodel <- lm(PlantGrowth$weight ~ PlantGrowth$group, data = PlantGrowth)
summary(mymodel)

# Question 5
# Yhat = 5.032 - .371*(trt1) + .494*(trt2)
Yhat = 5.032 - .371*(1) + .494*(0)
Yhat

# Question 6
mymodel$fitted.values %>%
  n.distinct()

# Question 7
newmodel <- lm(weight ~ relevel(group, ref = "trt1"), data = PlantGrowth)
newmodel5 <- lm(weight ~ relevel(group, ref = "trt2"), data = PlantGrowth)

summary(newmodel)
summary(newmodel5)
model.matrix(mymodel)
model.matrix(newmodel)

# Question 8
mean(PlantGrowth$weight)


# Set effect-coding manually
effect <- matrix(c(-1,1,0,-1,0,1), nrow = 3) 
# Fit a new regression model
mymodel2 <- lm(weight ~ group, data = PlantGrowth, contrasts = list(group = effect))
# Check how the dummy variables were coded
model.matrix(mymodel2)
# Check the output
summary(mymodel2) 

#Yhat = 5.073 - .412*(trt1) + .453*(trt2)
Yhat2 = 5.073 - .412*(1) + .453*(0)
Yhat2
predict(mymodel2, newdata = data.frame(group = "trt1"))

# Question 9
mycarmodel <- lm(mpg ~ wt + cyl + hp + factor(am), data = mtcars)
summary(mycarmodel)

# Question 10
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(am))) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE)

mycarmodel2 <- lm(mpg ~ cyl + hp + wt * factor(am), data = mtcars)
summary(mycarmodel2)

mtcars %>% 
  mutate(wt_cat = case_when(
    wt < median(wt) ~ "low",
    wt >= median(wt) ~ "high")) %>%
  ggplot(aes(x = hp, y = mpg, color = wt_cat)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE)

mycarmodel3 <- lm(mpg ~ cyl + wt * hp + factor(am), data = mtcars)
summary(mycarmodel3)