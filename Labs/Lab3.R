library(ggplot2)

x <- runif(n=10000, min=0, max=10000)
x_round <- round(x)
ggplot() + geom_bar(aes(x_round)) + scale_x_continuous(breaks=0:10)

x_floor <- floor(x)
ggplot() + geom_bar(aes(x_floor)) + scale_x_continuous(breaks=0:10)

library(dplyr)
starwars <- as_tibble(starwars)
?starwars
table(starwars$eye_color)
starwars %>% count(eye_color == "orange")

table(starwars$homeworld)
starwars %>% 
  filter(species == "human") %>%
  summarize(mean(height, na.rm=T)) %>%
  arrange(desc(height))
?arrange

starwars %>%
  group_by(homeworld) %>%
  filter(species == "Human") %>%
  arrange(desc(height)) %>%
  summarize(mean(height)) 

# Q10
a <- starwars %>%
  filter(!is.na(gender)) %>%
  group_by(homeworld) %>%
  summarize(mean(gender == "feminine"))
table(a)

# Q 8
starwars %>% 
  mutate(BMI = mass/(height/100)^2)  %>%
  top_n(1, BMI)
?arrange