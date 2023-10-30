# Rose Hedderman
# Lab 7 
# SDS 348

# Question 1
data <-c (8.029959, 1.822777, 2.991281, 5.482830, 6.195160, 6.203641, 2.240657,
          7.828524, 6.709812, 12.668532, 13.652440, 6.172207, 6.532104, 8.349607,
          2.372673, 1.679789, 8.562268, 2.825837, 10.773925, 5.481163, 2.315325,
          2.443164, 4.130867, 3.928907, 2.943242, 4.561477, 3.726978, 6.335047,
          4.765059, 11.939610, 8.479929, 2.857069, 6.864941, 8.284125, 5.411720,
          14.994230, 5.314071, 3.020857, 14.034124, 9.195873)
theory_quantiles <- quantile(data,1:20/20)
observed_quantiles <- qchisq(1:20/20, df = 8)
plot(theory_quantiles, observed_quantiles)
abline(a = 0,b=1)

# Question 2
head(PlantGrowth)
t.test(data = PlantGrowth$weight, ctrl ~ trt2)

mean_diff <- vector()

# Create many randomizations with a for loop
for(i in 1:5000){ 
  temp <- data.frame(weight = PlantGrowth$weight, group = sample(PlantGrowth$group)) 
  
  mean_diff[i] <- temp %>% 
    group_by(group) %>%
    summarize(means = mean(weight)) %>%
    summarize(mean_diff = diff(means)) %>%
    pull
}

# Question 3
# group by group and mean something
view(PlantGrowth)

# Question 4
PlantGrowth_c2 <- PlantGrowth %>%
  filter(group == "ctrl" | group == "trt2")
PlantGrowth_c2
options(pillar.sigfig=6)
t.test(data = PlantGrowth_c2, group ~ weight)
# Question 5
t.test(weight ~ group, data = PlantGrowth_c2, var.equal = TRUE)
PlantGrowth_c2 %>%
  group_by(group) %>%
  summarize(p.value = shapiro.test(weight)$p.value)

var.test(weight ~ group, data = PlantGrowth_c2)

sample(1:10)
sample(1:10, replace = TRUE)
set.seed(348)
sample(1:10)
sample(1:10, replace = TRUE)

# Question 6

PlantGrowth_c2 %>% 
  mutate(weight = sample(weight))
PlantGrowth_c2 %>% 
  mutate(weight = sample(weight)) %>%
  group_by(group) %>%
  summarize(means = mean(weight)) %>%
  summarize(diff_means = diff(means)) %>%
  pull


# Question 7
set.seed(348)
diff_means_random <- vector()

for(i in 1:1000){
  temp <- PlantGrowth_c2 %>% 
    mutate(weight = sample(weight))
  
  diff_means_random[i] <- temp %>% 
    group_by(group) %>%
    summarize(means = mean(weight)) %>%
    summarize(diff_means_random = -diff(means)) %>%
    pull
}
hist(diff_means_random)


# Question 8

# Question 9

# Question 10


