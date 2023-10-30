# Question 1
library(tidyverse)
library(dplyr)
library(cluster)
pokemon <- read.csv("C:/Users/roseh/OneDrive/Desktop/Spring 2021/SDS 348/pokemon.csv")
# pokemon
pokemon <- pokemon %>%
  select(-1) %>%
  select(-Total)
pokemon
# Question 2
pokemon <- pokemon %>% 
  mutate(Generation = as.factor(Generation))
pokemon_num <- pokemon %>% 
  column_to_rownames("Name") %>% 
  select_if(is.numeric)

?cor()
cor(pokemon_num) %>%
  # Save as a data frame
  as.data.frame %>%
  # Convert row names to an explicit variable
  rownames_to_column %>%# Pivot so that all correlations appear in the same column
  pivot_longer(-1, names_to = "other_var", values_to = "correlation") %>%
  # Specify variables are displayed alphabetically from top to bottom
  ggplot(aes(rowname, factor(other_var, levels = rev(levels(factor(other_var)))), fill=correlation)) +
  # Heatmap with geom_tile
  geom_tile() +
  # Change the scale to make the middle appear neutral
  scale_fill_gradient2(low="red",mid="white",high="blue") +
  # Overlay values
  geom_text(aes(label = round(correlation,2)), color = "black", size = 4) +
  # Give title and labels
  labs(title = "Correlation matrix for Abundance and Environmental factors", x = "", y = "") 
# Question 3
pokemon_dist <- pokemon_num %>% 
  dist %>% 
  as.matrix
dim(pokemon_dist)

# Question 4
pokemon_dist <- pokemon_dist %>%
  as.data.frame %>%
  rownames_to_column("pokemon1") %>%
  pivot_longer(-1,names_to = "pokemon2", values_to = "distance")
pokemon_dist
pokemon_dist %>%
  filter(pokemon1 == "Snorlax") %>%
  arrange(distance)

# Question 5



pamclust <- clust_data %>%
  mutate(cluster = as.factor(pam1$clustering))

# Make a plot of data colored by final cluster assignment
pamclust %>% 
  ggplot(aes(Sepal.Length, Petal.Width, color = cluster)) +
  geom_point()

# Calculate the means of each variable for each cluster
pamclust %>% 
  group_by(cluster) %>%
  summarize_if(is.numeric,~ mean(.x, na.rm=T))

# Find the observations that are the final medoids
iris[pam1$id.med,]

pam1 <- pokemon_num %>%
  pam(k=2)
pam1
#install.packages("factoextra")
library(factoextra)
fviz_nbclust(pokemon_num, FUNcluster = pam, method = "s")

# Question 10
eigen(pokemon_pca$rotation)
eigen() decomposition$values

# uestion 7
plot(pam1, which=1)
pokemon_pca <- pokemon_num %>%
  scale %>%
  prcomp
pokemon_pca
names(pokemon_pca)

# Question 8
pokemon_pca$rotation

#  Question 9
fviz_pca_var(pokemon_pca, col.var = "black")

# Compute the percentage of variance explained
percent <- 100* (pokemon_pca$sdev^2 / sum(pokemon_pca$sdev^2))
percent
# Scree plot with percentages as text
fviz_screeplot(pokemon_pca) + geom_text(aes(label = round(percent, 2)), size = 4, vjust = -0.5)

get_eig(pokemon_pca)
# Question 10
pokemon_pam <- pokemon_num %>%
  pam(k=2)
pokemon_pam
fviz_cluster(list(data=pokemon_num, cluster = pokemon_pam$clustering),  ellipse.type="convex", geom="point", stand=TRUE, palette="Dark2") + labs(title = "PAM") + theme(legend.position="bottom")

