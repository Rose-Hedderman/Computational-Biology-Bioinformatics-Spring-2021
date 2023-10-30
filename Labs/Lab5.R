# Question 1
library(readxl)
library(dplyr)
Lab5wkbk <- read_excel("C:\\Users\\roseh\\OneDrive\\Desktop\\Spring 2021\\SDS 348\\Lab5wkbk.xlsx")
#View(Lab5wkbk)

# question 2
#two <- Lab5wkbk 
# I don't really know..?

# Questi
Lab5wkbk %>%
  mutate(rainforest = case_when(Modules == "M10"|
                                  Modules == "M09"|
                                  Modules == "M08"|
                                  Modules == "M07"|
                                  Modules == "M06"|
                                  Modules == "M05"|
                                  Modules == "M04"|
                                  Modules == "M03"|
                                  Modules == "M02"|
                                  Modules == "M01"  ~ "dense", (
                                Modules == "M11"|
                                  Modules == "M12"|
                                  Modules == "M13"|
                                  Modules == "M14")
                                 ~ "open"))
head(Lab5wkbk)
d <- Lab5wkbk %>%
  filter(rainforest == "dense")
o <- Lab5wkbk %>%
  filter(rainforest == "open")
head(d)

cor(??) %>%
  # Save as a data frame
  as.data.frame %>%
  # Convert row names to an explicit variable
  rownames_to_column %>%
  # Pivot so that all correlations appear in the same column
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