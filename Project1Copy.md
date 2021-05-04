Project 1
================
SDS348 Spring 2021

# import data sets papers and authors

library(readxl) neurID &lt;-
read\_excel(“C:\\Users\\roseh\\OneDrive\\Desktop\\Spring 2021\\SDS
348\\papers.xlsx”) authors &lt;-
read\_excel(“C:\\Users\\roseh\\OneDrive\\Desktop\\Spring 2021\\SDS
348\\authors.xlsx”)

# I had to modify the dataset so the last column of authors was read as

# a character type and not logical, so I am going to remove that now

library(tidyverse) authors &lt;- authors %&gt;% mutate(institution =
na\_if(institution, “Sample”))

# now I will join the two datasets by source\_id

neurID &lt;- neurID %&gt;% inner\_join(authors, by=“source\_id”) neurID
%&gt;% group\_by(year) %&gt;% summarize(n\_distinct(source\_id))
