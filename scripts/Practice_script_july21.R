# sum of object
obj <- c(1,2,3,4,5)
sum(obj)

# load libraries 
library(tidyverse)
library(janitor)

# make sure your folder is the same as your project
# this is where files come from and go to
getwd()

# put data into environment
count_data <- readr::read_csv('ct_clean.csv')

# take a look at the data
# first six rows 
head(count_data)

library(readr)
ct_clean <- read_csv("ct_clean.csv")
View(ct_clean)
remove(ct_clean)

# more info
glimpse(count_data)

# see the whole darn thing
View(count_data)

# data wrangling
count_data2 <- count_data %>%
  select(Organism, Count) %>%
  group_by(Organism) %>%
  summarize(Count = sum(Count))

head(count_data2)

# make a plot
# Load ggplot2
library(ggplot2)

# Barplot
count_data2 <- count_data2 %>%
  filter(Count > 10000)

ggplot(
  # data we want to plot
  count_data2,
  # tell R what x and y values are
  aes(x = Organism, y = Count)) + 
  geom_bar(stat = "identity") + 
  theme_classic() + 
  scale_y_continuous(labels = scales::label_number(suffix = " M", scale = 1e-6)) +
  theme(axis.text.x = element_text(hjust = 1, angle = 45))

# load a bunch of files

