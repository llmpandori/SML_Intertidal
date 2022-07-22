
library(tidyverse)

#old file
count_old <- read_csv("data/Practice_data_2021/ct_clean.csv") %>%
  select(-X) %>%
  mutate(Type = 'Clean')

# new file (appended to ingrid)
count_new <- read_csv("data/Data_Management_2021/Appended_2021/sml_intertidal_count_2021.csv") %>%
  mutate(Type = 'Ingrid')

count <- full_join(count_old, count_new)

remove(count_old, count_new)

count_summary <- count %>%
  # get total orgs per transect per year for each data type
  group_by(Year, Transect, Type, Organism) %>%
  summarize(Count = sum(Count)) %>%
  # pivot wider so we can compare old and new
  pivot_wider(names_from = Type, values_from = Count)

View(filter(count_summary,  is.na(Ingrid)))

