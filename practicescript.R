# Practice script 

# load library
library(PNWColors)
library(tidyverse)

# percent cover
pct_cover <- read_csv("data/Data_Management_2021/Appended_2021/sml_intertidal_percent_cover_2021.csv")

# at what levels are asco found?
asco_levels <- pct_cover %>%
  filter(substr(Organism, 1, 4) == 'Asco') %>%
  filter(Percent_cover > 0)

# get the range of levels where ascophyllum is present 
range(asco_levels$Level)
# ascophyllum shows up levels 3-13

# at what levels are barnacles found?
# filter for barnacle data where cover is greater than 0
barn_levels <- pct_cover %>%
  filter(Organism == 'Semibalanus balanoides') %>%
  filter(Percent_cover > 0)

# get the range of levels where barnacles are present
range(barn_levels$Level)
# barnacles show up levels 1-13

linreg_data <- pct_cover %>%
  # filter for 2 conditions: organism = 'semibalanus...', or organism that starts with 'asco'
  filter(Organism == 'Semibalanus balanoides' |
         substr(Organism, 1, 4) == 'Asco') %>%
  # filter for levels where both are present (3-13)
  filter(Level >= 3 | Level <=13) %>%
  # set grouping variables
  group_by(Year, Organism) %>%
  summarize(Cover_mean = mean(Percent_cover)) %>%
  # remove na's (not - is na - column of choice)
  filter(!is.na(Cover_mean))

linreg_data2 <- linreg_data %>%
  pivot_wider(names_from = Organism, values_from = Cover_mean)

# run a linear regression
asco_model <- lm(`Semibalanus balanoides` ~ `Ascophyllum nodosum (canopy)`, linreg_data2)

# gives you p-values and r2 values
summary(asco_model)

# lets you check your assumptions
plot(asco_model)

# plot our results
ggplot(data = linreg_data2,
       mapping = aes(x = `Ascophyllum nodosum (canopy)`,
                     y = `Semibalanus balanoides`)) + 
  # adds regression line
  geom_smooth(method = 'lm', se = F) + 
  # add points
  geom_point(color = 'green')

# make plot with different color points for speciews
ggplot(data = linreg_data,
       mapping = aes(x = Year, y = Cover_mean, color = Organism)) +
  geom_point() + 
  geom_smooth(aes(fill = Organism), method = 'lm', se = F) + 
  scale_color_manual(values = pnw_palette(name = 'Sunset2', 3)) + 
  scale_fill_manual(values = pnw_palette(name = 'Sunset2', 3)) + 
  ylab('Percent cover (%)') +
  xlab('Survey year') + 
  theme_bw() +
  theme(panel.grid = element_blank())
  #scale_color_manual(values = c('green', 'firebrick1', 'dodgerblue'))






######### OLd code from count data beyond this line ############
count <- full_join(count_old, count_new)

remove(count_old, count_new)

count_summary <- count %>%
  # get total orgs per transect per year for each data type
  group_by(Year, Transect, Type, Organism) %>%
  summarize(Count = sum(Count)) %>%
  # pivot wider so we can compare old and new
  pivot_wider(names_from = Type, values_from = Count)

View(filter(count_summary,  is.na(Ingrid)))

