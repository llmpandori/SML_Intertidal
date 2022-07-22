# title: 2021 SML Transect Data Cleaning
# author: Lauren Pandori
# last edited: 7/22/22

##### packages #####
library(tidyverse)

##### pre-sets #####
assembled <- paste0(getwd(),'/data/Data_Management_2021/Appended_2021/')
clean <- paste0(getwd(),'/data/Data_Management_2021/Cleaned_2021/')

##### load data #####
# note that some data have incorrectly parsed columns (ex - cover as charachter)
# we will resolve these parsing issues later
category <- read_csv(paste0(assembled, 'sml_intertidal_categories_2021.csv'))
invert_size <- read_csv(paste0(assembled, 'sml_intertidal_size_2021.csv'))
mobile_count <- read_csv(paste0(assembled, 'sml_intertidal_count_2021.csv'))
cover <- read_csv(paste0(assembled, 'sml_intertidal_percent_cover_2021.csv'))
algae_length <- read_csv(paste0(assembled, 'sml_intertidal_algae_length_2021.csv'))

lapply(algae_length, unique)

##### General Notes #####
# in SML data set p = present (but unknown value) and
# nd = no data (but could have been collected)
# 'NA' = truly not applicable
# 'p' could be retained for biodiversity indices that use present/absent only

##### PERCENT COVER #####

# filtering data set to remove any instances of NA or nd or p 
# note, "p" should be kept in any biodiversity analyses
# also filtering out rows with errant notes in Percent_cover column
# all other Percent_cover entries are assumed real of observation of spp., converted to number if possible

# look at unique values for all cols
lapply(cover, unique)

unique(cover$Percent_cover) #view unique entries to find rows to filter out or replace

pc_filtered = pc_data %>%
  filter(Percent_cover != "p") %>% #leave in published data set
  filter(Percent_cover != "nd") %>% #leave in published data set
  filter(Percent_cover != "<1") %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "0. 5", 0.5)) %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "10, phymato", 10)) %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "10% (est'd canopy)", 10)) %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "15 (80 sub canopy)", 80)) %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "15% (est'd canopy)", 15)) %>%
  filter(Percent_cover != "1patch") %>%
  filter(Percent_cover != "1stipe") %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "25% (est'd canopy)", 25)) %>%
  filter(Percent_cover != "2patches") %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "34 phymato", 34)) %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "43 phymato", 43)) %>%
  filter(Percent_cover != "50%Sb") %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "60 phymato", 60)) %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "65 phymato", 65)) %>%
  filter(Percent_cover != "6stipes") %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "70 phymato", 70)) %>%
  filter(Percent_cover != "if a species is not listed to the left, please put it in the notes column. Thanks!") %>%
  mutate(Percent_cover = replace(Percent_cover, Percent_cover == "upper 15.6%/substrate 1.2%", 15.6))

unique(pc_filtered$Percent_cover) #check for any missed
pc_filtered["Percent_cover"]<- as.numeric(pc_filtered[,"Percent_cover"]) #change to numeric variable
is.numeric(pc_filtered$Percent_cover)
