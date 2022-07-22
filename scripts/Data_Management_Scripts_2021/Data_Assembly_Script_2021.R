# title: 2021 SML Transect Data Assembly Script
# author: Lauren Pandori
# last edited: 7/19/22

# append 2021 data on Kylla's clean 2020 data for 2021 SML intertidal intern poster

##### packages #####
library(tidyverse)
library(janitor)

##### presets #####
assembled <- paste0(getwd(),'/data/Data_Management_2021/Kylla_Clean_Data_2020/')
new_assembled <- paste0(getwd(),'/data/Data_Management_2021/Appended_2021/')
to_add <- paste0(getwd(),'/data/Data_Management_2021/Output_2021/')

##### algae length - append new data from 2020 and 2021 #####

# load last year data
assembled_algae <- read_csv(paste0(assembled,'sw_sz_clean.csv')) %>%
  remove_empty(which = c('rows', 'cols')) %>%
  mutate(Fucus_max_species = case_when(Fucus_max_species == 'Fucus spp.' ~ 'SP',
                                       Fucus_max_species == 'Fucus distichus' ~ 'FD',
                                       Fucus_max_species == 'Fucus spiralis' ~ 'FS',
                                       Fucus_max_species == 'Fucus vesiculosus' ~ 'FV',
                                       substr(Fucus_max_species,1,2) %in% c('nd', 'sp', 've') ~ 'SP',
                                       ))
  
# get data to add
new_algae1 <- read_csv(paste0(to_add, 'seaweed.long.csv')) %>%
  mutate(
    # make any ND's into NAs
    across(.cols = everything(), na_if, 'ND'),
    # consistent fucus spp names
    Fucus_max_species = case_when(Fucus_max_species  == 'Fspp.' ~ 'SP',
                                       Fucus_max_species == 'FSPP' ~ 'SP',
                                       Fucus_max_species == 'Spp.' ~ 'SP'),
    across(.cols = c(Fucus_maxlength, Asco_maxlength, Asco_maxbladders),
           as.numeric))

new_algae2 <- read_csv(paste0(to_add, 'seaweed.long2020.csv')) %>%
  mutate(
    # make any ND's into NA's
    across(.cols = everything(), na_if, 'ND'),
    Fucus_max_species = case_when(Fucus_max_species == 'FSP' ~ 'SP'),
    across(.cols = c(Fucus_maxlength, Asco_maxlength, Asco_maxbladders),
           as.numeric))

final_algae <- full_join(assembled_algae, full_join(new_algae1, new_algae2)) %>%
  remove_empty(which = c('rows', 'cols')) %>%
  mutate(Data_taken = case_when(substr(Data_taken,1,1) == 'y' ~ 'Yes',
                                substr(Data_taken,1,1) == 'n' ~ 'No'))

write_csv(final_algae, paste0(new_assembled, 'sml_intertidal_algae_length_2021.csv'))

remove(new_algae1, new_algae2, final_algae, assembled_algae)

##### Percent Cover - append new data from 2021 #####
new_cover <- read_csv(paste0(to_add, 'percent_cover_data_2021.csv'))
assembled_cover <- read.delim(paste0(assembled,'SML_Intertidal_Percent_Cover'), 
                              sep = ',') %>% select(-X) %>%
                              mutate(Replicate = as.numeric(Replicate),
                                     Percent_cover = as.numeric(Percent_cover))

new_assembled_cover <- full_join(new_cover, assembled_cover)

# save output
write_csv(new_assembled_cover, paste0(new_assembled, 'sml_intertidal_percent_cover_2021.csv'))

remove(new_cover, new_assembled_cover, assembled_cover)

##### Count data - append new data from 2021 #####
new_count <- read_csv(paste0(to_add, 'counts_data_2021.csv'),
                      col_types = cols(Replicate = col_character()))

assembled_count <- read.delim(paste0(assembled, 'SML_Intertidal_Counts'), sep = ',') %>%
  select(-X) %>%
  mutate(Count = as.numeric(Count))

new_assembled_count <- full_join(new_count, assembled_count)

write_csv(new_assembled_count, paste0(new_assembled, 'sml_intertidal_count_2021.csv'))

remove(new_count, assembled_count, new_assembled_count)
##### Size data - append new data from 2021 #####

new_size <- read_csv(paste0(to_add, 'counts_data_2021.csv'),
                     col_types = cols(Replicate = col_character(),
                                      Count = col_character(
                                      ))) 

assembled_size <- read.delim(paste0(assembled, 'SML_Intertidal_Counts'), sep = ',') %>%
  select(-X)

new_assembled_size <- full_join(new_size, assembled_size)

write_csv(new_assembled_size, paste0(new_assembled, 'sml_intertidal_size_2021.csv'))

remove(new_size, assembled_size, new_assembled_size)

##### Category data - append new data from 2021 ####
new_category <- read_csv(paste0(to_add, 'cat_long.csv'),
                         col_types = cols(Replicate = col_character(),
                                          Data_Taken = col_character())) %>%
  rename(Data_taken = Data_Taken)

assembled_category <- read.delim(paste0(assembled, 'SML_Intertidal_Categories'), sep = ',') %>%
  select(-X)

new_assembled_category <- full_join(new_category, assembled_category)

write_csv(new_assembled_category, paste0(new_assembled, 'sml_intertidal_categories_2021.csv'))

                          