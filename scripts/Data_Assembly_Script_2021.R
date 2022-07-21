# title: 2021 SML Transect Data Assembly Script
# author: Lauren Pandori
# last edited: 7/19/22

##### packages #####
library(tidyverse)

##### presets #####
assembled <- paste0(getwd(),'/data/Data_Management_2021/Appended_2020/')
new_assembled <- paste0(getwd(),'/data/Data_Management_2021/Appended_2021/')
to_add <- paste0(getwd(),'/data/Data_Management_2021/Output_2021/')

##### algae length - append new data from 2020 and 2021 #####

# load last year data
assembled_algae <- read_csv(paste0(assembled,'SML_Algae_Length.csv'),
                            col_types = cols(Year = col_double(), 
                            Transect = col_double(), Level = col_double(), 
                            Replicate = col_double(), Asco_maxlength = col_double(), 
                            Asco_maxbladders = col_double())) %>%
                    filter(assembled_algae, !Data_taken %in% c('no', 'yes', NA)))

# get data to add
new_algae1 <- read_csv(paste0(to_add, 'seaweed.long.csv'),
                       col_types = cols(Year = col_double(), 
                                        Transect = col_double(), Level = col_double(), 
                                        Replicate = col_double(), Asco_maxlength = col_double(), 
                                        Asco_maxbladders = col_double()))
new_algae2 <- read_csv(paste0(to_add, 'seaweed.long2020.csv'),
                       col_types = cols(Year = col_double(), 
                                        Transect = col_double(), Level = col_double(), 
                                        Replicate = col_double(), Asco_maxlength = col_double(), 
                                        Asco_maxbladders = col_double()))

final_algae <- full_join(assembled_algae, full_join(new_algae1, new_algae2))

write_csv(final_algae, paste0(new_assembled, 'SML_Algae_Length.csv'))

##### Percent Cover - append new data from 2021 #####
new_cover <- read_csv(paste0(to_add, 'percent_cover_data_2021.csv'))
assembled_cover <- read.delim(paste0(assembled,'SML_Intertidal_Percent_Cover'), 
                              sep = ',') %>% select(-X) %>%
                              mutate(Replicate = as.numeric(Replicate),
                                     Percent_cover = as.numeric(Percent_cover))

new_assembled_cover <- full_join(new_cover, assembled_cover)

# save output
write_csv(new_assembled_cover, paste0(new_assembled, 'SML_Intertidal_Percent_Cover.csv'))

##### 







                          