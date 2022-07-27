################################################
# title: 2020 Data Fill-In Code 
# author: Lauren Pandori
# started: 7/24/22
# last edited: 7/27/22
###############################################

##### Introduction #####

# this script fills in missing values in the sizes data for 2020


##### packages #####

library(readxl)
library(janitor)
library(reshape2)
library(data.table)
library(tidyverse)

##### presets (file locations, save locations, figure theme) #####

edi_file <- './data/edi_update_data_2021/edi/'
to_add <- './data/edi_update_data_2021/to_add/'
needs_process <- './data/edi_update_data_2021/needs_processing_2020/'

##### process 2020 size class and count data #####

# load data and pivot longer
# percent cover data for 2020 
cover_20 <- map(
  # generate list of files to add
  list.files(path = needs_process, pattern = 'Transect', 
             # full file path + all files
             all.files= T, full.names = T),
  # read 4th sheet of excel file (category data)
  read_xlsx, sheet = 1) %>%
  # remove "notes" columns
  map(select, -Notes) %>%
  map(pivot_longer, cols = -c(1:5), names_to = 'Organism', 
      values_to = 'Percent_cover') %>%
  # join list of data frames by common columns
  reduce(full_join, by = c("YEAR", "TRANSECT", "LEVEL", 
                           "REPLICATE", "DATA TAKEN?", 
                           "Organism", "Percent_cover")) %>%
  # make percent cover numeric
  mutate(Percent_cover = as.numeric(Percent_cover)) 

# category data for 2020 
cat_20 <- map(list.files(path = needs_process, pattern = 'Transect', 
                         all.files= T, full.names = T), read_xlsx, sheet = 4) %>%
  # make each organism a column and fill the columns with category values
  map(pivot_longer, cols = -c(1:5), names_to = 'Organism', 
      values_to = 'Percent_cover') %>%
  # change the category column to charachter formatting
  map(mutate, Percent_cover = as.character(Percent_cover)) %>%
  # join list of data frames by common columns
  reduce(full_join, by = c("YEAR", "TRANSECT", "LEVEL", 
                           "REPLICATE", "DATA TAKEN?", 
                           "Organism", "Percent_cover")) %>%
  mutate(Percent_cover = as.numeric(Percent_cover))

# pull in count data to eval whether there is nucella
count_20 <- map(list.files(path = needs_process, pattern = 'Transect', 
                           all.files= T, full.names = T), read_xlsx, sheet = 3) %>%
  # remove notes column
  map(select, -Notes) %>%
  # make each organism a column and fill the columns with category values
  map(pivot_longer, cols = -c(1:5), names_to = 'Organism', 
      values_to = 'Percent_cover') %>%
  # change the category column to charachter formatting
  map(mutate, Percent_cover = as.character(Percent_cover)) %>%
  # join list of data frames by common columns
  reduce(full_join, by = c("YEAR", "TRANSECT", "LEVEL", 
                           "REPLICATE", "DATA TAKEN?", 
                           "Organism", "Percent_cover")) %>%
  mutate(Percent_cover = as.numeric(Percent_cover))

# make list of spp to include
spplist <- c("Fucus spiralis *Base", "Fucus Spiralis Base", "Fucus spiralis Base", "Fucus spiralis base", "Modiolus" , "Mytilus", "Sb. balanoides", "Fucus sp. Base", "Fucus vesic Base", "Fucus distichus Base", "Fucus Base", "Ascophyllum Base","Nucella", "Mytilus edulis", "Semibalanus balanoides", "Modiolus")

# make a list of years, transects and levels where there should be size data
# bind cat and cover
size_list_20 <- rbind(cat_20, cover_20, count_20) %>%
  # filter for size spp (Fucus base, Mytilus, Modiolus, Semibalanus)
  filter(Organism %in% spplist) %>%
  # filter for cover > 0 and not NA
  filter(!is.na(Percent_cover) & Percent_cover > 0) %>%
  # get first word of organism (to get genus)
  mutate(Organism = if_else(word(Organism) == 'Sb.', 
                            'Semibalanus', word(Organism))) %>%
  # to remove redundancy, get rid of duplicate entries
  select(YEAR, TRANSECT, LEVEL, REPLICATE, Organism) %>%
  distinct() %>%
  mutate(match_col = paste(YEAR, TRANSECT, LEVEL, REPLICATE, Organism, sep = '_'))

# list of column names for size data (algae and inverts)
size_colnames <- c(
  "Year",
  "Transect",
  "Level",
  "Replicate",
  "Data_taken",
  "Fucus maxlength",
  'Fucus spp',
  'Ascophyllum maxlength',
  'Ascophyllum maxbladders',
  "Semibalanus balanoides_0-2",
  "Semibalanus balanoides_3-5",
  "Semibalanus balanoides_>5",
  "Mytilus edulis_0-5",
  "Mytilus edulis_6-10",
  "Mytilus edulis_11-20",
  "Mytilus edulis_21-30",
  "Mytilus edulis_>30",
  "Mytilus edulis_>50",
  "Modiolus modiolus_0-5",
  "Modiolus modiolus_6-10",
  "Modiolus modiolus_11-20",
  "Modiolus modiolus_21-30",
  "Modiolus modiolus_>30",
  "Modiolus modiolus_>50",
  "Nucella lapillus_0-10",
  "Nucella lapillus_11-20",
  "Nucella lapillus_>20"
)

# pull and bind size data
size_20 <- map(list.files(path = needs_process, pattern = 'Transect', 
                          all.files= T, full.names = T), 
               read_xlsx, sheet = 2, skip = 1) %>%
  # set names to be list of names above
  map(setNames, nm = size_colnames) %>%
  # join to make long dataset by column names
  reduce(full_join, by = c(size_colnames)) %>%
  # make long dataset
  pivot_longer(!1:5, names_to = 'Organism_long', values_to = 'Size') %>%
  # cut organism column to first word 
  mutate(Organism = word(Organism_long),
         match_col = paste(Year, Transect, Level, Replicate, Organism, sep = '_'))

# fill in places where there should be data (from size_list_20) with "ND"
# fill in places where there shouldn't be data with "NA"


size_20 <- size_20 %>% 
  mutate(Size = case_when(
    is.na(Size) & match_col %in% c(size_list_20$match_col) ~ 'ND',
    is.na(Size) & !match_col %in% c(size_list_20$match_col) ~ 'NA',
    T ~ Size)) %>%
  select(-match_col, -Organism)

# pivot wider to match incoming format
size_20 <- size_20 %>%
  pivot_wider(names_from = 'Organism_long', values_from = 'Size')

# export to add to google drive excel files
write_csv(size_20, paste0(needs_process, 'all_2020_transect_size_data.csv'))


