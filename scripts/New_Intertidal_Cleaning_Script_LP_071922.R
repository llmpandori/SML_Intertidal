# ---
# title: "New_Intertidal_Cleaning_Script"
# author: "Ingrid Ackermann"
# preceding author: "An T Nguyen"
# date: "August 23, 2021"
# 
# ---
# 
# ### Introduction
#   
# # General information
# - The first version of this cleaning script was written by An T Nguyen in 2018. The original script
#   cleaned and combined all data from 1985-2018. This new script is meant to clean join new years of 
#   data to the dataset. 
#   
# # Data key 
# - 0: Count of zero. Organism not present.
# - NA: Not applicable 
# - ND: No data. Data missing. 
# 
# # Transect information 
# - Priority transects: 02, 05, 07, 15, 20, 22, 24, 26, 28
# - Non-priority transets: 01, 08, 10, 11, 13, 14, 17, 18, 19, 21, 23, 25, 27
# - Post-2008, IEI (intertidal ecology interns) mostly recorded these transects: 05, 07, 15, 20, 26, with a couple 28s thrown in in 2009/2010
# - Post 2019, IEI recorded 05, 07, 15, 20, 22, 26
#  
# # Layout of code
# 1. Read in data 
# 2. Code seperated by classification 
#   - Percent cover (pc) 
#   - Counts (ct)
#   - Sizes (sz)
#   - Categories (cg)
#   - maximum seaweed lengths (mx)
# 3. Within each classification, clean data and combine into a file (file for pc, file for ct, etc.)
# 4. Join just-cleaned year(s) of a classification to the historic file
#   - eg. new year of categories data is joined to the historic set of categories data


# ---
# SETUP 
library(tidyverse)
library(readxl)
library(reshape2)
library(dplyr)
library(data.table)
library(stringr)
library(rstudioapi)
library(magrittr)

###################################################
### PERCENT COVER
setwd('./data/SML_2021_Input')
#read in data, list all file names matching "Intertidal" pattern
data.list <- list.files(path = getwd(), pattern="Transect", all.files = T)
#read first sheets (percent_cover) off of all files in data.list. result is a list of data frames
pc.df.list <- lapply(data.list, read_xlsx, sheet=1)
#"melting" each data frame into long format. The core five columns are kept as "ID" variables. "organism" column headers are melted into a "variable" column, while corresponding percent_cover values are melted into a "value" column.
pc.long <- lapply(pc.df.list, pivot_longer, cols=!1:5, names_to="Organism", values_to="Percent_cover")


#At this point each data frame in pc.long has _exactly_ 7 columns. 5 ID columns as mentioned above, plus "variable," and "value."
#so now we bind every data frame together by column index.
pc.post <- pc.long %>%
  rbindlist()

pc.bound <-
  rbindlist(list(pc.post), use.names = FALSE) %>% #binding the three sections together
  setNames(
    nm = c(
      "Year",
      "Transect",
      "Level",
      "Replicate",
      "Data_taken",
      "Organism",
      "Percent_cover"
    )
  ) %>% #rename all columns
  drop_na(Year, Transect, Level, Replicate, Organism) %>% #dropping rows with NAs in columns where doing so shouldn't drop any actual data
  mutate(
    Year = as.numeric(Year),
    Transect = as.numeric(Transect),
    Level = as.numeric(Level)
  ) %>% #making numeric columns
  mutate(Data_taken = ifelse(
    grepl("y|Just", Data_taken, ignore.case = TRUE) == TRUE,
    "yes",
    ifelse(
      grepl("n", Data_taken, ignore.case = TRUE) == TRUE,
      "no",
      Data_taken
    )
  )) %>% #standardizing Data_taken codes
  mutate(Percent_cover = ifelse(grepl("nd|plant|trace", Percent_cover, ignore.case = TRUE)==TRUE & grepl("Notes|Z|X|Quality", Organism, ignore.case = TRUE) == FALSE, "p", Percent_cover))

# Investigate for odd data/incorrrect entry
head(pc.bound)
unique(pc.bound$Percent_cover)

# look back @ dataset 
# fix in data entry 
# filter entire dataframe for is not numeric, so can see whole row when not numeric 
# need to apply this test for counts, etc. 
pc.prefiltered <- as.list(pc.bound$Percent_cover)
pc.numeric <- as.numeric(pc.prefiltered)
# pc.numeric %>% 
#   filter(pc.numeric!=`ND`)
is.numeric(pc.numeric)

unique(pc.numeric)
range(!is.na(pc.numeric))
hist((pc.numeric))

# Creating updated all_organisms_list for percent cover 
pc.unique=pc.bound %>%
  distinct(Organism)

write_csv(pc.unique,"pc.unique.csv")

#Percent_cover, taxa match & consolidation
#Name matching can potentially come earlier in the sequence. 

pc.org <-
  read_excel("all_organism_list_IA_KB.xlsx",
             sheet = 1,
             skip = 1) %>% 
  drop_na("preferred_code")

#VERY important note: if deciding to use this code for future uses. make sure to generate a new species list, because if this snippet doesn't find a matching "original_code", it will instead fill in a NA and NAs get dropped at the very end.
## Confirm w/ Kylla: "all_organism_list_IA_KB.xlsx" is new list? do we need to add the unique from this year and then use that?
pc.final <- pc.bound %>%
  left_join(pc.org[, 1:2], by = c("Organism" = "original_code")) %>%
  mutate(Organism=ifelse(is.na(preferred_code)==FALSE, preferred_code, NA)) %>%
  select(1:7) %>%
  drop_na(Organism) %>%
  group_by(Year, Transect, Level, Replicate, Data_taken, Organism) %>% #group by all variables
  dplyr::summarise(Percent_cover=max(Percent_cover)) # take maximum value if there are two or more to choose from. shouldn't affect "p" values

pc.notes <- pc.bound %>%
  filter(grepl("Note|Quality|Total", Organism) == TRUE, is.na(Percent_cover)==FALSE, Percent_cover != 0) %>%
  mutate(Source="percent_cover")

# Percent cover, writing to files

# writing to csv file
write.csv(pc.final %>% filter(grepl("Note|Quality|Total", Organism) == FALSE), file="percent_cover_data_2021.csv", row.names = FALSE)

###################################################
### COUNTS

#read in data, list all file names matching "Intertidal" pattern
data.list <- list.files(path=getwd(), pattern="Transect", all.files=TRUE)
#read first sheets (counts) off of all files in data.list. result is a list of data frames
ct.df.list <- lapply(data.list, read_xlsx, sheet=3)
#"melting" each data frame into long format. The core five columns are kept as "ID" variables. "organism" column headers are melted into a "variable" column, while corresponding percent_cover values are melted into a "value" column.
ct.long <- lapply(ct.df.list, pivot_longer, cols=!1:5, names_to="Organism", values_to="Counts")
#At this point each data frame in pc.long has _exactly_ 7 columns. 5 ID columns as mentioned above, plus "variable," and "value."
#so now we bind every data frame together by column index.
ct.post <- ct.long %>%
  rbindlist()

#counts, joining, summarizing
ct.bound <-
  rbindlist(list(ct.post), use.names = FALSE) %>% #binding the three sections together
  setNames(nm = c(
    "Year",
    "Transect",
    "Level",
    "Replicate",
    "Data_taken",
    "Organism",
    "Count"
  )) %>% #rename all columns
  drop_na(Year, Transect, Level, Replicate, Organism) %>% #dropping rows with NAs in columns where doing so shouldn't drop any actual data
  mutate(
    Year = as.numeric(Year),
    Transect = as.numeric(Transect),
    Level = as.numeric(Level)
  ) %>%
  mutate(Data_taken = ifelse(
    grepl("y", Data_taken, ignore.case = TRUE) == TRUE,
    "yes",
    ifelse(
      grepl("n", Data_taken, ignore.case = TRUE) == TRUE,
      "no",
      Data_taken
    )
  )) %>% #standardizing Data_taken codes
  mutate(Count = ifelse(grepl("nd|x", Count)==TRUE & grepl("Notes|Quality|Total", Organism, ignore.case= TRUE ) == FALSE, "p", Count))

# add in investigative chunk to search for non num

# NA Evaluation
# IA: Don't think we need, used to decide if data was taken
##  check data for holes
## check before and after
## if not different, delete, if vary, keep

# NAs evaluation
# ct.sum <- ct.bound %>%
#   mutate(Count = ifelse(grepl("[^0-9.]", Count) == TRUE, 0.3, Count)) %>%
#   dplyr::group_by(Year, Transect, Level, Replicate) %>%
#   dplyr::summarise(ct_row_sum = sum(as.numeric(Count), na.rm = TRUE)) %>%
#   left_join(pc.sum, by = c("Year", "Transect", "Level", "Replicate"))
# 
# ct.bound %<>%
#   left_join(ct.sum, by = c("Year", "Transect", "Level", "Replicate")) %>%
#   mutate(ct2 = ifelse(
#     is.na(ct1) == FALSE & ct1 != "",
#     ct1,
#     ifelse(
#       Data_taken == "yes" |
#         ct_row_sum > 0.0001 | pc_row_sum > 0.0001, 0, NA
#     )
#   )) %>%
#   mutate(Data_taken = ifelse(
#     is.na(Data_taken) == FALSE,
#     Data_taken,
#     ifelse(pc_row_sum > 0.001, "yes", "no")
#   )) %>% # retroactively fill in "yes" or "no" where Data_taken was originally NA, based on row sums 
#   select(1:6, 11) %>%
#   dplyr::rename(Count = ct2)


##Counts, taxa match & consolidation
# Updating all_organisms_list for counts

ct.unique=ct.bound %>%
  distinct(Organism)

write.csv(ct.unique,"ct.unique.csv")

ct.org <- read_excel("./all_organism_list_IA_KB.xlsx", sheet=3, skip = 1)

ct.bound %<>% 
  left_join(ct.org[, 1:2], by = c("Organism" = "original_code")) %>%
  mutate(Organism=ifelse(is.na(preferred_code)==FALSE, preferred_code, NA)) %>%
  select(1:7) %>%
  drop_na(Organism)

ct.notes <- ct.bound %>%
  filter(grepl("Note|Quality|Total", Organism) == TRUE, is.na(Count)==FALSE, Count != 0) %>%
  mutate(Source="counts")

#writing to csv file
write.csv(ct.bound, "counts_data_2021.csv", row.names=FALSE)

########################################################
# SIZES 
# sizes, all post 2008 transects

#defining names to use later
sz.names <- c(
  "Year",
  "Transect",
  "Level",
  "Replicate",
  "Data_taken",
  "Semibalanus balanoides_0-2",
  "Semibalanus balanoides_3-5",
  "Semibalanus balanoides_>5",
  "Mytilus edulis_0-5",
  "Mytilus edulis_6-10",
  "Mytilus edulis_11-20",
  "Mytilus edulis_21-30",
  "Mytilus edulis_>30",
  "Mytilus edulis_>30",
  "Modiolus modiolus_0-5",
  "Modiolus modiolus_6-10",
  "Modiolus modiolus_11-20",
  "Modiolus modiolus_21-30",
  "Modiolus modiolus_>30",
  "Modiolus modiolus_>30",
  "Nucella lapillus_0-10",
  "Nucella lapillus_11-20",
  "Nucella lapillus_>20"
)


#list all file names matching "Intertidal" pattern
# LP removed getwd() part of this code
data.list <- list.files(pattern="Transect", all.files=TRUE)

##error here!!!  #read second sheets (size) off of all files in data.list. result is a list of data frames
## run one at time, see which line 

sz.df.list <- lapply(lapply(data.list, read_xlsx, sheet=2, skip = 1), '[', -c(6:9))
sz.df.list <- lapply(sz.df.list, '[', c(1:23))

# LP edit - give names to columns
sz.df.list <- lapply(sz.df.list, setNames, nm = sz.names)

#"melting" each data frame into long format. The core five columns are kept as "ID" variables. "organism" column headers are melted into a "variable" column, while corresponding sizes values are melted into a "value" column.
sz.long <- lapply(sz.df.list, pivot_longer, cols=!1:5, 
                  names_to="Organism", values_to="Sizes") %>%
  rbindlist


sz.long <- sz.long %>%
  rename(Count = Sizes) %>%
  separate(., Organism, c('Organism', 'Size_class'), '_')

# output invert data
write_csv(sz.long, 'sz.long.csv')

##########################################################
# SEAWEED SIZES 

# SIZES 
# sizes, all post 2008 transects

#defining names to use later
# # order for later reference
#   "Year",
#   "Transect",
#   "Level",
#   "Replicate",
#   "Data_taken",
#   "Asco_maxbladders",
#   "Fucus_max_species",
#   'Fucus_maxlength',
#   'Asco_maxlength'

sz.names <- c(
  "Year",
  "Transect",
  "Level",
  "Replicate",
  "Data_taken",
  'Fucus_maxlength',
  "Fucus_max_species",
  'Asco_maxlength',
  "Asco_maxbladders"
)


#list all file names matching "Intertidal" pattern
# LP removed getwd() part of this code
setwd('./data/SML_2020_Input/')
data.list <- list.files(pattern="Transect", all.files=TRUE)

##error here!!!  #read second sheets (size) off of all files in data.list. result is a list of data frames
## run one at time, see which line 

seaweed.df.list <- lapply(lapply(data.list, read_xlsx, sheet=2, skip = 1), '[', c(1:9))

# LP edit - give names to columns
seaweed.df.list<- lapply(seaweed.df.list, setNames, nm = sz.names) %>% rbindlist

# output invert data
write_csv(seaweed.df.list, 'seaweed.long2020.csv')




