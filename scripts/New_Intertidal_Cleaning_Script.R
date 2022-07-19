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

# setwd("../SML 2021")
setwd("~/Desktop/SML R/SML 2021")


###################################################
### PERCENT COVER
  
#read in data, list all file names matching "Intertidal" pattern
data.list <- list.files(path=getwd(), pattern="Transect", all.files=TRUE)
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

write.csv(pc.unique,"pc.unique.csv")

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
sz.df.list <- lapply(data.list, read_xlsx, sheet=2, skip = 1)


#subsetting each data frame in list, dropping columns 6 through 9, which have FUCUS and ASCOPHYLLUM.
# check early early code
sz.df.list <- lapply(sz.df.list, "[", -c(6:9))
  
# LP - this code doesn't work, but was at line 296
# '[', ,-c(6:9)) 

#"melting" each data frame into long format. The core five columns are kept as "ID" variables. "organism" column headers are melted into a "variable" column, while corresponding sizes values are melted into a "value" column.
sz.long <- lapply(sz.df.list, pivot_longer, cols=!1:5, names_to="Organism", values_to="Sizes")

#At this point each data frame in sz.long has _exactly_ 7 columns. 5 ID columns as mentioned above, plus "variable," and "value."
#so now we bind every data frame together by column index.
# LP - I think this one should be chnaged from pc.long to sz.long on the right side of the arrow
sz.post <- sz.long %>%
  rbindlist()

sz.bound <- rbind(sz.left.long, sz.early, sz.post) %>%
  tidyr::drop_na(Year, Transect, Level, Replicate, VAR.Organism) %>% # drop NA values in columns where doing so would not drop any actual data, assuming every "core" column except for Data_taken is filled out
  filter(!(VAR.Organism %in% c("", " "))) %>% #filter out rows where Organism is empty
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
  mutate(Count = ifelse(grepl("nd|md", Count)==TRUE & grepl("Notes|Quality|Total", VAR.Organism, ignore.case= TRUE ) == FALSE, "p", Count))

names(sz.bound)[6:7] <-
  c("Organism", "Size_class")  #rename these two columns

lapply(sz.bound[, 1:4], unique)


# ##sizes, near-numeric values
# IA don't think we need
# #near-numeric value look like this: "<1" or "28%" or "~34". We want to ideally get rid of these non-number symbols, but only in primary records and not in any kind of "notes" columns. Otherwise, a note that reads "black zone sp= 80% Cyanobacteria plus 5% Verrucaria" will be reduced to "805."
# sz.bound %<>%
#   mutate(sz1=ifelse(grepl("Notes|Z|X|Quality", Organism, ignore.case = FALSE)==FALSE & grepl("[^0-9.]", Count)==TRUE & grepl("p", Count)==FALSE, gsub("[^0-9.]", "", Count), Count))
# 
# sz.diff <- sz.bound %>% filter(Count!=sz1)

#summing all size classes of a given species in one row.
sz.sum.spp <- sz.bound %>%
  group_by(Year, Transect, Level, Replicate, Organism) %>%
  dplyr::summarise(sp_sum = sum(as.numeric(Count_size), na.rm = TRUE))

#joining sz.bound (cleaned size class data), sz.sum.spp (species-specific sums), and ct.bound (cleaned counts data)
sz.test <-
  left_join(
    left_join(
      sz.bound,
      sz.sum.spp,
      by = c("Year", "Transect", "Level", "Replicate", "Organism")
    ),
    ct.bound,
    by = c("Year", "Transect", "Level", "Replicate", "Organism")
  ) %>%
  mutate(Count_size_fixed = ifelse((
    #conditionally make new column based on following set of conditions
    abs(as.numeric(Count) - sp_sum) >= 0.05 * as.numeric(Count) &
      # if absolute difference between sp_sum and count is larger than 5% of the count AND
      (sp_sum >= 50 &
         sp_sum <= 150) & #sp_sum is between 50 and 150 AND
      Year <= 2010 & # the year is before or on 2010 AND
      as.numeric(Count) > 0.1 # and the count is not zero. this is to prevent making change to "case2" rows as described above.
  ),
  # if all these conditions are satisfied then we are fairly sure that case1 is happening
  as.numeric(Count_size) * as.numeric(Count) / 100,
  # THEN make value of new column Count_raw to be percentage (Count_size) multiplied by total count (Count) divided over 100
  ifelse(Count %in% c(NA, "", " "), Count_size, Count_size) # OTHERWISE keep original value in Count_size
  )) %>%
  mutate(Count_total_fixed = ifelse(
    !(sp_sum %in% c(0, NA)) &
      # here we account for cases2. If sp_sum is NOT NA nor 0 AND
      (Count %in% c(0, NA)), # total count is either 0 or NA
    ifelse(Year > 2010, sp_sum, "p"), # if the year is after 2010, then fill in the sum of species size counts in total counts, otherwise (year is before 2010) put "p"
    Count # if nothing is satisfied then just fill in the original value
  )) %>% # Count is either 0 or NA, THEN fill in species sums for Counts, otherwise keep original value.
  mutate(
    Count_size_fixed = ifelse(
      Count_total_fixed == "p" &
        Count_total_fixed != Count,
      ifelse(Count_size != 0, "p", 0),
      Count_size_fixed
    ) # here we fill in "p" values for the size class data, whose corresponding total counts got turned into "p" above
  )

#make new dataframe of only rows that were changed in above process. Quality check.
sz.test.diff <-
  sz.test %>% filter(Count_size_fixed != Count_size |
                       Count_total_fixed != Count)

ct.final <- left_join(ct.bound, sz.test[, c(1:5, 6, 13)], by=c("Year", "Transect", "Level", "Replicate", "Organism")) %>%
  mutate(Count = ifelse(grepl("semi|myti|modi|nuce", Organism) == TRUE & is.na(Count_total_fixed)==TRUE, Count_total_fixed, Count)) %>%
  select(1:7) %>%
  distinct()

sz.final <- sz.test %>%
  drop_na(Organism) %>%
  group_by(Year, Transect, Level, Replicate, Data_taken.x, Organism, Size_class) %>%
  summarise(Count=max(Count_size_fixed))

sz.notes <- sz.final %>%
  filter(grepl("Notes|Quality", Organism)==TRUE, is.na(Count)==FALSE, Count != 0) %>%
  select(-7) %>%
  mutate(Source="Size_class")

## sizes, writing to files
write.csv(sz.final %>% filter(grepl("Note|Quality|Total", Organism)==FALSE), file="sizes_data_2020.csv", row.names=FALSE)
write.csv(ct.final %>% filter(grepl("Note|Quality|Total", Organism) == FALSE), file="counts_data_2020.csv", row.names=FALSE)


#############################################
# CATEGORIES 
#list all file names matching "Intertidal" pattern
data.list <- list.files(path=getwd(), pattern="Transect", all.files=TRUE)
#read first sheets (categories) off of all files in data.list. result is a list of data frames
cg.df.list <- lapply(data.list, read_xlsx, sheet=4)
#"melting" each data frame into long format. The core five columns are kept as "ID" variables. "organism" column headers are melted into a "variable" column, while corresponding percent_cover values are melted into a "value" column.
cg.long <- lapply(cg.df.list, pivot_longer, cols=!1:5, names_to="Organism", values_to="Categories")

#At this point each data frame in cg.long has _exactly_ 7 columns. 5 ID columns as mentioned above, plus "variable," and "value."
#so now we bind every data frame together by column index.
cg.post <- cg.long %>%
  rbindlist()

```
## categories, joining and summarizing
```{r cg_final}
cg.bound <-
  rbindlist(list(cg.post), use.names = FALSE) %>% #binding the three sections together
  setNames(nm = c(
    "Year",
    "Transect",
    "Level",
    "Replicate",
    "Data_taken",
    "Organism",
    "Category"
  )) %>% #rename all columns
  drop_na(Year, Transect, Level, Organism) %>% #dropping rows with NAs in columns where doing so shouldn't drop any actual data
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
  mutate(Category = ifelse(Category == "nd", "p", Category))

#categories, taxa match & consolidation
#creating updated all_organisms_list for categories 


cg.unique=cg.bound %>%
  distinct(Organism)

write.csv(cg.unique,"cg.unique.csv")

cg.org <- read_excel("./all_organism_list_IA_KB.xlsx.xlsx", sheet=2, skip = 1)

cg.bound %<>%
  left_join(cg.org[, 1:2], by=c("Organism"="original_code")) %>%
  mutate(Organism=ifelse(is.na(preferred_code)==FALSE, preferred_code, NA)) %>%
  drop_na(Organism) %>%
  select(-8)

#writing to csv file
write.csv(cg.bound %>% filter(grepl("Note|Quality|Total", Organism)==FALSE), file="categories_data_2020.csv", row.names=FALSE)


##########################################################
# SEAWEED SIZES 
mx.post <- lapply(lapply(data.list, read_excel, sheet=2, skip=1), `[`, c(1:9)) %>%
  rbindlist()

## IA: Do we have updated seaweed lengths to 2018? Then we just need to run 2018-2021 and can amend to older
## Then we don't need the "leftover" and "early" seaweed lentgh tibbles
mx.early <-
  read_excel("./Hal_compilations/AllFMBETransectData(Sizes)(Sort17Mar09)(hw).xls",
             skip = 1)[, c(1:10)] %>%
  drop_na(1) %>%
  select(1, 3:6, 9:10, 7:8)

setwd(paste(getwd(), "/leftover_transects", sep=""))

mx.left <- lapply(lapply(leftovers, read_excel, sheet = 2, skip = 1), `[`, c(1:6)) %>%
  rbindlist() %>%
  mutate(
    Transect = substring(`X__2`, 2, 3),
    Level = substring(`X__2`, 5, 6),
    Replicate = substring(`X__2`, 8, 9),
    Data_taken = NA
  ) %>%
  select(1, 7:10, 5:6, 3:4)

mx.bound <- rbindlist(list(mx.left, mx.early, mx.post)) %>%
  setNames(
    nm = c(
      "Year",
      "Transect",
      "Level",
      "Replicate",
      "Data_taken",
      "Fucus_maxlength",
      "Fucus_max_species",
      "Asco_maxlength",
      "Asco_maxbladders"
    )
  ) %>%
  mutate(
    Fucus_max_species_fixed = ifelse(
      is.na(Fucus_max_species) == TRUE |
        grepl("/|;|,|&|or", Fucus_max_species) == TRUE,
      Fucus_max_species,
      ifelse(grepl("FD|di|ede", Fucus_max_species, ignore.case = TRUE) == TRUE,
             "Fucus distichus",
             ifelse(
               grepl("FV|ves|vis|ver", Fucus_max_species, ignore.case = TRUE) == TRUE,
               "Fucus vesiculosus",
               ifelse(
                 grepl("FS|spi", Fucus_max_species, ignore.case = TRUE) == TRUE,
                 "Fucus spiralis",
                 ifelse(
                   grepl("sp.|spp|nd|?", Fucus_max_species) == TRUE,
                   "Fucus spp.",
                   Fucus_max_species
                 )
               )
             )
      )
    )) %>%
  mutate(
    Fucus_maxlength_fixed = ifelse(
      grepl("in", Fucus_maxlength)==TRUE,
      as.numeric(gsub("[^0-9.]", "", Fucus_maxlength))*2.54,
      ifelse(
        grepl("cm", Fucus_maxlength)==TRUE,
        gsub("[^0-9.]", "", Fucus_maxlength),
        Fucus_maxlength)
    ), 
    Asco_maxlength_fixed = ifelse(
      grepl("in", Asco_maxlength)==TRUE,
      as.numeric(gsub("[^0-9.]", "", Asco_maxlength))*2.54,
      ifelse(
        grepl("cm", Asco_maxlength)==TRUE,
        gsub("[^0-9.]", "", Asco_maxlength),
        Asco_maxlength)
    )) %>%
  mutate(Data_taken = ifelse(
    grepl("y", Data_taken, ignore.case = TRUE) == TRUE,
    "yes",
    ifelse(
      grepl("n", Data_taken, ignore.case = TRUE) == TRUE,
      "no",
      Data_taken
    ))) %>%
  select(1:5, 9:12) %>%
  dplyr::rename(Fucus_max_species=Fucus_max_species_fixed, Fucus_maxlength=Fucus_maxlength_fixed, Asco_maxlength=Asco_maxlength_fixed)

write.csv(mx.bound, file="fucus_asco_max_size.csv", row.names=FALSE)




