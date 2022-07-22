# title: 2021 SML Transect Data Cleaning
# author: Lauren Pandori
# last edited: 7/22/22

##### packages #####
library(tidyverse)

##### pre-sets #####
assembled <- paste0(getwd(),'/data/Data_Management_2021/Appended_2021/')
clean <- paste0(getwd(),'/data/Data_Management_2021/Cleaned_2021/')

##### load data #####
data_list <- 
