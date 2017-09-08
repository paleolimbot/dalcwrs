rm(list=ls())
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)

path <- "C:/Users/Owner/Documents/Lab Data/TOC-DOC"

# Function to create master file of DOC/TOC/TN data
 toc_readr <- function(path){
   
   final <- as_tibble()
   files <- list.files(path, pattern='*.xlsx|*.xls') 
   
   data_list <- lapply(files, function(i) read_excel(paste(path,"/", i, sep = ''),
                col_names = FALSE))
   
   data_tibble <- as_tibble()
   for(i in files){
     # Read in file
     data <- read_excel(paste(path, i, sep = "/"), col_names = FALSE) %>% select(1:2)
     data_tibble <- bind_rows(data_list, data)
   }
   final <- data_tibble %>% 
     select(x1 = X__1, x2 = X__2) %>% 
     extract(x2, "toc_doc", "NPOC:([0-9.]+)", remove = FALSE) %>%  
     extract(x2, "tn", "TN:([0-9.]+)") %>%  
     extract(x1, c("type"), "([TD]OC)", remove=FALSE) %>% 
     mutate(x1 = str_replace(x1, type, "")) %>% 
     mutate(x1 = str_replace(x1, "/TN", "")) %>% 
     filter(!is.na(type)) %>% 
     gather(-x1, -type, key = "param", value = "value") %>%
     filter(!is.na(value)) 
 }   

# specify folder containing toc/doc data
X <- toc_readr(path)




