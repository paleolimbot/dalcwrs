library(tidyverse)
library(readxl)
library(stringr)
rm(list = ls())

# The input to the function is the path where the excel files (.xls) are stored
icpms <- function(path){
  #Initialize data frame
  final <- as_tibble()
  files <- list.files(path, pattern='*.xlsx') 
  
  for(i in files){
    # Read in file
    data_file <- read_excel(paste(path, i, sep = "/"), skip = 4) %>% select(1:(ncol(.)-1))
    
    # Change column names to element plus unit
    names(data_file) <- data_file %>% names(.) %>% str_extract(., "([:alpha:]+)") %>%
      paste(., data_file[1,], sep = "_") 
    
    # Create vector of sample ids
    sample.ids <- distinct(data_file[-1,3]) %>% filter(!is.na(.), .!=0)
    names(sample.ids) <- "sample_id"
    
    # Vectors of means and sd's
    m <- filter(data_file[,-c(1:3)], data_file[,1]=="x") %>% mutate(type = "mean")
    s <- filter(data_file[,-c(1:3)], data_file[,1]=="s") %>% mutate(type = "sd")
    
    # Bind values with ids
    means <- bind_cols(sample.ids, m)
    sds <- bind_cols(sample.ids, s)
    
    # Put everything together and clean it up
    temp <- rbind(means, sds) %>% 
      gather(-sample_id, -type, key = "param", value = "value") %>%
      mutate(numeric_value = suppressWarnings(as.numeric(value))) %>% 
      #all NAs are from QC sd's
      filter(!is.na(numeric_value)) %>%
      separate(param, c("param", "unit"), sep = "_") %>%
      mutate(param = str_to_lower(param)) %>%
      select(sample_id, param, type, value = numeric_value, unit) 
    
    final <- bind_rows(final, temp)
  }
  return(final)
}

##### Check 
path <- "C:/Users/Owner/Documents/Lab Data/ICPMS/raw"

X <- icpms(path = path)


