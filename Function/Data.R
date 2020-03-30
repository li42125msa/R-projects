library(here)
library(plyr)
library(readxl)
library(lubridate)
library(dplyr)
library(ChainLadder)
trace(utils:::unpackPkgZip, edit=TRUE)
setwd("C:/R/Data");
wd_path1 = paste("C:/R/Data", "/excel files", sep = "")
list.files(wd_path1)

file_names = list.files(wd_path1)
file_paths = paste(wd_path1, "/", file_names, sep = "")
head(read_excel(file_paths[3]), 4)

loss_run_data  = loss_run_data %>% 
  mutate(maturity_in_months = (file_year - accident_year)*12)

merged_triangle = as.triangle(loss_run_data, 
                              dev = "maturity_in_months", 
                              origin = "accident_year", 
                              value = "paid")

merged_triangle