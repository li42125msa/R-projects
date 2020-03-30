library(tabulizer)
library(dplyr)
# Location of WARN notice pdf file
location <- 'http://www.edd.ca.gov/jobs_and_training/warn/WARN-Report-for-7-1-2016-to-10-25-2016.pdf'

# Extract the table
out <- extract_tables(location)
f <- system.file("examples", "data.pdf", package = "tabulizer")
b=extract_tables(f)
site <- "http://www.sedl.org/afterschool/toolkits/science/pdf/ast_sci_data_tables_sample.pdf"
matrix_results <- extract_tables(site)