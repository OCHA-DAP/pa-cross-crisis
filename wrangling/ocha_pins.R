### Collating OCHA PINs into one file

# libraries
library(tidyverse)
library(readxl)
library(countrycode)

# getting env variable
input_dir <- file.path(Sys.getenv("CC_DIR"), "inputs/OCHA PiN")
output_dir <- file.path(Sys.getenv("CC_DIR"), "data")

# creating a large dataframe
all_df <- data.frame()

# reading in files
file_list <- list.files(input_dir, pattern = "Plan")

for(i in file_list){
    # setting file path
    file_path <- file.path(input_dir, i)
    # getting year from file name
    file_year <- gsub(".*Action_(.+)_.*_as_on_\\d{4}-\\d{2}-\\d{2}.xlsx", "\\1", i)
    # reading in excel file
    file <- read_excel(file_path, sheet = "Export data")
    # renaming columns to add year
    file_sel <- file %>%
        select(starts_with("Plan") | starts_with("People in need")) %>% 
        pivot_wider(names_from = `Plan type`, values_from = `People in need`) %>%
        rename_with( ~ paste(.x, file_year), !starts_with("Plans"))
        #rename_with( ~ paste(.x, file_year), 
        #             starts_with("Plan type") | starts_with("Person in need"))
    
    # binding columns to data frame
    # creating one table
    
    if(nrow(all_df) == 0){
        all_df <- bind_rows(all_df, file_sel)
    } else {
        all_df <- full_join(all_df, file_sel, by="Plans", multiple = "all")
    }
    # printing loop value
    print(paste("Completed wrangling the file for:", file_year))
}

# writing to csv file
write.csv(all_df, file.path(output_dir, "ocha_pins.csv"))
