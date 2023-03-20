### Collating OCHA PINs into one file

# libraries
library(tidyverse)

# getting env variable
input_dir <- file.path(Sys.getenv("CC_DIR"), "inputs/OCHA PiN")

# reading in files
file_list <- list.files(input_dir, pattern = "Plan")
