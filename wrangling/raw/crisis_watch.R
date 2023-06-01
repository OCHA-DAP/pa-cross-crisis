library(tidyverse)
library(readxl)

###############################
#### ENVIRONMENT VARIABLES ####
###############################

# link to the Publications/cross_crisis/data folder on your machine
data_dir <- file.path(
    Sys.getenv("CC_DIR"),
    "data"
)

input_dir <- file.path(
    Sys.getenv("CC_DIR"),
    "inputs"
)

##########################
#### WRANGLE THE DATA ####
##########################

# TODO: fix analysis if CERF updates their file, but currently the file
# has duplicates and incorrect data so don't trust it for analysis
df <- read_excel(
    path = file.path(
        input_dir,
        "CERF UFE 2023-I_CIRV and Funding_Feb 2023.xlsx"
    ),
    sheet = "CrisisWatch"
)

df %>%
    select(
        iso3 = Country,
        matches("[0-9]{5}|\\.\\.\\.[0-9]+")
    ) %>%
    mutate(
        across(
            .cols = everything(),
            .fns = as.character
        )
    ) %>%
    pivot_longer(
        -iso3
    ) %>%
    mutate(
        date = as.Date(as.numeric(name), origin = "1899-12-30"),
        name = rep(c("situation", "risk_alert"), n() / 2)
    ) %>%
    fill(
        date
    ) %>%
    pivot_wider()
    
