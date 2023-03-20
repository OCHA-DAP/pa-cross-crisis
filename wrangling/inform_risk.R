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

#################################
#### READ IN THE DATA FRAMES ####
#################################

df <- read_excel(
    file.path(
        input_dir,
        "inform_risk.xlsx"
    )
)

df %>%
    rename_with(
        tolower
    ) %>%
    filter(
        indicatorid == "INFORM"
    ) %>%
    select(
        iso3,
        year = informyear,
        inform_risk = indicatorscore
    ) %>%
    arrange(
        iso3,
        year
    ) %>%
    write_csv(
        file.path(
            data_dir,
            "inform_risk.csv"
        )
    )
