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

df <- read_excel(
    path = file.path(
        input_dir,
        "CERF UFE 2023-I_CIRV and Funding_Feb 2023.xlsx"
    ),
    sheet = "CERF UFE 2023-I All Data",
    skip = 2,
    na = "-"
)

df %>%
    select(
        iso3 = `ISO3 Country Code`,
        matches("[0-9]{4}R[1-2]{1}")
    ) %>%
    pivot_longer(
        -iso3,
        names_to = c("year", "round"),
        names_sep = "R",
        values_to = "cerf_cirv"
    ) %>%
    filter(
        iso3 != "TEST"
    ) %>%
    group_by(
        year,
        round
    ) %>%
    mutate(
        cerf_cirv_rank = min_rank(desc(cerf_cirv))
    ) %>%
    write_csv(
        file.path(
            data_dir,
            "cerf_cirv.csv"
        )
    )
