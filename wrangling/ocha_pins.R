library(tidyverse)
library(readxl)
library(countrycode)
library(wpp2022)

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
    "inputs",
    "OCHA PiN"
)


############################
#### WRANGLING THE DATA ####
############################

# reading in files
file_list <- list.files(input_dir, pattern = "Plan")

df_pin <- map(
    .x = file_list,
    .f = \(fn) {
        df <- read_excel(file.path(input_dir, fn), sheet = "Export data")
        year <- str_extract(fn, "(?<=_)[0-9]{4}(?=_)")
        df$year <- year
        df
    } 
) %>%
    list_rbind() %>%
    transmute(
        iso3 = countryname(Plans, destination = "iso3c"),
        year,
        plan_type = `Plan type`,
        pins = `People in need`
    ) %>%
    filter(
        !is.na(pins)
    ) %>%
    arrange(
        iso3,
        year,
        plan_type
    ) %>%
    type_convert() %>%
    filter(
        !is.na(iso3)
    )

# get population data for rough PiN percentiles

data(pop1dt)
data(popproj1dt)

popproj1dt %>%
    bind_rows(
        pop1dt
    ) %>%
    mutate(
        iso3 = countrycode(country_code, origin = "unpd", destination = "iso3c")
    ) %>%
    select(
        iso3,
        year,
        pop
    ) %>%
    right_join(
        df_pin,
        by = c("iso3", "year")
    ) %>%
    mutate(
        pin_pct = pin / pct
    ) %>%
    arrange(
        iso3,
        year
    ) %>%
    write_csv(
        file.path(
            data_dir,
            "ocha_pins.csv"
        )
    )
