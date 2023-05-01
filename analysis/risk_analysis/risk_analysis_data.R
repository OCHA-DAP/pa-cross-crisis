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

###################
#### READ DATA ####
###################

df_irc <- read_excel(
    file.path(
        data_dir,
        "irc_emergency_watchlist.xlsx"
    ),
    na = "-"
) %>%
    mutate(
        irc_rank = rank,
        irc_listed = TRUE
    )

df_inform_sev <- read_csv(
    file.path(
        data_dir,
        "inform_severity.csv"
    )
)

df_inform_risk <- read_csv(
    file.path(
        data_dir,
        "inform_risk.csv"
    )
)

df_pin <- read_csv(
    file.path(
        data_dir,
        "ocha_pins.csv"
    )
)

##########################
#### WRANGLE THE DATA ####
##########################

df_sev_clean <- df_inform_sev %>%
    transmute(
        iso3,
        date,
        year = year(date),
        month = month(date),
        inform_severity,
        inform_pin = pin
    ) %>%
    arrange(
        iso3,
        date
    )

# create the primary data
df_joined <- map(
    .x = 0:2,
    .f = \(x) {
        start_year <- 2020 + x
        end_year <- start_year + 1
        
        df_sev_clean %>%
            filter(
                year %in% c(start_year, end_year)
            ) %>%
            group_by(iso3) %>%
            filter(
                any(year == start_year & month == 12) 
            ) %>%
            mutate(
                series = ifelse(
                    year == start_year,
                    month,
                    month + 12
                ),
                inform_severity_norm = inform_severity / inform_severity[year == start_year & month == 12],
                irc_year = end_year
            )
    }
) %>%
    list_rbind() %>%
    ungroup() %>%
    left_join(
        df_irc,
        by = c(
            "iso3" = "iso3",
            "irc_year" = "year"
        )
    ) %>%
    left_join(
        df_inform_risk,
        by = c(
            "iso3" = "iso3",
            "irc_year" = "year"
        )
    ) %>%
    mutate(
        series_id = paste0(
            ifelse(
                irc_listed,
                "aaa",
                "zzz"
            ),
            iso3,
            irc_year,
            sep = "-"
        ),
        irc_listed = replace_na(irc_listed, FALSE),
        irc_rank = case_when(
            !is.na(rank) ~ "Ranked",
            irc_listed ~ "Not ranked",
            TRUE ~ "Not listed"
        )
    )

# now look at changes in variables
df_change <- df_joined %>%
    group_by(
        iso3,
        irc_year
    ) %>%
    reframe(
        inform_sev_change = inform_severity[series == 24] - inform_severity[series == 12],
        inform_pin_change = inform_pin[series == 24] - inform_pin[series == 12],
        inform_risk = unique(inform_risk),
        irc_rank = unique(irc_rank)
    ) %>%
    ungroup()

# calculate pin change and add to the change variable

df_change_plot <- df_pin %>%
    group_by(
        iso3
    ) %>%
    mutate(
        ocha_pin_start = lag(pin_pct),
        ocha_hrp_pin_start = lag(hrp_pin_pct),
        ocha_pin_change = pin_pct - lag(pin_pct),
        ocha_hrp_pin_change = hrp_pin_pct - lag(hrp_pin_pct),
        funding_ask_change = (funding_ask - lag(funding_ask)) / lag(funding_ask),
        hrp_funding_ask_change = (hrp_funding_ask - lag(hrp_funding_ask)) / lag(hrp_funding_ask)
    ) %>%
    filter(
        !is.na(ocha_pin_change)
    ) %>%
    full_join(
        df_change,
        by = c("iso3", "year" = "irc_year")
    )
