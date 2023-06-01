# wrangle data for plotting of the risk indicators

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
        inform_severity_rank,
        inform_pin = pin,
        inform_pin_rank
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
    full_join(
        df_irc,
        by = c(
            "iso3" = "iso3",
            "irc_year" = "year"
        )
    ) %>%
    full_join(
        df_inform_risk,
        by = c(
            "iso3" = "iso3",
            "irc_year" = "year"
        )
    ) %>%
    full_join(
        df_pin,
        by = c(
            "iso3" = "iso3",
            "year" = "year"
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
        inform_sev_rank_change = inform_severity_rank[series == 24] - inform_severity_rank[series == 12],
        inform_pin_change = inform_pin[series == 24] - inform_pin[series == 12],
        inform_pin_rank_change = inform_pin_rank[series == 24] - inform_pin_rank[series == 12],
        inform_risk = unique(inform_risk),
        irc_rank = unique(irc_rank)
    ) %>%
    ungroup()

# calculate pin change and add to the change variable

df_change_final <- df_pin %>%
    group_by(
        iso3
    ) %>%
    mutate(
        ocha_pin_start = lag(ocha_pin_pct),
        ocha_hrp_pin_start = lag(ocha_hrp_pin_pct),
        ocha_pin_change = ocha_pin_pct - ocha_pin_start,
        ocha_hrp_pin_change = ocha_hrp_pin_pct - ocha_hrp_pin_start,
        ocha_funding_ask_change = (ocha_funding_ask - lag(ocha_funding_ask)) / lag(ocha_funding_ask),
        ocha_hrp_funding_ask_change = (ocha_hrp_funding_ask - lag(ocha_hrp_funding_ask)) / lag(ocha_hrp_funding_ask)
    ) %>%
    filter(
        !is.na(ocha_pin_change)
    ) %>%
    full_join(
        df_change,
        by = c("iso3", "year" = "irc_year")
    )

###################
#### SAVE DATA ####
###################

write_csv(
    df_joined,
    file.path(
        data_dir,
        "risk_joined.csv"
    )
)


write_csv(
    df_change_final,
    file.path(
        data_dir,
        "risk_change.csv"
    )
)
