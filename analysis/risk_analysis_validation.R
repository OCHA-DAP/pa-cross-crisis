library(tidyverse)
library(readxl)
library(gghdx)
gghdx()

data_dir <- file.path(
    Sys.getenv("CC_DIR"),
    "data"
)

plot_dir <- file.path(
    Sys.getenv("CC_DIR"),
    "plots"
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

###################
#### JOIN DATA ####
###################

df_sev_clean <- df_inform_sev %>%
    transmute(
        iso3,
        date,
        year = year(date),
        month = month(date),
        inform_severity
    ) %>%
    arrange(
        iso3,
        date
    )

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

#########################################
#### PLOTTING IRC WATCHLIST WITH SEV ####
#########################################

# generate a plot of all years normalized
# to see general pattern

df_joined %>%
    ggplot(
        aes(
            x = series,
            y = inform_severity_norm
        )
    ) +
    geom_line(
        aes(
            group = series_id,
            color = irc_listed,
            alpha = irc_listed
        ),
        linewidth = 0.3
    ) +
    geom_hline(
        yintercept = 1,
        color = hdx_hex("gray-dark"),
        linewidth = 1
    ) +
    geom_vline(
        xintercept = 12,
        color = hdx_hex("gray-dark"),
        linewidth = 1
    ) +
    geom_text_hdx(
        data = data.frame(
            x = c(13.5, 10),
            y = 1.9,
            label = c("Alert year", "Previous year")
        ),
        mapping = aes(
            x = x,
            y = y, 
            label = label
        ),
        check_overlap = TRUE,
        size = 8
    ) +
    scale_alpha_manual(
        values = c(0.4, 0.8)
    ) +
    scale_color_manual(
        values = unname(hdx_hex(c("gray-medium", "tomato-hdx"))),
        labels = c("Not on watchlist", "On IRC watchlist"),
        name = ""
    ) +
    scale_x_continuous(
        breaks = c(1, 6, 12, 18, 23),
        labels = c(
            "January",
            "June",
            "December",
            "June",
            "November"
        )
    ) +
    guides(
        alpha = FALSE
    ) +
    labs(
        x = "",
        y = "INFORM Severity (normalized)",
        title = "Change in INFORM severity for countries on IRC watchlist",
        subtitle = "Inform severity normalized to 1.0 in December of year before alert"
    ) +
    theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 20)
    )

ggsave(
    file.path(
        plot_dir,
        "irc_severity_before_after.png"
    ),
    height = 4,
    width = 5
)

# look across the years we have full data for
# without normalizing



df_sev_clean %>%
    left_join(
        df_irc,
        by = c("iso3", "year")
    ) %>%
    mutate(
        irc_listed = replace_na(irc_listed, FALSE)
    ) %>%
    filter(
        year %in% c(2021, 2022)
    ) %>%
    ggplot(
        aes(
            x = date,
            y = inform_severity,
            group = iso3,
            color = irc_listed
        )
    ) +
    geom_line() +
    scale_color_manual(
        values = unname(hdx_hex(c("gray-medium", "tomato-hdx"))),
        labels = c("Not on watchlist", "On IRC watchlist"),
        name = ""
    ) +
    facet_wrap(
        ~year,
        scales = "free_x"
    ) +
    labs(
        x = "",
        y = "INFORM Severity (normalized)",
        title = "Change in INFORM severity for countries on IRC watchlist",
        subtitle = "Inform severity normalized to 1.0 in December of year before alert"
    ) +
    theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 20)
    )

ggsave(
    file.path(
        plot_dir,
        "irc_severity_raw.png"
    ),
    height = 5,
    width = 4
)

# clearly the selection of countries are the most severe countries
# let's look at a scatter plot
df_joined %>%
    filter(
        series %in% c(12, 24)
    ) %>%
    mutate(
        time = ifelse(
            series == 12,
            "Start",
            "End"
        )
    ) %>%
    select(
        iso3, year = irc_year, inform_severity, irc_listed, rank, time
    ) %>%
    pivot_wider(
        names_from = time,
        values_from = inform_severity
    ) %>%
    ggplot(
        aes(
            x = End,
            y = Start
        )
    ) +
    geom_polygon(
        data = data.frame(
            End = c(0, 5, 5),
            Start = c(0, 0, 5)
        ),
        fill = hdx_hex("tomato-light")
    ) +
    geom_polygon(
        data = data.frame(
            End = c(0, 0, 5),
            Start = c(0, 5, 5)
        ),
        fill = hdx_hex("sapphire-light")
    ) +
    geom_point(
        aes(
            color = irc_listed
        ),
        size = 1
    ) +
    geom_text_hdx(
        data = data.frame(
            End = c(0.6, 0.4),
            Start = c(0.4, 0.6),
            label = c("Deterioration", "Improvement")
        ),
        mapping = aes(
            label = label
        ),
        angle = 42,
        fontface = "bold",
        color = hdx_hex('gray-light'),
        size = 7
    ) +
    scale_y_continuous_hdx() +
    coord_cartesian(clip = "off") +
    scale_color_manual(
        values = unname(hdx_hex(c("gray-light", "tomato-hdx"))),
        labels = c("Not on watchlist", "On IRC watchlist"),
        name = ""
    ) +
    labs(
        x = "INFORM Severity (end of alert year)",
        y = "INFORM Severity (end of previous year)",
        title = "Change in INFORM Severity across IRC watch year"
    ) +
    theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 20),
        legend.text = element_text(size = 14)
    )


ggsave(
    file.path(
        plot_dir,
        "irc_severity_diagonal.png"
    ),
    height = 4,
    width = 4
)


# clearly the selection of countries are the most severe countries
# let's look at a scatter plot
# but this time add in rank

df_joined %>%
    filter(
        series %in% c(12, 24)
    ) %>%
    mutate(
        time = ifelse(
            series == 12,
            "Start",
            "End"
        )
    ) %>%
    select(
        iso3, year = irc_year, inform_severity, irc_rank, rank, time
    ) %>%
    pivot_wider(
        names_from = time,
        values_from = inform_severity
    ) %>%
    ggplot(
        aes(
            x = End,
            y = Start
        )
    ) +
    geom_polygon(
        data = data.frame(
            End = c(0, 5, 5),
            Start = c(0, 0, 5)
        ),
        fill = hdx_hex("tomato-light")
    ) +
    geom_polygon(
        data = data.frame(
            End = c(0, 0, 5),
            Start = c(0, 5, 5)
        ),
        fill = hdx_hex("sapphire-light")
    ) +
    geom_point(
        aes(
            color = irc_rank
        ),
        size = 1
    ) +
    geom_text_hdx(
        data = data.frame(
            End = c(0.6, 0.4),
            Start = c(0.4, 0.6),
            label = c("Deterioration", "Improvement")
        ),
        mapping = aes(
            label = label
        ),
        angle = 42,
        fontface = "bold",
        color = hdx_hex('gray-light'),
        size = 7
    ) +
    scale_y_continuous_hdx() +
    coord_cartesian(clip = "off") +
    scale_color_manual(
        values = unname(hdx_hex(c("gray-light", "tomato-hdx", "gray-black"))),
        labels = c("Not on watchlist", "On IRC watchlist", "Ranked on watchlist"),
        name = ""
    ) +
    labs(
        x = "INFORM Severity (end of alert year)",
        y = "INFORM Severity (end of previous year)",
        title = "Change in INFORM Severity across IRC watch year"
    ) +
    theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 20),
        legend.text = element_text(size = 14)
    )


ggsave(
    file.path(
        plot_dir,
        "irc_severity_diagonal_ranked.png"
    ),
    height = 4,
    width = 4
)



###################################################
#### COMPARING INFORM RISK & AND IRC WATCHLIST ####
###################################################

# generate a plot of all years normalized
# to see general pattern

df_sev <- df_joined %>%
    group_by(
        iso3,
        irc_year
    ) %>%
    summarize(
        inform_sev_change = inform_severity[series == 24] - inform_severity[series == 12],
        inform_risk = unique(inform_risk),
        irc_rank = unique(irc_rank),
        .groups = "drop"
    )

p_inform_sev <- df_sev %>%
    ggplot(
        aes(
            x = inform_sev_change,
            y = inform_risk
        )
    ) +
    geom_rect(
        xmin = -Inf,
        xmax = 0,
        ymin = -Inf,
        ymax = Inf,
        fill = hdx_hex("sapphire-light")
    ) +
    geom_rect(
        xmin = 0,
        xmax = Inf,
        ymin = -Inf,
        ymax = Inf,
        fill = hdx_hex("tomato-light")
    ) +
    geom_point(
        color = hdx_hex("gray-light"),
        size = 1
    ) +
    geom_text_hdx(
        data = data.frame(
            inform_sev_change = c(-0.3, 0.3),
            inform_risk = 1.8,
            label = c("Improvement", "Deterioration")
        ),
        mapping = aes(
            label = label
        ),
        fontface = "bold",
        color = hdx_hex('gray-light'),
        size = 7
    ) +
    labs(
        x = "Change in INFORM Severity",
        y = "INFORM Risk",
        title = "Change in INFORM severity relative to INFORM Risk",
        subtitle = "Measured from December of the previous year to December of the risk year"
    ) +
    theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 20)
    )

ggsave(
    file.path(
        plot_dir,
        "irc_severity_change.png"
    ),
    plot = p_inform_sev,
    height = 3,
    width = 4
)

# and add on the IRC watchlist

p_inform_sev +
    geom_point(
        aes(
            color = irc_rank
        ),
        size = 1
    ) +
    scale_color_manual(
        values = unname(hdx_hex(c("gray-light", "tomato-hdx", "gray-black"))),
        labels = c("Not on watchlist", "On IRC watchlist", "Ranked on watchlist"),
        name = ""
    )


ggsave(
    file.path(
        plot_dir,
        "irc_severity_change_irc.png"
    ),
    height = 3,
    width = 4
)
    

#########################################
#### PLOTTING INFORM RISK & SEVERITY ####
#########################################

# generate a plot of all years normalized
# to see general pattern

p_inform <- df_joined %>%
    filter(
        series == 12
    ) %>%
    ggplot(
        aes(
            x = inform_severity,
            y = inform_risk
        )
    ) +
    scale_y_continuous_hdx() +
    labs(
        x = "INFORM Severity (end of previous year)",
        y = "INFORM Risk",
        title = "INFORM Risk and Severity",
    ) +
    coord_cartesian(
        clip = "off"
    ) +
    geom_point(
        size = 1
    ) +
    theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.line = element_blank(),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 20)
    )

ggsave(
    file.path(
        plot_dir,
        "inform_severity_risk.png"
    ),
    plot = p_inform,
    height = 4,
    width = 5
)

# add in IRC quickly

p_inform + 
    geom_point(
        aes(
            color = irc_rank
        ),
        size = 1
    ) +
    scale_color_manual(
        values = unname(hdx_hex(c("gray-light", "tomato-hdx", "gray-black"))),
        labels = c("Not on watchlist", "On IRC watchlist", "Ranked on watchlist"),
        name = ""
    ) +
    labs(
        title = "INFORM Risk & Severity and IRC watchlist"
    )

ggsave(
    file.path(
        plot_dir,
        "inform_severity_risk_irc.png"
    ),
    height = 4,
    width = 5
)

#######################################################
#### LOOK AT ACTUAL DISTRIBUTIONS ACROSS SOME BINS ####
#######################################################

# inform risk

df_sev %>%
    mutate(
        inform_risk_grouped = case_when(
            inform_risk < 4 ~ 3,
            inform_risk < 6 ~ 5,
            inform_risk < 8 ~ 7,
            inform_risk >= 8 ~ 9
        )
    ) %>%
    ggplot(
        aes(
            y = inform_sev_change,
            x = inform_risk_grouped,
            group = inform_risk_grouped
        )
    ) +
    geom_rect(
        xmin = -Inf,
        xmax = Inf,
        ymin = 0,
        ymax = Inf,
        fill = hdx_hex("tomato-light")
    ) +
    geom_rect(
        xmin = -Inf,
        xmax = Inf,
        ymin = -Inf,
        ymax = 0,
        fill = hdx_hex("sapphire-light")
    ) +
    geom_boxplot(
        fill = hdx_hex("gray-light"),
        color = hdx_hex("gray-black")
    ) +
    geom_text_hdx(
        data = data.frame(
            inform_sev_change = c(-1.2, 1.2),
            inform_risk_grouped = 3,
            label = c("Improvement", "Deterioration")
        ),
        mapping = aes(
            label = label
        ),
        fontface = "bold",
        color = hdx_hex('gray-light'),
        size = 7
    ) +
    labs(
        y = "Change in INFORM Severity",
        x = "INFORM Risk",
        title = "Change in INFORM severity relative to INFORM Risk",
        subtitle = "Measured from December of the previous year to December of the risk year"
    ) +
    theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 20),
        axis.line = element_blank()
    )

ggsave(
    file.path(
        plot_dir,
        "inform_severity_risk_boxplot.png"
    ),
    height = 3,
    width = 4
)

# IRC ranking


df_sev %>%
    ggplot(
        aes(
            y = inform_sev_change,
            x = irc_rank,
            group = irc_rank
        )
    ) +
    geom_rect(
        xmin = -Inf,
        xmax = Inf,
        ymin = 0,
        ymax = Inf,
        fill = hdx_hex("tomato-light")
    ) +
    geom_rect(
        xmin = -Inf,
        xmax = Inf,
        ymin = -Inf,
        ymax = 0,
        fill = hdx_hex("sapphire-light")
    ) +
    geom_text_hdx(
        data = data.frame(
            inform_sev_change = c(-1.2, 1.2),
            irc_rank = "Ranked",
            label = c("Improvement", "Deterioration")
        ),
        mapping = aes(
            label = label
        ),
        fontface = "bold",
        color = hdx_hex('gray-light'),
        size = 7
    ) +
    geom_boxplot(
        fill = hdx_hex("gray-light"),
        color = hdx_hex("gray-black")
    ) +
    scale_x_discrete(
        labels = c("Not on watchlist", "On IRC watchlist", "Ranked on watchlist"),
        name = ""
    ) +
    labs(
        y = "Change in INFORM Severity",
        x = "IRC watchlist status",
        title = "Change in INFORM severity relative to the IRC Watchlist",
        subtitle = "Measured from December of the previous year to December of the watchlist year"
    ) +
    theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 20),
        axis.line = element_blank()
    )

ggsave(
    file.path(
        plot_dir,
        "irc_watchlist_sev_boxplot.png"
    ),
    height = 3,
    width = 4
)
