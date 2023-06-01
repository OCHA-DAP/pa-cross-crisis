library(tidyverse)
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

df_joined <- read_csv(
    file.path(
        data_dir,
        "risk_joined.csv"
    )
)

df_change <- read_csv(
    file.path(
        data_dir,
        "risk_change.csv"
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
        alpha = "none"
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

df_joined %>%
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


# now look at INFORM PiN


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
        iso3, year = irc_year, inform_pin, irc_rank, rank, time
    ) %>%
    pivot_wider(
        names_from = time,
        values_from = inform_pin
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
        x = "INFORM PiN (end of alert year)",
        y = "INFORM PiN (end of previous year)",
        title = "Change in INFORM PiN across IRC watch year"
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
        "irc_inform_pin_diagonal_ranked.png"
    ),
    height = 4,
    width = 4
)

###################################################
#### COMPARING INFORM RISK & AND IRC WATCHLIST ####
###################################################

# generate a plot of all years normalized
# to see general pattern

p_inform_sev <- df_change %>%
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
        "risk_severity_change.png"
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
        "risk_severity_change_irc.png"
    ),
    height = 3,
    width = 4
)
    
#######################################################
#### COMPARING INFORM RISK & PIN AND IRC WATCHLIST ####
#######################################################

# generate a plot of all years normalized
# to see general pattern

p_inform_pin <- df_change %>%
    ggplot(
        aes(
            x = inform_pin_change,
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
            inform_pin_change = c(-0.3, 0.3),
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
        x = "Change in INFORM PiN",
        y = "INFORM Risk",
        title = "Change in INFORM PiN relative to INFORM Risk",
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
        "risk_pin_change.png"
    ),
    plot = p_inform_pin,
    height = 4,
    width = 5
)

# and add on the IRC watchlist

p_inform_pin +
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
        "risk_pin_change_irc.png"
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

df_change%>%
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


df_change %>%
    filter(
        !is.na(irc_rank)
    ) %>%
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


#######################################################
#### LOOK AT PIN DISTRIBUTIONS ACROSS SOME BINS ####
#######################################################

# inform risk

df_change %>%
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
            y = inform_pin_change,
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
            inform_pin_change = c(-2, 2),
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
        y = "Change in INFORM PiN",
        x = "INFORM Risk",
        title = "Change in INFORM PiN relative to INFORM Risk",
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
        "inform_pin_risk_boxplot.png"
    ),
    height = 3,
    width = 4
)

# IRC ranking

df_change %>%
    filter(
        !is.na(irc_rank)
    ) %>%
    ggplot(
        aes(
            y = inform_pin_change,
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
            inform_pin_change = c(-1.2, 1.2),
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
        y = "Change in INFORM PiN",
        x = "IRC watchlist status",
        title = "Change in INFORM PiN relative to the IRC Watchlist",
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
        "irc_watchlist_pin_boxplot.png"
    ),
    height = 3,
    width = 4
)


#########################
#### LOOK AT FUNDING ####
#########################

df_change %>%
    filter(
        !is.na(irc_rank)
    ) %>%
    ggplot(
        aes(
            y = ocha_funding_ask_change,
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
            ocha_funding_ask_change = c(-1.2, 4),
            irc_rank = "Ranked",
            label = c("Decreased funding", "Increased funding")
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
    scale_y_continuous_hdx(
        limits = c(-2, 5),
    ) +
    labs(
        y = "Change in funding request",
        x = "IRC watchlist status",
        title = "Change in funding request relative to the IRC Watchlist",
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
        "irc_watchlist_funding_boxplot.png"
    ),
    height = 3,
    width = 4
)

##########################
#### LOOK AT OCHA PIN ####
##########################

# general pattern for OCHA PiN and IRC

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
        iso3, year = irc_year, ocha_pin_pct, irc_rank, rank, time
    ) %>%
    pivot_wider(
        names_from = time,
        values_from = ocha_pin_pct
    ) %>%
    ggplot(
        aes(
            x = End,
            y = Start
        )
    ) +
    geom_polygon(
        data = data.frame(
            End = c(0, 1.6, 1.6),
            Start = c(0, 0, 1.6)
        ),
        fill = hdx_hex("tomato-light")
    ) +
    geom_polygon(
        data = data.frame(
            End = c(0, 0, 1.6),
            Start = c(0, 1.6, 1.6)
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
    scale_y_continuous_hdx(
        labels = scales::label_percent()
    ) +
    scale_x_continuous(
        labels = scales::label_percent()
    ) +
    scale_color_manual(
        values = unname(hdx_hex(c("gray-light", "tomato-hdx", "gray-black"))),
        labels = c("Not on watchlist", "On IRC watchlist", "Ranked on watchlist"),
        name = ""
    ) +
    labs(
        x = "OCHA PiN, % of population (end of alert year)",
        y = "OCHA PiN, % of population (end of previous year)",
        title = "Change in OCHA PiN across IRC watch year"
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
        "irc_inform_ocha_pin_pct_diagonal_ranked.png"
    ),
    height = 4,
    width = 4
)

# look at it as OCHA PiN % change

df_change %>%
    filter(
        !is.na(irc_rank)
    ) %>%
    ggplot(
        aes(
            y = ocha_pin_change,
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
            ocha_pin_change = c(-0.7, 0.5),
            irc_rank = "Ranked",
            label = c("Decreased PiN", "Increased PiN")
        ),
        mapping = aes(
            label = label
        ),
        fontface = "bold",
        color = hdx_hex('gray-light'),
        size = 7
    ) +
    coord_cartesian(
        clip = "off"
    ) +
    geom_boxplot(
        fill = hdx_hex("gray-light"),
        color = hdx_hex("gray-black")
    ) +
    scale_x_discrete(
        labels = c("Not on watchlist", "On IRC watchlist", "Ranked on watchlist"),
        name = ""
    ) +
    scale_y_continuous_hdx(
        labels = scales::label_percent()
    ) +
    labs(
        y = "Change in OCHA PiN (% of population)",
        x = "IRC watchlist status",
        title = "Change in OCHA PiNs relative to IRC watchlist"
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
        "irc_watchlist_ocha_pin_pct_boxplot.png"
    ),
    height = 3,
    width = 4
)


# look at INFORM Risk and OCHA PiN

df_change %>%
    ggplot(
        aes(
            y = ocha_pin_change,
            x = inform_risk
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
            ocha_pin_change = c(-0.7, 0.5),
            inform_risk = 1,
            label = c("Decreased PiN", "Increased PiN")
        ),
        mapping = aes(
            label = label
        ),
        fontface = "bold",
        color = hdx_hex('gray-light'),
        size = 7
    ) +
    coord_cartesian(
        clip = "off"
    ) +
    geom_point(
        fill = hdx_hex("gray-light"),
        color = hdx_hex("gray-black")
    ) +
    scale_y_continuous_hdx(
        labels = scales::label_percent()
    ) +
    labs(
        y = "Change in OCHA PiN (% of population)",
        x = "INFORM Risk",
        title = "Change in OCHA PiNs relative to INFORM Risk"
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
        "inform_risk_ocha_pin_pct.png"
    ),
    height = 4,
    width = 4
)


# look at INFORM Risk and OCHA PiN - boxplot

df_change %>%
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
            y = ocha_pin_change,
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
    geom_text_hdx(
        data = data.frame(
            ocha_pin_change = c(-0.7, 0.5),
            inform_risk_grouped = 3,
            label = c("Decreased PiN", "Increased PiN")
        ),
        mapping = aes(
            label = label
        ),
        fontface = "bold",
        color = hdx_hex('gray-light'),
        size = 7
    ) +
    coord_cartesian(
        clip = "off"
    ) +
    geom_boxplot(
        fill = hdx_hex("gray-light"),
        color = hdx_hex("gray-black")
    ) +
    scale_y_continuous_hdx(
        labels = scales::label_percent()
    ) +
    labs(
        y = "Change in OCHA PiN (% of population)",
        x = "INFORM Risk",
        title = "Change in OCHA PiNs relative to INFORM Risk"
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
        "inform_risk_ocha_pin_pct.png"
    ),
    height = 4,
    width = 4
)
