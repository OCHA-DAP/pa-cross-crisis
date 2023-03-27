# script to compare the OCHA PINS and INFORM severity
library(tidyverse)
library(gghdx)
library(lubridate)
library(showtext)

font_add_google("Source Sans Pro")
showtext_auto()

###############################
#### ENVIRONMENT VARIABLES ####
###############################

input_dir <- file.path(Sys.getenv("CC_DIR"), "data")

#################################
#### READ IN THE DATA FRAMES ####
#################################

ocha_pins <- read_csv(file.path(input_dir, "ocha_pins.csv"))
inform_severity <- read.csv(file.path(input_dir, "inform_severity.csv"))

#####################
#### CORRELATION ####
#####################

# using annual mean/max inform index
stat_df <- inform_severity %>%
    mutate(year = year(date), 
           inform_severity = as.numeric(str_replace(inform_severity, "x", ""))) %>%
    select(iso3, year, inform_severity) %>%
    filter(!is.na(iso3) & !is.na(inform_severity)) %>%
    group_by(iso3, year) %>%
    summarise(mean_inf_sev = mean(inform_severity, na.rm = T), 
              max_inf_sev = max(inform_severity, na.rm = T)) %>%
    ungroup() %>%
    merge(ocha_pins, by = c("iso3", "year")) %>%
    filter(plan_type == "HRP") 

yearly_df <- stat_df %>%
    group_by(year) %>%
    summarise(mean_cor = cor(mean_inf_sev, pins, method = "spearman"),
              mean_pvalue = cor.test(mean_inf_sev, pins, method = "spearman", exact = F)$p.value,
              max_cor = cor(max_inf_sev, pins, method = "spearman"),
              max_pvalue = cor.test(max_inf_sev, pins, method = "spearman", exact = F)$p.value)

yr <- yearly_df %>%
    pivot_longer(cols = c("mean_cor", "max_cor"), names_to = "stats", values_to = "correlation") %>%
    ggplot(aes(x=year, y=correlation, fill=stats)) + 
    geom_bar(stat="identity", position = "dodge") + 
    labs(title = "Correlation between INFORM and OCHA PINs over the years")
yr + theme_hdx() + scale_color_hdx()

# using all monthly values
all_df <- inform_severity %>%
    mutate(year = year(date), 
           month = month(date, label = T),
           inform_severity = as.numeric(str_replace(inform_severity, "x", ""))) %>%
    select(iso3, year, month, inform_severity) %>%
    filter(!is.na(iso3)) %>%
    merge(ocha_pins, by = c("iso3", "year")) %>%
    filter(plan_type == "HRP") 

monthly_df <- all_df %>%
    group_by(year, month) %>%
    summarise(all_cor = cor(inform_severity, pins, method = "spearman"),
              all_pvalue = cor.test(inform_severity, pins, method = "spearman", exact = F)$p.value)
monthly_df %>%
    mutate(year = as.factor(year)) %>%
    ggplot(aes(x=month, y=all_cor, fill=year)) + 
    geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
    labs(title = "Correlation between INFORM and OCHA PINs by month") +
    theme_hdx() + 
    scale_color_hdx()

# checking if for each year, the same countries are in the top ten
pin_top10 <- stat_df %>%
    group_by(year) %>%
    slice_max(pins, n=10)

inform_top10mean <- stat_df %>%
    group_by(year) %>%
    slice_max(mean_inf_sev, n=10)

inform_top10max <- stat_df %>%
    group_by(year) %>%
    slice_max(max_inf_sev, n=10)

top10_df <- data.frame(year = unique(stat_df$year))
for(yr in unique(stat_df$year)){
    yr_index <- which(yr == top10_df$year)
    imean_vs_imax <- (inform_top10mean$iso3[inform_top10mean$year == yr]) %in% (inform_top10max$iso3[inform_top10max$year == yr])
    imean_vs_pin <- (inform_top10mean$iso3[inform_top10mean$year == yr]) %in% (pin_top$iso3[pin_top$year == yr])
    imax_vs_pin <- (inform_top10max$iso3[inform_top10max$year == yr]) %in% (pin_top$iso3[pin_top$year == yr])
    top10_df$informmean_vs_informmax[yr_index] <- sum(imean_vs_imax)/length(imean_vs_imax)
    top10_df$informmean_vs_pin[yr_index] <- sum(imean_vs_pin)/length(imean_vs_pin)
    top10_df$informmax_vs_pin[yr_index] <- sum(imax_vs_pin)/length(imax_vs_pin)
}
write_csv(top10_df, "C:/Users/pauni/Desktop/Work/OCHA/CrossCrisis/top10_informvpins.csv")
