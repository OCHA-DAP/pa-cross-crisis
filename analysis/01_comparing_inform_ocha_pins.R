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
    pivot_longer(cols = c("mean_cor", "max_cor"), names_to = "stats", values_to = "cor") %>%
    ggplot(aes(x=year, y=cor, fill=stats)) + 
    geom_bar(stat="identity", position = "dodge")
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
    theme_hdx() + 
    scale_color_hdx()
