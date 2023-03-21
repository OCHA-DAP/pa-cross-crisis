library(tidyverse)
library(rvest)
library(readxl)
library(lubridate)

###############################
#### ENVIRONMENT VARIABLES ####
###############################

# link to the Publications/cross_crisis/data folder on your machine
data_dir <- file.path(
    Sys.getenv("CC_DIR"),
    "data"
)

#################################
#### READ IN THE DATA FRAMES ####
#################################

download_links <- read_html("https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Severity/Results-and-data") %>%
    html_elements(".INFORMDownloadLink") %>%
    html_attr("href")

excel_links <- download_links[str_detect(download_links, ".xlsx$")]

link_download <- function(path) {
    url_path <- paste0("https://drmkc.jrc.ec.europa.eu", path)
    download.file(url = url_path, destfile = f <- tempfile(fileext = ".xlsx"))
    df <- read_excel(
        path = f,
        sheet = "INFORM Severity - country",
        skip = 1
    ) %>%
        tail(-2) %>%
        mutate(
            `Last updated` = as.Date(`Last updated`) # ensure row binding is ok
        )
    
    # get the date of the analysis
    raw_date <- str_extract(path, "[a-z]+_[0-9]{4}(?=(_update|[_0-1]{0,2})\\.xlsx)")
    df$date <- dmy(paste0("1 ", raw_date))
    
    df
}

df <- map(
    excel_links,
    link_download
) %>%
    list_rbind() %>%
    select(
        iso3 = ISO3,
        date,
        date_updated = `Last updated`,
        inform_severity = `INFORM Severity Index`,
        reliability = Reliability,
        crisis_impact = `Impact of the crisis`,
        geographical = Geographical,
        human = Human,
        affected_conditions = `Conditions of people affected`,
        pin = `People in need`,
        concretation_conditions = `Concentration of conditions`,
        crisis_complexity = `Complexity of the crisis`,
        society_safety = `Society and safety`,
        operating_environment = `Operating environment`
    )

write_csv(
    df,
    file.path(data_dir, "inform_severity.csv")
)
    
