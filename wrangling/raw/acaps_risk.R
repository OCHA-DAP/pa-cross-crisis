library(tidyverse)
library(readxl)
library(httr2)

###############################
#### ENVIRONMENT VARIABLES ####
###############################

# link to the Publications/cross_crisis/data folder on your machine
data_dir <- file.path(
    Sys.getenv("CC_DIR"),
    "data"
)

######################
#### WRAP THE API ####
######################

# drop NULL values from list
null_converter <- function(x) {
    if (is.list(x)) {
        lapply(x, null_converter)
    } else {
        if (is.null(x)) {
            NA
        } else {
            x
        }
    }
}

next_url <- "https://api.acaps.org/api/v1/risk-list"
df_list <- list()

while (!is.null(next_url)) {
    resp <- request(next_url) %>%
        req_headers(
            Authorization = paste("Token", Sys.getenv("ACAPS_API_KEY"))
        ) %>%
        req_perform() %>%
        resp_body_json()
    
    df <- resp$results %>%
        map(
            .f = \(x) {
                # sometimes conflicts between crisis IDs and ISO3
                # for instance USA included as ISO3 if it's an aggressor
                # but not really a risk to the US in the same way
                if (!is.null(x$crisis_id) & (length(x$crisis_id) < length(x$iso3))) {
                    x <- x[!(names(x) %in% c("country", "iso3"))]
                } else if ((length(x$crisis_id) > length(x$iso3))) {
                    x <- x[!(names(x) %in% c("crisis_id"))]
                }
                
                # need to replace NULL with NA
                x <- null_converter(x)
                
                x %>%
                    as_tibble() %>%
                    unnest(
                        cols = where(is.list)
                    )
            }
        ) %>%
        list_rbind()
    
    df_list <- c(df_list, list(df))
    next_url <- resp$`next`
}

df_list %>%
    list_rbind() %>%
    mutate(
        iso3 = ifelse(
            is.na(iso3),
            str_sub(crisis_id, end = 3),
            iso3
        ) 
    ) %>%
    filter(
        iso3 != "REG"
    ) %>%
    write_csv(
        file.path(
            data_dir,
            "acaps_risk.csv"
        )
    )

