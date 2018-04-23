

library(tidyverse)

read_gather_rename <- function(fp, nm){
  df <- read_csv(fp) %>% gather()
  names(df) <- c("key", nm)
  df
}

census <- list()
census[["filepaths"]] <- list.files("data-raw", pattern = "csv", full.names = TRUE)
census[["names"]] <- gsub(".*\\/DEC_10_SF1_|\\..*", "", census[["filepaths"]])
census_yrly <- 
  map2(census[["filepaths"]], census[["names"]], read_gather_rename) %>% 
  reduce(full_join) %>% distinct() %>% select(-QTP2_metadata) %>% 
  filter(!is.na(QTP2_ann) & QTP2_ann != "0100000US" & QTP2 != "0100000US") %>% 
  modify_at("QTP2_ann", 
            function(x) gsub("\\;?\\ Total\\ population\\ \\(all\\ ages\\)", "", x)) %>% 
  separate("QTP2_ann", c("np", "sex", "age_cat", "age_yr"), "\\ -\\ ") %>% 
  filter(np == "Number" & !is.na(age_yr) & sex == "Both sexes") %>% 
  distinct(age_yr, QTP2) %>% rename(count = QTP2) %>%
  modify_at("age_yr", function(x) gsub("(Under\\ )?|\\ years?", "", x))
census_yrly[1,1] <- 0

devtools::use_data(census_yrly)
