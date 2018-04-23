

library(tidyverse); library(zeallot); devtools::load_all("augur")
cancers <- read_csv("cache/cancers_joined-vars.csv.gz", progress = FALSE)

#joining for two things: code counts and date nearest to primary diagnosis

claims_dates_df <- function(filepath) {
  claims_df <- read_matches(filepath, progress = FALSE)
  names(claims_df) <- tolower(names(claims_df))
  if(!("from_dtd" %in% names(claims_df))){
    try({
      claims_df[["from_dtd"]] <- claims_df[["adm_d"]]
      claims_df[["from_dtm"]] <- claims_df[["adm_m"]]
      claims_df[["from_dty"]] <- claims_df[["adm_y"]]
        })
  }
  claims_df$from_dmy <- 
    with(claims_df, paste(from_dtd,from_dtm,from_dty, sep = "-"))
  claims_df <- claims_df[,c("patient_id", "from_dmy")]
  claims_df[["from_dmy"]] <- lubridate::dmy(claims_df[["from_dmy"]])
  claims_df
}

#' accepts two dfs, each with only two columns 'patient_id' and 'from_dmy'
days_between_claims <- function(claims_df1, claims_df2, nm){
  diff_nm <- paste("days_dx", nm, sep = "_")
  first_nm <- paste("date_dx", nm, sep = "_")
  names(claims_df1) <- c("patient_id", "dt_1")
  names(claims_df2) <- c("patient_id", "dt_2")
  cl_df <- claims_df <- left_join(claims_df1, claims_df2)
  closest_days <- function(date_1, date_2) {
    min(abs(difftime(date_1, date_2, units = "days")), na.rm = TRUE)
  }
  closest_first_date <- function(date_1, date_2){
    tr <- date_1[which.min(abs(difftime(date_1, date_2, units = "days")))]
    if(length(tr) != 1) { return(NA)
    } else{ return(tr) }
  }
  cl_df %>% group_by(patient_id) %>% 
    summarise(!!diff_nm := closest_days(dt_1, dt_2), 
              !!first_nm := closest_first_date(dt_1, dt_2))
}

dx_matches <- claims_dates_df("cache/diagnoses")
img_df <- claims_dates_df("cache/dx-imaging")

date_differences <- days_between_claims(dx_matches, img_df, "cpt_img")

cancers <- left_join(cancers, date_differences)

#=============================================

write_csv(cancers, "cache/cancers_prerecode.csv.gz")


