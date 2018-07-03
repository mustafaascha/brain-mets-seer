


#' Function to select claims dataframe variables for use in `join_claims`
#' 
#' 
#' @param claims_df The claims dataframe (or `extract_slice` output)
#' 
#'@return A dataframe consisting of (patient_id, from_dtd, from_dtm, from_dty),  
#'        or, in the case of the medpar files, (patient_id, adm_m, adm_d, 
#'        adm_y, dis_m, dis_d, dis_y) 
#'
select_dates_for_join <- function(claims_df) {
  names(claims_df) <- tolower(names(claims_df))
  date_vars <- c("from_dtm", "from_dtd", "from_dty")
  alt_date_vars <- c("thru_dtm", "thru_dtd", "thru_dty")
  all_dates <- c(date_vars, alt_date_vars)
  mp_adm_vars <- c("adm_m", "adm_d", "adm_y")
  mp_dis_vars <- c("dis_m", "dis_d", "dis_y")
  mp_vars <- c(mp_adm_vars, mp_dis_vars)

  if(all(mp_vars %in% names(claims_df))) {
    claims_df[,mp_vars] <- lapply(claims_df[,mp_vars], as.numeric)
    claims_df[,date_vars] <- 
      mapply(claims_df[,mp_adm_vars], claims_df[,mp_dis_vars],
               FUN = function(x, y) round((x + y) / 2))
    claims_df[,date_vars] <- lapply(claims_df[,date_vars], as.numeric)
  }
  if(all(alt_date_vars %in% names(claims_df))){
    claims_df[,all_dates] <- lapply(claims_df[,all_dates], as.numeric)
    claims_df[["from_dty"]] <- 
      ifelse(claims_df[["from_dty"]] > 2020, 
             claims_df[["thru_dty"]], claims_df[["from_dty"]])
  } 
  claims_df[,date_vars] <- lapply(claims_df[,date_vars], as.numeric)
  claims_df[,c("patient_id", date_vars)]
}


#' Read a set of `extract_all` CSVs, winnow down columns
#' 
#' @param matches_path The path to a directory containing `extract_` 
#'                     function results saved as CSVs
#' @param ... Arguments passed to `read_csv`
#' @export
#' @importFrom purrr map map_df
#' @importFrom readr read_csv
#' 
read_matches <- function(matches_path, ...) {
  map_df(list.files(matches_path, full.names = TRUE), 
    read_select <- function(x) {
      to_return <- select_dates_for_join(read_csv(x, ...))
      to_return[["patient_id"]] <- as.character(to_return[["patient_id"]])
      to_return
    }
  )
}

#' Merge a PEDSF df with output from `extract_slice`
#' 
#' This function will accept a PEDSF dataframe and an `extract_slice`-made 
#' dataframe, returning the original PEDSF dataframe with four extra columns, 
#' each named with a suffix according to the name of the dataframe passed 
#' to `the_matches`. The first added column has the prefix `counts_` and 
#' represents the number of times that `patient_id` was seen in `the_matches`; 
#' the second column is prefixed `days_`, representing the smallest number 
#' of days from primary cancer  diagnosis (rounded to the first of the month) 
#' to the claim in `the_matches`; the fourth added column is prefixed `date_`,
#' and is the `the_matches`  date closest to primary cancer date of diagnosis. 
#' 
#' 
#' This function was written for the purpose of implementing filter criteria 
#' on the basis of Medicare claims data, where the required patient population
#' must have had a claims diagnosis within some number of days of another 
#' variable (here, date of primary cancer diagnosis). 
#' 
#' @param the_cancers A PEDSF dataframe
#' @param the_matches Output from `extract_slice`
#' 
#' @export
#' @importFrom rlang enquo quo_name !! := 
#' @importFrom dplyr %>% group_by left_join summarise select starts_with n tbl_df
#' @importFrom lubridate dmy 
#' @return This function returns the dataframe passed to `the_cancers`, 
#'         but with three extra columns for each of 
#' 
join_claims <- function(the_cancers, the_matches) {
  new_name <- quo_name(enquo(the_matches))
  mk_nm <- function(x) paste0(x, new_name)
  counts_nm <- mk_nm("counts_")
  days_nm <- mk_nm("days_prim_"); date_nm <- mk_nm("first_date_")
  cancer_df_vars <- c("patient_id", "dx_month", "dx_year")
  date_vars <- c("dte_cd", "dx_date")
  message(paste("Merging", deparse(substitute(the_cancers)), "with", new_name))

  clms_days <- left_join(the_matches, the_cancers[,cancer_df_vars]) 
  clms_days[["dte_cd"]] <- with(clms_days, paste(from_dtd, from_dtm, from_dty))
  clms_days[["dx_date"]] <- with(clms_days, paste("15", dx_month, dx_year))
  clms_days <- clms_days %>% select(-starts_with("from"), -dx_month, -dx_year)
  clms_days[,date_vars] <- lapply(clms_days[,date_vars], dmy)
  clms_days[["days_until_cd"]] <- 
    with(clms_days, as.numeric(difftime(dte_cd, dx_date, units = "days")))

  message(paste("Beginning", new_name, "summary"))
  min_days_til <- function(x) {
    ifelse(all(is.na(x)), NA, min(abs(x), na.rm = TRUE))
  }
  to_join <- group_by(clms_days, patient_id) %>% 
    summarise(!!counts_nm := n(),
              !!days_nm := min_days_til(days_until_cd),
              !!date_nm := min(dte_cd, na.rm = TRUE))
  left_join(the_cancers, to_join, by = "patient_id")
}

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


n_day_fn <- function(df, v, n){ 
  counts_nm <- paste0("counts_", v)
  days_nm <- paste0("days_prim_", v)
  ifelse(df[[counts_nm]] > 0 & df[[days_nm]] <= n,
         1, 0)
}
                                                                               
make_prim_names <- function(number) {
  the_vars <- c("dx_matches", "cpt_img", "cpt_rad", "cpt_neu", "biopsy")
  paste0("medicare_", number, "_prim_", the_vars)
}
                                                                               
apply_n_day_fn <- function(df, number){ 
  the_vars <- c("dx_matches", "cpt_img", "cpt_rad", "cpt_neu", "biopsy")       
  lapply(the_vars, function(x) n_day_fn(df, x, number)) 
}
                                                                               



