
#' Back up pipe intermediates for later inspection
#'
#' This function is meant to be placed within a sequence of piping operations,
#' provided an argument `nm` to name the object that will be saved in a
#' `backups` object in the `.GlobalEnv`. It will return the `stuff` passed to
#' it.
#'
#' @param stuff The object to backup
#' @param nm The name to give `stuff` for storage in `backups`
#'
#' @return `stuff`, unchanged
#' @export
#'
#' @examples
back_that_up <- function(stuff, nm){
  if(!exists("global_backups")) {
    global_backups <<- list()
  }
  global_backups[[nm]] <<- stuff
  stuff
}


#' Final preparation for presentation of classification metrics
#'
#' @param df A data.frame containing the following variables: "which_cancer",
#'   "synchronous", "claims_code", "medicare_count", "seer_count",
#'   "sensitivity", "PPV", "specificity", "kappa".
#' @param which_primary The primary cancer to select from `df` for presentation
#'
#' @return A list containing another list of tables plus a caption. The nested
#'   list of tables is called "class" and contains a table of SEER counts called
#'   "SEER_Count" along with a "metrics" table describing classification metrics
#'   in a presentable way.
#' @export
#'
#' @examples
show_classification <- function(df, which_primary) {
  classification_metrics_caption <-
    "For the years 2010 through 2012, the following reflect classification metrics for Medicare claims code algorithms predicting the presence of brain metastases at the same time as primary cancer diagnosis."
  to_show <-
    df %>%
    filter(which_cancer %in% which_primary) %>%
    prep_classifimetry()
  to_show[[2]] <-
    to_show[[2]] %>% arrange(desc(Timing), Primary_Cancer, desc(Claims_code)) %>%
    select(Timing,
           Primary_Cancer,
           Claims_code,
           Count,
           Sensitivity,
           PPV,
           Kappa)
  suppress_repeat <- function(vctr) {
    vctr <- as.character(vctr)
    c(vctr[1],
      ifelse(vctr[seq(vctr)[-1]] == vctr[seq(vctr)[-length(vctr)]],
             "", vctr[seq(vctr)[-1]]))
  }
  to_suppress <- c("Timing", "Primary_Cancer", "Claims_code")
  to_show[[2]][, to_suppress] <-
    lapply(to_show[[2]][, to_suppress], suppress_repeat)
  if (!exists("backups")) {
    backups <- list()
  }
  backups[["classification"]] <- to_show
  list(class = to_show, caption = classification_metrics_caption)
}

#' Make a demographics/patient characteristics manuscript table from the
#' multiple similar tables created using the same process but with different
#' strata.
#'
#' @param tbl_list
#' @param which_algo
#'
#' @return
#' @export
#'
#' @examples
make_table <- function(tbl_list, which_algo) {
  if (length(unique(map_int(tbl_list, nrow))) != 1) {
    stop("Tables should have the same number of rows!")
  }
  to_return <-
    data.frame(
      Variable = rownames(tbl_list[[1]]),
      `Overall` = tbl_list[["default"]][, 1],
      `SEER Synchronous` = tbl_list[["seer_br_mets"]][, 2],
      `Medicare Lifetime` = tbl_list[[which_algo]][, 2],
      stringsAsFactors = FALSE
    )
  rownames(to_return) <- NULL
  to_return
}


#' Title
#'
#' @param df 
#' @param which_columns 
#'
#' @return
#' @export
#'
#' @examples
suppress_columns <- function(df, which_columns) {
  suppress_small_n <- function(x) {
    first_number <- function(x) {
      x <- trimws(x, "both")
      as.numeric(regmatches(x, gregexpr("[0-9]{0,6}\\.?[0-9]{1,5}\\ ", x)))
    }
    x[first_number(x) < 12 & first_number(x) > 0] <- NA
    x
  }
  dont_replace <- grep("cs_mets|eod10_pn|cs_size", df[["Variable"]])
  df[-dont_replace, which_columns] <-
    lapply(df[-dont_replace, which_columns], suppress_small_n)
  df
}



#' Title
#'
#' @return
#' @export
#'
#' @examples
prep_weights <- function() {
  library(magrittr)
  adj <- list()
  adj[["seer2000"]] <-
    seer_adj_example %>% slice(15:n()) %>%
    modify_at("Age", function(z)
      gsub("\\-",  " to ", z)) %>% rename(pop = Age) %>%
    select(pop, count = U.S._2000_Standard_Populations) %>%
    modify_at("count", as.numeric) %>%
    mutate(weight = count / sum(count, na.rm = TRUE))
  #browser()
  adj[["census2010"]] <-
    augur::census2010 %>%
    select(pop, count = count_2010) %>%
    slice(c(4, 5, 7, 8, 9, 12)) %>%
    gather(v, k,-pop) %>%
    modify_at("k", as.numeric) %>%
    spread(v, k)
  adj$census2010[["weight"]] <-
    adj$census2010$count / sum(adj$census2010$count)
  adj$census2010 <-
    rbind(
      adj$census2010[1:4, ],
      data.frame(
        pop = "85+",
        count = sum(adj$census2010$count[5:6]),
        weight = sum(adj$census2010$weight[5:6]),
        stringsAsFactors = FALSE
      )
    )
  adj$census2010$pop <- gsub("\\ years", "", adj$census2010$pop)
  
  adj[["totals"]] <-
    augur::census2010 %>% select(-ends_with("all")) %>% slice(2)
  #make sure the right rows were selected
  stopifnot(adj$census2010$count %>% sum == adj$total_65_above$count_2010)
  adj
}


#' Winnow down classification metrics into something more manageable
#'
#' @param df 
#'
#'@export
prep_classifimetry <- function(df) {
  df <- df %>% arrange(desc(which_cancer, synchronous, claims_code))
  rows_to_switch <-
    which(df$synchronous == "Lifetime" &
            df$claims_code == "CNS + imaging")
  for (row_num in rows_to_switch) {
    old_order <- c(rows_to_switch, rows_to_switch + 1)
    new_order <- rev(old_order)
    df[old_order, ] <- df[new_order, ]
  }
  names(df) <-
    c(
      "Primary_Cancer",
      "Timing",
      "Claims_code",
      "Count",
      "SEER_Count",
      "Sensitivity",
      "PPV",
      "Specificity",
      "Kappa"
    )
  keepers <-
    c("Primary_Cancer",
      "Timing",
      "Claims_code",
      "Count",
      "Sensitivity",
      "PPV",
      "Kappa")
  #df[["Kappa"]][df[["Timing"]] == "Lifetime"] <- NA
  df[["Claims_code"]][df[["Claims_code"]] == "CNS + imaging"] <-
    "CNS Metastasis w/Diagnostic Imaging"
  df[["Claims_code"]][df[["Claims_code"]] == "CNS"] <-
    "CNS Metastasis"
  df[["Claims_code"]] <-
    relevel(factor(df[["Claims_code"]]), ref = "CNS Metastasis w/Diagnostic Imaging")
  df[["Primary_Cancer"]] <-
    factor(df[["Primary_Cancer"]], levels = c("lung", "breast", "skin"))
  seer_df <-
    df %>% select(Primary_Cancer, SEER_Count) %>% distinct() %>% arrange(Primary_Cancer)
  list(SEER_Count = seer_df,
       metrics = df[, keepers])
}



#' Work with the `histo_annum` df
#'
#' @param df 
#'
#'@export
munge_ip <- function(df, which_algo) {
  algo_keys <-
    list(
      medicare_00_dx_dx = "Medicare Lifetime CNS",
      medicare_60_dx_img = "Medicare Lifetime CNS w/Intracranial Imaging",
      medicare_60_prim_dx_matches = "Medicare Synchronous CNS",
      medicare_60_prim_dx_img = "Medicare Synchronous CNS w/Intracranial Imaging",
      seer_bm_01 = "SEER Synchronous Brain"
    )
  df[["algorithm"]] <-
    reduce2(names(algo_keys), algo_keys,
            function(initi, to_replace, replacement) {
              gsub(to_replace, replacement, initi)
            }, .init = df[["algo"]])
  to_return <- df[!with(df, algo == "seer_bm_01" & dx_year < 2010), ]
  to_return[["synchronous"]] <-
    ifelse(grepl("Synchronous", to_return[["algorithm"]]), "Synchronous", "Lifetime")
  #to_return[["algorithm"]] <- gsub("Synchronous\\ |Lifetime\\ ", "", to_return[["algorithm"]])
  to_return[["algo_v"]] <-
    as.character(factor(
      to_return[["algo_value"]],
      levels = 1:0,
      labels = c("Positive", "Negative")
    ))
  
  to_return %>% ungroup() %>% data.frame(stringsAsFactors = FALSE) %>%
    filter(algo %in% c("seer_bm_01", which_algo))
}

#' For a tidyr::nest-ed df, return a list whose contents are named according to
#' the first column of that df
#'
#' @param nested_df
nest_to_list <- function(nested_df){
  the_list <- nested_df[["data"]]
  names(the_list) <- nested_df[[1]]
  the_list
}

#' Get incidence proportions grouped by `...`
#'
#'
#' @param df 
#' @param ... 
#'
#'@export
group_ip <- function(df, ...) {
  groupers <- quos(...)
  df <- df %>%
    group_by(algo_value,
             algo_v,
             which_cancer,
             algo,
             algorithm,
             synchronous,
             !!!groupers) %>%
    summarise(group_cnt = sum(cnt, na.rm = TRUE)) %>% ungroup()
  
  df <-
    df %>%
    group_by(which_cancer, algo, algorithm, synchronous,!!!groupers) %>%
    summarise(group_total = sum(group_cnt, na.rm = TRUE)) %>%
    right_join(df) %>% ungroup()
  
  df[["inc_prop"]] <-
    sprintf("%.2f", 100 * with(df, group_cnt / group_total))
  df[["show"]] <-
    paste0(df$inc_prop, "%",
           "**", df$group_cnt,
           sep = "")
  df[["which_cancer"]] <-
    stringr::str_to_title(df[["which_cancer"]])
  
  df
}









#' Estimate adjusted incidence rate for a list of pop count dataframes, 
#' using 2000 and 2010 65+ population weights
#'
#' @param df A dataframe containing a list-column of `munge_counts` dataframes
#'
#' @return
#' @export
#' 
#' @importFrom dplyr `%>%` 
#' @importFrom purrr map imap_dfc reduce
#' @importFrom rlang enquo quo_name
#' 
#' @examples
ageadj_munged_df <- function(df) {
  df_nm <- quo_name(enquo(df))
  list_col_data <- df[["data"]] %>% back_that_up("aair_listcol")
  #browser()
  age_adj <- function(rates, std_df) {
    #stopifnot(nrow(rates) == nrow(std_df))
    ageadjust(rates[["crude_count"]], rates[["total"]], std_df[["count"]])
  }
  
  wts <- prep_weights()
  
  list(adj_00 = wts$seer2000,
       adj_10 = wts$census2010) %>%                      back_that_up(paste0(df_nm, "_aair_stds", collapse = "")) %>%
    map( ~ map(list_col_data, age_adj, std_df = .x)) %>%  back_that_up(paste0(df_nm, "_aair_aa_init", collapse = "")) %>%
    map( ~ reduce(.x, bind_rows)) %>%                     back_that_up(paste0(df_nm, "_aair_df", collapse = "")) %>%
    imap_dfc(function(df, df_nm) {
      names(df) <-
        paste(gsub("\\.", "_", names(df)),
              gsub("[^0-9]", "", df_nm),
              sep = "_")
      df
    })
}






#replace_aair_w_crude <- function()


# #The following doesn't include a confidence interval! That CI would be defined as follows: 
# # The age-adjusted incidence rate (AAIR) was calculated as an age-weighted sum over each year using census values from 2010. More explicitly, \(AAIR = \sum_{a=66}^{105+} \lambda_a(t) P_{a,2010} \left(\sum_{a'=66}^{105+} P_{a',2010} \right)^{-1}\), where \(\lambda_a(t)\) is the age- and year-specific rate, and \(P_{a,2010}\) is the age-specific proportions in the US 2010 population. The standard error was calculated as \(SE = \lambda(t) / \sqrt{n_0}\), where \(n_0\) was the unweighted sum of cases during that year [@keyfitz1966sampling, @akushevich].
# 
# nest_times <- papes$age_over_time %>% group_by(algo, dx_year, which_cancer) %>% tidyr::nest()
# nest_times[["data"]] <- map(nest_times[["data"]], munge_counts)
# 
# #check data
# #map(nest_times$data, names) %>% map(paste0, collapse = "*") %>% unlist %>% table
# #map_chr(nest_times$data, function(x) paste(map_chr(x, class), collapse = "*")) %>% table
# wts <- prep_weights()
# 
# nest_times$adj_rates_10 <- 
#   map_dbl(nest_times$data, function(rates) sum(wts$census2010[["weight"]] * rates[["rate"]]))
# 
# nest_times$adj_rates_00 <- 
#   map_dbl(nest_times$data, function(rates) sum(wts$seer2000[["weight"]] * rates[["rate"]]))
# 
# nest_times$crude_rates <- map_dbl(nest_times$data, function(rates) sum(rates[["rate"]]))
# 
# rates_df <- 
#   nest_times %>% select(algo, dx_year, which_cancer, adj_rates_10, adj_rates_00, crude_rates) %>%
#   filter(!(algo == "seer_bm_01" & dx_year < 2010)) %>% 
#   group_by(algo, which_cancer) %>% summarise(adj_ann_ave_10 = mean(adj_rates_10), 
#                                              adj_ann_ave_00 = mean(adj_rates_00),
#                                              crude_ann_ave = mean(crude_rates)) %>% 
#   filter(algo %in% c("seer_bm_01", "medicare_00_dx_dx"))
# pander(rates_df)




