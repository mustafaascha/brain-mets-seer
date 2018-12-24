

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
           Specificity,
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

rm_rnm_rws <- function(to, to_rm) {
  to <- to[-to_rm,]
  rownames(to) <- NULL
  to[["Variable"]] <- 
    reduce2(c("\\(.*", 
              "age_dx",   "age_cut", 
              "cs_mets",  "eod10_pn", 
              "beh03v",   "cs_size",
              "d_ssg00",  "race_v",
              "histo",    
              "prstatus_v", "erstatus_v", "her2_v", "brst_sub_v",
              "csmetsdxb_pub_v", "csmetsdxliv_pub_v", "csmetsdxlung_pub_v",
              "Regional\\,\\ ext", "Regional\\,\\ LN",
              "_",        "\\ ="
    ),
    c("", 
      "Age - continuous", "Age - categorical", 
      "Number of metastases found at primary diagnosis", 
      "Number of metastasis-positive nodes at primary diagnosis", 
      "Behavior", "Size in mm",
      "SEER Stage", "Race",
      "Histology", 
      "PR Status", "ER Status", "HER2 Status", "SEER Subtype",
      "Bone metastases", "Liver metastases", "Lung metastases",
      "Regional, extension and nodes", "Regional, lymph nodes only",
      " ", ":"
    ), 
    function(init, rp, repl) { gsub(rp, repl, init) },
    .init = to[["Variable"]])
  to
}

#' Remove observations that would compromise privacy or confidentiality. 
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

#' Winnow down classification metrics into something more manageable
#'
#' @param df 
#'
#'@export
prep_classifimetry <- function(df, keep_these = NULL) {
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
    c("Primary_Cancer",  "Timing",
      "Claims_code",     "Count",
      "SEER_Count",      "Sensitivity",
      "PPV",             "Specificity",
      "Kappa")
  if(missing(keep_these)){
    keepers <-
      c("Primary_Cancer",   "Timing",
        "Claims_code",      "Count",
        "Sensitivity",      "PPV",
        "Specificity",
        "Kappa")
  } else {
    keepers <- keep_these
  }
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
  to_return[["algo_v"]] <-
    as.character(factor(
      to_return[["algo_value"]],
      levels = 1:0,
      labels = c("Positive", "Negative")
    ))
  
  to_return %>% ungroup() %>% data.frame(stringsAsFactors = FALSE) %>%
    filter(algo %in% c("seer_bm_01", which_algo))
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
    group_by(algo_value,   algo_v,
             which_cancer, algo,
             algorithm,    synchronous,
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

#' Show one of the clinical/demographic characteristics tables
#'
#' @param x The table
#' @param extras Something to add to the caption
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
pander_to <- function(x, extras = NULL, ...) {
  table_one_caption <- "This table only includes the subjects with primary diagnosis during 2010 through 2012."
  if(!missing(extras)) {
    table_one_caption <- paste(table_one_caption, extras)
  }
  pander(x, caption = table_one_caption, ...)
}

#' Back up pipe intermediates for later inspection
#'
#' This function is meant to be placed within a sequence of piping operations,
#' provided an argument `nm` (or, if no `nm` is provided, the object name) to 
#' name the object that will be saved in a `backups` object in the `.GlobalEnv`. 
#' It will return the `stuff` passed to it.
#'
#' @param stuff The object to backup
#' @param nm The name to give `stuff` for storage in `backups`
#'
#' @return `stuff`, unchanged
#' @export
#'
#' @examples
back_up <- function(stuff, nm = NULL){
  if(!exists("global_backups")) {
    global_backups <<- list()
  }
  if(missing(nm)) {
    nm <- quo_name(enquo(stuff))
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
primary_classification <- function(df, which_primary, ...) {
  classification_metrics_caption <-
    "For the years 2010 through 2012, the following reflect classification metrics for Medicare claims code algorithms predicting the presence of brain metastases at the same time as primary cancer diagnosis."
  to_show <-
    df %>%
    filter(which_cancer %in% which_primary) %>%
    prep_classifimetry(...)
  to_show[[2]] <-
    to_show[[2]] %>% arrange(desc(Timing), Primary_Cancer, desc(Claims_code)) %>%
    select(Timing,      Primary_Cancer,
           Claims_code, Count,
           Sensitivity, PPV,
           Specificity,
           Kappa)

  list(class = to_show, caption = classification_metrics_caption)
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
strata_classification <- function(df, which_primary) {
  classification_metrics_caption <-
    "For the years 2010 through 2012, the following reflect classification metrics for Medicare claims code algorithms predicting the presence of brain metastases at the same time as primary cancer diagnosis."
  to_show <-
    df %>%
    filter(which_cancer %in% which_primary) %>%
    prep_classifimetry()
  to_show[[2]] <-
    to_show[[2]] %>% arrange(desc(Timing), Primary_Cancer, desc(Claims_code)) %>%
    select(Timing,      Primary_Cancer,
           Claims_code, Count,
           Sensitivity, PPV,
           Specificity,
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

#' Make standardized population weights
#'
#' @return
#' @export
#' 
#' @import magrittr
#' @import dplyr
#' @import purrr
#' @import tidyr
#'
#' @examples
prep_weights <- function() {
  adj <- list()
  adj[["seer2000"]] <-
    augur::seer_adj_example %>% slice(15:n()) %>%
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

#' Work with the `histo_annum` df
#'
#' @param df 
#'
#'@export
do_ip <- function(df, which_algo, ...) {
  groupers <- quos(...)
  algo_keys <-
    list(
      medicare_00_dx_dx = "Medicare Lifetime CNS",
      medicare_60_dx_img = "Medicare Lifetime CNS w/Intracranial Imaging",
      medicare_60_prim_dx_matches = "Medicare Synchronous CNS",
      medicare_60_prim_dx_img = "Medicare Synchronous CNS w/Intracranial Imaging",
      seer_bm_01 = "SEER Synchronous Brain"
    )
  gsub_this <- function(initl, to_r, repl) gsub(to_r, repl, initl)
  df[["algorithm"]] <-
    reduce2(names(algo_keys), algo_keys, gsub_this, .init = df[["algo"]])

  df[["synchronous"]] <-
    ifelse(grepl("Synchronous", df[["algorithm"]]), "Synchronous", "Lifetime")
  
  df[["algo_v"]] <-
    as.character(factor(df[["algo_value"]],
                    levels = 1:0, labels = c("Positive", "Negative")))
  
  df <- df %>% ungroup() %>% 
    data.frame(stringsAsFactors = FALSE) %>%
    filter(algo %in% c("seer_bm_01", which_algo)) %>% 
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

#' For a tidyr::nest-ed df, return a list whose contents are named according to
#' the first column of that df
#'
#' @param nested_df
nest_to_list <- function(nested_df){
  the_list <- nested_df[["data"]]
  names(the_list) <- nested_df[[1]]
  the_list
}

#' Replace any duplicate with "", leaving only the first occurrence in place
#'
#'@export
perma_dupes <- function(vctr) {
  vctr <- as.character(vctr)
  new_things <- c()
  for (i in seq_along(vctr)) {
    if (!(vctr[i] %in% new_things)) {
      new_things <- c(vctr[i], new_things)
    } else {
      vctr[i] <- ""
    }
  }
  vctr
}


#' Reshape and rename positive/negative/missing counts in the paper counts dfs
#'
#' @param df 
#'
#' @return data.frame
#' @export
#'
#' @examples
munge_counts <- function(df) {
  tr <- spread(df, algo_value, cnt)
  names(tr) <- gsub("\\.", "", make.names(names(tr)))
  names(tr)[1] <- "pop"
  if ("X1" %in% names(tr)) {
    tr[["total"]] <- apply(tr[, -1], 1, function(x) {
      message(paste(x, collapse = ", "))
      x <- ifelse(length(x) == 0, 0, as.numeric(x))
      sum(x, na.rm = TRUE)
    })
    tr[["rate"]] <- tr[["X1"]] / tr[["total"]]
    #tr <- tr[,c("pop", "X1", "rate")]
    names(tr)[grep("X1", names(tr))] <- "crude_count"
    names(tr)[grep("X0", names(tr))] <- "crude_negs"
    names(tr)[grep("XNA", names(tr))] <- "crude_missing"
    tr[["pop"]] <- as.character(tr[["pop"]])
    tr[, -1] <- lapply(tr[, -1], as.numeric)
    #tr[["crude_count"]] <- ifelse(is.na(tr[["crude_count"]]), 0, tr[["crude_count"]])
    #tr[["rate"]] <- ifelse(is.na(tr[["rate"]]), 0, tr[["rate"]])
  }
  tr
}


#' Age adjustment function
#'
#'  This is a modified version of the `epitools` function `ageadjust.direct`. 
#'
#' @param count Number of observations of the event of interest
#' @param pop   Count in the population 
#' @param stdpop Standard population counts
#' @param conf.level 
#'
#' @return
#' @export
#' @seealso epitools
#' @examples
ageadjust <- function(count, pop, stdpop, conf.level = 0.95) {
  c(alpha, cruderate, rate, stdwt) %<-%
    list(
      1 - conf.level,
      sum(count, na.rm = T) / sum(pop, na.rm = T),
      count / pop,
      stdpop / sum(stdpop)
    )
  if(all(is.na(stdwt))|all(is.na(pop))) { stop() }
  c(dsr, dsr.var, wm) %<-%
    list(sum(stdwt * rate, na.rm = T),
         sum((stdwt ^ 2) * (count / pop ^ 2), na.rm = T),
         max(stdwt / pop, na.rm = T))
  gamma.lci <-
    qgamma(alpha / 2,
           shape = (dsr ^ 2) / dsr.var,
           scale = dsr.var / dsr)
  gamma.uci <-
    qgamma(
      1 - alpha / 2,
      shape = ((dsr + wm) ^ 2) / (dsr.var + wm ^ 2),
      scale = (dsr.var + wm ^ 2) / (dsr + wm)
    )
  
  c(
    crude.rate = cruderate,
    adj.rate = dsr,
    lci = gamma.lci,
    uci = gamma.uci
  )
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
ageadj_munge_df <- function(df) {
  df_nm <- quo_name(enquo(df))

  list_col_data <- 
    df[["data"]] %>% back_up("aair_listcol") %>% 
    map(function(the_df) {
      if(!("total" %in% names(the_df))) { 
        the_df[["total"]] <- rowSums(the_df[,-grep("pop", names(the_df))])
        }
      the_df
    })

  age_adj <- function(rates, std_df) {
    #if(nrow(rates) != nrow(std_df)) browser()
    stopifnot(nrow(rates) == nrow(std_df))
    ageadjust(rates[["crude_count"]], rates[["total"]], std_df[["count"]])
  }
  
  wts <- prep_weights()
  
  list(adj_00 = wts$seer2000,
       adj_10 = wts$census2010) %>%                      
    map( ~ map(list_col_data, age_adj, std_df = .x)) %>%  
    map( ~ reduce(.x, bind_rows)) %>%                     
    imap_dfc(function(df, df_nm) {
      names(df) <-
        paste(gsub("\\.", "_", names(df)),
              gsub("[^0-9]", "", df_nm),
              sep = "_")
      df
    })
}

#' Calculate AAIR by `...`
#'
#' @param unnested_df 
#' @param which_algo 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
aair_by <- function(unnested_df, which_algo, ...) {
  gps <- quos(...)
  gp_nms <- map_chr(gps, quo_name)
  
  nest_and_munge <- function(df, ...) {
    gps <- quos(...)
    nest_df <- df %>% group_by(!!!gps) %>% tidyr::nest()
    nest_df[["data"]] <- map(nest_df[["data"]], munge_counts)
    nest_df
  }
  nested_df <- nest_and_munge(unnested_df, dx_year,!!!gps)
  fe <- ageadj_munge_df(nested_df) 
  
  rename_df <- function(df, nms) {
    names(df) <- nms
    df
  }
  
  tr <-
    bind_cols(nested_df[, -grep("data", names(nested_df))], fe) %>%
    select(-dx_year) %>%
    group_by(!!!gps) %>%
    #take the mean of all years' rates
    summarise_all( ~ (mean(.x, na.rm = TRUE))) %>%
    rename_df(., nms = c(gp_nms,
                         gsub(
                           "rate_", "", paste("aa_", names(fe), sep = "")
                         ))) %>%
    #rename because the crude rates are the same
    rename(aa_crude = aa_crude_00) %>% select(-aa_crude_10) %>%
    filter(algo %in% c("seer_bm_01", which_algo))
  
  cols_to_return <-
    c(gp_nms, "aa_crude", names(tr)[grep("_10$", names(tr))])
  
  tr[, cols_to_return]
}

#' Title
#'
#' @param aair_df 
#' @param gp_nms 
#'
#' @return
#' @export
#'
#' @examples
rates_fn <- function(aair_df, gp_nms, with_ci = TRUE, denominator = NULL) {
  if(missing(denominator)) {
    denominator <- 1e5
  }
  names(aair_df) <- c(gp_nms, "crude", "aa1", "alc", "auc")
  aair_df[, c("crude", "aa1", "alc", "auc")] <-
    lapply(aair_df[, c("crude", "aa1", "alc", "auc")], function(x) {
      sprintf("%.0f", x * denominator)
      })
  #lapply(aair_df[,c("aa1", "alc", "auc")], function(x) sprintf("%.1f", x * 1e5))
  if (with_ci) {
    aair_df[["AAIR"]] <-
      with(aair_df, paste(aa1, " (", alc, "-", auc, ")", sep = ""))
    #with(aair_df, paste(crude, "_", aa1, " (", alc, "-", auc, ")", sep = ""))
  } else {
    aair_df[["AAIR"]] <- aair_df[["aa1"]]
  }
  aair_df[, c("aa1", "alc", "auc")] <- NULL
  aair_df
}


#' Title
#'
#' @param a Things to find
#' @param b Things to replace
#' @param x Things
#'
#' @return
#' @export
#'
#' @examples
gsub_reduce <- function(a, b, x) {
  gsub_this <- function(init, a, b) gsub(a, b, init)
  reduce2(a, b, gsub_this, .init = x)
}


#' Make table histology labels look nicer
#'
#' @param histonames 
#'
#' @return
#' @export
#'
#' @importFrom purrr reduce2
#'
#' @examples
clean_table_histonames <- function(histonames) {
  reduce2(
    c(
      "Non-Small Cell Carcinoma", "Small Cell Carcinoma",
      "Squamous Cell Carcinoma", "([Cc]ar)(cinoma)",
      "Triple Negative", "Mal. Mel. In Junct. Nevus", ",\ Nos$" ),
    c("NSCLC", "SCLC", "Squamous", "\\1\\.", "Triple (-)", 
      "MMJN", ""),
    function(init, a, b) gsub(a, b, init), 
    .init = histonames
  )
}

relabel_ip <- function(show_counts_df, histo = FALSE) {
  gsub_this <- function(init, x, y) gsub(x, y, init)
  gsub_reduce <- function(a, b, z) reduce2(a, b, gsub_this, .init = z)
  
  scd <- show_counts_df
  names(show_counts_df) <-
    gsub_reduce(c("\\ ", "IP$"), c("_", "%"), names(show_counts_df))
  
  show_counts_df[, grepl("\\%", names(show_counts_df))] <-
    lapply(show_counts_df[, grepl("\\%", names(show_counts_df))],
           function(props) gsub("\\%", "", props))
  
  subcol_labels <-
    gsub_reduce(
      c("_?(Neg|Pos)_?", "m_group_total",
        "group_total", "_?Med\\._?",
        "(SBM|LBM)_?", "_?Mis_?"
      ),
      c("", "", "", "", "", ""),
      colnames(show_counts_df)
    )
  show_counts_df <- rbind(subcol_labels, show_counts_df)
  names(show_counts_df) <- gsub("\\%", "Pct", names(show_counts_df))
  
  ip_toreplace <- c(
    "m_group_total", "group_total",
    "Pct_Neg",       "Neg_Count",
    "SBM_Pct",       "Pos_Count",
    "Pct_Mis",       "Mis_Count",
    "Pct_Med._Neg",  "Neg_Med._Count",
    "LBM_Pct",       "LBM_Count",
    "Primary" )
  ip_replacements <- c(
    "Medicare", "SEER",
    "Absent",   "",
    "Present",  "",
    "Missing",  "",
    "Absent",   "",
    "Present",  "",
    "Site" )
  
  colnames(show_counts_df) <-
    gsub_reduce(ip_toreplace, ip_replacements, colnames(show_counts_df))
  
  if (histo) {
    show_counts_df[["Histology"]] <-
      gsub_reduce(
        c(
          "Non-Small Cell Carcinoma", "Small Cell Carcinoma",
          "Squamous Cell Carcinoma", "([Cc]ar)(cinoma)",
          "Triple Negative" ),
        c("NSCLC", "SCLC", "Squamous", "\\1\\.", "Triple (-)"),
        show_counts_df[["Histology"]]
      )
  }
  
  show_counts_df[1, 1:2] <- ""
  show_counts_df
}

#' Group a dataframe by `algorithm` and make a list whose names are the algo
#'
#' @param df A data frame containing an `algorithm` column
#'
#' @return A list of data.frames
#' @export
#' @importFrom dplyr %>% group_by
#' @examples
#' 
nest_list_by_algo <- function(df){
  df %>% 
    group_by(algorithm) %>% tidyr::nest() %>% nest_to_list()
}

bind_and_show <- function(df_list, histo = FALSE) {
  df <- bind_cols(df_list)
  if (histo) {
    df <- df %>% select(-m_XNA,-m_Primary,-m_Histology)
  } else {
    df <- df %>% select(-m_XNA,-m_Primary)
  }
  df %>%
    separate("Negative", c("% Neg", "Neg Count"), "\\*\\*") %>%
    separate("Positive", c("SBM_IP", "Pos Count"), "\\*\\*") %>%
    separate("XNA", c("% Mis", "Mis Count"), "\\*\\*") %>%
    separate("m_Negative", c("% Med. Neg", "Neg Med. Count"), "\\*\\*") %>%
    separate("m_Positive", c("LBM_IP", "LBM Count"), "\\*\\*")
}

spread_rename_ip <- function(df, histo = FALSE) {
  tr <- df %>%
    spread(algo_v, show) %>%
    map_df(function(words) {
      words <- gsub("\\ brain|\\ cns|\\,\\ nos", "", tolower(words))
      stringr::str_to_title(gsub("hronous|icare", ".", words))
    })
  if (histo) {
    return(tr %>% rename(
      Primary = which_cancer,
      XNA = `<NA>`,
      Histology = the_strata
    ))
  } else {
    return(tr %>% rename(Primary = which_cancer, XNA = `<NA>`))
  }
}

select_showvars <- function(df) {
  select(
    df,
    Site,
    Histology,
    SEER = SEER_SBM,
    Total = SEER,
    Present,
    Absent,
    Missing,
    Medicare = Medicare_LBM,
    Total1 = Medicare,
    Present1,
    Absent1
  )
}

reorder_primary <- function(df) {
  df %>%
    modify_at("Primary",  function(x)
      factor(x, levels = c("skin", "lung", "breast"))) %>%
    arrange(Primary) %>%
    modify_at("Primary", as.character)
}


#' Stratum-specific classification data.frame cleaner
#'
#' @param df 
#' @export
clean_strat_class <- function(df) {
  df %>% 
    select(which_cancer, the_strata, measure, predicted_positives, actual_positives, 
           sensitivity, PPV, specificity, kappa, kappa_lci, kappa_hci) %>% 
    modify_at(c("sensitivity", "PPV", "specificity", "kappa", "kappa_lci", "kappa_hci"), 
              ~ sprintf("%.2f", .x)) %>% 
    modify_at("which_cancer", function(z) factor(z, levels = c("lung", "breast", "skin"))) %>% 
    arrange(which_cancer, the_strata) %>% 
    modify_at("which_cancer", compose(perma_dupes, str_to_title)) %>% 
    (function(df) {
      for (i in seq_along(df[["the_strata"]])) {
        if (i > 1) {
          if (df[["the_strata"]][i] == df[["the_strata"]][i - 1]) {
            df[["the_strata"]][i] <- ""
          }
        }
       }
      names(df)[1:8] <- 
        c("Primary",   "", 
          "Algorithm", "Predicted", 
          "True",  "Sens.", "PPV", "Specificity")
      df[["Kappa"]] <- 
        with(df, paste(kappa, paste("(", kappa_lci, " - ", kappa_hci, ")", sep = "")))
      df[,c("kappa", "kappa_lci", "kappa_hci")] <- NULL
      df
    })
}


#' Intended for use with 'papes$strata_only'
#'
#' @param df 
#'
#' @return
#' @export
clean_crude_sbm <- function(df) {
  df %>% 
    spread(algo_value, cnt) %>% 
    rename(Present = `1`, Absent = `0`, Missing = `<NA>`) %>%
    filter(algo == "seer_bm_01") %>% 
    #divide each by three because we have three years' time, and we want per-annum values
    # I know these things are mathematically equivalent, but...it's what I said I would do
    #PPA = present per annum
    mutate(PPA = Present / 3, APA = Absent / 3, MPA = Missing / 3) %>% 
    mutate(IR = 1e4 * (PPA / (APA + MPA + PPA))) %>% 
    #IPP = incidence proportion positive
    mutate(IPP = 100 * (Present / (Present + Absent + Missing)), 
           IPA = 100 * (Absent / (Present + Absent + Missing)),
           IPM = 100 * (Missing / (Present + Absent + Missing))) %>% 
    mutate(IRS = paste("*", sprintf("%.1f", IR)))
}


#' Given an integer vector, hyphenate consecutive runs and paste into a char
#'
#' @param vctr 
#'
#' @return
#' @export
rle_hyphenate <- function(vctr) {
  diff_1 <- function(x) as.numeric(diff(as.integer(as.character(x))) != 1)
  rle_diff <- function(x) rle(cumsum(c(0, diff_1(x))))[["lengths"]]
  consecs <- rle_diff(vctr)
  hyphenates <- list()
  vct_position <- 0
  for (i in seq_along(consecs)) {
    if (consecs[i] == 1) {
      vct_position <- vct_position + 1
      hyphenates[[i]] <- vctr[vct_position]
    } else {
      the_first <- vctr[vct_position + 1]
      the_last <- vctr[vct_position + consecs[i]]
      vct_position <- vct_position + consecs[i]
      hyphenates[[i]] <- paste(the_first, "-", the_last, sep = "")
    }
  }
  paste(hyphenates, collapse = ", ")
}

hyphenate_histocodes <- function(df) {
  df[["hist03v"]] <- as.numeric(as.character(df[["hist03v"]]))
  df <- arrange(df, hist03v)
  biggest <- max(df[["hist03v"]], na.rm = TRUE)
  smallest <- min(df[["hist03v"]], na.rm = TRUE)
  if (biggest == (smallest + nrow(df) - 1) &
      nrow(df) > 1) {
    return(paste(smallest, biggest, sep = "-"))
  } else {
    rle_hyphenate(df[["hist03v"]])
  }
}


bind_ct_tables <- function(a_list) {
  reduce(a_list, bind_rows) %>% rm_second_name()  
}

prepare_classification_selector <- 
  function(histo_df) {
    function(msr) {
      histo_df %>% 
        filter(measure %in% c(msr)) %>% 
        clean_strat_class() %>% 
        add_second_name() %>% 
        select(-Algorithm) %>% 
        modify_at(c("Predicted", "True"), censor_few) %>% 
        modify(as.character) %>% 
        modify_at("x", clean_table_histonames)
    }
}


prepare_histo_key <- function(histo_key_df) {
  fix_mel_names <- 
    function(z) tolower(gsub("Mal\\.\\ Mel\\.\\ In\\ Junct\\.\\ Nevus", 
                             "malignant Melanoma in junctional nevus", z))
  rm_nos <- function(z) str_to_title(gsub("\\,\\ NOS$", "", z))
  shorten_car <- function(z) gsub("ca\\.$", "carcinoma", z)
  histo_key_df <- 
    histo_key_df %>% 
    rename(histo = hist03v_v) %>% 
      select(-Freq) %>% 
      # modify_at("histo", rm_nos) %>% 
      # modify_at("histo", fix_mel_names) %>% 
      modify_at("histo", compose(shorten_car, fix_mel_names, rm_nos)) %>% 
      group_by(which_cancer, histo) %>% 
      nest()
  
  #I don't need to do this biggest/smallest stuff...I need to remove it
  histo_key_df[["codes"]] <-
    map_chr(histo_key_df[["data"]], hyphenate_histocodes)
  
  histo_key_df
}

censor_few <- function(z) {
  ifelse(z <= 11 & z != 0, "*", paste(z, "   "))
}

add_ci <- function(kappa_calc) {
  strings <- unlist(strsplit(kappa_calc, split = "\\("))
  paste(strings[[1]], "(95% CI:", strings[[2]], collapse = "")
}

new_ct_row <- function(lbl) {
  data.frame(
    Primary = "", 
    x = lbl, 
    Predicted = "", 
    True = "", 
    Sens. = "", 
    PPV = "", 
    Specificity = "",
    Kappa = "", 
    stringsAsFactors = FALSE
  ) 
}

add_second_name <- function(a_df) {
  names(a_df)[2] <- "x"
  a_df
}

rm_second_name <- function(a_df) {
  names(a_df)[2] <- ""
  a_df
}

in_text_cm <-   function(cm_df) {
  # Separate out the confidence intervals
  cm_df %>% 
  (function(df) {
    vls <- strsplit(df[["Kappa"]], split = "[^0-9.]")
    df["kli"] <- map_chr(vls, 3)
    df["kui"] <- map_chr(vls, 6)
    df[["Kappa"]] <- map_chr(vls, 1) 
    df
  }) %>% 
  # Fill the columns back up (after perma_dupes)
  modify_at(c("Timing", "Primary_Cancer"), 
            function(vctr) {
              for (i in seq_along(vctr)) {
                if (vctr[i] == "") {
                  vctr[i] <- vctr[i - 1]
                }
              }
              vctr
            }) %>% 
  rename(the_time = Timing, the_cancer = Primary_Cancer, codes = Claims_code) %>% 
  # Make code algorithm names easier to reference in function
  modify_at("codes", function(vctr){
    vctr <- as.character(vctr)
    vctr[vctr == "CNS Metastasis"] <- "dx"
    vctr[vctr == "CNS Metastasis w/Diagnostic Imaging"] <- "dximg"
    vctr
  }) %>%  back_up("zzz2") %>% 
  # Make code algorithm names easier to reference in function
  modify_at("the_time", function(vctr) {
    vctr[vctr == "Synchronous"] <- "sync"
    vctr[vctr == "Lifetime"] <- "life"
    vctr
  }) %>% 
  # Format for printing
  modify_at(c("Sensitivity", "PPV", "Kappa", "kli", "kui"), 
            function(thing) sprintf("%.2f", as.numeric(thing)))
}

intxt_cm_fn_fctry <- function(cm_df) {
  # Make function for in-text classification metrics reference 
  function(which_cancer, timing, the_codes){
    cm_df[with(cm_df,
                 the_time == timing & 
                 the_cancer == which_cancer & 
                 codes == the_codes),
          ]
  }
}

munge_ip_for_intext <- function(ip_aair_df) {
  ipa <- ip_aair_df
  
  names(ipa) <- 
    c("which_cancer", "histo", "sbm_aair", "sbm_cnt", "sbm_ip", 
      "sbm_neg", "sbm_na", "lbm_aair", "lbm_cnt", "lbm_ip", "lbm_neg")
  
  ipa <- ipa %>% slice(-1) %>% 
    modify_at( c("sbm_aair", "lbm_aair"), 
               function(aair_vct) gsub("\\)", "", aair_vct)) %>% 
    modify_at(c("which_cancer", "histo"), tolower) %>% 
    modify_at("histo", function(x) gsub("(\\(|\\/).*", "", x))
  
  ipa <- 
    reduce(    
          list(
              list("sbm_aair", c("sbm_aair", "sbm_ci"), "\\ \\("), 
              list("sbm_ci",   c("sbm_lci", "sbm_hci"), "-"), 
              list("lbm_aair", c("lbm_aair", "lbm_ci"), "\\ \\("), 
              list("lbm_ci",   c("lbm_lci", "lbm_hci"),  "-")
              ), 
           function(df, rs) {
             #separate this, separate into, separate by
             c(to_s, in2, s_by) %<-% rs
             separate(df, to_s, in2, s_by, extra = "drop", fill = "left")
           }, 
           .init = ipa
          )
  
  gather(ipa, msr, val, -which_cancer, -histo) %>% 
    modify_at("histo", function(z) gsub("triple\\ ", "triple", z)) %>% 
    modify_at("histo", function(z) gsub("\\ |\\&", "", z)) %>% 
    separate(msr, c("which", "msr"), sep = "_") %>% 
    filter(histo != "") %>% 
    spread(which, val) %>% 
    gather(timing, value, -which_cancer, -histo, -msr) %>% 
    spread(msr, value) %>% 
    data.frame(stringsAsFactors = FALSE) %>% 
    modify_at("ip", ~ sprintf(fmt = "%.1f", quietly(as.numeric)(.x)[["result"]]))
}

histo_key_fn_fctry <- function(histo_key_df) {
  histo_key_df <- prepare_histo_key(histo_key_df)
  function(cnc, hst) {
    if (is.numeric(hst)) {
      which_hst <- hst
    } else if (is.character(hst)) {
      which_hst <-
        grep(hst, histo_key_df$histo[histo_key_df$which_cancer == cnc])
    }
    hst <- histo_key_df$histo[histo_key_df$which_cancer == cnc][which_hst]
    with(histo_key_df,
         list(h = hst, c = codes[which_cancer == cnc & histo == hst]))
  }
}

ip_aair_names_fn <- function(vctr) {
  reduce2(c("1$", "SEER",     "Medicare",     
            #"Present", "Absent", 
            "Missing", "Total"), 
          c("",   "SEER SBM", "Medicare LBM", 
            #"(+)", "\\(-)",  
            "N/A", "At risk"), 
          function(init, x, y) gsub(x, y, init),
          .init = vctr)
}

aair <- function(aair_df, gp_nms, ...) {
  dots_nms <- deparse(substitute(...))
  #browser()
  # if (rlang::dots_n(...) > 0) {
  if (any("the_strata" %in% dots_nms)){ 
    selector <- function(p_df) {
      select(p_df,
             Primary, Stratum, 
             SEER_SBM = seer_bm_01, 
             Medicare_LBM = which_algo_to_use)
    }
  } else if ("tnm6" %in% dots_nms) {
    selector <- function(p_df) {
      select(p_df,
             Primary, Stratum, TNM6 = tnm6,
             SEER_SBM = seer_bm_01, 
             Medicare_LBM = which_algo_to_use)
    }
  } else {
    selector <- function(p_df) {
      select(p_df,
             Primary,
             SEER_SBM = seer_bm_01, 
             Medicare_LBM = which_algo_to_use)
    }
  }
  rtrn <- 
    filter(aair_df, !is.na(cnt)) %>% 
      aair_by(which_algo_to_use, algo, which_cancer, ...) %>% 
      rates_fn(gp_nms = gp_nms) %>% 
      ungroup() %>% 
      select(-crude) %>% 
      spread(Algorithm, AAIR) %>% 
      reorder_primary()
  
  selector(rtrn)
}

rm_rows_finder <- function(the_table_ones) {
  grep_it <- function(cncr) {
    function(a, z) c(a, grep(z, the_table_ones[[cncr]][["Variable"]]))
  }
  list(
    breast_rows_to_rm =   
      reduce(c("llt",     "other", "ulf", "icd_c", "HER2",
               "cs_mets", "eod10_pn", "beh03v"), 
             grep_it("breast"), 
             .init = c()), 
    lung_rows_to_rm =   
      reduce( c("brst_sub", "PR",       "HER2",  "ER",  
                "her2_v",   "llt",      "other", "ulf", 
                "icd_c",    "status",   "csmetsdxlung_pub_v", 
                "cs_mets",  "eod10_pn", "beh03v"), 
              grep_it("lung"), 
              .init = c()), 
    skin_rows_to_rm =
      reduce(c("brst_sub", "PR",       "HER2",  "ER",  
               "her2_v",   "llt",      "other", "ulf", 
               "icd_c",    "status",   "csmetsdxlung_pub_v", 
               "cs_mets",  "eod10_pn", "beh03v"), 
             grep_it("skin"), 
             .init = c())
  )  
}

breast_renamer <- function(the_table, ptrn, replacement) {
  the_table[["Variable"]] <- 
    ifelse(grepl(ptrn, the_table[["Variable"]]), 
           replacement, the_table[["Variable"]])
  the_table
}

ipa_shower <- function(ipa_df) {
  function(cncr, hsto, times) {
    ipa_df[with(ipa_df, 
                which_cancer == cncr & 
                  histo == hsto & 
                  timing == times),
           ]
  }
}

t_o_fn <- function(df){
  #names reflect: Variable, Overall, SEER SBM, Medicare LBM
  names(df) <- c("v", "o", "s", "m")
  qs <- function(df, x, y) {
    quietly(separate
    )(data = df, col = x, into = y, 
      sep = "\\ \\(\\ {0,4}")[["result"]]
  }
  df <- 
    reduce2(c("o", "s", "m"), 
            list(c("on", "op"), c("sn", "sp"), c("mn", "mp")), 
            qs, .init = df) 
  to_fx <- c("op", "sp", "mp")
  df[,to_fx] <- 
    lapply(df[,to_fx], function(z) gsub("\\)", "", z))
  df
}






