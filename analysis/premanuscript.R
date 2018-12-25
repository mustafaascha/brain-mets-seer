

if (!exists("cancers")) {
  library(tidyverse)
  library(tableone)
  library(zeallot)
  library(devtools)
  devtools::load_all("augur")
  devtools::load_all("manuscript")

  paper_products <- list()

  source("analysis/load.R")

  #-------------------------------------------------

  cancers_1013 <- 
    cancers %>% 
    filter(dx_year >= 2010 & 
          (which_cancer != "breast" | brst_sub_v != "Not 2010+ Breast"))

  #these are from the "load-exclude" script
  paper_products[["exclusion"]] <- paper_stuff[[2]]

  measure_vars <-  
    c(
      "medicare_60_prim_dx_matches",        
      "medicare_60_prim_dx_img",
      "medicare_60_dx_img",
      "medicare_00_dx_dx"
    ) 

  measure_labels <- 
    gsub("^medicare_", "", unlist(map(measure_vars, ~ rep_len(.x, 3))))

  paper_products[["classifimetry"]] <- 
    map_df(measure_vars,
           partial(primary_classifimeter, df = cancers_1013)) %>% 
    (function(df) {
      ts <- mutate(df, msr = measure_labels)
      ts <- bind_cols(ts[,c("which_cancer", "msr")], 
                      bind_rows(ts[["classification_metrics"]]))
      paper_products[["all_classifimetry"]] <<- ts
      df 
    }) %>% 
    clean_paper_metrics(df_labels = measure_labels)

  no_a_c <- 
    filter(cancers_1013, 
           !grepl("autopsy", radiatn_v))

  #by histo classification============================================
  paper_products[["histo_metrics"]] <- 
    map(measure_vars, 
      function(which_measure){
        strat_dfs <- 
          cancers_1013 %>% 
            group_by(which_cancer, the_strata) %>% 
            tidyr::nest()
        strat_fns <- 
            map(strat_dfs$data, 
              ~ partial(strat_classifimeter, df = .x, gold_std = "seer_bm_01"))
        strat_dfs <- 
            bind_cols(strat_dfs, map_df(strat_fns, function(f) f(which_measure)))
        strat_dfs <- strat_dfs %>% select(-data)
        strat_dfs 
      }
    ) %>% 
    map2_df(measure_vars, function(df, mv) mutate(df, measure = mv))

  #annum counts=========================================

  class_vrs <- 
    c("seer_bm_01", "medicare_60_prim_dx_matches", 
      "medicare_60_dx_img", "medicare_00_dx_dx", 
      "medicare_60_prim_dx_img")

  cncrs_cnt <- partial(gp_count, df = cancers)

  map_cv <- partial(map_dfr, .x = class_vrs)

}
#cancers[["tnm"]] <- cancers[["d_ssg00"]]

paper_products[["histo_annum"]] <- 
  map_cv(~ cncrs_cnt(vr = .x, dx_year, the_strata))

paper_products[["ss_primary"]] <- 
  with(cancers, table(which_cancer, tnm6, medicare_60_dx_img)) %>% 
  data.frame()

paper_products[["ss_histo_annum"]] <- 
  map_cv(~ cncrs_cnt(vr = .x, dx_year, the_strata, tnm6))

paper_products[["ss_histo_age_over_time"]] <- 
  map_cv(~ cncrs_cnt(vr = .x, age_cut, dx_year, the_strata, tnm6))

paper_products[["d_ss_histo_annum"]] <- 
  map_cv(~ cncrs_cnt(vr = .x, dx_year, the_strata, d_ssg00))

paper_products[["d_ss"]] <- 
  map_cv(~ cncrs_cnt(vr = .x, d_ssg00))

paper_products[["d_ss_table"]] <- 
  with(cancers, table(which_cancer, d_ssg00, medicare_60_dx_img)) %>% 
  data.frame()

paper_products[["d_ss_histo_age_over_time"]] <- 
  map_cv(~ cncrs_cnt(vr = .x, age_cut, dx_year, the_strata, d_ssg00))

paper_products[["age_over_time"]] <- 
  map_cv(~ cncrs_cnt(vr = .x, age_cut, dx_year))

paper_products[["histo_age_over_time"]] <- 
  map_cv(~ cncrs_cnt(vr = .x, age_cut, the_strata, dx_year))

paper_products[["strata_only"]] <- 
  map_cv(~ cncrs_cnt(vr = .x, the_strata))

#table ones===============================================

varnames <- 
  list(demos = c("age_dx", #"age_cut", 
                  "race_v"),
       primary = c("histo", 
                   "beh03v",
                   "cs_size", "d_ssg00",
                   "cs_mets",
                   "eod10_pn",
                   "csmetsdxb_pub_v", "csmetsdxliv_pub_v",   
                   "csmetsdxlung_pub_v"#, 
                   ),
       #breast: histo, tumor size/grade/stage/tnm
       breast = c("her2_v", "prstatus_v", "erstatus_v", 
                  "brst_sub_v"))
       #consider excluding carcinoids? see goncalves 2016

to_numeric <- c("age_dx", "cs_size", "eod10_pn", "cs_mets")
cancers[,to_numeric] <- lapply(cancers[,to_numeric], as.numeric)

#I should do synchronous and lifetime....duh! Not SEER vs Medicare! 
the_strata <- 
  c("medicare_60_dx_img",
    "seer_br_mets", "seer_medicare", "default")
tbl_one_vrs <- with(varnames, c(demos, primary, breast))

grouped <- cancers_1013 %>% group_by(which_cancer) %>% tidyr::nest()

tbl_strat <- function(strat) { 
  tbl_one_vars <- with(varnames, c(demos, primary))
  to_return <- map(grouped$data, function(gp_df) tbl_one(gp_df, tbl_one_vrs, strat))
  names(to_return) <- grouped$which_cancer 
  to_return 
} 

table_ones <- map(the_strata, tbl_strat) 
names(table_ones) <- the_strata

table_ones <- purrr::transpose(table_ones)

paper_products[["table_ones"]] <- table_ones

library(lubridate) 

paper_products <-
  modify_if(paper_products, is.data.frame,
            ~ as.data.frame(ungroup(.x)), stringsAsFactors = FALSE)

paper_products[["histo_key"]] <-
  with(cancers, table(hist03v, hist03v_v, which_cancer)) %>%
  data.frame %>%
  filter(Freq != 0 & hist03v_v %in% toupper(unique(cancers$histo))) %>%
  arrange(desc(Freq))

paper_products[["age_sex_race_strat"]] <-
  map(c("seer_bm_01", measure_vars, "imaging"),
      function(msr) {
        if(msr == "seer_bm_01") {
          the_df <- cancers_1013
        }
        else {
          the_df <- cancers 
        }
        tv <- c("age_cut", "which_cancer", "race_v",
                "s_sex_v", "the_strata", msr)
        filter(data.frame(table(the_df[,tv])), Freq != 0)
  })

names(paper_products[["age_sex_race_strat"]]) <-
  c("seer_bm_01", measure_vars)

paper_products[["pop_counts"]] <-
  map(list(cancers, cancers_1013, cancers_full),
      ~ table(.x[,c("which_cancer", "dx_year")]))

names(paper_products[["pop_counts"]]) <- c("selected", "past_ten", "full")

write_rds(paper_products, "paper_products.rds")
