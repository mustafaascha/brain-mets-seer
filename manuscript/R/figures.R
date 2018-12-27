

gather_not <- function(a_df, not) {
  gps <- rlang::syms(setdiff(names(a_df), not))
  gather(a_df, k, !!not, -c(!!!gps)) %>% 
    select(-k)
}

stack_new_values <- function(a_df, new_col_name, new_values) {
  reduce(new_values,
         function(df, n_v) {
           n_df <- df
           n_df[[new_col_name]] <- n_v
           if (!(new_col_name %in% names(df))) return(n_df)
           else bind_rows(df, n_df)
         },
         .init = a_df) %>% 
    filter(!is.na(!!rlang::sym(new_col_name)))
}

cnt_df_fn_fctry <- function(count_dfs) {
  function(vr, nm) {
    mutate(count_dfs[[vr]], measure = nm) %>% 
      rename(Present = !!sym(vr)) %>% 
      modify_if(sorta_char, as.character)
  }
}

arrange_plots <- function(...) {
  cowplot::plot_grid(..., labels = c("a.", "b."), label_fontface = "plain")
}

make_plot_ips <- function(df) {
  df <- 
    df %>% 
    group_by(Present) %>% 
    summarise(Freq = sum(Freq)) %>% 
    spread(Present, Freq, fill = 0)
  if(length(df) == 1) {
    df[["1"]] <- 0
  }
  names(df) <- c("absent", "present")
  df[["total"]] <- df[["absent"]] + df[["present"]]
  df[["IP"]] <- df[["present"]] / df[["total"]]
  df[["IP"]] <- ifelse(df[["present"]] < 11, 0, df[["IP"]])
  #df[,c("absent", "present", "total")] <- NULL
  df[,c("absent", "total")] <- NULL
  df
}

recode_strata <- function(x){
  forcats::fct_recode(gsub("\\, Nos", "", as.character(x)), 
                      NSCLC = "Non-Small Cell Carcinoma", 
                      SCLC  = "Small Cell Carcinoma", 
                      `Squamous CC` = "Squamous Cell Carcinoma")
}

make_by_sex <- function(plot_df){
  by_sex <- 
    group_by(plot_df, which_cancer, s_sex_v, the_strata, measure) %>% 
    tidyr::nest() 
  by_sex[["data"]] <- map(by_sex[["data"]], make_plot_ips)
  by_sex <- unnest(by_sex)
  by_sex[["the_strata"]] <- recode_strata(by_sex[["the_strata"]])
  by_sex
}

make_by_race <- function(plot_df) {
  by_race <- 
    group_by(plot_df, which_cancer, race_v, the_strata, measure) %>% 
    tidyr::nest() 
  by_race[["data"]] <- map(by_race[["data"]], make_plot_ips)
  by_race <- unnest(by_race)
  
  lc_histos <- 
    c("Non-Small Cell Carcinoma", 
      "Small Cell Carcinoma", 
      "Squamous Cell Carcinoma")
  
  #if(all(lc_histos %in% as.character(by_race[["the_strata"]]))) {
    by_race[["the_strata"]] <- 
      forcats::fct_recode(gsub("\\, Nos", "", as.character(by_race[["the_strata"]])), 
                          NSCLC = "Non-Small Cell Carcinoma", 
                          SCLC  = "Small Cell Carcinoma", 
                          `Squamous CC` = "Squamous Cell Carcinoma")
  #}
  by_race[["race_v"]] <- 
    forcats::fct_recode(as.character(by_race[["race_v"]]), 
                        AI = "American Indian",
                        WNH = "White Non-Hispanic", 
                        WH  = "White Hispanic", 
                        API = "Asian/Pacific Islander")
  by_race
}

gp_cncr <- function(a_df) {
  nest_to_list(tidyr::nest(dplyr::group_by(a_df, which_cancer)))
}

make_text_y <- function(df, seer, medicare) {
  mutate(df, 
         text_y = ifelse(measure == "SEER SBM", seer, medicare))
}

plot_lung_df <- function(dfs, seer_y, medicare_y) {
  lvls <- 
    c(
      "Squamous CC", 
      "Carcinoma", 
      "Adenocarcinoma",  
      "NSCLC", 
      "SCLC"
    ) %>% rev()
  
  dfs[["lung"]] %>% 
    make_text_y(seer = seer_y, medicare = medicare_y) %>% 
    modify_at("present", censor_few) %>% 
    modify_at("the_strata", 
              ~ factor(.x, levels = lvls))
}

vctr_or <- function(vctr, fn_1, fn_2) {
  fn_1(vctr) | fn_2(vctr)
}

sorta_char <- function(a_vctr) {
  vctr_or(a_vctr, is.character, is.factor)
}

add_estimator_name <- function(estimator_dfs) {
  function(vr, nm) {
    mutate(estimator_dfs[[vr]], measure = nm) %>% 
      rename(Present = !!sym(vr)) %>% 
      modify_if(sorta_char)
  }
}

label_this <- function(this, that) {
  labs(y = "Incidence Proportion (%)", x = "", 
       title = paste(this, "cancers"), 
       fill = that)
}

bind_relevel_filter_pdf <- function(algos_list) {
  reduce(algos_list, compose(tbl_df, bind_rows)) %>% 
    modify_at("measure", ~ factor(.x, levels = c("SEER SBM", "Medicare LBM"))) %>% 
    filter(race_v != "Other" & the_strata != "Other")
}

plot_skin_df <- function(a_df, seer_pos, medi_pos) {
  make_text_y(a_df, seer_pos, medi_pos) %>% 
    modify_at("the_strata", 
              ~ factor(.x, levels = c("Nevi & Melanomas", 
                                      "Mal. Mel. In Junct. Nevus"))) %>% 
    modify_at("the_strata",
              ~ forcats::fct_recode(.x,
                                    `MMJN` = "Mal. Mel. In Junct. Nevus",
                                    `N & M` = "Nevi & Melanomas"
              )) %>% 
    modify_at("the_strata", ~ factor(.x, levels = c("MMJN", "N & M"))) %>% 
    modify_at("present", censor_few)
}

plot_breast_df <- function(a_df, seer_pos, medi_pos) {
  filter(a_df, the_strata != "Duct Carcinoma") %>% 
    make_text_y(seer_pos, medi_pos) %>% 
    modify_at("the_strata", ~ factor(.x, levels = c("Her2-/Hr+",  "Her2+/ Hr(+/-)", 
                                                    "Triple Negative"))) %>% 
    modify_at("present", censor_few)
}

bp_this <- function(df, the_fill, sz = 4, ...) {
  the_fill <- enquo(the_fill)
  ggplot(df, aes(x = the_strata, y = IP * 100, 
                 fill = eval_tidy(the_fill, data = df))) + 
    facet_wrap(~ measure) + #, scales = "free_x") + 
    geom_bar(stat = "identity", position = position_dodge(), color = "black")
}

lines_df <- function(df, xs, y) {
  df[["ws"]] <- 
    map2(df[[y]], seq(nrow(df)) + 3, ~ seq(0, max(.x), length.out = .y / 5))
  unnest(df) %>% distinct()
}

