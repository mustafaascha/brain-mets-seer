#preferably for pure functions, impure functions are included in analysis scripts

classifimeter <- function(test_measure, gold_std, test_pos, gold_pos){
  c(tm, gs) %<-% list(ifelse(test_measure == test_pos, 1, 
                        ifelse(is.na(test_measure), NA, 0)), 
                      ifelse(gold_std == gold_pos, 1, 
                        ifelse(is.na(gold_std), NA, 0)))
  ct <- class_table <- addmargins(table(tm, gs))
  kvpv <- irr::kappa2(data.frame(tm, gs))[c("value", "p.value")]
  psy_ck <- psych::cohen.kappa(cbind(tm, gs))
  c(a, b, c, d) %<-% list(ct[2,2], ct[2,1], ct[1,2], ct[1,1]) 
  #browser()
  #         Actual    # Table str:  #a = 2, 2 
  #        1     0    #    0    1   #b = 2, 1
  #Pred 1  a     b    # 0  d    c   #c = 1, 2
  #     0  c     d    # 1  b    a   #d = 1, 1
  list(true_positives = a,
       predicted_positives = a + b, 
       actual_positives = a + c, 
       sensitivity = a / (a + c), 
       true_negatives = d,
       predicted_negatives = c + d, 
       actual_negatives = b + d, 
       specificity = d / (b + d), 
       PPV = a / (a + b), 
       NPV = d / (c + d), 
       accuracy = (a + d) / (a + b + c + d),
       kappa = kvpv[["value"]],
       kappa_p = kvpv[["p.value"]], 
       kappa_lci = psy_ck[["confid"]]["unweighted kappa", "lower"],
       kappa_hci = psy_ck[["confid"]]["unweighted kappa", "upper"]
       )
}

strat_classifimeter <- function(df, test_measure, gold_std){
  classifimeter(df[[test_measure]], df[[gold_std]], 1, 1)
}

paper_classifimeter <- function(df, test_msre){
  classifimeter(df[[test_msre]],
                df[["seer_br_mets"]], 
                1, "SEER_Positive")
}

primary_classifimeter <- function(df, test_msre, classifimeter){
  gc <- grouped_cancers <- df %>% group_by(which_cancer) %>% nest   
  gc[["classification_metrics"]] <-                                      
    map(gc[["data"]], paper_classifimeter, test_msre = test_msre)        
  gc[["data"]] <- NULL                                                   
  gc   
}


clean_paper_metrics <- function(metrics_df, df_labels) {                                       
                                                                                               
  metrics_df[["which_measure"]] <- df_labels                                                   
  metrics_df <- metrics_df[,c(ncol(metrics_df), 1:(ncol(metrics_df) - 1))]                     
  metrics_df <-                                                                                
    bind_cols(metrics_df, map_df(metrics_df$classification_metrics, data.frame))               
                                                                                               
  rounder <- function(x) sprintf("%.4f", x)                                                  
  to_round <-                                                                              
    c("sensitivity", "specificity", "PPV", "NPV", 
      "kappa", "kappa_p", "kappa_lci", "kappa_hci") 
  metrics_df[,to_round] <- lapply(metrics_df[,to_round], rounder)                    
  metrics_df <- metrics_df[,-grep("classification", names(metrics_df))]                        
                                                                                               
  metrics_df[["code_within"]] <-                                                               
    paste(grep_raw(metrics_df[["which_measure"]], "^[0-9]+"), "days")                          
  metrics_df[["start_event"]] <-                                                               
    map_chr(strsplit(metrics_df[["which_measure"]], "_"), 2)                                   
  metrics_df[["end_event"]] <-                                                                 
    gsub("NA", "",                                                                             
      map_chr(strsplit(metrics_df[["which_measure"]], "_"),                                    
            function(x) paste(x[3], x[4])))                                                    
  metrics_df[["which_measure"]] <- NULL                                                        
  metrics_df$event <- with(metrics_df, paste(start_event, end_event))                          
                                                                                               
  metrics_df$code_within[metrics_df$code_within == "00 days"] <- "Anytime"                     
                                                                                               
  f_relevel <- function(fct, old_levels, new_levels){                                          
    fct <- as.character(fct)                                                                   
    for(i in seq_along(old_levels)){ 
      fct[fct == old_levels[i]] <- new_levels[i]
    }      
    fct         
  }                                                                                            
  old_lev <- sort(unique(metrics_df$event))                                                    
  new_lev <- c("CNS", "CNS + imaging", "CNS + imaging", "CNS")                                 
  metrics_df$claims_code <- f_relevel(metrics_df$event, old_lev, new_lev)                      
  metrics_df$synchronous <-                                                                    
       ifelse(metrics_df$start_event == "prim", "Synchronous", "Lifetime")
  metrics_df[["kappa"]] <- 
    with(metrics_df, 
         paste(kappa, paste("(", kappa_lci, " - ", kappa_hci, ")", 
                            sep = "")))
  metrics_df <-                                                                                
    metrics_df %>%                                                                             
    select(which_cancer,                                                                       
          synchronous, claims_code,                                                            
          #start_event, end_event, code_within,                                                
          medicare_count = predicted_positives,                                                
          seer_count = actual_positives,                                                       
          sensitivity, PPV, specificity, kappa) %>% 
    arrange(which_cancer)                                                                      
                                                                                               
  metrics_df                                                                                   
                                                                                               
}                         


auto_list <- function(...){
  the_dl <- rlang::dots_list(...)
  names(the_dl) <- map_chr(quos(...), quo_name)
  the_dl
}

switch_levels <- function(x) {
  ifelse(x == "Code Present", "Positive", 
         ifelse(x == "Not Present", "Negative", 
                NA))
}

count_by_year <- function(df, gp_vr) { 
  gp_vr <- enquo(gp_vr)
  df %>% 
    group_by(dx_year, seer_br_mets, !!gp_vr, which_cancer) %>% 
    summarise(counts = n()) %>% spread(seer_br_mets, counts) %>%
    arrange(desc(which_cancer))
}


tbl_one <- function(df, vrs, strt){
  tbl_fn <- function(...) {
    print(CreateTableOne(vars = vrs, data = df, ...), printToggle = FALSE)
  }
  if (strt == 'default') { 
    tbl_fn()
  } else { 
    tbl_fn(strata = strt)
  }
}

table_against_seer <- function(df, to_table){
  tt <- to_table <- enquo(to_table)
  df %>% 
  group_by(which_cancer, csmetsdxbr_pub_v, !!tt) %>% 
  summarise(count = n())
}

img_dx_test <- function(df, n_days){                                  
  c(dx_nm, img_nm) %<-%                                               
    list(paste0("medicare_", n_days, "_prim_dx_matches", collapse = ""),
         paste0("medicare_", n_days, "_prim_cpt_img", collapse = ""))     
  as.numeric(((df[[dx_nm]] + df[[img_nm]]) == 2))
}                                                                     





table_dx_screen <- function(df, n_days) {                                          
  vr_fn <- function(vr) paste0("medicare_", n_days, vr)                            
  nm_fn <- function(nm) paste("Medicare", n_days, "days", nm)                      
  c(dx_var, sc_var) %<-% lapply(c("_prim_dx_matches", "_prim_cpt_img"), vr_fn)     
  c(dx_nm, sc_nm) %<-% lapply(c("diagnosis", "screening"), nm_fn)                  
  c(df[[dx_nm]], df[[sc_nm]]) %<-% list(df[[dx_var]], df[[sc_var]])                
  df[["SEER Synchronous"]] <- df[["seer_bm_01"]]                                   
  #list(vr_fn, nm_fn, dx_var, sc_var, dx_nm, sc_nm)                                
  medicare_table <- table(df[[sc_nm]], df[[dx_nm]])                                
  dimnames(medicare_table) <-                                                      
    list(c(sc_nm, "No imaging"), c(dx_nm, "No diagnosis"))                         
  seer_table <- table(df[["SEER Synchronous"]], df[[sc_nm]])                       
  dimnames(seer_table) <-                                                          
    list(c(sc_nm, "No imaging"),                                                   
         c("SEER Synchronous diagnosis", "No diagnosis"))                          
  list(medicare_table = medicare_table,                                            
       medicare_prop_table = prop.table(medicare_table, margin = 2),               
       seer_table = seer_table,                                                    
       seer_prop_table = prop.table(seer_table, margin = 2))                       
}                                                                                  
                                                                                   
counts_tables <- function(df) {                                        
  dx_screening <- pryr::partial(table_dx_screen, df = df)             
  screening_days <- c(30, 60, 90)                                     
  dx_screen_tables <- lapply(screening_days, dx_screening)            
  names(dx_screen_tables) <- paste0("Screen_", screening_days)        
  dx_screen_tables                                                    
}                                                                     
                                                                      
                                                                      
get_html_table <- function(the_url, ...) {                            
  the_site <- xml2::read_html(the_url)                                
  the_html_table <- rvest::html_table(the_site, header = TRUE, ...)   
}                                                                     
                                                                      



