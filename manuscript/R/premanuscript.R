


gsub_reduce <- function(initial_thing, to_replacexs, replacements){
  reduce2(to_replacexs, replacements,                              
        function(char_vector, to_replace, replacement){            
          gsub(to_replace, replacement, char_vector, perl = TRUE)  
        },                                                         
        .init = initial_thing)                                     
}                                                                  



df_top_n <- function(df, which_cancer) {
  how_many <- switch(which_cancer, breast = 4, lung = 5, skin = 2)
  mutate(df, histo = keep_top_n(hist03v_v, how_many))
}



keep_top_n <- function(fctr, n){
  fctr <- as.character(fctr)
  #take the top n
  keepers <- names(sort(table(fctr), decreasing = TRUE)[seq(1, n)])
  fctr[!(fctr %in% keepers)] <- "Other"
  #remove anything with fewer than 11
  new_tbl <- table(fctr)
  new_keepers <- names(new_tbl[new_tbl <= 11 & new_tbl > 0])
  fctr[!(fctr %in% keepers)] <- "Other"
  as.character(fctr)
}






gp_count <- function(df, vr, ...){
  if(vr == "seer_bm_01"){
    df <- df %>% filter(dx_year >= 2010)
  }
  c(df[["algo_value"]], gps) %<-% list(df[[vr]], quos(...))
  to_return <-
    df %>% group_by(algo_value, which_cancer, !!!gps) %>% summarise(cnt = n()) %>%
       mutate(algo = vr)
  #to_return[["cnt"]][to_return$cnt <= 11 & to_return$cnt > 0] <- NA
  to_return
}
                                                                    

