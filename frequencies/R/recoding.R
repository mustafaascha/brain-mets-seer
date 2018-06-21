



keygen <- function(kdf, df_var, key_df_var) {
  kdf <- kdf[kdf[["Var"]] == key_df_var,]
  kdf <- kdf[!duplicated(kdf),c("Code", "Meaning")]
  names(kdf) <- c(df_var, paste0(df_var, "_v"))
  kdf
}

lj_coerce <- function(df1, df2){
  join_by <- intersect(names(df1), names(df2))
  if(length(join_by) != 1) {
    stop("Can't coerce more than one 'by' column")
  }
  c(df1[[join_by]], df2[[join_by]]) %<-%
    lapply(list(df1[[join_by]], df2[[join_by]]), as.character)
  left_join(df1, df2)
}


recoder_fn <- function(pedsf_df, key_var, df_var, recode_df) {
  p_recodes <-
    recode_df %>%                                              
    distinct(Code, .keep_all = TRUE)
  key_df <-
    p_recodes[p_recodes$Var == key_var, c("Code", "Meaning")]
  new_name <- paste0(df_var, "_v")
  names(key_df) <- c(df_var, new_name)
  to_return <- left_join(pedsf_df, key_df)
  to_return
}



