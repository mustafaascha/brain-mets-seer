#' Recode PEDSF variables
#' 
#' This function joins variables by their value to a dataframe decoder
#' 
#' 
#' @param df The `data.frame` containing PEDSF data to recode. 
#' @param which_variable The name of the variable to be recoded, unquoted.
#' @param which_key The dataframe containing key-value pairs for recoding. 
#' @param max.dist Approximate-ness of the `df` variable name being matched 
#'           to the PEDSF recode dataframe variable name
#' @param var_as_is A logicalswitch indicating whether to accept `which_variable` as-is and containing its own values versus doing an `enquo` on `which_variable`
#' 

recode_variable <- function(df, which_variable, which_key, max.dist = 0, var_as_is = FALSE){
  if(var_as_is){
    vr <- which_variable
  } else {
    vr <- rlang::enquo(which_variable)
  }
  recode_var <- rlang::quo_text(vr)
  
  to_join <- grep(pattern = tolower(recode_var),
                   x = which_key[["Var"]])

  tj_varname <- unique(which_key[["Var"]][to_join])
  if(length(tj_varname) > 1){
    print(tj_varname)
    stop("Please specify a unique variable name, more than one match was found", 
         call. = FALSE)
  } else if(length(tj_varname) < 1){
    stop("That value of `which_variable` doesn't seem to exist in the `df`.")
  }
  to_join <- which_key[to_join,-1]
  names(to_join) <- c(recode_var, 
                      paste(recode_var, "_meaning", sep = ""))

  joined <- dplyr::left_join(df, to_join, by = recode_var)
  joined
}

#' Recode PEDSF variables
#' 
#' @param ... One or more of the arguments passed to `recode_variable`, among `max.dist` for variable names matching and `for_real` as a logical switch indicating whether to `enquo` `which_variable` or instead accept `which_variable` as-is.
#' @rdname recode_variable
#' @seealso recode_dfs
#' @export
recode_pedsf <- function(df, which_variable, ...){
  recode_variable(df, 
                  rlang::enquo(which_variable), 
                  which_key = pedsf_utils[["pedsf_recodes"]], 
                  var_as_is = TRUE, 
                  ...)
}

#' 





