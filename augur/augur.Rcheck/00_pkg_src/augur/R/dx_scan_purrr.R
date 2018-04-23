#' Returns TRUE if a diagnosis code is present, as a dataframe for each code
#' 
#' This is a version of `split_and_scan` based on `purrr` rather than `base`
#' 
#' Data should be provided as it is returned by `read_medicare`. That is, 
#' it should be a character array whose contents are of the form:
#' "00000  00000  00000  00000  00000", in fixed-width format. 
#' 
#' 
#' @param data_codes Actual codes, whitespace between each code
#' @param ctp Codes against which to check each diagnosis, codes to pull
#' @export
#' @return data.frame of TRUE/FALSE for each code, where TRUE indicates that
#'         the code is present in an element of the data. 
#' @importFrom purrr map_dfr
#' 
#' 

split_and_scan <- function(data_codes, ctp){
  actual_codes <- strsplit(data_codes, "\\ +")
  if(length(ctp) == 1){
    grepl(ctp, actual_codes)
  } else{
    to_return <- 
      map_dfr(actual_codes, 
                     function(x) ctp %in% x)
    to_return <- data.frame(rbind(to_return))
    names(to_return) <- paste("Code_", ctp, sep = "")
    to_return
  }
}

#' Merge data.frames of logical columns from scanning functions
#' 
#' This function will merge two of the output from `split_and_scan`
#' 
#' @param scan_1 The first scan to merge
#' @param scan_2 The second scan to merge
#' @param ctp Codes to pull, these are essential for names
#' 
merge_scans <- function(scan_1, scan_2, ctp){
  to_return <- 
    map2_dfc(scan_1, scan_2,
             function(x, y) x | y)
  #It's okay not to use stringsAsFactors = FALSE because there are no strings
  to_return <- data.frame(rbind(to_return))
  names(to_return) <- paste("Code", ctp, sep = "_")
  to_return
}


#' For each code in a character array representing codes from a claims row, 
#' return TRUE/FALSE if that row contains a code of interest
#' 
#' @param df An NCH dataframe made by `read_medicare`
#' @param codes_to_pull `scan_nch` will test for the presence of these codes
#' @export
#' 
#' @return data.frame of TRUE/FALSE for each code of interest
#' @importFrom purrr map2_dfc
#' 
#' 

scan_nch_dx <- function(df, codes_to_pull){
  #diagnosis vars: dgn_cd, pdgns_cd, linediag
  #main_dgns and prin_dgns should be of same str(), maybe different values
  to_return <- 
    map2_dfc(split_and_scan(df[["dgn_cd"]], codes_to_pull), 
             split_and_scan(df[["pdgns_cd"]], codes_to_pull),
             function(x, y) x | y)
  #It's okay not to use stringsAsFactors = FALSE because there are no strings
  to_return <- data.frame(rbind(to_return))
  names(to_return) <- paste("Code", codes_to_pull, sep = "_")
  to_return
}

#' Function to retrieve outpatient diagnosis code matches
#' 
#' 
#' @param df An outpatient dataframe made by `read_medicare`
#' @param codes_to_pull `scan_op_dx` will test for the presence of these codes  
#' @export
#' 
#' @return `data.frame` of TRUE/FALSE columns for each code of interest  
#' 
#' 

scan_op_dx <- function(df, codes_to_pull){
    split_and_scan(df[["dgn_cd"]], codes_to_pull)
}

#' Function to retrieve outpatient diagnosis code matches
#' 
#'   
#' @param df An outpatient dataframe made by `read_medicare`
#' @param codes_to_pull `scan_op_dx` will test for the presence of these codes 
#' @export 
#' 
#' @return `data.frame` of TRUE/FALSE columns for each code of interest 
#'    
#'

scan_pde_dx <- function(df, codes_to_pull){
  #diagnosis vars: dgn_cd, pdgns_cd, linediag
  #main_dgns and prin_dgns should be of same str(), maybe different values
  to_return <- merge_scans(split_and_scan(df[["dgn_cd"]], codes_to_pull), 
                           split_and_scan(df[["pdgns_cd"]], codes_to_pull), 
                           codes_to_pull)
  #It's okay not to use stringsAsFactors = FALSE because there are no strings
  to_return
}




