#' Make a names conversion key from an infile
#' 
#' @param to_read The SAS infile provided by IMS. 
#' 
#'
#' @export
#' 

infile_names <- function(to_read){
  infile_lines <- readLines(to_read)
  equals_lines <- infile_lines[grep("=", infile_lines)]
  no_and_lines<- equals_lines[-grep("\\ +\\&", equals_lines)]
  code <- "options|infile|proc"
  nocode <- no_and_lines[-grep(code, no_and_lines)]
  split_lines <- strsplit(nocode, "\\ *=\\ *")
  get_short_names <- function(x) gsub("\\ *|\\\t", "", x[[1]])
  get_long_names <- function(x) gsub('\\"', "", x[[2]])
  data.frame(
    short_names = vapply(split_lines, get_short_names, character(1)),
    long_names =  vapply(split_lines, get_long_names, character(1)),
    stringsAsFactors = FALSE
    )
}

#' Replace names with those from an infile
#' 
#' @param df The dataframe whose names to change
#' @param infile The infile to use for names conversion
#' @param to_short Switch to use for converting back to short names
#' @export

switch_names <- function(df, infile, to_short = FALSE){
  if(to_short){
    new_names <- infile_names(to_read = infile)[["short_names"]]
  } else {
    new_names <- infile_names(to_read = infile)[["long_names"]]
  }
  
  names(df) <- new_names
  df
}

