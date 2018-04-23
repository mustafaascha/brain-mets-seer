


#' Returns TRUE if a code is present in a string vector, as a dataframe for each code
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
#' @importFrom purrr map
#' 
#' 

scan_codes <- function(data_codes, ctp){
  if(length(ctp) == 1){
    grepl(ctp, data_codes)
  } else {
    to_return <-  map(ctp, function(x) grepl(x, data_codes))
    names(to_return) <- paste("Code_", ctp, sep = "")
    data.frame(to_return)
  }
}


#' Read Medicare claims files, return rows that have `pttrn` in `codes_column`
#' 
#' This function will accept a directory containing claims files, and a 
#'   pattern specifying which filetype (as indicated by the first letters
#'   in the filename, as provided by seer) to read. Note that this 
#'   pattern can also be used to specify a single or set of files, in 
#'   addition to any general claims filename prefix.
#' 
#' @param directory This is the directory containing those SEER-Medicare 
#'                  claims files to be read
#' @param pttrn This pattern will be matched to files contained in `directory`,
#'              these files will be read and should have `infile_df` as their
#'              infile_df.
#' @param infile_df This is a `data.frame` whose contents include three variables 
#'                  `var`, `positions`, and `widths`, each describing the ASCII 
#'                   file to be read. 
#' @param codes_df This is a `data.frame` containing a variable 'Code' that 
#'                 contains the codes to be extracted
#' @param codes_column The column of this filetype that contains relevant codes
#' @param parallel_map There's an `mclapply` function used to read and
#'                     scan claims files, this switch indicates whether to 
#'                     use `mclapply`
#' @param corecount The number of cores to use for `mclapply`, if it is used.
#' 
#' @examples
#' 
#' # Set up folder filepaths
#' outpt_data_path <- 
#'    system.file("extdata", "outpat.txt.gz", package = "augur")
#' 
#' @export
#' @return A dataframe as specified by `infile_df` and returned by `read_medicare`
#' @seealso extract_all_diagnoses, extract_all_procedures
#' 
#' @importFrom purrr map
#' @importFrom pryr partial
#' @importFrom parallel mclapply
#' 
extract_slice_codes <- function(directory, pttrn, infile_df, 
                                codes_df, codes_column,
                                parallel_map = FALSE, corecount = 2){
  sample_filepaths <- list.files(directory, pattern = pttrn, full.names = TRUE)

  the_codes <- gsub("\\.", "", codes_df$Code)
  read_scan_es <- 
    partial(read_scan, 
            infile_df = infile_df, codes = the_codes, codes_column = codes_column)

  if(parallel_map){
    scan_dfs <- 
      mclapply(sample_filepaths, read_scan_es, mc.cores = corecount)
  } else {
    scan_dfs <- map(sample_filepaths, read_scan_es)
  }
  names(scan_dfs) <- 
    gsub("\\.txt\\.gz", "", list.files(directory, pattern = pttrn))
  scan_dfs
}

#' Keep rows whose code is present in a specified column
#' 
#' @param filepath A SEER-Medicare claims file path. 
#' @param infile_df This is the infile_df used to read the `filepath`. It 
#'                  must contain three variables `var`, `positions`, and 
#'                  `widths`, each describing the ASCII data file to be read. 
#' @param codes This is an array of codes that will be searched for in 
#'              `codes_column`
#' @param codes_column This is the column of the Medicare claims file data
#'                     that will be searched for matching codes
#' @return A `tibble` of the format specified by `infile_df`
#' @importFrom dplyr slice
#' @importFrom purrr map
#' @export
#' 
read_scan <- function(filepath, infile_df, codes, codes_column) {
  print(paste("scanning", filepath, 
              "for", paste(codes[1:4], collapse = ", "), '...'))
  claims_df <- read_medicare(filepath, infile_df, progress = FALSE)
  scan_df <- scan_codes(claims_df[[codes_column]], codes)
  rows_to_slice <- unique(unlist(map(scan_df, which)))
  slice(claims_df, rows_to_slice)
}







