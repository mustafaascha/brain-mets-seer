#' Read all of a specific type of claims file
#' 
#' This function will look in `data_files_path` for a specific type of SEER-Medicare file, and read all files of that type that are contained therein. 
#'
#' @param infile_path The SAS input file to be used for reading ASCII FWF files. 
#' @param data_path The directory containing claims files of interest.
#' @param fileformat.type The type of infile to be read, and the type of SEER-Medicare file that will be read from the `data_files_path`. This must be one of "dme", "ccflag", "nch", "medpar", "outsaf", "pdesaf", "pedsf", "tract", or "zipcode". The default value uses `get_filetype` to regex the filetype out of its filename, though this is not a reliable approach and is discouraged. 
#' @param limit The number of records to read from a file.
#' 
#' @importFrom purrr map2_df
#' 
#' @export
#' 
read_this_type <- function(infile_path,
                           data_path,
                           fileformat.type = 'default',
                           limit = 10){
  if(fileformat.type == 'default'){
    this_type <- get_filetype(infile_path)
  } else{
    this_type <- fileformat.type
  }
  filepaths_df <- get_file_df(infile_path = infile_path,
                              data_files_directory = data_path)
  filepaths_df <- filepaths_df[filepaths_df$medicare_type == this_type,]
  read_for_map <- function(x, y) read_medicare(x, y, n_max = limit)
  map2_df(filepaths_df$filepaths,
          filepaths_df$format_paths,
          read_for_map)
}

