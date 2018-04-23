
#' Returns a dataframe listing all ASCII data file filepaths listed with 
#' their respective SAS infile filepaths, and the "type" of medicare file.
#'
#' @param infiles_directory The directory containing SAS infiles provided by
#'                     SEER-Medicare
#' @param data_files_directory The directory containing ASCII FWF claims files.
#' 
#' @export
#' 
get_files_df <- function(infiles_directory, data_files_directory){ 

  format_files <- list_txt_files(infiles_directory)

  names(format_files)[grep(pattern = "filepaths", names(format_files))] <-
       "format_paths"
  files_to_read <- list_txt_files(data_files_directory)

  dplyr::left_join(files_to_read, format_files)
}

#' Returns a dataframe listing all ASCII data file filepaths of a 
#' specific type listed with its corresponding SAS infile filepaths,
#' and the "type" of medicare file.
#'
#' @param infile_path The directory containing SAS infiles provided by
#'                     SEER-Medicare
#' @param data_files_directory The directory containing ASCII FWF claims files.
#' 
#' @export
#' 
get_file_df <- function(infile_path, data_files_directory){
  format_files <-
          data.frame(format_paths = infile_path,
                     medicare_type = get_filetype(infile_path),
                     stringsAsFactors = FALSE)
  files_to_read <- list_txt_files(data_files_directory)
  
  dplyr::left_join(files_to_read, format_files)
}

#' Retrieve file list and types from directory containing
#' claims data
#'
#' @param files_path The directory containing claims files of interest.
#'
#' @export
#'
list_txt_files <- function(files_path){
    file_names <- list.files(files_path, full.names = TRUE, pattern = "txt")
  file_types <- vapply(file_names, get_filetype, character(1))
    data.frame(filepaths = file_names, medicare_type = file_types,
                            stringsAsFactors = FALSE)
}


#' Get the names of files from their filepaths, 
#' useful because medicare claims are presented 
#' with one SAS input file for each type of   
#' claims file.  
#' 
#' @param filepath This is the path to a SAS infile text document.
#' 
#' @export
#' 
get_filetype <- function(filepath){                                    
    #rev, strsplit, regmatches                                           
    filename <- strsplit(regmatches(filepath,                            
                         regexpr("(?<=\\/).*(?=(\\.txt|.txt\\.gz)$)",   
                                filepath, perl = TRUE)),                       
                                      "\\/")                                                    
  if(length(filename) == 0){                                           
     print(filepath)                                                    
     return("")                                                         
   }                                                                    
  filename <- rev(filename[[1]])[1]                                    
  filename <- gsub("\\..*", "", filename)                              
  filename <- gsub("[0-9]", "", filename)                              
  filename <- tolower(filename)                                        
  if(filename == "outpat"){                                            
    return("outsaf")                                                   
  }                                                                    
  if(filename == "ptd") {                                              
    return("ptden")                                                    
  }                                                                    
  filename                                                             
}                                                                      





