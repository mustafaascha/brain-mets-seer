#' Get the names of files from their filepaths,                        
#' useful because medicare claims are presented                        
#' with one SAS input file for each type of                            
#' claims file.                                                        
#'                                                                     
#' @param filepath This is the path to a SAS infile text document.     
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

