#' Formatting helper for standard SAS infiles 
#'                                                                             
#' @param sas_input_file Path to the file with variable definitions            
#' @seealso read_medicare                                                      
.init_format_medi <- function(sas_input_file) {                                 
  #Note: "char_count" and "width" are equivalent
                                                                              
  #The file is organized such that variables are defined after the             
  # word "INPUT", where each variable is preceeded by "@" and                  
  # followed by its type and width in the FWF.                                 
  medi_sas <- readLines(sas_input_file)                                        
  medi_sas <- medi_sas[grepl("@", as.character(medi_sas)) &                    
                       !grepl("^\\/\\*", as.character(medi_sas)) &             
                       !grepl("^\\ +\\@\\;", as.character(medi_sas))]          
  medi_sas <-gsub(pattern = '^\\ +@', replacement = '', medi_sas)              
  #second and third listed variables are not actually their own variables,     
  #  ignore it for now. Maybe a nice feature, auto-choose which                
  #  ID to use as the variable name                                            
  to_remove <- "^\\ \\/\\*\\ @001"
  medi_sas <- medi_sas[!grepl(to_remove, medi_sas)]
                                                                               
  medi_sas <- strsplit(medi_sas, '\\ +')                                       
  var_position <- vapply(medi_sas, function(x) x[1], character(1))             
  var_name <- vapply(medi_sas, function(x) x[2], character(1))                 
  char_count <- vapply(medi_sas,                                               
      function(x) gsub('[A-Za-z]|\\$|\\(|\\)|\\..*','', x[3]),                 
      character(1))                                                            
  char_count <- as.numeric(char_count)                                         
  var_type <- vapply(medi_sas,                                                 
      function(x) gsub('[0-9]|\\$|\\(|\\)|\\.', '', x[3]),                     
      character(1))                                                            
  var_type <- ifelse(is.na(var_type), "n", "c")                                
  #special case: input on same line as first variable                          
  if (!is.na(var_name[1]) & (var_name[1] == "input" | var_name[1] == "INPUT")){ 
    var_name[1] <- medi_sas[1][[1]][4]                                         
    char_count[1] <- gsub('[A-Za-z]|\\$|\\(|\\)|\\.','', medi_sas[1][[1]][5]) 
  }                                                                           
                                                                              
  to_return <- data.frame(positions = var_position,                           
               vars = var_name,                                               
               widths = char_count,                                           
               col_types = var_type,                                          
               stringsAsFactors = FALSE)

                                                                              
  vars_that_should_be_expanded <-
    which(grepl("\\(", to_return$vars))

  if(length(vars_that_should_be_expanded) > 0){
    vtsbe <- vars_that_should_be_expanded
    new_widths <-
      with(to_return,
           as.numeric(var_position[vtsbe + 1]) -
           (as.numeric(var_position[vtsbe]))
           )
    to_return$widths[vtsbe] <- as.character(new_widths)
  }
  to_return$vars <- 
    gsub("\\(", "", to_return$vars)
  to_return$vars <- 
    gsub("[0-9]+-.+", "", to_return$vars)
  to_return
}


#' Parse a SAS FWF format file
#' 
#' @param sas_input_file The SAS file describing a FWF claims file.
#' @param formatfile.type The SEER-Medicare infile type being formatted.
#' 
format_medi <- function(sas_input_file, formatfile.type = "default"){ 
  #these are cool files 
  easy_to_parse <- c("ccflag", "dme", "medpar",
                     "pdesaf", "ptden", "tract",
                     "zipcode")
  formatfile.type <- ifelse(formatfile.type != "default", 
                            formatfile.type,
                            tolower(get_filetype(sas_input_file)))
  formatfile.type <- gsub("_infile", "", formatfile.type)
  # print(formatfile.type)   
  if(formatfile.type %in% easy_to_parse) {
    #fix this warnings suppression, don't want this technical debt
    to_return <- suppressWarnings(.init_format_medi(sas_input_file))
    if(to_return$positions[1] == ""){
      to_return$positions[1] <- "1"
    }

  } else if(formatfile.type == "nch") {
    
    to_return <- suppressWarnings(.init_format_medi(sas_input_file))
    if(to_return$positions[1] == ""){
      to_return$positions[1] <- "1"
    }
    to_return$widths[to_return$vars == "dgn_cd"] <- 84
    
  } else if(formatfile.type == "outsaf" |
          formatfile.type == "outpat") {
  outsaf_format <- suppressWarnings(.init_format_medi(sas_input_file))
  outsaf_format <- outsaf_format[outsaf_format$vars != "/*@001",]
  make_procedure_dxdate <- function(n, starting_position){
    data.frame(positions = c(starting_position, 
                             starting_position + 7), #15 = total prcr charlength
               vars = c(paste("prcdr_cd", n, sep = "_"), 
                        paste("prcdr_dt", n, sep = "_")),
               widths = c(7, 8),
               col_types = rep_len("c", 2),
               stringsAsFactors = FALSE)
  }
  #first row to fix is that which immediately precedes the
  # variables to expand, which is why it has no width. If there were
  # a position available, the width would be position - width. 
  # As is, the width is NA, and is the first formatting spec to fix
  first_row_to_fix <- min(which(is.na(outsaf_format$widths)))
  add_to_this <- outsaf_format[1:(first_row_to_fix),]
  add_to_this[,c("positions", "widths")] <- 
    lapply(add_to_this[,c("positions", "widths")], as.integer)
  add_to_this$widths[first_row_to_fix] <- 6*7
  #initial position is the first to be filled with expanded rows
  initial_position <- with(add_to_this,
    positions[first_row_to_fix] + widths[first_row_to_fix])
  #first row to expand: prc
  prc_to_add <- list()
  for(i in seq_len(13)){
    prc_to_add[[i]] <- 
      make_procedure_dxdate(i, initial_position + ((i - 1) * 15))
  }
  #second row to expand: ocr
  make_ocr_dxdt <- function(n, starting_position){
    data.frame(positions = c(starting_position,  #2 = charlength of preceding var
                             starting_position + 2),  
               vars = c(paste("ocrnc_cd", n, sep = "_"), 
                        paste("ocrnc_dt", n, sep = "_")),
               widths = c(2, 8),
               col_types = rep_len("c", 2),
               stringsAsFactors = FALSE)
  }
  initial_position <- 1035
  ocr_to_add <- list()
  for(i in seq_len(14)){
    ocr_to_add[[i]] <- 
      make_ocr_dxdt(i, initial_position + ((i - 1) * 10))
  }
  #middle rows, between prc and ocr
  rlt_to_add <- data.frame(positions = 1025, vars = "rlt_cond12", 
                           widths = 10, col_types = "c", 
                           stringsAsFactors = FALSE)
  to_return <- 
    dplyr::bind_rows(add_to_this, 
                     prc_to_add, 
                     rlt_to_add, 
                     ocr_to_add)



  } else if(formatfile.type == "pedsf"){
    pedsf_format <- pedsf_utils[["pedsf_format"]]
    #just use the formatfile I pulled
    pedsf_format$widths[pedsf_format$widths == ""] <- 
      c("2", "1", "8", "53", "12", "24", "2", "2", "12")
    pedsf_format$widths[grep("\\*", pedsf_format$widths)] <- "12"
    pedsf_format <- pedsf_format[pedsf_format$positions != "COL",]
    to_return <- pedsf_format
  }
  
  if(formatfile.type != "outsaf" & formatfile.type != "outpat"){
  #touch ups
  patterns_to_remove <- 
    c("\\*@001", "SEER_Cases", "Case_Number")
  regex_to_use <- paste(patterns_to_remove, collapse = "|")
  rows_to_remove <- grepl(regex_to_use, to_return$vars)
  to_return <- to_return[!rows_to_remove, ]
  }

  to_return

}                                                                   


#Outsaf problems:
    #last n rows are meant to be repeated and positions calculated. 
    # positions       vars        widths col_types
    # inc+0           prcdr_cd(j)   <NA>         c  
    # inc+7           prcdrdtm(j)   <NA>         c  
    # inc+9           prcdrdtd(j)   <NA>         c  
    # inc+11           prcdrdty(j)  <NA>         c  
    # 1025 (rlt_cond1-rlt_cond2)    <NA>         c  
    # inc+0           ocrnc_cd(j)   <NA>         c  
    # inc+2           ocrncdty(j)   <NA>         c  
    # inc+6           ocrncdtm(j)   <NA>         c  
    # inc+8           ocrncdtd(j)   <NA>         c  
    
  #Corresponding code from the claims infile is: 
   # @613 (dgn_cd1-dgn_cd25) ($char7.)  /* $char5. and only 10 codes in 2010
   # @788 (edgnsd1-edgnsd6)  ($char7.)  /* new in 2012 Linkage  
   #     @;                         
   # array prcdr_cd(13) $ prcdr_cd1-prcdr_cd13;  /* only 6 codes in 2010 Linkage */
   # array prcdrdtm(13) $ prcdrdtm1-prcdrdtm13;
   # array prcdrdtd(13) $ prcdrdtd1-prcdrdtd13;
   # array prcdrdty(13) $ prcdrdty1-prcdrdty13;
   # inc = 830;
   # do j = 1 to 13;
   #  input
   #    @inc+0  prcdr_cd(j) $char7.  /* $char4. and only 5 codes in 2010 Linkage */
   #    @inc+7  prcdrdtm(j) $char2.  /* only 6 codes in 2010 Linkage */
   #    @inc+9  prcdrdtd(j) $char2.  /* only 6 codes in 2010 Linkage */
   #    @inc+11 prcdrdty(j) $char4.  /* only 6 codes in 2010 Linkage */
   #    @;
   #    inc=inc+15;
   # end;
   # input
   #   @1025 (rlt_cond1-rlt_cond2) ($char2.)  /* new in 2012 Linkage */
   #   @;
   # array ocrnc_cd(14) $ ocrnc_cd1-ocrnc_cd14;
   # array ocrncdty(14) $ ocrncdty1-ocrncdty14;
   # array ocrncdtm(14) $ ocrncdtm1-ocrncdtm14;
   # array ocrncdtd(14) $ ocrncdtd1-ocrncdtd14;
   # inc = 1035;
   # do j = 1 to 14;
   #   input 
   #    @inc+0  ocrnc_cd(j)  $char2.  /* new in 2012 Linkage */
   #    @inc+2  ocrncdty(j)  $char4.  /* new in 2012 Linkage */
   #    @inc+6  ocrncdtm(j)  $char2.  /* new in 2012 Linkage */
   #    @inc+8  ocrncdtd(j)  $char2.  /* new in 2012 Linkage */ 
   #    @;
   #    inc=inc+10;
   # end;
   # input;
   # drop j inc


