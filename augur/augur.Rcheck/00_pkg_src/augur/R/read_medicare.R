#' Read a medicare claims file
#' 
#' @param data_file The claims file to be read
#' @param format_file The SAS infile provided by SEER-Medicare
#' @param ... Arguments forwarded to `read_fwf` from `readr`
#' @export
#' @importFrom readr read_fwf fwf_positions cols col_character
#' 
#' 
read_medicare <- function(data_file, format_file, ...){
    #replace "format_file" and "format function" arguments with relevant
    # switch statements upon development completion?
  if(is.data.frame(format_file)) {
    format_df <- format_file
  } else {
    format_df <- format_medi(format_file)
  }
  format_df$positions <- as.numeric(format_df$positions)
  format_df$widths <- as.numeric(format_df$widths)
# fwf_pos <- fwf_widths(as.numeric(format_df$widths),
#                       format_df$vars)
  fwf_pos <- with(format_df, 
                  fwf_positions(positions, 
                                positions + widths - 1, 
                                vars))
  medi_data <- read_fwf(data_file,
                 fwf_pos, 
                 col_types = cols(.default = col_character()),
                 #col_types = format_df$col_types,
                 ...) 
  medi_data 
} 
