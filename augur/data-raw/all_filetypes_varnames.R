

#=================================================        
#make a DF of all the files' formats + variable names     
devtools::load_all()         

formats_to_get <- list.files("../infiles", full.names = TRUE)                      

medicare_formats <- lapply(formats_to_get, format_medi)   
varnames_to_check <-         
    mapply(medicare_formats, gsub("\\..*", "", list.files("../infiles")),            
                    FUN = function(medicare_format, which_one) {     
                    vars <- medicare_format$vars                    
                    which_file <- rep_len(as.character(which_one), nrow(medicare_format))    
                    data.frame(vars, which_file, stringsAsFactors = FALSE)                   
                    })                 
varnames_to_check <- t(varnames_to_check)                 

var_df <- list()             
for (i in seq_len(length(formats_to_get))) {              
    var_df[[i]] <- data.frame(varnames_to_check[i,],        
                              stringsAsFactors = FALSE)     
}                            
all_filetypes_varnames <- do.call(rbind, var_df)
all_filetypes_varnames$vars <- tolower(all_filetypes_varnames$vars)
names(all_filetypes_varnames) <- c("Var", "which_file")

rm(formats_to_get,           
   medicare_formats, i,      
   varnames_to_check, var_df)

#files_w_appendix_vars <- all_filetypes_varnames[all_filetypes_varnames$Var %in% appendix$Var,]
#rm(all_filetypes_varnames)

