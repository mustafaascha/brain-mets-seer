source("data-raw/format_medi.R")

df_to_ascii <- function(df){
  convert_to_ascii <- which(vapply(df, is.character, logical(1)))
  for (i in seq_along(convert_to_ascii)){
   Encoding(df[[convert_to_ascii[i]]]) <- "latin1"
  }
  df[,convert_to_ascii] <- 
    lapply(df[,convert_to_ascii], 
           function(x) iconv(x, "latin1", "ASCII", sub = ''))
  df
}

source("data-raw/pedsf_format.R")
source("data-raw/appendix_recodes.R")
appendix_recodes <- df_to_ascii(appendix_recodes)
source("data-raw/pedsf_recodes.R")
source("data-raw/pedsf_names.R")
#These statements must be executed in this order because
#  all_filetypes_varnames depends on pedsf_utils
pedsf_utils <- list(pedsf_format = pedsf_format, 
                    pedsf_names = pedsf_names, 
                    pedsf_recodes = pedsf_recodes)
pedsf_utils[2:3] <- lapply(pedsf_utils[2:3], df_to_ascii)
source("data-raw/all_filetypes_varnames.R")
source("data-raw/dissertation_codes.R")

#radio_chemo_codes <- list(chemo_drugs = read.csv("data-raw/chemo_drugs.csv", 
#                                           stringsAsFactors = FALSE),
#                          chemo_procs = read.csv("data-raw/chemo_procedures.csv",
#                                           stringsAsFactors = FALSE),
#                          radio_procs = read.csv("data-raw/radio_procedures.csv",
#                                           stringsAsFactors = FALSE),
#                          radio_diags = read.csv("data-raw/radio_diagnoses.csv",
#                                           stringsAsFactors = FALSE))
#radio_chemo_codes <- lapply(radio_chemo_codes, df_to_ascii)

source("data-raw/infiles.R")
source("data-raw/age-adj.R")

census2010 <- readr::read_csv("data-raw/tabula-census2010.csv")
names(census2010) <- tolower(names(census2010))

devtools::use_data(all_filetypes_varnames, 
                   pedsf_utils,
                   dissertation_codes, 
                   census2010,
                   #radio_chemo_codes,
                   appendix_recodes, 
                   infiles,
                   seer_adj_example, 
                   internal = TRUE)


devtools::use_data(all_filetypes_varnames, 
                   pedsf_utils,
                   dissertation_codes, 
                   census2010,
                   #radio_chemo_codes,
                   appendix_recodes, 
                   infiles, 
                   seer_adj_example)
