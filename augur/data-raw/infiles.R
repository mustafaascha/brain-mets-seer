
infile_paths <- 
  list.files("data-raw/infiles", 
             pattern = "infile", full.names = TRUE)

load_all()

source("data-raw/format_medi.R")

infiles <- lapply(infile_paths, format_medi)

infile_names <- 
  gsub("_inf.*", "", unlist(list.files("data-raw/infiles", pattern = "infile")))

names(infiles) <- infile_names

