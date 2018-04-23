

library(tidyverse)          
devtools::load_all("augur") 
                            

pedsf_infile <- augur::infiles[["pedsf"]]

#breast==========================================================
breast <- 
  map_df(list.files("seerm", pattern = "pedsf.breast", full.names = TRUE), 
         read_medicare, format_df = pedsf_infile)
names(breast) <- tolower(names(breast))

#lung=============================================================
lung <- 
  map_df(list.files("seerm", pattern = "pedsf.lung", full.names = TRUE), 
         read_medicare, format_df = pedsf_infile)

names(lung) <- tolower(names(lung))

#skin================================================================
skin <- read_medicare("seerm/pedsf.skin.cancer.txt.gz", pedsf_infile)
names(skin) <- tolower(names(skin))

cancers <- list(breast = breast, lung = lung, skin = skin)

#rm(breast, lung, skin)

write_rds(cancers, "cache/cancers_loaded.rds")


