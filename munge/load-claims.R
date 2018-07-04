

library(tidyverse)          
library(zeallot)
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

cancers <- 
  list(breast = breast, lung = lung, skin = skin) %>% 
  imap_dfr(function(cncr_df, the_cancer) {
      cncr_df[["which_cancer"]] <- the_cancer 
      cncr_df
    }  
  )

dx_matches <- read_matches("cache/diagnoses")
cancers <- join_claims(cancers, dx_matches)
rm(dx_matches)

cpt_img <- read_matches("cache/dx-imaging")
cancers <- join_claims(cancers, cpt_img)
rm(cpt_img)

dx_matches <- claims_dates_df("cache/diagnoses")
img_df <- claims_dates_df("cache/dx-imaging")
date_differences <- days_between_claims(dx_matches, img_df, "cpt_img")
rm(dx_matches, img_df)

cancers <- left_join(cancers, date_differences)

#=============================================

write_csv(cancers, "cache/cancers_prerecode.csv.gz")


