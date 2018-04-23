

library(tidyverse)
library(zeallot)
devtools::load_all("augur")
cancers <- read_rds("cache/cancers_loaded.rds")
#joining for two things: code counts and date nearest to primary diagnosis

medicare_plus_seer <- function(cncr_df, the_cancer) {
  cncr_df[["which_cancer"]] <- the_cancer 
  cncr_df
}  
cancers <- map2_df(cancers, names(cancers), medicare_plus_seer)

dx_matches <- read_matches("cache/diagnoses")
cancers <- join_claims(cancers, dx_matches)

cpt_img <- read_matches("cache/dx-imaging")
cancers <- join_claims(cancers, cpt_img)
rm(cpt_img)

write_csv(cancers, "cache/cancers_joined-vars.csv.gz")



