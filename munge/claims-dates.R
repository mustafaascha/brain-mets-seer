

library(tidyverse)
library(zeallot)
devtools::load_all("augur")

cancers <- 
  read_csv("cache/cancers_joined-vars.csv.gz", progress = FALSE)

dx_matches <- claims_dates_df("cache/diagnoses")
img_df <- claims_dates_df("cache/dx-imaging")

date_differences <- days_between_claims(dx_matches, img_df, "cpt_img")

cancers <- left_join(cancers, date_differences)

#=============================================

write_csv(cancers, "cache/cancers_prerecode.csv.gz")


