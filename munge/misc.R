
library(tidyverse)
devtools::load_all("augur")

cancers <- read_csv("cache/cancers_before_exclusion.csv.gz", progress = FALSE)

for_exclusion <- cancers[,grep("_e\\.?[1-3]?$", names(cancers))]
excluded_tables <- map(for_exclusion, table)
exclusions <- apply(for_exclusion, 1, any)
exclusions <- ifelse(is.na(exclusions), FALSE, exclusions)
cancers <- cancers[!exclusions,]
rm(for_exclusion, exclusions)

#if the count is missing, it means the count is zero
na_to_zero <- grep("counts_", names(cancers))
fix_nas <- function(x) ifelse(is.na(x), 0, x)
cancers[,na_to_zero] <- lapply(cancers[,na_to_zero], fix_nas)

#this is an integer value, necessarily
to_int <- grep("days_prim_", names(cancers))
cancers[,to_int] <- lapply(cancers[,to_int], as.numeric)

cancers$seer_br_mets <- 
  with(cancers, 
       ifelse(csmetsdxbr_pub_v == "None", "SEER_Negative", 
         ifelse(csmetsdxbr_pub_v == "Yes", "SEER_Positive", NA)))
  
cancers[,make_prim_names("60")] <- apply_n_day_fn(cancers, 60)


cancers[["medicare_60_dx_img"]]  <- 
  with(cancers, ifelse(days_dx_cpt_img <= 60, 1, 0))


write_csv(cancers, "cache/cancers.csv.gz")


