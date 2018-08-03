
library(tidyverse)
devtools::load_all("augur")

cancers <- read_csv("cache/cancers_postrecode.csv.gz", progress = FALSE)

#do a few by hand
cancers$hmocnt_e <- as.numeric(cancers$hmocnt1991_2015) > 0
cancers$age_dx_e <- as.numeric(cancers$age_dx) < 65
cancers$dx_year_e <- as.numeric(cancers$dx_year) < 2008

#Multiple primaries = any cancer unaccounted for in the data for that site
# that is to say, the sequence number should be less than or equal to the
# number of records provided for that patient id

seq_df <- 
  cancers %>% group_by(patient_id, which_cancer) %>% 
  summarise(num_records = n(), 
            max_seq = max(as.numeric(seq_num), na.rm = TRUE)) %>% 
  mutate(to_keep = max_seq <= num_records) %>% 
  filter(to_keep)

cancers$all_same_cancer <- 
  ifelse(cancers$patient_id %in% seq_df$patient_id, 
         "All seq nums accounted for by one site", 
         ifelse(!(cancers$patient_id %in% seq_df$to_keep),
                "Unaccounted primary cancers present", 
                NA
               ) 
         )

#strategy: if greatest seqno is accounted for in that cancer dataframe, keep

# See extra exclusions above
# nobody had missing or unknown sex
# nobody had missing or unknown date of death
#   see: table(cancers$ser_dody, cancers$stat_rec)
vfe <- vars_for_exclusion <- 
  c("typefup_v",       
    "med_stcd_v", "med_stcd_v", "med_stcd_v", 
    "med_stcd_v", "race_v",  "srv_time_mon", 
   #"hmocnt1991_2015", "age_dx_e", 
    "vrfydth_v", "payerdx_v", 
    "payerdx_v"
    , "all_same_cancer"
    ) 

vte <- values_to_exclude <- 
  c("Autopsy Only or Death Certificate Only case", 
    "ESRD only", "Aged with ESRD", "Disabled", 
    "Disabled with ESRD", "Unknown", "9999", 
    #TRUE, TRUE, 
    "No", "Insurance status unknown", 
    "Not insured"
    , "Unaccounted primary cancers present"
    )

exclude <- function(df, vr, value_to_exclude){
  vte <- value_to_exclude
  print(paste("Making exclusion variable for", 
              sum(df[[vr]] == vte, na.rm = TRUE), 
              "subjects because", vr, 
              "was equal to", vte, sep = " "))
  df[[vr]] == vte 
}


cancers[,paste0(vfe, "_e")] <- 
  mapply(vfe, vte, FUN = function(x, y) exclude(cancers, x, y ))

write_csv(cancers, "cache/cancers_before_exclusion.csv.gz")

#===============================================================

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


