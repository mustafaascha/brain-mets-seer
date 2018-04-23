
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

# nobody had missing or unknown sex
# nobody had missing or unknown date of death
#   see: table(cancers$ser_dody, cancers$stat_rec)
vfe <- vars_for_exclusion <- 
  c("typefup_v",       
    "med_stcd_v", "med_stcd_v", "med_stcd_v", 
    "med_stcd_v", "race_v",  "srv_time_mon", 
   #"hmocnt1991_2015", "age_dx_e", 
    "vrfydth_v", "payerdx_v", 
    "payerdx_v", "all_same_cancer") 

vte <- values_to_exclude <- 
  c("Autopsy Only or Death Certificate Only case", 
    "ESRD only", "Aged with ESRD", "Disabled", 
    "Disabled with ESRD", "Unknown", "9999", 
    #TRUE, TRUE, 
    "No", "Insurance status unknown", 
    "Not insured", "Unaccounted primary cancers present")

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


