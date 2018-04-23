#this is a test schange
library(tidyverse)

devtools::load_all("augur")


dx_codes <- 
  augur::dissertation_codes %>% filter(Treatment.Type == "Diagnostic ICD")

dme_icd_dx <- 
  extract_slice_codes("seerm", "dme", augur::infiles[["dme"]], dx_codes, "pdgns_cd") %>% 
  bind_rows
write_csv(dme_icd_dx, "cache/diagnoses/dme-icd-dx-p.csv.gz")








