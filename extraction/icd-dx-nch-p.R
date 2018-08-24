
library(tidyverse)

devtools::load_all("augur")


dx_codes <- 
  augur::dissertation_codes %>% filter(Treatment.Type == "Diagnostic ICD")

nch_icd_dx <- 
  extract_slice_codes("seerm", "nch", augur::infiles[["nch"]], dx_codes, "pdgns_cd") %>% 
  bind_rows
write_csv(nch_icd_dx, "cache/diagnoses/icd-nch-dx-p.csv.gz")







