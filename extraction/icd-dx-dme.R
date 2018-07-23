
library(tidyverse)

devtools::load_all("augur")


dx_codes <- 
  augur::dissertation_codes %>% filter(Treatment.Type == "Diagnostic ICD")

dme_icd_dx <- 
  extract_slice_codes("seerm", "dme", augur::infiles[["dme"]], dx_codes, "dgn_cd") %>% 
  bind_rows
write_csv(dme_icd_dx, "cache/diagnoses/dme-icd-dx.csv.gz")


