
library(tidyverse)

devtools::load_all("augur")


dx_codes <- 
  augur::dissertation_codes %>% filter(Treatment.Type == "Diagnostic ICD")


mpr_icd_dx <- 
  extract_slice_codes("seerm", "medpar", augur::infiles[["medpar"]], dx_codes, "DGN_CD") %>% 
  bind_rows
write_csv(mpr_icd_dx, "cache/diagnoses/icd-dx-mpr.csv.gz")








