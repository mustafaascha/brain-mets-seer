
library(tidyverse)

devtools::load_all("augur")


dx_codes <- 
  augur::dissertation_codes %>% filter(Treatment.Type == "Diagnostic ICD")

out_icd_dx <- 
  extract_slice_codes("seerm", "outsaf", augur::infiles[["outpat"]], dx_codes, "dgn_cd") %>% 
  bind_rows
write_csv(out_icd_dx, "cache/diagnoses/outsaf-icd-dx.csv.gz")







