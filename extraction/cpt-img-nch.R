
library(tidyverse)

devtools::load_all("augur")

code_table <- with(augur::dissertation_codes, 
                   table(Treatment.Type, Code.Classification))

px_codes <- 
  augur::dissertation_codes %>% filter(Treatment.Type == "Diagnostic Imaging")

nch_cpt_img <- 
  extract_slice_codes("seerm", "nch", augur::infiles[["nch"]], px_codes, "hcpcs") %>% 
  bind_rows
write_csv(nch_cpt_img, "cache/dx-imaging/cpt-img-nch.csv.gz")







