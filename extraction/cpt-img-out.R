
library(tidyverse)

devtools::load_all("augur")

code_table <- with(augur::dissertation_codes, 
                   table(Treatment.Type, Code.Classification))

px_codes <- 
  augur::dissertation_codes %>% filter(Treatment.Type == "Diagnostic Imaging")

out_cpt_img <- 
  extract_slice_codes("seerm", "outsaf", augur::infiles[["outpat"]], px_codes, "hcpcs") %>% 
  bind_rows
write_csv(out_cpt_img, "cache/dx-imaging/cpt-img-out.csv.gz")







