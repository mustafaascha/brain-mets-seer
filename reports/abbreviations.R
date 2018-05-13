

abbreviations <- 
  data.frame(
    Abbreviation = c(
      "SEER",     "BM",      "SBM",      "LBM",
      "AAIR",     "MEDPAR",  "NCH",      "DME", 
      "OUTSAF",   "CPT",     "ICD-9-CM", "WNH", 
      "WH",       "ICD-O-3", "HMO",      "PCI",      
      "Her2",     "ER",      "PR",       "Hr", 
      "CI",       "SCLC",    "NSCLC",    "Car.", 
      "CNS"
    ), 
    Meaning = c(
      "Surveillance, Epidemiology, and End-Results", "Brain Metastases", 
      "Synchronous Brain Metastases", "Lifetime Brain Metastases", 
      "Average Annual Age-Adjusted Incidence Rate", 
      "Medicare Part A", "National Carrier Claims", "Durable Medical Equipment", 
      "Outpatient standard analysis files", "Current PRocedural Terminology", 
      "International Classification of Disease, Ninth Revision, Clinical Modification", 
      "White Non-Hispanic", "White Hispanic", 
      "International Classification of Diseases, Oncology, 3rd Revision", 
      "Healthcare Management Organization", "Prophylactic Cranial Irradiation", 
      "Human Epidermal Growth Factor Receptor 2", 
      "Estrogen Receptor", "Progesterone Receptor", "Hormone Receptor", 
      "Confidence Interval", "Small-Cell Lung Cancer", "Non-Small-Cell Lung Cancer", 
      "Carcinoma", "Central Nervous System"
    ), 
    stringsAsFactors = FALSE) %>% 
  arrange(Abbreviation) %>% 
  modify_at("Abbreviation", ~ paste("**", .x, "**", sep = ""))