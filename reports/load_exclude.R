
library(tidyverse)
library(zeallot)


library(tableone)

devtools::load_all("augur")
devtools::load_all("frequencies")

paper_stuff <- list()
paper_products <- list()

cancers <- read_csv("cache/cancers.csv.gz", progress = FALSE)
cancers_full <- read_csv("cache/cancers_before_exclusion.csv.gz", progress = FALSE)

expected_whocodes <- c(22030, 25010, 26000)
expected_cancers <- 
  map(expected_whocodes, ~ table(cancers$siterwho == .x))
names(expected_cancers) <- paste("x", expected_whocodes, sep = "")

expected_cancers_full <- 
  map(expected_whocodes, ~ table(cancers_full$siterwho == .x))
names(expected_cancers_full) <- paste("x", expected_whocodes, sep = "")

 #  we're working with lung, skin, and breast cancers
 #  is this necssaru pr correct?                 
paper_products[["not_right_cancer"]] <- 
  list(cancers = expected_cancers, 
       cancers_full = expected_cancers_full)

cancers <- cancers %>% filter(siterwho %in% c(22030, 25010, 26000))    
cancers_full <- cancers_full %>% filter(siterwho %in% c(22030, 25010, 26000))  

rep_pat <- "(Malignant\\ neoplasm\\:|Carcinoma\\ in\\ situ|Malignant\\ neoplasm\\ of\\ other\\ and\\ ill-defined\\ sites\\:)\\ (of)?"
cancers$icd <- gsub(rep_pat, "", cancers$icd10)
stems <- 
  list(mis = c("(Melanoma", "in", "situ)"), 
       malm = c("(Malignant", "melanoma)"),
       olo = c("(Overlapping", "lesion)"), 
       cars = c("(Carcinoma", "in", "situ)"), 
       cst = c("(Connective", "and", "soft", "tissue)"), 
       mnp = c("(Malignant", "neoplasm)"), 
       qdrnt = "(^.*quadrant)",
       lobe = "(^.*lobe)",
       inc = "(including.*$)", 
       unsp = "(unspecified$)", 
       oth = "(other", "and", "unspecified", "parts)"
       )
to_replace <- 
  c(paste0(map(stems, function(x) paste0(tolower(x), collapse = "\\ ")), 
    collapse = "|"), 
    "\\,|of\\ |:\\ ", 
    "skin",  
    "\\ +")
replacements <- 
  c("", "", 
    "", 
    "\\ ")

cancers$icd <- 
  reduce2(to_replace, replacements, 
          function(starting, fst, snd) {
            new_x <- trimws(tolower(starting), "both")
            gsub(pattern = fst, replacement = snd, x = new_x)
          }, 
          .init = cancers$icd)

cancers$icd[cancers$icd == ""] <- NA
#cancers$icd <- ifelse(grepl("[lL]ower\\ [lL]imb", cancers$icd), "llt", cancers$icd)
#cancers$icd <- ifelse(grepl("[tT]run[ck](al)?", cancers$icd), "llt", cancers$icd)
#cancers$icd <- ifelse(grepl("[Ff]ace", cancers$icd), "ulf", cancers$icd)
#cancers$icd <- ifelse(grepl("[uU]pper\\ [lL]imb", cancers$icd), "ulf", cancers$icd)
grep_repl <- 
to_remove <- 
  c("[lL]ower\\ [lL]imb", "[tT]run[ck](al)?", "[Ff]ace", 
    "[uU]pper\\ [lL]imb", "[sS]calp|[nN]eck|[eE]ar|[eE]ye|[lL]ip")
replacements <- c("llt", "llt", "ulf", "ulf", "ulf") 

cancers <- 
  reduce2(to_remove, replacements, 
          function(df, to_rep, repmnt) {
            df[["icd"]] <- 
              ifelse(grepl(to_rep, df[["icd"]]), repmnt, df[["icd"]])
            df
          },
         .init = cancers)  

skins <- which(cancers$which_cancer == "skin")
cancers$icd_c[skins] <- 
  ifelse((cancers$icd[skins] %in% c("llt", "ulf")), 
         cancers$icd[skins],
         "other")
  

#fix NAs so they're correctly represented
cancers[,grep("csmetsdx", names(cancers))] <- 
  lapply(cancers[,grep("csmetsdx", names(cancers))], 
        function(x) ifelse(x == "Unknown" | x == "Not applicable", NA, x)
        )

names_grid <- 
  expand.grid("medicare", c(30, 60, 90), 
              c(paste("prim", c("dx_matches", "cpt_rad", 
                                "biopsy", "cpt_img", "cpt_neu"), 
                      sep = "_"), 
                "dx_img"), 
              stringsAsFactors = FALSE)

na_to_fix <- apply(names_grid, 1, paste0, collapse = "_")

cancers[,na_to_fix] <- 
  lapply(cancers[,na_to_fix], function(x) ifelse(is.na(x), 0, x))

#cancers$dx_year <- factor(cancers$dx_year)
cancers$dx_year_c <- cut(as.numeric(cancers$dx_year), c(1975, 2007:2012))

# exclusions table ==========================================
# nobody had missing or unknown sex                          
# nobody had missing or unknown date of death                
#   see: table(cancers$ser_dody, cancers$stat_rec)

vfe <- vars_for_exclusion <-
    c("hmocnt", "age_dx", "dx_year", "typefup",
      "med_stcd", "med_stcd", "med_stcd", 
      "med_stcd", "race",  "srv_time_mon",
     #"hmocnt1991_2015", "age_dx_e",
      "vrfydth", "payerdx_v",
      "payerdx_v", "all_same_cancer")
vte <- values_to_exclude <-
    c("Used HMO", "Under 65 years", "Before 2008",
      "Autopsy/Death Certificate",
      "ESRD only", "Aged with ESRD", "Disabled",
      "Disabled with ESRD", "Unknown", "9999",
     #"Used HMO", "Age < 65", 
      "No", "Insurance status unknown",
      "Not insured", "Unaccounted primaries present")
exclusion_df_vars <- 
  c("patient_id", "which_cancer", 
    names(cancers_full)[grep("_e\\.?[0-5]?$", names(cancers_full))])
exclusion_df <- cancers_full[,exclusion_df_vars]

medicare_eligibility_vars <- 
  c("hmocnt_e", "age_dx_e",
    "med_stcd_v_e", "med_stcd_v_e.1", "med_stcd_v_e.2",
    "med_stcd_v_e.3", "payerdx_v_e", "payerdx_v_e.1")

medicare_exclusions <- 
  exclusion_df[,c("patient_id", "which_cancer", medicare_eligibility_vars)]
medicare_exclusions[["any_medicare"]] <- 
  apply(medicare_exclusions[,3:10], 1, any)
medicare_exclusions_summary <- 
  medicare_exclusions %>% 
  group_by(which_cancer, any_medicare) %>% 
  summarise(ex_med = n())

general_exclusions <- 
  exclusion_df[,!(names(exclusion_df) %in% medicare_eligibility_vars)]
general_exclusions[["any_general"]] <- 
  apply(general_exclusions[,3:7], 1, any)
general_exclusions_summary <- 
  general_exclusions %>% 
  group_by(which_cancer, any_general) %>% 
  summarise(ex_gen = n()) 

m_bind <- medicare_exclusions_summary %>% 
    filter(any_medicare) %>% select(-any_medicare)
g_bind <- general_exclusions_summary %>% 
    filter(any_general) %>% select(-any_general)

paper_stuff[["selected_ns"]] <- 
  reduce(list(m_bind, g_bind), left_join)


exclusion_map <- 
  exclusion_df %>% group_by(which_cancer) %>% 
# distinct(patient_id, .keep_all = TRUE) %>%
  select(-patient_id) %>%  nest
exclusion_map$n <- map_int(exclusion_map$data, ~ nrow(.x))

exclusion_sums <- 
  map_dfr(exclusion_map$data, 
          function(exclusion_df) {
          map_df(exclusion_df, 
                 function(exclusion_column){ 
                   sum(exclusion_column, na.rm = TRUE)
                 })
          })

#transpose and return to dataframe with variable column
#backup_em <- exclusion_map
exclusion_map <- bind_cols(exclusion_map, exclusion_sums) %>% select(-data)
exclusion_map <- data.frame(t(exclusion_map), stringsAsFactors = FALSE)
names(exclusion_map) <- exclusion_map[1,]
exclusion_map <- exclusion_map[-1,]
exclusion_map$Var <- rownames(exclusion_map)
rownames(exclusion_map) <- NULL
exclusion_map$Vars <- c("n", vfe)
exclusion_map$Values <- c("", vte)
exclusion_map <- exclusion_map[,c(4:6, 1:3)]

paper_stuff[["exclusions"]] <- exclusion_map

rm(exclusion_map, exclusion_df_vars, exclusion_df, exclusion_sums, 
   general_exclusions, general_exclusions_summary, 
   medicare_exclusions, medicare_exclusions_summary,
   vfe, vars_for_exclusion, vte, values_to_exclude, 
   m_bind, g_bind, 
   medicare_eligibility_vars)


to_numeric <- c("age_dx", "cs_size", "eod10_pn", "cs_mets")                  
cancers[,to_numeric] <- lapply(cancers[,to_numeric], as.numeric)  



cgp <- cancers %>% group_by(which_cancer, dx_year) %>% tidyr::nest()
cgp$data <- map2(cgp$data, cgp$which_cancer, df_top_n)
cancers <- unnest(cgp)

rm(to_numeric, cgp) 

medicare_vars <- names(cancers)[grep("^medicare_[0-9]", names(cancers))]
cancers[,medicare_vars] <- 
  lapply(cancers[,medicare_vars], 
    function(the_var) ifelse(is.na(the_var), 0, the_var))

cancers$seer_bm_01 <-                                                     
  with(cancers, 
       ifelse(is.na(seer_br_mets), NA, as.numeric(seer_br_mets == "SEER_Positive")))

prim_to_dx_img <-
  c("medicare_30_prim_dx_img", "medicare_60_prim_dx_img",
    "medicare_90_prim_dx_img")
cancers[,prim_to_dx_img] <- 
  lapply(c(30, 60, 90), function(x) img_dx_test(cancers, x))
cancers$medicare_00_dx_dx <- as.numeric(cancers$counts_dx_matches >= 1)
                                                                          
#differences between subjects diagnosed by seer compared to medicare         
cancers$seer_br_2 <- with(cancers, ifelse(seer_bm_01 == 1, 2, seer_bm_01))   
cancers$seer_medicare <-                                                     
  factor(with(cancers, seer_br_2 + medicare_90_prim_dx_matches),             
         levels = 0:3,                                                       
         labels = c("None", "Medicare", "SEER", "Both"))                     

cancers$cs_size[as.numeric(cancers$cs_size) >= 988] <- NA

cancers$age_cut <- 
  cut(as.numeric(cancers$age_dx), c(65, 70, 75, 80, 85, 115), include.lowest = TRUE,
    labels = c("65 to 69", "70 to 74", "75 to 79", "80 to 84", "85+"))

cancers$brst_sub_v[cancers$which_cancer %in% c("lung", "skin")] <- "Not 2010+ Breast"

cancers$brst_sub_v[cancers$brst_sub_v %in% c("Her2+/HR-", "Her2+/HR+")] <- "Her2+/ Hr(+/-)"

cancers$erstatus_v <- paste("ER", cancers$erstatus_v)
cancers$prstatus_v <- paste("PR", cancers$prstatus_v)
cancers$her2_v <- paste("HER2", cancers$her2_v)

cancers$histo <- stringr::str_to_title(tolower(cancers$histo))

cancers$the_strata <- 
with(cancers, ifelse(which_cancer == "skin", icd_c, histo))

cancers$the_strata <- 
with(cancers, 
    ifelse(which_cancer == "breast" & dx_year > 2009, 
           brst_sub_v, the_strata))

cancers$the_strata <- stringr::str_to_title(cancers$the_strata)
cancers$the_strata <- 
  ifelse(cancers$the_strata == "Unknown", "Other", 
         cancers$the_strata)

old_values <- 
  c("Unknown", 
    "Mucinous Adenocarcinoma",
    "Adenoid Cystic & Cribriform Ca.",
    "Lobular And Other Ductal Ca.", 
    "Not 2010\\+ Breast" 
    #"Face", "Upper Limb", "Lower Limb"
    )
new_values <- 
  c("Other", 
    "Other", 
    "Other", 
    "Duct Carcinoma",
    "Other"
    #"Face or Limb", "Face or Limb", "Face or Limb"
   )

cancers$the_strata <- gsub_reduce(cancers$the_strata, old_values, new_values)

cancers$the_strata <- 
  ifelse(cancers$the_strata %in% c("Her2+/Hr-", "Her2+/Hr+"), 
         "Her2+/ Hr(+/-)", cancers$the_strata)

#this is not the most efficient approach
cancers <- 
  reduce2(list(c("N. Am. Native", "Hispanic", "Asian"), 
              c("N. Am. Native", "Hispanic", "Black", "Asian")), 
         c("breast", "skin"), 
         function(df, oths, cncr){
          df[["race_v"]] <- 
            ifelse(df[["which_cancer"]] == cncr & df[["race_v"]] %in% oths, 
                   "Other", df[["race_v"]])
            df},
          .init = cancers)











