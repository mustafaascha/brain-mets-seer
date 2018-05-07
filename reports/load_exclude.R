
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
      "payerdx_v"
      #, "all_same_cancer"
      )
vte <- values_to_exclude <-
    c("Used HMO", "Under 65 years", "Before 2008",
      "Autopsy/Death Certificate",
      "ESRD only", "Aged with ESRD", "Disabled",
      "Disabled with ESRD", "Unknown", "9999",
     #"Used HMO", "Age < 65", 
      "No", "Insurance status unknown",
      "Not insured"
      #, "Unaccounted primaries present"
      )
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

cancers$depth <- as.numeric(cancers$cs_ssf1) / 10

cancers$depth[cancers$cs_ssf1 %in% c("988", "998", "999")] <- NA

cancers$breslow <- 
  cut(cancers$depth, 
      breaks = c(0, 1, 4, Inf), 
      labels = c("Less than 1", "1 to 4", "More than 4")) %>% 
  as.character()

cancers$breslow <- 
  cut(cancers$depth, 
      breaks = c(0, 4, Inf), 
      labels = c("0 to 4", "More than 4")) %>% 
  as.character()

cancers$breslow[is.na(cancers$breslow)] <- "Missing"

cancers$the_strata <- cancers$histo
  #with(cancers, ifelse(which_cancer == "skin", breslow, histo))

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

cancers$the_strata <- 
  reduce2(old_values, new_values, 
          function(init, a, b) gsub(a, b, init),
          .init = cancers$the_strata)

cancers$the_strata <- 
  ifelse(cancers$the_strata %in% c("Her2+/Hr-", "Her2+/Hr+"), 
         "Her2+/ Hr(+/-)", cancers$the_strata)

cancers$race_v <- cancers$rac_recy_v

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

hispanic_origin_values <- 
  c("Cuban", 
    "Dominican Republic", 
    "Mexican", 
    "NHIA Surname Match Only", 
    "Other specified Spanish/Hispanic Origin including Europe",
    "Puerto Rican", 
    "South or Central American excluding Brazil", 
    "Spanish/Hispanic/Latino, NOS")

cancers[["race_v"]] <- 
  ifelse(cancers[["nhiade_v"]] %in% hispanic_origin_values & 
         cancers[["race_v"]] == "White", 
         "White Hispanic", 
         gsub("White", "White Non-Hispanic", cancers[["race_v"]]))
         














