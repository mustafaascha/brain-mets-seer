
library(tidyverse) 
library(zeallot)
library(Hmisc)

papes <- read_rds("paper_products.rds")

devtools::load_all("augur")
devtools::load_all("frequencies")

#munge_group_ip <- function(df, which_algo, ...) group_ip(munge_ip(df, which_algo), ...)

#which algorithm will be used for lifetime stuff==========================
which_algo_to_use <- "medicare_60_dx_img"


to <- table_ones <- 
  map(papes[["table_ones"]], make_table, which_algo = which_algo_to_use) %>% 
  map(suppress_columns, which_columns = 2:4)

#table-one for in-text reference=================
to <- 
  map(to, function(df){
    #names reflect: Variable, Overall, SEER SBM, Medicare LBM
      names(df) <- c("v", "o", "s", "m")
      qs <- function(df, x, y) {
        quietly(separate
                )(data = df, col = x, into = y, 
                  sep = "\\ \\(\\ {0,4}")[["result"]]
      }
      df <- 
        reduce2(c("o", "s", "m"), 
                list(c("on", "op"), c("sn", "sp"), c("mn", "mp")), 
                qs, .init = df) 
      to_fx <- c("op", "sp", "mp")
      df[,to_fx] <- lapply(df[,to_fx], function(z) gsub("\\)", "", z))
      df
    }) %>% 
  modify_at(c("on", "op", "sn", "sp", "mn", "mp"), as.numeric) %>% 
  map(function(df, nm, vr) function(vr) df[grep(vr,df$v),])
names(to) <- map_chr(names(to), function(z) substr(z,1,1))


rm_rws <- function(to, to_rm) {
  to <- to[-to_rm,]
  rownames(to) <- NULL
  to[["Variable"]] <- 
    gsub_reduce(c("\\(.*", 
                  "age_dx",   "a ge_cut", 
                  "cs_mets",  "eod10_pn", 
                  "beh03v",   "cs_size",
                  "d_ssg00",  "race_v",
                  "histo",    
                  "prstatus_v", "erstatus_v", "her2_v", "brst_sub_v",
                  "csmetsdxb_pub_v", "csmetsdxliv_pub_v", "csmetsdxlung_pub_v",
                  "_",        "\\ ="
    ),
    c("", 
      "Age (continuous)", "Age (categorical)", 
      "Number of metastases found at primary diagnosis", 
      "Number of metastasis-positive nodes at primary diagnosis", 
      "Behavior", "Size (mm)",
      "SEER Stage", "Race",
      "Histology", 
      "PR Status", "ER Status", "HER2 Status", "SEER Subtype",
      "Bone metastases", "Liver metastases", "Lung metastases",
      " ", ":"
    ), 
    to[["Variable"]])
  to
}

table_ones <- 
  map2(table_ones[c("breast", "lung", "skin")], 
       list(
         breast_rows_to_rm =   
           reduce(c("llt",     "other", "ulf", "icd_c",
                    "cs_mets", "eod10_pn", "beh03v"), 
                  function(a, z) c(a, grep(z, table_ones[["breast"]][["Variable"]])), 
                  .init = c()), 
         lung_rows_to_rm =   
           reduce( c("brst_sub", "PR",       "HER2",  "ER",  
                     "her2_v",   "llt",      "other", "ulf", 
                     "icd_c",    "status",   "csmetsdxlung_pub_v", 
                     "cs_mets",  "eod10_pn", "beh03v"), 
                 function(a, z) c(a, grep(z, table_ones[["lung"]][["Variable"]])), 
                 .init = c()), 
         skin_rows_to_rm =
           reduce(c("brst_sub", "PR",       "HER2",  "ER",  
                    "her2_v",   "llt",      "other", "ulf", 
                    "icd_c",    "status",   "csmetsdxlung_pub_v", 
                    "cs_mets",  "eod10_pn", "beh03v"), 
                  function(a, z) c(a, grep(z, table_ones[["skin"]][["Variable"]])), 
                  .init = c())
            ), 
      rm_rws
      )

table_ones[["breast"]] <- 
  reduce2(c("Hr$", "Adenoid"), 
          c("Her2+/HR(+/-)", "Adenoid Car."), 
          function(the_table, ptrn, replacement) {
            the_table[["Variable"]] <- 
              ifelse(grepl(ptrn, the_table[["Variable"]]), 
                     replacement, the_table[["Variable"]])
            the_table
          },
          .init = table_ones[["breast"]])

#classification ===================================

show_class_freq <- pryr::partial(primary_classification, df = papes$classifimetry)
class_df <- show_class_freq(c("lung", "breast", "skin")) %>% back_up("class_df")

#strat_ip========================================
#group_ip(munge_ip(df, which_algo), ...)
strat_ip <- 
  papes$histo_annum %>% do_ip(which_algo = which_algo_to_use, the_strata) %>% 
  back_up("strat_ip_munged") %>% 
  select(which_cancer, algorithm, the_strata, group_total, show, algo_v) %>% 
  spread_rename_ip(histo = TRUE) %>% 
  back_up("strat_ip_spread") %>% 
   bind_rows( 
     data.frame(Primary = "Breast",
                algorithm = "Seer Sync.",
                Histology = c("Duct Carcinoma"#, "Adenoid Cystic & Cribriform Ca.", 
                              #"Lobular And Ductal",              "Mucinous Adenocarcinoma"
                              ),
                group_total = "0", 
                Negative = " ** ", Positive = " ** ", XNA = " ** ", 
                stringsAsFactors = FALSE)) %>% 
  arrange(desc(algorithm), desc(Primary), Histology) %>% nest_list_by_algo()
names(strat_ip[[2]]) <- paste("m", names(strat_ip[[2]]), sep = "_")

ip_show <- 
  bind_and_show(strat_ip, histo = TRUE) %>% 
  back_up("ip_show_bound") %>% 
  relabel_ip(histo = TRUE) %>% 
  back_up("ip_show_munged") 

aair_strat <- 
  papes$histo_age_over_time %>% filter(!is.na(cnt)) %>% 
  aair_by(which_algo_to_use, algo, which_cancer, the_strata) %>% 
  rates_fn(gp_nms = c("Algorithm", "Primary", "Stratum")) %>% ungroup() %>% 
  select(-crude) %>% spread(Algorithm, AAIR) %>% 
  reorder_primary() %>% 
  select(Primary, Stratum, SEER_SBM = seer_bm_01, Medicare_LBM = which_algo_to_use)


aair_strat <- 
  bind_rows(data.frame(Primary = "", Stratum = "", 
                       SEER_SBM = "AAIR", Medicare_LBM = "AAIR", 
                       stringsAsFactors = FALSE), 
            aair_strat)

ip_aair <- list(overall = list(), by_histo = list())
ip_aair[["by_histo"]] <- bind_cols(ip_show, aair_strat[,-(1:2)]) %>% select_showvars()

#start site_ip=====      
site_ip <- 
  papes$histo_annum %>% do_ip(which_algo = which_algo_to_use) %>% 
  select(which_cancer, algorithm, group_total, show, algo_v) %>% 
  spread_rename_ip() %>% 
  arrange(desc(algorithm), desc(Primary)) %>% nest_list_by_algo()
names(site_ip[[2]]) <- paste("m", names(site_ip[[2]]), sep = "_")

site_ip_show <- bind_and_show(site_ip) %>% relabel_ip()
site_ip_show <- site_ip_show[-1,]

site_aair <- 
  papes$age_over_time %>% 
  filter(!is.na(cnt)) %>% 
  aair_by(which_algo_to_use, algo, which_cancer) %>%
  rates_fn(gp_nms = c("Algorithm", "Primary")) %>% 
  ungroup() %>% 
  select(-crude) %>% 
  spread(Algorithm, AAIR) %>% 
  reorder_primary() %>% 
  select(Primary, 
         SEER_SBM = seer_bm_01, 
         Medicare_LBM = which_algo_to_use)

ip_aair[["overall"]] <- 
  bind_cols(site_ip_show, site_aair) %>% 
  mutate(Histology = "1_Overall") %>% 
  select_showvars()

#make ip_aair====
ip_aair <- 
  ip_aair %>% bind_rows() %>%
  modify_at("Site", function(x) factor(x, levels = c("", "Lung", "Breast", "Skin"))) %>% 
  arrange(Site, Histology) %>% 
  modify_at("Histology", function(x) gsub("^1_", "", x)) %>% 
  filter(Total != 0) 

#=======================================
#crude stuff========================
crude_df <- 
  papes$strata_only  %>% 
  clean_crude_sbm() %>% 
  filter(which_cancer == "breast")
  
pats_tr <- c("Her2", "Other", "Tripl")
grepats <- pryr::partial(grepl, pattern = paste0(pats_tr, collapse = "|"))
ia_to_replace <- which(grepats(ip_aair$Histology) & ip_aair$Site == "Breast")
#Using crude values because very low proportion in SEER SBM led to unstable estimates
ip_aair[["SEER"]][ia_to_replace] <- crude_df$IRS
ip_aair[["Total"]][ia_to_replace] <- 
  apply(crude_df[,c("Absent", "Present", "Missing")], 1, sum)

ip_aair[ia_to_replace,c("Present", "Absent", "Missing")] <-
  lapply(crude_df[, c("IPP", "IPA", "IPM")], sprintf, fmt = "%.2f")

ip_aair[["SEER"]][ip_aair$Site == "Skin" & ip_aair$Histology == "0 To 4"] <- 
  (clean_crude_sbm(papes$strata_only) %>% 
   filter(which_cancer == "skin" & the_strata  == "0 To 4"))$IRS

names(ip_aair) <- 
  gsub_reduce(c("1$", "SEER",     "Medicare",     
                "Present", "Absent", "Missing", "Total"), 
              c("",   "SEER SBM", "Medicare LBM", 
                "(+)", "\\(-)",  "NA", "At-risk"), 
              names(ip_aair))

#in-text ip aair object ================================================
ipa <- ip_aair
names(ipa) <- 
  c("which_cancer", "histo", "sbm_aair", "sbm_cnt", "sbm_ip", 
    "sbm_neg", "sbm_na", "lbm_aair", "lbm_cnt", "lbm_ip", "lbm_neg")

ipa <- ipa %>% slice(-1) %>% 
  modify_at( c("sbm_aair", "lbm_aair"), 
         function(aair_vct) gsub("\\)", "", aair_vct)) %>% 
  modify_at(c("which_cancer", "histo"), tolower) %>% 
  modify_at("histo", function(x) gsub("(\\(|\\/).*", "", x))

#separate this, separate into, separate by
to_sep <- 
  list(
    list("sbm_aair", c("sbm_aair", "sbm_ci"), "\\ \\("), 
    list("sbm_ci",   c("sbm_lci", "sbm_hci"), "-"), 
    list("lbm_aair", c("lbm_aair", "lbm_ci"), "\\ \\("), 
    list("lbm_ci",   c("lbm_lci", "lbm_hci"),  "-")
  )

ipa <- 
  reduce(to_sep, 
         function(df, rs) {
            c(to_s, in2, s_by) %<-% rs
            separate(df, to_s, in2, s_by)
          }, 
         .init = ipa)

ipa <- 
  gather(ipa, msr, val, -which_cancer, -histo) %>% 
  modify_at("histo", function(z) gsub("triple\\ ", "triple", z)) %>% 
  separate(msr, c("which", "msr"), sep = "_") %>% 
  filter(histo != "") %>% 
  spread(which, val) %>% 
  gather(timing, value, -which_cancer, -histo, -msr) %>% 
  spread(msr, value) %>% 
  data.frame(stringsAsFactors = FALSE) %>% 
  modify_at("ip", ~ sprintf(fmt = "%.1f", as.numeric(.x)))


back_up(ipa)


#====================================

show_inc <- function(cncr, hsto, times) {
  ipa[with(ipa, which_cancer == cncr & histo == hsto & timing == times),]
}

histos <- ipa %>% group_by(which_cancer) %>% tidyr::nest()
histos$hsts <- map(histos$data, function(x) gsub("\\ ", "_", (unique(x[["histo"]]))))

#make a function to retrieve incidence measures for each cancer/histology
ip <- 
  lapply(c("lung", "breast", "skin"), function(cncr) {
    hst_nms <- unlist(histos$hsts[histos$which_cancer == cncr])
    tr <- #to_return 
      map(hst_nms, function(hsto) function(tms) {
        ipa %>% filter(which_cancer == cncr & histo == hsto & timing == tms)
      })
    names(tr) <- hst_nms
    tr
  })
names(ip) <- c("l", "b", "s")
#ip[["lengths"]] <- map_int(ip[c("l", "b", "s")], length)

names(ip$b) <- gsub("-", "n", names(ip$b))
names(ip$b) <- gsub("\\+", "p", names(ip$b))

#exclusion tables ========================================================

med_e_v <- c("hmocnt", "age_dx", "med_stcd", "payerdx_v")
med_e_nv <- c("Used HMO", "Age < 65 years", "Non-age Medicare Qual", "Insurance")
med_e <- gen_e <- papes$exclusion
med_e <- med_e[med_e$Vars %in% med_e_v,-1]

med_e$Vars <- 
  reduce2(med_e_v, med_e_nv, function(i, tr, r) gsub(tr, r, i), .init = med_e$Vars)
rownames(med_e) <- NULL

med_e <- med_e %>% select(-Values) %>% group_by(Vars) %>% 
  summarise_all(function(x) sum(as.numeric(x))) 

gen_e <- gen_e[gen_e$Vars %in% setdiff(gen_e$Vars, med_e_v),]
gen_e$Values[gen_e$Vars %in% c("race", "srv_time_mon", "vrfydth")] <- 
  "Unknown race, survival time, or death not verified"
gen_e <- gen_e %>% slice(-1) %>% select(Values, breast, lung, skin) %>% 
  group_by(Values) %>% summarise_all(function(x) sum(as.numeric(x))) 
names(gen_e) <- c("Vars", "breast", "lung", "skin")
gen_e$Vars[1:3] <- 
  c("Autopsy/Death certificate diagnosis", 
    "Diagnosis before 2008", "Multiple primary diagnoses")

#clean exclusion up a bit =========
names(med_e) <- str_to_title(names(med_e))
names(gen_e) <- str_to_title(names(gen_e))

exclusionary <- 
  mutate(gen_e, Purpose = "General") %>% 
  bind_rows(mutate(med_e, Purpose = "Medicare")) %>% 
  select(Purpose, everything()) %>% 
  modify_at("Purpose", perma_dupes)
names(exclusionary)[2] <- ""

rm(med_e, gen_e)
#in-text SEER count object ==============================

sc <- class_df$class$SEER_Count

#in-text classification metrics stuff=====================
cm <- 
  class_df$class$metrics %>% 
  (function(df) {
    vls <- strsplit(df[["Kappa"]], split = "[^0-9.]")
    df["kli"] <- map_chr(vls, 3)
    df["kui"] <- map_chr(vls, 6)
    df[["Kappa"]] <- map_chr(vls, 1) 
    df
   }) %>% 
  modify_at(c("Timing", "Primary_Cancer"), 
            function(vctr) {
              for (i in seq_along(vctr)) {
                if (vctr[i] == "") {
                  vctr[i] <- vctr[i - 1]
                }
              }
              vctr
            }) %>% 
  rename(the_time = Timing, the_cancer = Primary_Cancer, codes = Claims_code) %>% 
  modify_at("codes", function(vctr){
    vctr[vctr == "CNS Metastasis"] <- "dx"
    vctr[vctr == "CNS Metastasis w/Diagnostic Imaging"] <- "dximg"
    vctr
  }) %>%  back_up("zzz2") %>% 
  modify_at("the_time", function(vctr) {
    vctr[vctr == "Synchronous"] <- "sync"
    vctr[vctr == "Lifetime"] <- "life"
    vctr
  }) %>% 
  modify_at(c("Sensitivity", "PPV", "Kappa", "kli", "kui"), 
            function(thing) sprintf("%.2f", as.numeric(thing)))

#stopifnot(identical(cm[,4:7], class_df$class$metrics[,4:7]))

get_cm <- function(which_cancer, timing, the_codes){
  cm[with(cm,the_time == timing & the_cancer == which_cancer & codes == the_codes),]
}
#I prefer not to do a nested map, but this seems to be an easy way to expand..how can curry? 
class_fns <- 
  map(c(l = "lung", b = "breast", s = "skin"), 
      function(the_cancer) {
        function(timing, codes) get_cm(the_cancer, timing, the_codes = codes)
        })

#==============================================================

#stratum-specific classification===============

show_strat_class <- partial(primary_classification, df = papes$histo_metrics)

sync_strat_class <- 
  papes$histo_metrics %>% 
  filter(measure %in% c("medicare_60_prim_dx_img", 
                        "medicare_60_prim_dx_matches")) %>% 
  modify_at("measure", ~ ifelse(.x == "medicare_60_prim_dx_img", 
                                "CNS Mets. w/Diagnostic Imaging", 
                                "CNS Metastasis")) %>% 
  clean_strat_class()
  
  
life_strat_class <- 
  papes$histo_metrics %>% 
  filter(measure %in% c("medicare_60_dx_img", 
                        "medicare_00_dx_dx")) %>% 
  modify_at("measure", ~ ifelse(.x == "medicare_60_dx_img", 
                                "CNS Mets. w/Diagnostic Imaging", 
                                "CNS Metastasis")) %>% 
  clean_strat_class()

# For description of histology codes in methods =============================

histo_key <- 
  papes$histo_key %>% 
  select(-Freq) %>% 
  modify_at("histo", 
            function(z) str_to_title(gsub("\\,\\ NOS$", "", z))) %>% 
  modify_at("histo", 
            function(z) tolower(gsub("Mal\\.\\ Mel\\.\\ In\\ Junct\\.\\ Nevus", 
                                     "malignant Melanoma in junctional nevus", z))) %>% 
  modify_at("histo", 
            function(z) gsub("ca\\.$", "carcinoma", z)) %>% 
  group_by(which_cancer, histo) %>% 
  nest()

#I don't need to do this biggest/smallest stuff...
histo_key[["codes"]] <- 
  map_chr(histo_key[["data"]], 
          function(df) {
            df[["hist03v"]] <- as.numeric(as.character(df[["hist03v"]]))
            df <- arrange(df, hist03v)
            biggest <- max(df[["hist03v"]], na.rm = TRUE)
            smallest <- min(df[["hist03v"]], na.rm = TRUE)
            if (biggest == (smallest + nrow(df) - 1) & nrow(df) > 1) {
              return(paste(smallest, biggest, sep = "-"))
            } else {
              rle_hyp(df[["hist03v"]])
            }
          })  

hst_c <- function(cnc, hst) {
  if (is.numeric(hst)) { 
    which_hst <- hst
  } else if (is.character(hst)) {
    which_hst <- grep(hst, histo_key$histo[histo_key$which_cancer == cnc])
  }
  hst <- histo_key$histo[histo_key$which_cancer == cnc][which_hst]
  with(histo_key, 
    list(h = hst, c = codes[which_cancer == cnc & histo == hst]))
}














