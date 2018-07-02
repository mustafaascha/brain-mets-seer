
library(tidyverse) 
library(zeallot)
library(Hmisc)

papes <- read_rds("paper_products.rds")

devtools::load_all("augur")
devtools::load_all("frequencies")


#which algorithm will be used for lifetime estimates==========================
which_algo_to_use <- "medicare_60_dx_img"

to <- table_ones <- 
  map(papes[["table_ones"]], make_table, which_algo = which_algo_to_use) %>% 
  map(suppress_columns, which_columns = 2:4)

#table-one for in-text reference=================
to <- 
  map(to, t_o_fn) %>% 
  modify_at(c("on", "op", "sn", "sp", "mn", "mp"), as.numeric) %>% 
  map(function(df, nm, vr) function(vr) df[grep(vr,df$v),])
names(to) <- map_chr(names(to), function(z) substr(z,1,1))

table_ones <- 
  map2(table_ones[c("breast", "lung", "skin")], 
      rm_rows_finder(table_ones), 
      rm_rnm_rws)

table_ones[["breast"]] <- 
  reduce2(c("Hr$", "Adenoid"), 
          c("Her2+/HR(+/-)", "Adenoid Car."), 
          breast_renamer,
          .init = table_ones[["breast"]])



#strat_ip========================================
strat_ip <- 
  papes$histo_annum %>% 
  do_ip(which_algo = which_algo_to_use, the_strata) %>% 
  select(which_cancer, algorithm, the_strata, group_total, show, algo_v) %>% 
  spread_rename_ip(histo = TRUE) %>% 
   bind_rows( 
     data.frame(Primary = "Breast",
                algorithm = "Seer Sync.",
                Histology = c("Duct Carcinoma"#, "Adenoid Cystic & Cribriform Ca.", 
                              #"Lobular And Ductal",              "Mucinous Adenocarcinoma"
                              ),
                group_total = "0", 
                Negative = " ** ", Positive = " ** ", XNA = " ** ", 
                stringsAsFactors = FALSE)) %>% 
  arrange(desc(algorithm), desc(Primary), Histology) %>% 
  nest_list_by_algo()
names(strat_ip[[2]]) <- paste("m", names(strat_ip[[2]]), sep = "_")

ip_show <- 
  bind_and_show(strat_ip, histo = TRUE) %>% 
  relabel_ip(histo = TRUE)

aair_strat <- 
  aair(papes$histo_age_over_time, 
       c("Algorithm", "Primary", "Stratum"), 
       the_strata)

aair_strat <- 
  bind_rows(data.frame(Primary = "", Stratum = "", 
                       SEER_SBM = "AAIR", Medicare_LBM = "AAIR", 
                       stringsAsFactors = FALSE), 
            aair_strat)

ip_aair <- list(overall = list(), by_histo = list())
ip_aair[["by_histo"]] <- 
  bind_cols(ip_show, aair_strat[,-(1:2)]) %>% 
  select_showvars()

#start site_ip=====      
site_ip <- 
  papes$histo_annum %>% 
  do_ip(which_algo = which_algo_to_use) %>% 
  select(which_cancer, algorithm, group_total, show, algo_v) %>% 
  spread_rename_ip() %>% 
  arrange(desc(algorithm), desc(Primary)) %>% 
  nest_list_by_algo()
names(site_ip[[2]]) <- paste("m", names(site_ip[[2]]), sep = "_")

site_ip_show <- bind_and_show(site_ip) %>% relabel_ip()
site_ip_show <- site_ip_show[-1,]

site_aair <- aair(papes$age_over_time, c("Algorithm", "Primary")) 

ip_aair[["overall"]] <- 
  bind_cols(site_ip_show, site_aair) %>% 
  mutate(Histology = "1_Overall") %>% 
  select_showvars()

#make ip_aair====
ip_aair <- 
  ip_aair %>% bind_rows() %>%
  modify_at("Site", 
            function(x) factor(x, levels = c("", "Lung", "Breast", "Skin"))
            ) %>% 
  arrange(Site, Histology) %>% 
  modify_at("Histology", function(x) gsub("^1_", "", x)) %>% 
  filter(Total != 0) 

single_digits <- c( "Present", "Absent", "Missing", "Present1", "Absent1")

ip_aair[-1,single_digits] <- 
  lapply(ip_aair[-1,single_digits], 
         compose(function(x) sprintf("%.1f", x), as.double))

#crude ========================
crude_df <- 
  clean_crude_sbm(papes$strata_only) %>% 
  filter(which_cancer == "breast")
  
pats_tr <- c("Her2", "Other", "Tripl")
grepats <- pryr::partial(grepl, pattern = paste0(pats_tr, collapse = "|"))
ia_to_replace <- which(grepats(ip_aair$Histology) & ip_aair$Site == "Breast")

#Using crude values because very low counts pf SEER SBM led to unstable estimates
ip_aair[["SEER"]][ia_to_replace] <- crude_df$IRS
ip_aair[["Total"]][ia_to_replace] <- 
  apply(crude_df[,c("Absent", "Present", "Missing")], 1, sum)

ip_aair[ia_to_replace,c("Present", "Absent", "Missing")] <-
  lapply(crude_df[, c("IPP", "IPA", "IPM")], sprintf, fmt = "%.1f")

ssr <- skin_suppress_r <- 15
ssc <- skin_suppress_c <- c(3, 5:7)
ip_aair[ssr,ssc] <- "**"

names(ip_aair) <- ip_aair_names_fn(names(ip_aair))

#in-text ip aair object ================================================
ipa <- munge_ip_for_intext(ip_aair)
show_inc <- ipa_shower(ipa)

# This is here for histocompatibility (to ensure histology names are consistently referred)
ipah <- ipa %>% group_by(which_cancer) %>% tidyr::nest()
ipah$hsts <- map(ipah$data, function(x) gsub("\\ ", "_", (unique(x[["histo"]]))))

# In-text reference function for incidence measures by each cancer/histology
ip <- 
  lapply(c("lung", "breast", "skin"), function(cncr) {
    hst_nms <- unlist(ipah$hsts[ipah$which_cancer == cncr])
    tr <- 
      map(hst_nms, function(hsto) function(tms) {
        ipa %>% filter(which_cancer == cncr & histo == hsto & timing == tms)
      })
    names(tr) <- hst_nms
    tr
  })
names(ip) <- c("l", "b", "s")

names(ip$b) <- gsub_reduce(names(ip$b), c("-", "\\+"), c("n", "p"))
names(ip$s) <- gsub_reduce(names(ip$s), c("\\.", "\\&_"), c("", ""))

#classification ===================================
show_class_freq <- pryr::partial(primary_classification, df = papes$classifimetry)
class_df <- show_class_freq(c("lung", "breast", "skin"))

#in-text SEER count object ==============================
sc <- class_df$class$SEER_Count

main_classification_table <- 
  prep_classifimetry(papes$classifimetry) %>% 
  (function(dfs) left_join(dfs[[1]], dfs[[2]])) %>% 
  modify_at("Primary_Cancer", str_to_title) %>% 
  modify_at("Claims_code", as.character) %>% 
  filter(Claims_code == "CNS Metastasis w/Diagnostic Imaging" & Timing == "Synchronous") %>% 
  select(-Timing, -Claims_code) %>% 
  rename(Medicare_Count = Count)
row.names(main_classification_table) <- NULL
colnames(main_classification_table) <- 
  gsub("_", " ", colnames(main_classification_table))

#in-text classification metrics stuff=====================
cm <- in_text_cm(class_df$class$metrics)
get_cm <- intxt_cm_fn_fctry(cm)
class_fns <- 
  map(c(l = "lung", b = "breast", s = "skin"), 
      function(the_cancer) {
        function(timing, codes) {
          get_cm(the_cancer, timing, the_codes = codes)
        }
      })

#stratum-specific classification===============
ct_selector <- prepare_classification_selector(papes$histo_metrics)

classification_table_to_show  <-
  bind_ct_tables(
    list(
      label1 = new_ct_row("**Algorithm: Synchronous**"),
      prim_dx_img = ct_selector("medicare_60_prim_dx_img"),
      label2 = new_ct_row("**Algorithm: Lifetime**"),
      spacer = new_ct_row(""),
      dx_img = ct_selector("medicare_60_dx_img")
    )
  )

supplementary_classification <-
  bind_ct_tables(
    list(
      label1 = new_ct_row("**Algorithm: Synchronous**"),
      prim_dx_img = ct_selector("medicare_60_prim_dx_matches"),
      label2 = new_ct_row("**Algorithm: Lifetime**"),
      spacer = new_ct_row(""),
      dx_img = ct_selector("medicare_00_dx_dx")
    )
  )

# Function for methods reference to histology codes =============================

hst_c <- histo_key_fn_fctry(papes$histo_key)


