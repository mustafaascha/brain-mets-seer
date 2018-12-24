
library(zeallot)
library(tidyverse)
devtools::load_all("augur")
devtools::load_all("manuscript")
#load("cache/before_recode.RData")
#library(tidyverse)
cancers <- read_csv("cache/cancers_prerecode.csv.gz", progress = FALSE)

seer_recode <- 
  partial(recoder_fn, recode_df = pedsf_utils$pedsf_recodes)

cancers <- seer_recode(cancers, "payer_dx1", "payerdx")

key_vars_to_recode <- 
  c(
    "radbrn",
    "csmetsdxliv_pub",
    "csmetsdxbr_pub",
    "csmetsdxb_pub",
    "csmetsdxlung_pub",
    "grade",
    "radsurg",
    "rad",
    "nhiade",
    "erstat",
    "intprim",
    "marst",
    "mat_type",
    "med_stcd",
    "numdigit",
    "prstat",
    "race",
    "sex",
    "srvmflag",
    "stat_rec",
    "typefu",
    "vrfydth",
    "her2rec",
    "brstsub"
  )

df_vars_to_recode <- 
  c(                  
    "rad_brn",
    "csmetsdxliv_pub",
    "csmetsdxbr_pub",
    "csmetsdxb_pub",
    "csmetsdxlung_pub",
    "grade",
    "rad_surg",
    "radiatn",
    "nhiade",
    "erstatus",
    "intprim",
    "mar_stat",
    "mat_type",
    "med_stcd",
    "numdigit",
    "prstatus",
    "race",
    "s_sex",
    "srv_time_mon_flag",
    "stat_rec",
    "typefup",
    "vrfydth",
    "her2",
    "brst_sub"
  )

#edit pedsf_utils to suit needs-------
unique_recodes <- 
  pedsf_utils$pedsf_recodes %>% distinct(Var, Code, .keep_all = TRUE)
to_lose <- 
  unique_recodes$Var == "race" & grepl("\\(", unique_recodes$Meaning)
unique_recodes <- unique_recodes[-which(to_lose),]
unique_recodes$Code[unique_recodes$Var == "reg_id"] <- 
  gsub("15", "", unique_recodes$Code[unique_recodes$Var == "reg_id"])
unique_recodes[unique_recodes %in% c("Blank", "blank")] <- ""
unique_recodes$Meaning[unique_recodes$Code == "1= 0%-<5% poverty"] <- 
  "0%-<5% poverty"
unique_recodes$Code[unique_recodes$Code == "1= 0%-<5% poverty"] <- 
  "1"
unique_recodes$Meaning[unique_recodes$Meaning == "Reviewed and confirmed that"] <- 
  "Reviewed and confirmed correct demo. characteristics"

problem_value <- "Grade I; grade i; grade 1; well differentiated; differentiated, NOS"
unique_recodes$Meaning[unique_recodes$Meaning == problem_value] <- "Grade I"

problem_value <-  "cell type not determined, not stated or not applicable"
unique_recodes$Meaning[unique_recodes$Meaning == problem_value] <- "NA"

problem_value <-  "Unmarried or domestic partner (same sex or opposite sex or unregistered)"
unique_recodes$Meaning[unique_recodes$Meaning == problem_value] <- "Unmarried"

unique_recodes$Meaning[unique_recodes$Meaning %in% c("B-cell", "T-cell")] <- NA

cancers <- 
  pedsf_utils$pedsf_recodes %>% 
  filter(Var == "reg_id") %>% 
  mutate(reg_at_dx = gsub("^15", "", Code)) %>% 
  select(reg_at_dx, reg_at_dx_v = Meaning) %>% 
  right_join(cancers)


pedsf_keygen <- pryr::partial(keygen, kdf = unique_recodes)
key_dfs      <- map2(df_vars_to_recode, key_vars_to_recode, pedsf_keygen)
cancers      <- reduce(key_dfs, lj_coerce, .init = cancers)

#more specific recoding=====================================


cancers$insrec_pub <-
  factor(cancers$insrec_pub, levels = 1:4,
         labels = c("Uninsured", "Medicaid", "Insured", "Insured Nonspecific")) %>%
  as.character()

cancers$beh03v <-
  factor(cancers$beh03v, levels = 2:3, labels = c("Carcinoid", "Malignant")) %>%
  as.character()

ssg_labels <-
cancers$d_ssg00 <-
  factor(cancers$d_ssg00, 
         levels = c(0:5, 7, 9), 
         labels = c("In situ", "Localized", 
                    "Regional, direct", "Regional, LN",
                    "Regional, ext", "Regional, NOS", 
                     "Distant", "Unknown")

        )

cancers$eod10_pn[cancers$eod10_pn == 99] <- NA
cancers$eod10_pn[cancers$eod10_pn == 98] <- NA
cancers$eod10_pn[cancers$eod10_pn == 97] <- NA
cancers$cs_mets[cancers$cs_mets == 99] <- NA
cancers$cs_mets[cancers$cs_mets == 98] <- NA
cancers$cs_size[as.numeric(cancers$cs_size) >= 988] <- NA

#histo_url <- "https://seer.cancer.gov/icd-o-3/sitetype.icdo3.d20150918.xls"
#download.file(histo_url, "~/brain-metastases/munge/seer_histo.xls")
histocodes <- readxl::read_xls("munge/seer_histo.xls")
names(histocodes) <-
  c("siterec", "sitedesc", "histo", "histodesc", "histobeh", "histobehdesc")
split_stuff <- strsplit(histocodes$histobeh, "/")
histocodes[,c("hist03v", "Behavior")] %<-%
  list(map_chr(split_stuff, 1), map_chr(split_stuff, 2))
histocodes$hist03v <- as.numeric(histocodes$hist03v)
h_codes <- histocodes
histocodes <-
  histocodes %>% select(hist03v, histodesc) %>%
  distinct(hist03v, .keep_all = TRUE) %>%
  filter(hist03v %in% unique(cancers$hist03v))

new_codes <- 
  c("8983", "8213", "8325", "9970", 
    "8711", "8392", "8825", "8406") %>% as.numeric()
histocodes <- 
  bind_rows(histocodes, 
    data.frame(hist03v =   new_codes, 
               histodesc = c("Adenomyoepitheliomia with carcinoma", 
                             "Serrated adenocarcinoma", 
                              "Granular cell carcinoma", 
                              "Myelodysplastic neoplasm", 
                              "Glomangiosarcoma", 
                              "Skin carcinoma", 
                              "Myofibroblastic sarcoma", 
                              "Adenocarcinoma NOS"), 
               stringsAsFactors = FALSE)
  )


histo_key <- with(histocodes, setNames(hist03v, histodesc))

cancers$hist03v_v <- 
  reduce2(histo_key, names(histo_key), .init = cancers$hist03v, 
    function(hst_vec, hk, n_hk) {
      if(hk %in% unique(hst_vec)) {
        ifelse(hst_vec == hk, n_hk, hst_vec)
      } else { 
        hst_vec
      }
    })
  

rm_unk <- function(x) {
  levels_to_collapse <- c("Unknown", "Not 1990+ Breast", "Not 2010+ Breast", "Borderline")
  ifelse(x %in% levels_to_collapse, "Other and unknown", x)
}

to_rm_unkn <- c("her2_v", "prstatus_v", "erstatus_v")

cancers[,to_rm_unkn] <- lapply(cancers[,to_rm_unkn], rm_unk)

cancers$d_ssg00[cancers$d_ssg00 == "Regional, NOS"] <- "Unknown"

cancers$grade_v[cancers$grade_v == "N K cell (natural killer cell)"] <- NA

cancers$mar_stat_v[cancers$mar_stat_v %in% c("Unknown", "Unmarried", "Separated")] <- 
  "Unknown or other"

cancers$rac_recy_v <-
  as.character(
    factor(cancers$rac_recy, levels = c(1:4, 9),
          labels = c("White", "Black",
                     "American Indian", "Asian/Pacific Islander", 
                      NA))
  ) 

source("munge/tnm.R")

write_csv(cancers, "cache/cancers_postrecode.csv.gz")
