
library(tidyverse)
devtools::load_all("augur")
#load("cache/before_recode.RData")
#library(tidyverse)
cancers <- read_csv("cache/cancers_prerecode.csv.gz", progress = FALSE)

#cancers[["dx_code_count"]] <-  
#    ifelse(is.na(cancers[["dx_code_count"]]), 0, cancers[["dx_code_count"]])
                                                                             
seer_recode <- function(pedsf_df, key_var, df_var) {
  p_recodes <- 
    pedsf_utils$pedsf_recodes %>% 
    distinct(Code, .keep_all = TRUE)
  key_df <- 
    p_recodes[p_recodes$Var == key_var, c("Code", "Meaning")]
  new_name <- paste0(df_var, "_v")
  names(key_df) <- c(df_var, new_name)
  to_return <- left_join(pedsf_df, key_df)
  #to_return <- to_return[,names(to_return) != df_var]
  #names(to_return)[new_name == names(to_return)] <- df_var
  to_return
}

cancers <- seer_recode(cancers, "payer_dx1", "payerdx")


key_vars_to_recode <- 
c("radbrn",
"csmetsdxliv_pub",
"csmetsdxbr_pub",
"csmetsdxb_pub",
"csmetsdxlung_pub",
"grade",
"radsurg",
"rad",
"nosrg",
"nhiade",
"adjajc6t",
"adjajc6n",
"adjajc6m",
"origin",
"dajccstg",
"dxconf",
"erstat",
"histrec",
"intprim",
"linkflag",
"m_sex",
"marst",
"mat_type",
"med_stcd",
"numdigit",
"other_tx1",
"prstat",
"race",
"rsncd1",
"sex",
"srvmflag",
"sssurg",
"stat_rec",
"typefu",
"vrfydth",
"yobflg1",
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
"no_surg",
"nhiade",
"t_value",
"n_value",
"m_value",
"origin",
"dajcc7stg",
"dx_conf",
"erstatus",
"histrec",
"intprim",
"linkflag",
"m_sex",
"mar_stat",
"mat_type",
"med_stcd",
"numdigit",
"othr_rx",
"prstatus",
"race",
"rsncd1",
"s_sex",
"srv_time_mon_flag",
"ss_surg",
"stat_rec",
"typefup",
"vrfydth",
"yobflg1",
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
#technique #1 --------------------------------------

#for_recode <- function(vr, k_vr, k_df){
#  k_df <- k_df[k_df$Var == k_vr, c("Code", "Meaning")]
#  factor(factor(vr, levels = k_df$Code, labels = k_df$Meaning))
#}
#for_seer_recode <- pryr::partial(for_recode, k_df = unique_recodes)
#
#UNCOMMENT THESE TO RUN THE "for_recode" FUNCTIONS
#cancers[,paste(df_vars_to_recode, "v", sep = "_")] <- 
#cancers[,df_vars_to_recode] <- 
#  map2(cancers[,df_vars_to_recode], key_vars_to_recode, for_seer_recode)

#----------------------------------------------------------

#technique #2 --------------------------------------

#The general idea, written as pipes (with an extra gsub)
cancers <- 
  pedsf_utils$pedsf_recodes %>% 
  filter(Var == "reg_id") %>% 
  mutate(reg_at_dx = gsub("^15", "", Code)) %>% 
  select(reg_at_dx, reg_at_dx_v = Meaning) %>% 
  right_join(cancers)


keygen <- function(kdf, df_var, key_df_var) {
  kdf <- kdf[kdf[["Var"]] == key_df_var,]
  kdf <- kdf[!duplicated(kdf),c("Code", "Meaning")]
  names(kdf) <- c(df_var, paste0(df_var, "_v"))
  kdf
}
pedsf_keygen <- pryr::partial(keygen, kdf = unique_recodes)
key_dfs <- map2(df_vars_to_recode, key_vars_to_recode, pedsf_keygen)

library(zeallot)
lj_coerce <- function(df1, df2){
  join_by <- intersect(names(df1), names(df2))
  if(length(join_by) != 1) {
    stop("Can't coerce more than one 'by' column")
  }
  c(df1[[join_by]], df2[[join_by]]) %<-%
    lapply(list(df1[[join_by]], df2[[join_by]]), as.character)
  left_join(df1, df2)
}

cancers <- reduce(key_dfs, lj_coerce, .init = cancers)

#more specific recoding=====================================


cancers$urbrur <-
  factor(as.numeric(cancers$urbrur), levels = c(1:5,9),
        labels = c("Big Metro", "Metro", "Urban",
                  "Less Urban", "Rural", "Unknown")) %>% as.character()

cancers$insrec_pub <-
  factor(cancers$insrec_pub, levels = 1:4,
         labels = c("Uninsured", "Medicaid", "Insured", "Insured Nonspecific")) %>%
  as.character

cancers$beh03v <-
  factor(cancers$beh03v, levels = 2:3, labels = c("Carcinoid", "Malignant")) %>%
  as.character

ssg_labels <-
  c("In situ", "Localized", "Regional, direct", "Regional, LN",
    "Regional, ext", "Regional, NOS", "Distant", "Unknown")
cancers$d_ssg00 <-
  factor(cancers$d_ssg00, levels = c(0:5, 7, 9), labels = ssg_labels)
rm(ssg_labels)

cancers$eod10_pn[cancers$eod10_pn == 99] <- NA
cancers$eod10_pn[cancers$eod10_pn == 98] <- NA
cancers$eod10_pn[cancers$eod10_pn == 97] <- NA
cancers$cs_mets[cancers$cs_mets == 99] <- NA
cancers$cs_mets[cancers$cs_mets == 98] <- NA
cancers$cs_size[as.numeric(cancers$cs_size) >= 988] <- NA



#histo_url <- "https://seer.cancer.gov/icd-o-3/sitetype.icdo3.d20150918.xls"
#download.file(histo_url, "~/brain-metastases/documentation/seer_histo.xls")
histocodes <-
  readxl::read_xls("documentation/seer_histo.xls")
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

#These warnings may indicate incompatibility with R3.4
#cancers$hist03v_v <-
#  suppressWarnings(
#    factor(cancers$hist03v, levels = histocodes$hist03v,
#         labels = histocodes$histodesc))

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




#site_url <- "https://www.cms.gov/Medicare/Coding/ICD10/Downloads/2016-Code-Descriptions-in-Tabular-Order.zip"
#download.file(site_url, "~/brain-metastases/documentation/icd/icd10.zip")
#!/bin/bash
#cd documentation
#unzip icd10.zip
#cd ..

#cancers <- 
#  left_join(cancers,
#    read_table("documentation/icd/icd10cm_codes_2016.txt", 
#              col_names = c("icdot10v", "icd10")))

#cancers$site <-
#  gsub("(Malignant\\ neoplasm\\ of|Carcinoma|Melanoma|Malignant\\ Melanoma)(\\ in\\ situ)?(\\ of)?\\ ", 
#    "", cancers$icd10)
#
#cancers <- left_join(cancers, site_conversions)

#using ICD data from GNUHealth 
# http://health.gnu.org/
# file:///home/mustafa/Downloads/gnuhealth-3.2.9/health_icd10/data/diseases.xml

library(XML)

dx_xml <- xmlParse("documentation/diseases.xml", useInternalNodes = TRUE)
dx_xml <- xmlToList(dx_xml)[[1]] 
dx_xml <- dx_xml[-length(dx_xml)]

diseases <- 
  data.frame(icdot10v = gsub("\\.", "", map_chr(dx_xml, function(x) x[[2]][[1]])), 
             icd10 = map_chr(dx_xml, function(x) x[[1]][[1]]), 
             stringsAsFactors = FALSE)

cancers <- left_join(cancers, diseases)

#cancers$site <-
#  gsub("(Malignant\\ neoplasm\\ of|Carcinoma|Melanoma|Malignant\\ Melanoma)(\\ in\\ situ)?(\\ of)?\\ ", 
#    "", cancers$icd10)


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
to_reps <- 
  c("[lL]ower\\ [lL]imb", "[tT]run[ck](al)?", "[Ff]ace", "[uU]pper\\ [lL]imb", 
    "[sS]calp|[nN]eck|[eE]ar|[eE]ye|[lL]ip")
replacements <- c("llt", "llt", "ulf", "ulf", "ulf") 

cancers <- 
  reduce2(to_reps, replacements, 
    function(df, to_rep, repmnt) {
      df[["icd"]] <- ifelse(grepl(to_rep, df[["icd"]]), repmnt, df[["icd"]])
      df
    },
    .init = cancers)  
skins <- which(cancers$which_cancer == "skin")
cancers$icd_c[skins] <- 
  ifelse((cancers$icd[skins] %in% c("llt", "ulf")), cancers$icd, "other")
  

cancers$rac_recy_v <-
  as.character(
    factor(cancers$rac_recy, levels = c(1:4, 9),
          labels = c("White", "Black",
                     "American Indian", "Asian/Pacific Islander", 
                      NA))
  ) 



write_csv(cancers, "cache/cancers_postrecode.csv.gz")
