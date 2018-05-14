

library(tidyverse) 
library(zeallot)
library(Hmisc)
library(rlang)

papes <- read_rds("paper_products.rds")
devtools::load_all("augur")
devtools::load_all("frequencies")

if (!exists("which_algo_to_use")) { which_algo_to_use <- "medicare_60_dx_img" }
the_plots <- list()

#bring the SEER and Medicare BM estimates together
plot_df <- 
  map2(c("seer_bm_01", which_algo_to_use), c("SEER SBM", "Medicare LBM"),
      function(vr, nm) mutate(papes$age_sex_race_strat[[vr]], measure = nm) %>% 
        rename(Present = !!sym(vr))) %>% 
    reduce(compose(tbl_df, bind_rows)) %>% 
  modify_at("measure", ~ factor(.x, levels = c("SEER SBM", "Medicare LBM"))) %>% 
  filter(race_v != "Other" & the_strata != "Other")
  
#summarize by measures of interest
by_sex  <- make_by_sex(plot_df) %>% gp_cncr
by_race <- make_by_race(plot_df) %>% gp_cncr

bp_theme <-
    theme_bw() + 
    theme(axis.text.y = element_text(angle = 45, color = "black"), 
          legend.position = "bottom")

the_plots[["lung_strat_sex"]] <-
  plot_lung_df(by_sex, 0.17, 0.245) %>% 
  bp_this(s_sex_v) + 
  scale_fill_grey(start = 0.2, end = 0.6) +
  bp_theme + 
  label_this_histo("Lung", "Sex")


the_plots[["lung_strat_race"]] <-
  plot_lung_df(by_race, 0.2, 0.26) %>% 
  bp_this(race_v) + 
  scale_fill_grey(start = 0, end = 0.8) + 
  bp_theme + 
  label_this_histo("Lung", "Race")

the_plots[["skin_strat_sex"]] <-
  by_sex$skin %>% 
  make_text_y(0.0225, 0.055) %>% 
  modify_at("the_strata", 
            ~ factor(.x, levels = c("Nevi & Melanomas", 
                                    "Mal. Mel. In Junct. Nevus"))) %>% 
  modify_at("present", censor_few) %>% 
  bp_this(s_sex_v) + 
  scale_fill_grey(start = 0.2, end = 0.6) +
  bp_theme + 
  label_this_histo("Skin", "Sex")

the_plots[["breast_strat_race"]] <-
  by_race$breast %>% filter(the_strata != "Duct Carcinoma") %>% 
    make_text_y(0.008, 0.047) %>% 
    modify_at("the_strata", ~ factor(.x, levels = c("Her2-/Hr+",  "Her2+/ Hr(+/-)", 
                                                    "Triple Negative"))) %>% 
    modify_at("present", censor_few) %>% 
    bp_this(race_v) + 
    scale_fill_grey(start = 0, end = 0.8) + 
    bp_theme + 
    labs(y = "Incidence Proportion", x = "Breast cancer subtype", fill = "Race")



























