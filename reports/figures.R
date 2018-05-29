

library(tidyverse) 
library(zeallot)
library(Hmisc)
library(rlang)
library(grid)
library(gridExtra)

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

  
#summarize by measures of interest, get crude per annum
by_sex  <- 
  make_by_sex(plot_df) %>% gp_cncr %>% 
  map(function(a_df) {
    a_df[a_df$measure == "SEER SBM", "present"] <- 
      round(a_df[a_df$measure == "SEER SBM", "present"] / 3)
    a_df[a_df$measure == "Medicare LBM", "present"] <- 
      round(a_df[a_df$measure == "Medicare LBM", "present"] / 5)
    a_df
  })
by_race <- 
  make_by_race(plot_df) %>% gp_cncr %>% 
  map(function(a_df) {
    a_df[a_df$measure == "SEER SBM", "present"] <- 
      round(a_df[a_df$measure == "SEER SBM", "present"] / 3)
    a_df[a_df$measure == "Medicare LBM", "present"] <- 
      round(a_df[a_df$measure == "Medicare LBM", "present"] / 5)
    a_df
  })

bp_theme <-
    theme_bw() + 
    theme(legend.position = "bottom", 
          legend.key.size = unit(0.35, "cm"))

ty_slant <- theme(axis.text.y = element_text(angle = 45, color = "black")) 
tx_slant <- 
  theme(axis.text.x = element_text(angle = 45, color = "black", hjust = 1))

race_size <- 3

the_plots[["breast_race"]] <-
  by_race$breast %>% filter(the_strata != "Duct Carcinoma") %>% 
  make_text_y(0.8, 4.7) %>% 
  modify_at("the_strata", ~ factor(.x, levels = c("Her2-/Hr+",  "Her2+/ Hr(+/-)", 
                                                  "Triple Negative"))) %>% 
  modify_at("present", censor_few) %>% 
  bp_this(race_v, sz = race_size) + 
  scale_fill_grey(start = 0, end = 0.8) + 
  bp_theme + 
  label_this("Breast", "") + 
  tx_slant

the_plots[["lung_race"]] <-
  plot_lung_df(by_race, 22, 30) %>% 
  bp_this(race_v, sz = race_size) + 
  scale_fill_grey(start = 0, end = 0.8) + 
  bp_theme + 
  label_this("Lung", "") + 
  coord_flip() + 
  ty_slant

sex_size <- 4

the_plots[["lung_sex"]] <-
  plot_lung_df(by_sex, 17, 24.5) %>% 
  bp_this(s_sex_v, sz = sex_size) + 
  scale_fill_grey(start = 0.2, end = 0.6) +
  bp_theme + 
  label_this("Lung", "") + 
  coord_flip() + 
  ty_slant

the_plots[["skin_sex"]] <-
  by_sex$skin %>% 
  make_text_y(2.25, 5.5) %>% 
  modify_at("the_strata", 
            ~ factor(.x, levels = c("Nevi & Melanomas", 
                                    "Mal. Mel. In Junct. Nevus"))) %>% 
  modify_at("the_strata",
            ~ forcats::fct_recode(.x,
                                  `MMJN` = "Mal. Mel. In Junct. Nevus",
                                  `N & M` = "Nevi & Melanomas"
            )) %>% 
  modify_at("the_strata", ~ factor(.x, levels = c("MMJN", "N & M"))) %>% 
  modify_at("present", censor_few) %>% 
  bp_this(s_sex_v, sz = sex_size) + 
  scale_fill_grey(start = 0.2, end = 0.6) +
  bp_theme + 
  label_this("Skin", "")




























