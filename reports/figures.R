

library(tidyverse) 
library(zeallot)
library(Hmisc)
library(rlang)

papes <- read_rds("paper_products.rds")
devtools::load_all("augur")
devtools::load_all("frequencies")

if(!exists("which_algo_to_use")) { which_algo_to_use <- "medicare_60_dx_img" }
the_plots <- list()

#bring the SEER and Medicare BM estimates together
plot_df <- 
  map2(c("seer_bm_01", which_algo_to_use), c("SEER SBM", "Medicare LBM"),
      function(vr, nm) mutate(papes$age_sex_race_strat[[vr]], measure = nm) %>% 
        rename(Present = !!sym(vr))) %>% 
    reduce(compose(tbl_df, bind_rows)) %>% 
  modify_at("measure", ~ factor(.x, levels = c("SEER SBM", "Medicare LBM")))
  
#summarize by measures of interest

by_sex <- make_lung_by_sex(plot_df)
by_race <- make_by_race(plot_df)

# by_age <- 
#   group_by(plot_df, which_cancer, age_cut, the_strata, measure) %>% 
#   tidyr::nest()
# 
# by_age[["data"]] <- map(by_age[["data"]], make_plot_ips) %>% tbl_df
# by_age <- unnest(by_age)


the_plots[["lung-strat-sex"]] <-
  by_sex %>% filter(which_cancer == "lung") %>% 
  modify_at("the_strata", 
            ~ factor(.x, levels = c("Other", "Adenocarcinoma", "Carcinoma", 
                                    "SCLC", "NSCLC", "Squamous CC"))) %>% 
  ggplot(aes(x = the_strata, y = IP, fill = s_sex_v)) + 
  facet_wrap(~ measure, scales = "free_x") + 
  geom_bar(stat = "identity", position = position_dodge(), color = "black") + 
  coord_flip() + 
  scale_fill_grey(start = 0.2, end = 0.6) + 
  theme_bw() + 
  theme(axis.text.y = element_text(angle = 45), 
        legend.position = "bottom") + 
  labs(y = "Incidence Proportion", 
       x = "Lung cancer histology", 
       fill = "Sex")


the_plots[["lung-strat-race"]] <-
  by_race %>% filter(which_cancer == "lung") %>% 
    modify_at("the_strata", 
              ~ factor(.x, levels = c("Other", "Adenocarcinoma", "Carcinoma", 
                                      "SCLC", "NSCLC", "Squamous CC"))) %>% 
    ggplot(aes(x = the_strata, y = IP, fill = race_v)) + 
    geom_bar(stat = "identity", position = position_dodge(), color = "black") + 
    facet_wrap( ~ measure) + 
    coord_flip() + 
    scale_fill_grey(start = 0, end = 0.8) + 
    theme_bw() + 
    theme(axis.text.y = element_text(angle = 45), 
          legend.position = "bottom") +
    labs(y = "Incidence Proportion", 
         x = "Lung cancer histology", 
         fill = "Race")

the_plots[["breast-strat-race"]] <-
  by_race %>% filter(which_cancer == "breast" & the_strata != "Duct Carcinoma") %>% 
    modify_at("the_strata", ~ factor(.x, levels = c("Other", "Her2-/Hr+",  "Her2+/ Hr(+/-)", 
                                                    "Triple Negative"))) %>% 
    #  modify_at("race_v", ~ factor(.x, levels = c("Black", "White", "Other"))) %>% 
    ggplot(aes(x = the_strata, y = IP, fill = race_v)) + 
    geom_bar(stat = "identity", position = position_dodge(), color = "black") + 
    facet_wrap( ~ measure, scales = "free_x") + 
    coord_flip() + 
    scale_fill_grey(start = 0, end = 0.8) + 
    theme_bw() + 
    theme(axis.text.y = element_text(angle = 45), 
          legend.position = "bottom") +
    labs(y = "Incidence Proportion", 
         x = "Breast cancer subtype", 
         fill = "Race")



# papes$age_sex_race_strat$seer_bm_01 %>% 
#   filter(which_cancer == "breast") %>% 
#   group_by(which_cancer, race_v, the_strata, seer_bm_01) %>% 
#   summarise(count = sum(Freq)) %>% 
#   data.frame %>% 
#   modify_at("seer_bm_01", ~ ifelse(.x == 1, "SBM_Present", "SBM_Absent")) %>% 
#   spread(seer_bm_01, count) %>% 
#   select(-which_cancer)
























