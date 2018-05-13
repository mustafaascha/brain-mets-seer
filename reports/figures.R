

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
  modify_at("measure", ~ factor(.x, levels = c("SEER SBM", "Medicare LBM")))
  
#summarize by measures of interest
by_sex  <- make_by_sex(plot_df) %>% gp_cncr
by_race <- make_by_race(plot_df) %>% gp_cncr

the_plots[["lung_strat_sex"]] <-
  by_sex$lung %>% 
  mutate(text_y = ifelse(measure == "SEER SBM", 0.17, 0.245)) %>% 
  modify_at("present", function(z) ifelse(z > 10, paste(z, "  "), "*")) %>% 
  modify_at("the_strata", 
          ~ factor(.x, levels = c("Other", "Adenocarcinoma", "Carcinoma", 
                                  "SCLC", "NSCLC", "Squamous CC"))) %>% 
  ggplot(aes(x = the_strata, y = IP, fill = s_sex_v)) + 
  facet_wrap(~ measure, scales = "free_x") + 
  geom_bar(stat = "identity", position = position_dodge(), color = "black") + 
    geom_text(aes(label = present, y = text_y), 
              position = position_dodge(width = 1)) + 
    coord_flip() + 
  scale_fill_grey(start = 0.2, end = 0.6) + 
  theme_bw() + 
  theme(axis.text.y = element_text(angle = 45), 
        legend.position = "bottom") + 
  labs(y = "Incidence Proportion", 
       x = "Lung cancer histology", 
       fill = "Sex")


the_plots[["lung_strat_race"]] <-
  by_race$lung %>% 
    mutate(text_y = ifelse(measure == "SEER SBM", 0.2, 0.26)) %>% 
    modify_at("the_strata", 
              ~ factor(.x, levels = c("Other", "Adenocarcinoma", "Carcinoma", 
                                      "SCLC", "NSCLC", "Squamous CC"))) %>% 
    modify_at("present", function(z) ifelse(z > 10, paste(z, "   "), "*")) %>% 
    ggplot(aes(x = the_strata, y = IP, fill = race_v)) + 
    geom_bar(stat = "identity", position = position_dodge(), color = "black") + 
    geom_text(aes(label = present, y = text_y), 
              position = position_dodge(width = 1)) + 
    facet_wrap( ~ measure, scales = "free_x") + 
    coord_flip() + 
    scale_fill_grey(start = 0, end = 0.8) + 
    theme_bw() + 
    theme(axis.text.y = element_text(angle = 45), 
          legend.position = "bottom") +
    labs(y = "Incidence Proportion", 
         x = "Lung cancer histology", 
         fill = "Race")

the_plots[["skin_strat_sex"]] <-
  by_sex$skin %>% 
  mutate(text_y = ifelse(measure == "SEER SBM", 0.05, 0.1)) %>% 
  modify_at("the_strata", 
            ~ factor(.x, levels = c("Other", "Nevi & Melanomas", 
                                    "Mal. Mel. In Junct. Nevus"))) %>% 
  modify_at("present", function(z) ifelse(z > 10, paste(z, "  "), "*")) %>% 
  ggplot(aes(x = the_strata, y = IP, fill = s_sex_v)) + 
  facet_wrap(~ measure, scales = "free_x") + 
  geom_bar(stat = "identity", position = position_dodge(), color = "black") + 
  geom_text(aes(label = present, y = text_y), 
            position = position_dodge(width = 1)) + 
  coord_flip() + 
  scale_fill_grey(start = 0.2, end = 0.6) + 
  theme_bw() + 
  theme(axis.text.y = element_text(angle = 45), 
        legend.position = "bottom") + 
  labs(y = "Incidence Proportion", 
       x = "Skin cancer histology", 
       fill = "Sex")

# the_plots[["skin-strat-race"]] <-
#   by_race$skin %>% 
#   mutate(text_y = ifelse(measure == "SEER SBM", 0.032, 0.19)) %>% 
#    modify_at("the_strata", 
#              ~ factor(.x, levels = c("Other", "Nevi & Melanomas", 
#                                      "Mal. Mel. In Junct. Nevus"))) %>% 
#   modify_at("present", function(z) ifelse(z > 10, paste(z, "  "), "*")) %>% 
#   ggplot(aes(x = the_strata, y = IP, fill = race_v)) + 
#   geom_bar(stat = "identity", position = position_dodge(), color = "black") + 
#   geom_text(aes(label = present, y = text_y),
#             position = position_dodge(width = 1)) +
#   facet_wrap( ~ measure, scales = "free_x") + 
#   coord_flip() + 
#   scale_fill_grey(start = 0, end = 0.8) + 
#   theme_bw() + 
#   theme(axis.text.y = element_text(angle = 45), 
#         legend.position = "bottom") +
#   labs(y = "Incidence Proportion", 
#        x = "Skin cancer histology", 
#        fill = "Race")

by_race[["breast"]][["b_y"]] <- 
  ifelse(by_race[["breast"]][["measure"]] == "SEER SBM", 
         0.008, 0.047)

the_plots[["breast_strat_race"]] <-
  by_race$breast %>% filter(the_strata != "Duct Carcinoma") %>% 
    modify_at("the_strata", ~ factor(.x, levels = c("Other", "Her2-/Hr+",  "Her2+/ Hr(+/-)", 
                                                    "Triple Negative"))) %>% 
    modify_at("present", function(z) ifelse(z > 10, paste(z, "  "), "*")) %>% 
    ggplot(aes(x = the_strata, y = IP, fill = race_v)) + 
    geom_bar(stat = "identity", position = position_dodge(), color = "black") + 
    geom_text(aes(label = present, y = b_y),
              position = position_dodge(width = 1)) +
    facet_wrap( ~ measure, scales = "free_x") + 
    coord_flip() + 
    scale_fill_grey(start = 0, end = 0.8) + 
    theme_bw() + 
    theme(axis.text.y = element_text(angle = 45), 
          legend.position = "bottom") +
    labs(y = "Incidence Proportion", 
         x = "Breast cancer subtype", 
         fill = "Race")



























