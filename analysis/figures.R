
library(rlang)
library(grid)
library(gridExtra)

if (!exists("which_algo_to_use")) { which_algo_to_use <- "medicare_60_dx_img" }
the_plots <- list()

#bring the SEER and Medicare BM estimates together
plot_df <- 
  map2(
      c("seer_bm_01", which_algo_to_use), 
      c("SEER SBM", "Medicare LBM"),
      cnt_df_fn_fctry(papes$age_sex_race_strat)
      ) %>% 
  bind_relevel_filter_pdf()

#summarize by measures of interest, get crude per annum
c(by_sex, by_race)  %<-% 
  map(list(make_by_sex, make_by_race), ~ gp_cncr(.x(plot_df)))
#  map(list(make_by_sex(plot_df), make_by_race(plot_df)), 
#      gp_map_div_by_years)

bp_theme <-
    theme_bw() + 
    theme(legend.position = "bottom", 
          legend.key.size = unit(0.35, "cm"))

c(ty_slant, tx_slant, race_size) %<-% 
  list(theme(axis.text.y = element_text(angle = 45, color = "black")), 
       theme(axis.text.x = element_text(angle = 45, color = "black", hjust = 1)),
       3)

library(ggtextures)

images <- paste("http://mustafa.fyi/assets/s", c(1, 3:4), ".jpeg", sep = "")

the_plots[["breast_race"]] <-
  plot_breast_df(by_race$breast, 0.8, 4.7) %>% 
  #bp_this(race_v, sz = race_size) + 
  ggplot(aes(x = the_strata, y = IP * 100, image = race_v)) + 
  facet_wrap(~ measure) + #, scales = "free_x") + 
  geom_textured_bar(stat = "identity", aes(), position = position_dodge()) + 
  scale_image_manual(values = images) + 
  # scale_fill_grey(start = 1, end = 1) + 
  bp_theme + 
  label_this("Breast", "") + 
  labs(image = "") +
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
  plot_skin_df(by_sex$skin, 2.25, 5.5) %>% 
  ggplot(aes(x = the_strata, y = IP * 100, image = s_sex_v)) + 
  facet_wrap(~ measure) + #, scales = "free_x") + 
  geom_textured_bar(stat = "identity", aes(), position = position_dodge()) + 
  scale_image_manual(values = images) + 
  # bp_this(s_sex_v, sz = sex_size) + 
  # scale_fill_grey(start = 0.2, end = 0.6) +
  bp_theme + 
  label_this("Skin", "")
