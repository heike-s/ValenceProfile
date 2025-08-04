library(dplyr)
library(tidyr)
library(magrittr)
library(here)
library(ggplot2)
library(lme4)
library(broom.mixed)
library(writexl)
library(emmeans)
library(ggpubr)

##############################################################################

setwd(here('..','ValenceProfile','ValenceProfile'))
source('styles.R')

# Load data ----
syls <- read.csv('data/syl_annotation.csv')
data_pred <- readRDS('data/plsda_predicted.rds')
data_session_cue <- readRDS('data/data_session_cue.rds')
data_session_pre <- readRDS('data/data_session_pre.rds')
data_session_pre_nt <- readRDS('data/data_session_pre_nt.rds')
data_el_cue <- readRDS('data/data_el_cue.rds')
data_el_pre <- readRDS('data/data_el_pre.rds')


##############################################################################

### Fig 4a - Pre-cue ----
# Prepare data
data_session_pre_nt %>%
    filter(protocol %in% c('Aversive', 'Mixed'),
           day %in% c('D01','D13','D15')) %>%
    mutate('All Rear' = syl3 + syl9 + syl15 + syl20 + syl24,
           'All Locomote' = syl16 + syl19 + syl21 + syl22,
           '% Explore' = (`All Rear` + `All Locomote`) / 200 * 100,
           '% Pause' = syl0 / 200 * 100,
           protocol = factor(protocol, levels = c('Mixed','Aversive')),
           day = as.factor(gsub('D','', day))) -> data_session_pre_nt_plots

### Explore by protocol and sex ----
# Plot
ggplot(data_session_pre_nt_plots, aes(day, `% Explore`, 
            group = protocol, fill = sex, col = sex, shape = protocol)) +
    stat_summary(fun = 'mean', geom = 'line', size = 1) +   
    stat_summary(fun.data = 'mean_se', size = 1) + 
    scale_fill_manual(values = c('black', 'white')) + 
    scale_color_manual(values = c('black', 'black')) + 
    scale_shape_manual(values = c(21,23)) + 
    scale_y_continuous(expand = c(0,0), breaks = c(0,25,50)) + 
    coord_cartesian(ylim = c(0,50)) + 
    facet_grid(sex ~ .)+ 
    plot_theme + 
    theme(axis.title.x = element_blank(),
          panel.spacing = unit(1, 'cm'))
ggsave('plots/Fig4a_explore_pre.pdf', width = 4, height = 6.5)

# LME Model
explore_model <- lmer(`% Explore` ~ protocol * sex * as.factor(day) + (1|id), 
                           data = data_session_pre_nt)
emm_prot_sex <- emmeans(explore_model, ~ protocol | sex)
contrast_sex <- emmeans::contrast(emm_prot_sex, method = "pairwise", adjust = "tukey")
emm_prot_sex_day <- emmeans(explore_model, ~ protocol | sex * day)
contrast_sex_day <- emmeans::contrast(emm_prot_sex_day, method = "pairwise", adjust = "tukey")
contrasts_list <- list("Contrast: protocol | sex" = contrast_sex,
                       "Contrast: protocol | sex * Day" = contrast_sex_day)

export_lmer_summary(explore_model, 'data/lmer/Fig4a_explore_pre_lme_summary.xlsx')
export_emmeans_contrasts(contrasts_list, 
                "data/lmer/Fig4a_explore_emmeans.xlsx", one_sheet = TRUE)


### Pause by protocol and sex ----
# Plot
ggplot(data_session_pre_nt_plots, aes(day, `% Pause`, 
            group = protocol, fill = sex, col = sex, shape = protocol)) +
    stat_summary(fun = 'mean', geom = 'line', size = 1) +   
    stat_summary(fun.data = 'mean_se', size = 1) + 
    scale_fill_manual(values = c('black', 'white')) + 
    scale_color_manual(values = c('black', 'black')) + 
    scale_shape_manual(values = c(21,23)) + 
    scale_y_continuous(expand = c(0,0), breaks = c(0,50,100)) + 
    coord_cartesian(ylim = c(0,100)) + 
    facet_grid(sex ~ .)+ 
    plot_theme + 
    theme(axis.title.x = element_blank(),
          panel.spacing = unit(1, 'cm'))
ggsave('plots/Fig4a_pause_pre_av.pdf', width = 4, height = 6.5)

# LME Model
explore_model <- lmer(`% Pause` ~ protocol * sex * as.factßor(day) + (1|id), 
                           data = data_session_pre_nt)
emm_prot_sex <- emmeans(explore_model, ~ protocol | sex)
contrast_sex <- emmeans::contrast(emm_prot_sex, method = "pairwise", adjust = "tukey")
emm_prot_sex_day <- emmeans(explore_model, ~ protocol | sex * day)
contrast_sex_day <- emmeans::contrast(emm_prot_sex_day, method = "pairwise", adjust = "tukey")
contrasts_list <- list("Contrast: protocol | sex" = contrast_sex,
                       "Contrast: protocol | sex * Day" = contrast_sex_day)

export_lmer_summary(explore_model, 'data/lmer/Fig4a_pause_pre_lme_summary.xlsx')
export_emmeans_contrasts(contrasts_list, 
                "data/lmer/Fig4a_pause_emmeans.xlsx", one_sheet = TRUE)  


##############################################################################

### Fig 4b - CS- Correlations ----
# Prepare data
data_session_pre %>%
    filter(protocol %in% c('Aversive', 'Mixed'),
           day %in% c('D13','D15')) %>%
    mutate('All Rear' = syl3 + syl9 + syl15 + syl20 + syl24,
           'All Locomote' = syl16 + syl19 + syl21 + syl22,
           '% Explore' = (`All Rear` + `All Locomote`) / 200 * 100,
           '% Pause' = syl0 / 200 * 100,
           protocol = factor(protocol, levels = c('Mixed','Aversive')),
           day = as.factor(gsub('D','', day))) -> data_pre

data_session_cue %>%
    filter(protocol %in% c('Aversive', 'Mixed'),
           day %in% c('D13','D15'),
           type_sub %in% c('CSm','CSS')) %>%
    mutate('All Rear' = syl3 + syl9 + syl15 + syl20 + syl24,
           'All Locomote' = syl16 + syl19 + syl21 + syl22,
           '% Explore' = (`All Rear` + `All Locomote`) / 200 * 100,
           '% Pause' = syl0 / 200 * 100,
           protocol = factor(protocol, levels = c('Mixed','Aversive')),
           day = as.factor(gsub('D','', day))) -> data_cue_csm

data_pre$window = 'Pre'
data_cue_csm$window = 'Cue'
data_csm_pre <- rbind(data_cue_csm, data_pre)
data_csm_pre %<>%
    ungroup() %>%
    select(id, protocol, model, day, `% Pause`, `% Explore`, type_sub, sex, window) %>%
    pivot_wider(names_from = window, values_from = c(`% Pause`,`% Explore`))

# Correlation % Explore
ggplot(data_csm_pre, aes(`% Explore_Pre`, `% Explore_Cue`, 
             group = type_sub, fill = type_sub, col = type_sub, shape = day)) +
    geom_point(alpha = 0.35) + 
    stat_cor(method="pearson", p.accuracy = 0.001, r.accuracy = 0.01, 
             label.y.npc = c(0.65, 0.5), label.x.npc = 0.05,  size = 6) + 
    geom_smooth(method='lm', formula= y~x, alpha = 0.1, linewidth = 2) + 
    scale_fill_manual(values = col_cs) + 
    scale_color_manual(values = col_cs) + 
    scale_y_continuous(expand = c(0,0), breaks = c(0,25,50), name = 'Cue') + 
    scale_x_continuous(expand = c(0,0), breaks = c(0,25,50), name = 'Pre Cue') + 
    coord_cartesian(ylim = c(0,50), xlim = c(0,50)) + 
    facet_grid(sex ~ protocol)+ 
    plot_theme + 
    theme(panel.spacing = unit(1, 'cm'), 
          strip.background = element_blank(),
          strip.text = element_blank())
ggsave('plots/Fig4b_explore_cuepre_correlation.pdf', width = 7, height = 6.5)

# Correlation % Pause
ggplot(data_csm_pre, aes(`% Pause_Pre`, `% Pause_Cue`, 
             group = type_sub, fill = type_sub, col = type_sub, shape = day)) +
    geom_point(alpha = 0.35) + 
    stat_cor(method="pearson", p.accuracy = 0.001, r.accuracy = 0.01, 
             label.y.npc = c(1), label.x.npc = 0.05,  size = 6) + 
    geom_smooth(method='lm', formula= y~x, alpha = 0.1, linewidth = 2) + 
    scale_color_manual(values = col_cs) + 
    scale_fill_manual(values = col_cs) + 
    scale_y_continuous(expand = c(0,0), breaks = c(0,50,100), name = 'Cue') + 
    scale_x_continuous(expand = c(0,0), breaks = c(0,50,100), name = 'Pre Cue') + 
    coord_cartesian(ylim = c(0,100), xlim = c(0,100)) + 
    facet_grid(sex ~ protocol)+ 
    plot_theme + 
    theme(panel.spacing = unit(1, 'cm'), 
          strip.background = element_blank(),
          strip.text = element_blank())
ggsave('plots/Fig4_pause_cuepre_correlation.pdf', width = 7.2, height = 6.5)


##############################################################################

### Figure 4c, 4d, 4e - Early / Late ----
# Pre cue differences
ggplot(data_el_pre, aes(day, `% Pause`, group = paste(sex, window), 
        fill = sex, col = sex, shape = protocol)) +
    stat_summary(aes(linetype = window), fun = 'mean', geom = 'line', size = 1) +   
    stat_summary(fun.data = 'mean_se', size = 1, linewidth = 1) + 
    scale_fill_manual(values = c('black', 'white')) + 
    scale_color_manual(values = c('black', 'black')) + 
    scale_shape_manual(values = c(21,23)) + 
    scale_y_continuous(expand = c(0,0), name = '% Pause') + 
    coord_cartesian(ylim = c(0,100)) + 
    facet_grid(. ~ protocol)+ 
    plot_theme + 
    theme(axis.title.x = element_blank(),
          panel.spacing = unit(0.5, 'cm'))
ggsave('../plots/Fig4c_earlylate_pre_pause_diff.pdf', width = 5, height = 3.25)

data_el_pre %>%
    filter(day == 'D13') %>%
ggplot(., aes(sex, `% Pause`, group = paste(sex, window), 
            fill = sex, col = sex, shape = protocol)) +
    stat_summary(aes(linetype = window), fun = 'mean', 
            geom = 'line', size = 1, position = position_dodge(width = 0.5)) +   
    stat_summary(fun.data = 'mean_se', size = 1, 
            position = position_dodge(width = 0.5), linewidth = 1) + 
    scale_fill_manual(values = c('black', 'white')) + 
    scale_color_manual(values = c('black', 'black')) + 
    scale_shape_manual(values = c(21,23)) + 
    scale_y_continuous(expand = c(0,0), name = '% Pause') + 
    coord_cartesian(ylim = c(0,100)) + 
    facet_grid(. ~ protocol)+ 
    plot_theme + 
    theme(axis.title.x = element_blank(),
          panel.spacing = unit(0.5, 'cm'))
ggsave('../plots/Fig4c_earlylate_pre_pause_diff_d13.pdf', width = 5, height = 3.25)

# LME model
lme_pause_pre <- lmer(`% Pause` ~ protocol * window * as.factor(day) * sex + (1|id), 
                   data = data_trial_occs_pre)
emm_prot_el <- emmeans(lme_pause_pre, ~ window | protocol * day * sex)
contrast_pre_el <- emmeans::contrast(emm_prot_el, method = "pairwise", adjust = "tukey")
contrasts_list <- list("Contrast: Window | protocol * Day * sex" = contrast_pre_el)
export_lmer_summary(lme_pause_pre, 'data/lmer/Fig4c_pause_pre_el_lme_summary.xlsx')
export_emmeans_contrasts(contrasts_list, 
                    "data/lmer/Fig4c_pause_pre_el_emmeans.xlsx", one_sheet = TRUE)


# Cue differences CS-
data_el_cue %>%
    filter(type_sub == 'CSm') %>%
ggplot(., aes(day, `% Pause`, group = paste(sex, window), 
        fill = sex, col = type_sub, shape = protocol)) +
    stat_summary(aes(linetype = window), fun = 'mean', geom = 'line', size = 1) +   
    stat_summary(fun.data = 'mean_se', size = 1) + 
    scale_fill_manual(values = c(col_cs[1], 'white')) + 
    scale_color_manual(values = col_cs) + 
    scale_shape_manual(values = c(21,23)) + 
    scale_y_continuous(expand = c(0,0), name = '% Pause') + 
    coord_cartesian(ylim = c(0,100)) + 
    facet_wrap(. ~ protocol, nrow = 1)+ 
    plot_theme + 
    theme(axis.title.x = element_blank(),
          panel.spacing = unit(0.5, 'cm'))
ggsave('../plots/Fig4_earlylate_csm_pause_diff.pdf', width = 5, height = 3.25)

data_el_cue %>%
    filter(day == 'D13' & type_sub == 'CSm') %>%
ggplot(., aes(sex, `% Pause`, group = paste(sex, window), 
            fill = sex, col = sex, shape = protocol)) +
    stat_summary(aes(linetype = window), fun = 'mean', 
            geom = 'line', size = 1, position = position_dodge(width = 0.5)) +   
    stat_summary(fun.data = 'mean_se', size = 1, 
            position = position_dodge(width = 0.5)) + 
    scale_fill_manual(values = c(col_cs[1], 'white')) + 
    scale_color_manual(values = c(col_cs[1], col_cs[1])) + 
    scale_shape_manual(values = c(21,23)) + 
    scale_y_continuous(expand = c(0,0), name = '% Pause') + 
    coord_cartesian(ylim = c(0,100)) + 
    facet_grid(. ~ protocol)+ 
    plot_theme + 
    theme(axis.title.x = element_blank(),
          panel.spacing = unit(0.5, 'cm'))
ggsave('../plots/Fig4_earlylate_csm_pause_diff_d13.pdf', width = 5, height = 3.25)

# LME model
lme_pause_csm <- lmer(`% Pause` ~ protocol * window * as.factor(day) * sex + (1|id), 
                   data = data_trial_occs[data_trial_occs$type_sub == 'CSm',])
emm_prot_csm_el <- emmeans(lme_pause_csm, ~ window | protocol * day * sex)
contrast_csm_el <- emmeans::contrast(emm_prot_csm_el, method = "pairwise", adjust = "tukey")
contrasts_list <- list("Contrast: Window | protocol * Day * sex" = contrast_csm_el)
export_lmer_summary(lme_pause_csm, 'data/lmer/Fig4d_pause_csm_el_lme_summary.xlsx')
export_emmeans_contrasts(contrasts_list, 
                    "data/lmer/Fig4d_pause_pre_csm_emmeans.xlsx", one_sheet = TRUE)


# Cue differences CSS
data_el_cue %>%
    filter(type_sub == 'CSS') %>%
ggplot(., aes(day, `% Pause`, group = paste(sex, window), fill = sex, col = type_sub, shape = protocol)) +
    stat_summary(aes(linetype = window), fun = 'mean', geom = 'line', size = 1) +   
    stat_summary(fun.data = 'mean_se', size = 1) + 
    scale_fill_manual(values = c(col_cs[c(3)], 'white')) + 
    scale_color_manual(values = col_cs[3]) + 
    scale_shape_manual(values = c(21,23)) + 
    scale_y_continuous(expand = c(0,0), name = '% Pause') + 
    coord_cartesian(ylim = c(0,100)) + 
    facet_wrap(type_sub ~ protocol, nrow = 1)+ 
    plot_theme + 
    theme(axis.title.x = element_blank(),
          panel.spacing = unit(0.5, 'cm'))
ggsave('../plots/Fig4_earlylate_css_pause_diff.pdf', width = 5, height = 3.25)

data_el_cue %>%
    filter(day == 'D13' & type_sub == 'CSS') %>%
ggplot(., aes(sex, `% Pause`, group = paste(sex, window), fill = sex, col = sex, shape = protocol)) +
    stat_summary(aes(linetype = window), fun = 'mean', geom = 'line', size = 1, position = position_dodge(width = 0.5)) +   
    stat_summary(fun.data = 'mean_se', size = 1, position = position_dodge(width = 0.5)) + 
    scale_fill_manual(values = c(col_cs[3], 'white')) + 
    scale_color_manual(values = c(col_cs[3], col_cs[3])) + 
    scale_shape_manual(values = c(21,23)) + 
    scale_y_continuous(expand = c(0,0), name = '% Pause') + 
    coord_cartesian(ylim = c(0,100)) + 
    facet_grid(. ~ protocol)+ 
    plot_theme + 
    theme(axis.title.x = element_blank(),
          panel.spacing = unit(0.5, 'cm'))
ggsave('../plots/Fig4_earlylate_css_pause_diff_d13.pdf', width = 5, height = 3.25)

# LME model
lme_pause_css <- lmer(`% Pause` ~ protocol * window * as.factor(day) * sex + (1|id), 
                   data = data_trial_occs[data_trial_occs$type_sub == 'CSS',])
emm_prot_css_el <- emmeans(lme_pause_css, ~ window | protocol * day * sex)
contrast_css_el <- emmeans::contrast(emm_prot_css_el, method = "pairwise", adjust = "tukey")
contrasts_list <- list("Contrast: Window | protocol * Day * sex" = contrast_css_el)
export_lmer_summary(lme_pause_css, 'data/lmer/Fig4e_pause_css_el_lme_summary.xlsx')
export_emmeans_contrasts(contrasts_list, 
                    "data/lmer/Fig4e_pause_pre_css_emmeans.xlsx", one_sheet = TRUE)


##############################################################################