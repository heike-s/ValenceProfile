# Analyzing PLSDA predictions throughout learning. Analyzing % Exploration
# acros models. 
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(glue)
library(ggplot2)
library(ggpubr)
library(lme4)
library(emmeans)

##############################################################################

setwd(here('..','ValenceProfile','ValenceProfile'))
source('styles.R')

# Load data ----
syls <- read.csv('data/syl_annotation.csv')
data_pred <- readRDS('data/plsda_predicted.rds')
data_session_cue <- readRDS('data/data_session_cue.rds')
data_session_pre_nt <- readRDS('data/data_session_pre_nt.rds')


##############################################################################

### Figure 3a - Learning trajectories on subset of days ----
data_pred %>%
    filter(day %in% c(paste0('D0',c(1,4,7)),paste0('D',c(10,13)))) %>%
    drop_na(value) %>%
    mutate(errordir = ifelse(name %in% c('ypred_csrcsm', 'ypred_csscsm') & 
                        type_sub == 'CSm', 'down', 'up'),
           errordir = ifelse(name == 'ypred_csrcss' & 
                        type_sub == 'CSR', 'down', errordir), 
           protocol = factor(protocol, 
                levels = c('Mixed', 'Appetitive', 'Aversive')),
           model = factor(model, levels = c('train', 'test', 'single')),
           day = factor(as.numeric(gsub('D', '', day)), levels = seq(1,15)),
           model_protocol = paste(type_sub, protocol),
           sex_protocol = paste(type_sub, sex), 
           singlemixed = ifelse(protocol == 'Mixed', 'Mixed', 'Single'), 
           name = factor(name, 
                levels = c('ypred_csrcsm', 'ypred_csscsm', 
                           'ypred_csrcss'))) -> data_training


ggplot(data_training, aes(as.factor(day), value, fill = paste(sex, type_sub), 
        color = type_sub, group = sex_protocol, shape = protocol)) +
    geom_hline(yintercept = 0) + 
    stat_summary(fun = mean, geom = 'line', size = 1) +
    stat_summary(data = data_training[data_training$errordir == 'up',],
                 fun.data = mean_se, size = 1, linewidth = 1, geom = 'ribbon', color = NA,
                 aes(ymin = after_stat(y),
                     ymax = after_stat(ymax), alpha = sex)) +
    stat_summary(data = data_training[data_training$errordir == 'down',],
                 fun.data = mean_se, size = 1, linewidth = 1, geom = 'ribbon', color = NA,
                 aes(ymin = after_stat(ymin),
                     ymax = after_stat(y), alpha = sex)) +
    scale_y_continuous(name = 'Predicted Y', expand = c(0,0), breaks = c(-1,0,1)) + 
    scale_color_manual(values = col_cs) + 
    scale_fill_manual(values = c(col_cs, col_cs)) +
    scale_alpha_manual(values = c(0.7, 0.3)) +
    scale_shape_manual(values = c(21,22,23)) +
    facet_grid(singlemixed ~ name, space = 'free', scales = 'free_x') + 
    coord_cartesian(ylim = c(-1.5,1.5)) + 
    plot_theme + 
    theme(panel.spacing = unit(1, 'cm'),
          strip.text = element_blank(),
          axis.title = element_blank())
ggsave('../plots/Fig3a_learning_overview_ribbon.pdf', width = 7.5, height = 6.5)


##############################################################################

### Figure 3b - Acquisition on recall day ----
data_pred %>%
    filter(day == 'D15') %>%
    drop_na(value) %>%
    mutate(protocol = factor(protocol, levels = c('Mixed', 'Appetitive', 'Aversive')),
           name = factor(name, 
                    levels = c('ypred_csrcsm', 'ypred_csscsm', 
                               'ypred_csrcss')))  -> data_recall

ggplot(data_recall, aes(name, value, fill = paste(sex, type_sub), 
        color = type_sub, shape = protocol, group = paste(type_sub, protocol))) +
    geom_hline(yintercept = 0) + 
    stat_summary(data = data_recall[data_recall$type_sub == 'CSm',], fun.data = mean_se, 
            size = 1, linewidth = 1, position = position_dodge2(width = 0.65, preserve = 'single')) +
    stat_summary(data = data_recall[data_recall$type_sub == 'CSR',], fun.data = mean_se, 
            size = 1, linewidth = 1, position = position_dodge2(width = 0.65, preserve = 'single')) +
    stat_summary(data = data_recall[data_recall$type_sub == 'CSS',], fun.data = mean_se, 
            size = 1, linewidth = 1, position = position_dodge2(width = 0.65, preserve = 'single')) +
    scale_y_continuous(name = 'Predicted Y', expand = c(0,0), breaks = c(-1,0,1)) + 
     scale_color_manual(values = col_cs) + 
     scale_fill_manual(values = c(col_cs, 'white', 'white', 'white')) + 
    scale_alpha_manual(values = c(0.7, 0.3)) +
    scale_shape_manual(values = c(21,22,23)) +
    facet_grid(sex ~ ., space = 'free', scales = 'free_x') + 
    coord_cartesian(ylim = c(-1.5,1.5)) + 
    plot_theme + 
    theme(panel.spacing = unit(1, 'cm'),
          strip.text = element_blank(),
          axis.title = element_blank())
ggsave('../plots/Fig3b_recall_protocolcomp.pdf', width = 3.5, height = 5.5)


##############################################################################

### Figure 3c - % Explore on habitutaion day, early training and late training ----
data_session_pre_nt %>%
    filter(day %in% c('D00','D01','D13','D15')) %>%
    mutate('All Rear' = syl3 + syl9 + syl15 + syl20 + syl24,
           'All Locomote' = syl16 + syl19 + syl21 + syl22,
           'Explore' = `All Rear` + `All Locomote`,
           protocol = ifelse(day == 'D00', 'Habituation', protocol),
           protocol = factor(protocol, 
                levels = c('Habituation', 'Mixed', 'Appetitive', 'Aversive'))) %>%
    group_by(id, model, day, protocol, sex) %>%
    summarise(Explore = mean(Explore) / 200 * 100) -> data_explore

ggplot(data_explore, aes(day, Explore / 200 * 100, group = sex, fill = sex, shape = protocol)) + 
    stat_summary(fun = mean, geom = 'line', size = 1) +
    stat_summary(fun.data = mean_se, size = 1, linewidth = 1) +
    scale_y_continuous(name = '% Explore', expand = c(0,0), 
            breaks = c(0,25,50), limits = c(0,50)) + 
    scale_fill_manual(values = c('black', 'white')) + 
    scale_shape_manual(values = c(21, 21, 22, 23)) + 
    facet_grid(. ~ protocol, space = 'free', scales = 'free') + 
    #coord_cartesian(ylim = c(0, 50)) + 
    plot_theme + 
    theme(panel.spacing = unit(0.5, 'cm'),
          strip.text = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text())
ggsave('plots/Fig3c_Explore_sexDifferences.pdf', width = 5, height = 3.25)


# LME model - Habitutaion
data_explore %>%
    filter(day == 'D00') %>%
    lm(Explore ~ sex, data = .) -> lm_hab
mm_hab <- emmeans(lm_hab, ~ sex)
hab_contrast <- summary(pairs(mm_hab, adjust = 'tukey'))
contrasts_list <- list("Contrast: sex | protocol" = hab_contrast)
export_lmer_summary(lm_hab, 'data/lmer/Fig3c_habituation.xlsx')
export_emmeans_contrasts(contrasts_list, 
        "data/lmer/Fig3c_hab_emmeans.xlsx", one_sheet = TRUE) 

# LME model - Protocol & Day & Sex
data_explore %>%
    filter(day != 'D00') %>%
    lmer(Explore ~ sex * protocol * day + (1|id), data = .) -> lm_prot 
mm_prot <- emmeans(lm_prot, ~ sex | protocol)
prot_contrast <- summary(pairs(mm_prot), adjust = 'tukey')
contrasts_list <- list("Contrast: sex | protocol" = prot_contrast)
export_lmer_summary(lm_prot, 'data/lmer/Fig3c_protocols.xlsx')
export_emmeans_contrasts(contrasts_list, 
        "data/lmer/Fig3c_protocols_emmeans.xlsx", one_sheet = TRUE)


##############################################################################

### Figure 3d + 3e - Correlations between cue predictors and exploration ----
# Subset data
data_session_cue %>%
    filter(day %in% c('D15'), 
           !(protocol == 'Aversive' & type_sub %in% c('CSR')),
           !(protocol == 'Appetitive' & type_sub %in% c('CSS'))) %>%
    mutate('All Rear' = syl3 + syl9 + syl15 + syl20 + syl24,
           'All Locomote' = syl16 + syl19 + syl21 + syl22,
           'Explore' = `All Rear` + `All Locomote`) -> data_recall

# Figure 3d - Port distance and exploration
data_recall %>%
    filter(type_sub == 'CSR') %>% 
ggplot(., aes(mean_port_dist_snout * (-1), Explore / 200 * 100, 
                group = sex, fill = sex, color = sex,
                shape = protocol, linetype = sex)) +
    geom_point(alpha = 0.5, size = 1) + 
    stat_cor(method="pearson",p.accuracy = 0.001, r.accuracy = 0.01) + 
    geom_smooth(method='lm', formula= y~x, se = FALSE) + 
    scale_fill_manual(values = c(col_cs[2], 'white')) + 
    scale_color_manual(values = c(col_cs[2], col_cs[2])) + 
    scale_shape_manual(values = c(21,22))+ 
    scale_y_continuous(name = '% Explore', expand = c(0,0), breaks = c(0,25,50)) + 
    scale_x_continuous(name = 'Port Distance', expand = c(0,0), breaks = c(-300, -150, 0)) + 
    coord_cartesian(ylim = c(0,50), xlim = c(-300, 0)) + 
    plot_theme + 
    theme(panel.spacing = unit(1, 'cm'))
ggsave('plots/Fig3d_explore_portdist_cor.pdf', width = 3.25, height = 3.25)

# Figure 3e - Pause and exploration
data_recall %>%
    filter(type_sub == 'CSS') %>% 
ggplot(., aes(syl0 / 200 * 100, Explore / 200 * 100, 
                group = sex, fill = sex, col = sex, 
                shape = protocol, linetype = sex)) +
    geom_point(alpha = 0.5, size = 1) + 
    stat_cor(method="pearson",p.accuracy = 0.001, r.accuracy = 0.01) + 
    geom_smooth(method='lm', formula= y~x, se = FALSE) + 
    scale_fill_manual(values = c(col_cs[3], 'white')) + 
    scale_color_manual(values = c(col_cs[3], col_cs[3])) + 
    scale_shape_manual(values = c(21,23))+ 
    scale_y_continuous(name = '% Explore', expand = c(0,0), breaks = c(0,10,20)) + 
    scale_x_continuous(name = '% Pause', expand = c(0,0), breaks = c(50, 100)) + 
    coord_cartesian(ylim = c(0,20), xlim = c(25,100)) + 
    plot_theme + 
    theme(panel.spacing = unit(1, 'cm'))
ggsave('plots/Fig3e_explore_pause_cor.pdf', width = 3.25, height = 3.25)


##############################################################################

### Figure S5 - Appetitive learning ----
# Data preparation
data_session_cue %>%
    filter(type_sub != 'CSS', protocol %in% c('Appetitive', 'Mixed'),
           !(protocol == 'Appetitive' & type_sub %in% c('CSS'))) %>%
    mutate('All Rear' = syl3 + syl9 + syl15 + syl20 + syl24,
           'All Locomote' = syl16 + syl19 + syl21 + syl22,
           'Explore' = `All Rear` + `All Locomote`,
           protocol = factor(protocol, levels = c('Mixed','Appetitive'))) -> data_session

# Figure S5a - Port distance
ggplot(data_session, aes(day, mean_port_dist_snout, group = paste(protocol, type_sub), 
            fill = paste(sex, type_sub), col = type_sub, shape = protocol)) +
    stat_summary(fun = 'mean', geom = 'line', size = 0.75) +   
    stat_summary(fun.data = 'mean_se', size = 0.75, linewidth = 0.75) + 
    scale_fill_manual(values = c(col_cs[c(1,2)], 'white', 'white')) + 
    scale_color_manual(values = col_cs) + 
    scale_shape_manual(values = c(21,22)) + 
    scale_y_continuous(expand = c(0,0), breaks = c(0,100,200,300), 
                       limits = c(0,300), name = 'Port Distance') + 
    coord_cartesian(ylim = c(50,250)) + 
    facet_wrap(sex ~ ., nrow = 2, scales = 'free')+ 
    plot_theme + 
    theme(panel.grid.major.y = element_line('gray80'),
          axis.title.x = element_blank(),
          panel.spacing = unit(1.5, 'cm'))
ggsave('../plots/FigS5_port_dist.pdf', width = 5.5, height = 7.5)


# Figure S5c - Port orientation
ggplot(data_session, aes(day, mean_oriented_port, group = paste(protocol, type_sub), 
            fill = paste(sex, type_sub), col = type_sub, shape = protocol)) +
    stat_summary(fun = 'mean', geom = 'line', size = 0.75) +   
    stat_summary(fun.data = 'mean_se', size = 0.75, linewidth = 0.75) + 
    scale_fill_manual(values = c(col_cs[c(1,2)], 'white', 'white')) + 
    scale_color_manual(values = col_cs) + 
    scale_shape_manual(values = c(21,22)) + 
    scale_y_continuous(expand = c(0,0), breaks = c(0,4,8), 
                       name = 'Port Orientation') + 
    coord_cartesian(ylim = c(0,8)) + 
    facet_wrap(sex ~ ., nrow = 2, scales = 'free')+ 
    plot_theme + 
    theme(panel.grid.major.y = element_line('gray80'),
          axis.title.x = element_blank(),
          panel.spacing = unit(1.5, 'cm'))
ggsave('../plots/FigS5_port_orient.pdf', width = 5.5, height = 7.5)


# Figure S5c - % Explore
ggplot(data_session, aes(day, `Explore` / 200 * 100, group = paste(protocol, type_sub), 
            fill = paste(sex, type_sub), col = type_sub, shape = protocol)) +
    stat_summary(fun = 'mean', geom = 'line', size = 0.75) +   
    stat_summary(fun.data = 'mean_se', size = 0.75, linewidth = 0.75) + 
    scale_fill_manual(values = c(col_cs[c(1,2)], 'white', 'white')) + 
    scale_color_manual(values = col_cs) + 
    scale_shape_manual(values = c(21,22)) + 
    scale_y_continuous(expand = c(0,0), breaks = c(0,10,20,30,40), 
                       name = '% Explore') + 
    coord_cartesian(ylim = c(0,40)) + 
    facet_wrap(sex ~ ., nrow = 2, scales = 'free')+ 
    plot_theme + 
    theme(panel.grid.major.y = element_line('gray80'),
          axis.title.x = element_blank(),
          panel.spacing = unit(1.5, 'cm'))
ggsave('../plots/FigS5_explore.pdf', width = 5.5, height = 7.5)



##############################################################################