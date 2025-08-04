# Uses PLS-DA models to predict cue type in different paradigms on recall day.
# Identifies most useful predictors for each cue type. 
library(mdatools)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(glue)
library(here)

##############################################################################

setwd(here('..','ValenceProfile','ValenceProfile'))
source('styles.R')

# Load data ----
syls <- read.csv('data/syl_annotation.csv')
data_session_cue <- readRDS('data/data_session_cue.rds')

# Load models ----
model_csrcsm = readRDS('plsda_models/model_csrcsm.rds')
model_csscsm = readRDS('plsda_models/model_csscsm.rds')
model_csrcss = readRDS('plsda_models/model_csrcss.rds')


##############################################################################

### Apply models throughout training ----
dual_days = c(paste0('D', c('01','04','07',10,13,15)))
data_session_cue$ypred_csrcsm = NA
data_session_cue$ypred_csscsm = NA
data_session_cue$ypred_csrcss = NA

for (pred_day in unique(data_session_cue$day)) {    
    # reward model
    data_session_cue %>%
        filter(day == pred_day,
               type_sub %in% c('CSm', 'CSR'),
               protocol %in% c('Mixed', 'Appetitive')) %>%
        select(-c(ypred_csrcsm, ypred_csscsm, ypred_csrcss)) -> data_day_reward
    data_day_reward_x = data_day_reward[,9:(ncol(data_day_reward))]
    data_day_reward_x = prep.autoscale(data.matrix(data_day_reward_x), center = TRUE, scale = TRUE)
    test_day_reward = predict(model_csrcsm, data_day_reward_x, data_day_reward$type_sub)
    pred_day_reward = test_day_reward$y.pred[,paste('Comp', model_csrcsm$ncomp.selected),'CSR']
    data_session_cue$ypred_csrcsm[data_session_cue$day == pred_day & 
                              data_session_cue$protocol %in% c('Mixed', 'Appetitive') &
                              data_session_cue$type_sub %in% c('CSm','CSR')] = pred_day_reward

    if (pred_day %in% dual_days) {
        # shock model
        data_session_cue %>%
            filter(day == pred_day,
                   type_sub %in% c('CSm', 'CSS'),
                   protocol %in% c('Mixed', 'Aversive')) %>%
            select(-c(ypred_csrcsm, ypred_csscsm, ypred_csrcss)) -> data_day_shock
        data_day_shock_x = data_day_shock[,9:(ncol(data_day_shock))]
        data_day_shock_x = prep.autoscale(data.matrix(data_day_shock_x), center = TRUE, scale = TRUE)
        test_day_shock = predict(model_csscsm, data_day_shock_x, data_day_shock$type_sub)
        pred_day_shock = test_day_shock$y.pred[,paste('Comp', model_csscsm$ncomp.selected),'CSS']
        data_session_cue$ypred_csscsm[data_session_cue$day == pred_day & 
                                  data_session_cue$protocol %in% c('Mixed', 'Aversive') &
                                  data_session_cue$type_sub %in% c('CSm','CSS')] = pred_day_shock
        
        # valence model
        data_session_cue %>%
            filter(day == pred_day,
                   type_sub %in% c('CSR', 'CSS'),
                   protocol %in% c('Mixed')) %>%
            select(-c(ypred_csrcsm, ypred_csscsm, ypred_csrcss)) -> data_day_valence
        data_day_valence_x = data_day_valence[,9:(ncol(data_day_valence))]
        data_day_valence_x = prep.autoscale(data.matrix(data_day_valence_x), center = TRUE, scale = TRUE)
        test_day_valence = predict(model_csrcss, data_day_valence_x, data_day_valence$type_sub)
        pred_day_valence = test_day_valence$y.pred[,paste('Comp', model_csrcss$ncomp.selected),'CSS']
        data_session_cue$ypred_csrcss[data_session_cue$day == pred_day & 
                                  data_session_cue$protocol == 'Mixed' &
                                  data_session_cue$type_sub %in% c('CSR','CSS')] = pred_day_valence
    }
}


# Save prediction dataframe
data_pred = data_session_cue[,c(1:8, (ncol(data_session_cue)-2):ncol(data_session_cue))]
data_pred %<>%
    pivot_longer(cols = c('ypred_csrcsm', 'ypred_csscsm', 'ypred_csrcss')) %>%
    filter(name == 'ypred_csrcsm' & type_sub == 'CSR'|
           name == 'ypred_csrcsm' & type_sub == 'CSm'|
           name == 'ypred_csscsm' & type_sub == 'CSS'|
           name == 'ypred_csscsm' & type_sub == 'CSm'|
           name == 'ypred_csrcss' & type_sub == 'CSS'| 
           name == 'ypred_csrcss' & type_sub == 'CSR')
saveRDS(data_pred, 'data/plsda_predicted.rds')


##############################################################################

### Figure 2b - Prediction on recall day ----
data_pred %>%
    filter(day == 'D15') %>%
    mutate(sex = factor(sex, levels = c('Male', 'Female')),
           type_sex = paste(sex, type_sub),
           model = factor(str_to_title(model), levels = c('Single', 'Test', 'Train')),
           name = recode(name, "ypred_csrcsm" = "CS- vs CSR", 
                               "ypred_csrcss" = "CSR vs CSS",
                               "ypred_csscsm" = "CS- vs CSS")) -> pred_recall

ggplot(pred_recall, aes(model, value, color = type_sex, fill = type_sex, group = sex)) + 
    geom_hline(yintercept = 0, col = 'gray70') + 
    geom_point(size = 2, shape = 21, stroke = 1,
                position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.65)) + 
    scale_color_manual(values = c(alpha(col_cs,0.55), col_cs)) + 
    scale_fill_manual(values = alpha(c(col_cs, rep('white', 3)), 0.35)) + 
    scale_y_continuous(expand = c(0,0), name = 'Predicted Y') + 
    facet_grid(. ~ name) + 
    plot_theme + 
    coord_flip(ylim = c(-2, 2)) +
    theme(axis.title = element_blank(),
          panel.spacing = unit(1, 'cm'))
ggsave('../plots/Fig2b_D15_Prediction.pdf', width = 12, height = 3.5)

##############################################################################

### Figure 2c - Prediction accuracy ----

data_pred %>%
    filter(day == 'D15') %>%
    drop_na(value) %>%
    mutate(protocol = factor(protocol, levels = c('Mixed', 'Appetitive', 'Aversive')),
           model = factor(model, levels = c('train', 'test', 'single')),
           day = factor(as.numeric(gsub('D', '', day)), levels = seq(1,15)),
           model_protocol = paste(type_sub, protocol),
           name = factor(name, levels = c('ypred_csrcsm', 'ypred_csscsm', 'ypred_csrcss'))) %>%
    ungroup() %>%
    mutate(pred_type = 'CSR') %>%
    mutate(pred_type = ifelse((name == 'ypred_csrcsm' & value < 0), 'CSm', pred_type),
            pred_type = ifelse((name == 'ypred_csscsm' & value < 0), 'CSm', pred_type),
            pred_type = ifelse((name == 'ypred_csscsm' & value > 0), 'CSS', pred_type),
            pred_type = ifelse((name == 'ypred_csrcss' & value > 0), 'CSS', pred_type)) %>%
    group_by(name, type_sub, day, pred_type, protocol, sex, model) %>%
    count() %>%
    mutate(correct = ifelse(type_sub == pred_type, 'T', 'F')) %>% 
    group_by(day, protocol, correct, name, sex, model) %>%
    summarise(summed_correct = sum(n)) %>% 
    pivot_wider(names_from = correct, values_from = summed_correct, values_fill = 0) %>%
    mutate(Total = F + T,
           Accuracy = round(T / Total * 100),
           single_mixed = ifelse(protocol == 'Mixed', 'Mixed', 'Single')) -> training_accuracy


ggplot(training_accuracy, aes(name, model, fill = Accuracy, label = Accuracy)) + 
    geom_tile(color = 'black', size = 0.5) + 
    geom_text(size = 7) + 
    facet_grid(model ~ sex, scales = 'free', space = 'free') + 
    scale_fill_viridis_c(option = 'plasma', limits = c(50,100)) +
    plot_theme + 
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(), 
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          legend.position = 'bottom', 
          legend.key.size = unit(1, 'cm'),
          legend.title = element_text(size = 20))
ggsave('../plots/Fig2c_accuracy.pdf', width = 4.5, height = 6.5)


##############################################################################

### Figure 2d - Regression coefficients ----
csrcsm_coeffs <- as.data.frame(getRegcoeffs(model_csrcsm, full = TRUE))
csrcsm_coeffs$model <- 'CS- vs CSR'
csrcsm_coeffs$factor <- rownames(csrcsm_coeffs)

csrcss_coeffs <- as.data.frame(getRegcoeffs(model_csrcss, full = TRUE))
csrcss_coeffs$model <- 'CSR vs CSS'
csrcss_coeffs$factor <- rownames(csrcss_coeffs)

csscsm_coeffs <- as.data.frame(getRegcoeffs(model_csscsm, full = TRUE))
csscsm_coeffs$model <- 'CS- vs CSS'
csscsm_coeffs$factor <- rownames(csscsm_coeffs)

coeffs = rbind(csrcsm_coeffs, csrcss_coeffs, csscsm_coeffs)
coeffs$sig = ifelse(coeffs$`p-value` < 0.05, 'Significant', 'Not Significant')
coeffs$Estimated = coeffs$Estimated * (-1)
coeffs$`2.5%` = coeffs$`2.5%` * (-1)
coeffs$`97.5%` = coeffs$`97.5%` * (-1)
coeffs = filter(coeffs, factor != 'Intercept')
coeffs$factor = gsub('syl', 'Syllable ', coeffs$factor)
coeffs$factor = recode(coeffs$factor, 'mean_closest_corner_dist_base' = 'Corner Distance',
                                      'mean_oriented_port' = 'Port Orientation',
                                      'mean_port_dist_snout' = 'Port Distance')
coeffs$factor = factor(coeffs$factor, levels = c(paste0('Syllable ', seq(24,0)), 
                                                 'Port Distance',
                                                 'Port Orientation',
                                                 'Corner Distance'))

coeffs$model_direction = 'Not Significant'
coeffs$model_direction = ifelse((coeffs$model == 'CS- vs CSR' &
                                coeffs$sig == 'Significant' & 
                                coeffs$Estimated < 0), 'CS-', coeffs$model_direction)
coeffs$model_direction = ifelse((coeffs$model == 'CS- vs CSR' &
                                coeffs$sig == 'Significant' & 
                                coeffs$Estimated > 0), 'CSR', coeffs$model_direction)
coeffs$model_direction = ifelse((coeffs$model == 'CS- vs CSS' &
                                coeffs$sig == 'Significant' & 
                                coeffs$Estimated < 0), 'CS-', coeffs$model_direction)
coeffs$model_direction = ifelse((coeffs$model == 'CS- vs CSS' &
                                coeffs$sig == 'Significant' & 
                                coeffs$Estimated > 0), 'CSS', coeffs$model_direction)
coeffs$model_direction = ifelse((coeffs$model == 'CSR vs CSS' &
                                coeffs$sig == 'Significant' & 
                                coeffs$Estimated < 0), 'CSR', coeffs$model_direction)
coeffs$model_direction = ifelse((coeffs$model == 'CSR vs CSS' &
                                coeffs$sig == 'Significant' & 
                                coeffs$Estimated > 0), 'CSS', coeffs$model_direction)
coeffs$group = syls$Group[match(coeffs$factor, syls$Metric)]
coeffs$group = factor(coeffs$group, levels = c('Location', 'Attend', 'Other', 'Rear', 'Locomote', 'Turn', 'Jump'))
coeffs$factor_new = syls$Metric_new[match(coeffs$factor, syls$Metric)]
coeffs$factor_new = factor(coeffs$factor_new, levels = c('Port Orientation', 'Port Distance', 'Corner Distance',
                                paste('Attend', seq(6,1)), 'Pause', 'Lick', 'Groom',
                                paste('Rear', seq(6,1)), paste('Locomote', seq(4,1)), 
                                paste('Turn', seq(5,1)), paste('Jump', seq(3,1))))

ggplot(coeffs, aes(Estimated, factor_new, fill = model_direction, color = model_direction, group = model, alpha = sig)) + 
    geom_vline(xintercept = 0) + 
    geom_col(position = 'identity', width = 0.7) + 
    geom_errorbar(aes(xmax = `2.5%`, xmin = `97.5%`, y = factor_new, color = model_direction,), width = 0.25, size = 1) + 
    scale_x_continuous(limits = c(-0.4, 0.4), name = 'Regression Coefficient') + 
    scale_alpha_manual(values = c(0.3, 0.7)) + 
    scale_fill_manual(values = c(col_cs, 'white')) + 
    scale_color_manual(values =c(col_cs, 'gray70')) + 
    facet_grid(group ~ model, space = 'free', scales = 'free') + 
    plot_theme + 
    theme(axis.title.y = element_blank(), panel.spacing = unit(0.5, 'cm'))
ggsave('../plots/Fig2d_RegCoeffs.pdf', width = 10, height = 8)


##############################################################################

### Figure 2e - Individual predictors ----
data_session_cue$ttprotocol = paste(data_session_cue$model, data_session_cue$protocol)
data_session_cue %>%
    mutate(type_sex = paste(sex, type_sub),
           protocol = factor(protocol, levels = c('Mixed', 'Appetitive', 'Aversive')),
           ttprotocol = factor(paste(model, protocol), 
                               levels = c('train Mixed', 'test Mixed', 
                                          'single Appetitive', 'single Aversive')),
           type_sub = recode(type_sub, 'CSm' = 'CS-')) %>%
    filter(day == 'D15',
           !(protocol == 'Aversive' & type_sub %in% c('CSR')),
           !(protocol == 'Appetitive' & type_sub %in% c('CSS'))) %>%
    mutate('All Rear' = syl3 + syl9 + syl15 + syl20 + syl24,
           'All Locomote' = syl16 + syl19 + syl21 + syl22,
           'Rear + Locomote' = `All Rear` + `All Locomote`) -> data_session_predictors

# Port orientation
ggplot() + 
    geom_hline(yintercept = 0) + 
    geom_point(data = data_session_predictors[data_session_predictors$sex == 'Male',], 
                    aes(x = mean_oriented_port, 0.25, 
                    color = type_sub, fill = 'white', group = type_sub),
                    linewidth = 1, alpha = 0.5) +
    geom_density(data = data_session_predictors[data_session_predictors$sex == 'Male',], 
                    aes(x = mean_oriented_port, after_stat(scaled), 
                    color = type_sub, fill = 'white', group = type_sub),
                    linewidth = 1, alpha = 0.25) +
    geom_point(data = data_session_predictors[data_session_predictors$sex == 'Female',], 
                    aes(x = mean_oriented_port, -0.25, 
                    color = type_sub, fill = 'white', group = type_sub),
                    linewidth = 1, alpha = 0.5) +
    geom_density(data = data_session_predictors[data_session_predictors$sex == 'Female',], 
                    aes(x = mean_oriented_port, -after_stat(scaled), 
                    color = type_sub, fill = type_sub, group = type_sub),
                    linewidth = 1) +
    scale_x_continuous(name = 'Port Orientation', expand = c(0,0), breaks = c(0,4,8), limits = c(0,10)) +
    scale_color_manual(values = col_cs) + 
    scale_fill_manual(values = c(alpha(col_cs, 0.25), 'white')) + 
    facet_wrap(protocol ~ type_sub, nrow = 1) + 
    coord_flip() + 
    ylim(c(-1, 1)) + plot_theme + 
    theme(axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          panel.spacing = unit(0.5, 'cm'),
          strip.text = element_blank())
ggsave('../plots/Fig2e_Port_Orientation_Violins.pdf', width = 6.5, height = 2.5)

# Port distance
ggplot() + 
    geom_hline(yintercept = 0) + 
    geom_point(data = data_session_predictors[data_session_predictors$sex == 'Male',], 
                    aes(x = mean_port_dist_snout, 0.25, 
                    color = type_sub, fill = 'white', group = type_sub),
                    linewidth = 1, alpha = 0.5) +
    geom_density(data = data_session_predictors[data_session_predictors$sex == 'Male',], 
                    aes(x = mean_port_dist_snout, after_stat(scaled), 
                    color = type_sub, fill = 'white', group = type_sub),
                    linewidth = 1, alpha = 0.25) +
    geom_point(data = data_session_predictors[data_session_predictors$sex == 'Female',], 
                    aes(x = mean_port_dist_snout, -0.25, 
                    color = type_sub, fill = 'white', group = type_sub),
                    linewidth = 1, alpha = 0.5) +
    geom_density(data = data_session_predictors[data_session_predictors$sex == 'Female',], 
                    aes(x = mean_port_dist_snout, -after_stat(scaled), 
                    color = type_sub, fill = type_sub, group = type_sub),
                    linewidth = 1) +
    scale_x_continuous(name = 'Port Distance', expand = c(0,0), breaks = c(0,200,400), limits = c(0,400)) + 
    scale_color_manual(values = col_cs) + 
    scale_fill_manual(values = c(alpha(col_cs, 0.25), 'white')) + 
    facet_wrap(protocol ~ type_sub, nrow = 1) + 
    coord_flip() + 
    ylim(c(-1, 1)) + plot_theme + 
    theme(axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          panel.spacing = unit(0.5, 'cm'),
          strip.text = element_blank())
ggsave('../plots/Fig2e_Port_Distance_Violins.pdf', width = 6.5, height = 2.5)

# Pausing
ggplot() + 
    geom_hline(yintercept = 0) + 
    geom_point(data = data_session_predictors[data_session_predictors$sex == 'Male',], 
                    aes(x = syl0, 0.25, 
                    color = type_sub, fill = 'white', group = type_sub),
                    linewidth = 1, alpha = 0.5) +
    geom_density(data = data_session_predictors[data_session_predictors$sex == 'Male',], 
                    aes(x = syl0, after_stat(scaled), 
                    color = type_sub, fill = 'white', group = type_sub),
                    linewidth = 1, alpha = 0.25) +
    geom_point(data = data_session_predictors[data_session_predictors$sex == 'Female',], 
                    aes(x = syl0, -0.25, 
                    color = type_sub, fill = 'white', group = type_sub),
                    linewidth = 1, alpha = 0.5) +
    geom_density(data = data_session_predictors[data_session_predictors$sex == 'Female',], 
                    aes(x = syl0, -after_stat(scaled), 
                    color = type_sub, fill = type_sub, group = type_sub),
                    linewidth = 1) +
    scale_x_continuous(name = '% Pause', expand = c(0,0), 
                       breaks = c(0,100,200), limits = c(0,200),
                       labels = c(0,25,50)) + 
    scale_color_manual(values = col_cs) + 
    scale_fill_manual(values = c(alpha(col_cs, 0.25), 'white')) + 
    facet_wrap(protocol ~ type_sub, nrow = 1) + 
    coord_flip() + 
    ylim(c(-1, 1)) + plot_theme + 
    theme(axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          panel.spacing = unit(0.5, 'cm'),
          strip.text = element_blank())
ggsave('../plots/Fig2e_Pause_Violins.pdf', width = 6.7, height = 2.5)

# Explore
ggplot() + 
    geom_hline(yintercept = 0) + 
    geom_point(data = data_session_predictors[data_session_predictors$sex == 'Male',], 
                    aes(x = `Rear + Locomote`, 0.25, 
                    color = type_sub, fill = 'white', group = type_sub),
                    linewidth = 1, alpha = 0.5) +
    geom_density(data = data_session_predictors[data_session_predictors$sex == 'Male',], 
                    aes(x = `Rear + Locomote`, after_stat(scaled), 
                    color = type_sub, fill = 'white', group = type_sub),
                    linewidth = 1, alpha = 0.25) +
    geom_point(data = data_session_predictors[data_session_predictors$sex == 'Female',], 
                    aes(x = `Rear + Locomote`, -0.25, 
                    color = type_sub, fill = 'white', group = type_sub),
                    linewidth = 1, alpha = 0.5) +
    geom_density(data = data_session_predictors[data_session_predictors$sex == 'Female',], 
                    aes(x = `Rear + Locomote`, -after_stat(scaled), 
                    color = type_sub, fill = type_sub, group = type_sub),
                    linewidth = 1) +
    scale_x_continuous(name = '% Explore', expand = c(0,0), 
                       breaks = c(0,60,120), limits = c(0,120),
                       labels = c(0,15,30)) + 
    scale_color_manual(values = col_cs) + 
    scale_fill_manual(values = c(alpha(col_cs, 0.25), 'white')) + 
    facet_wrap(protocol ~ type_sub, nrow = 1) + 
    coord_flip() + 
    ylim(c(-1, 1)) + plot_theme + 
    theme(axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          panel.spacing = unit(0.5, 'cm'),
          strip.text = element_blank())
ggsave('../plots/Fig2e_Explore_Violins.pdf', width = 6.7, height = 2.5)



##############################################################################

### Figure S4 - All predictors ----
data_session_predictors %>%
    pivot_longer(cols = -c(id, type_sub, model, day, protocol, sex, Box, 
                           Tone_Condition, CSDS_Condition, ttprotocol, type_sex),
                 names_to = 'metric', values_to = 'value') -> data_session_predictors_long

for (m in unique(data_session_predictors_long$metric)) {
    data_session_predictors_long %>%
        filter(metric == m) -> data_session_predictors_syl
    ggplot() + 
        geom_hline(yintercept = 0) + 
        geom_point(data = data_session_predictors_syl[data_session_predictors_syl$sex == 'Male',], 
                        aes(x = value, 0.25, 
                        color = type_sub, fill = 'white', group = type_sub),
                        linewidth = 1, alpha = 0.5) +
        geom_density(data = data_session_predictors_syl[data_session_predictors_syl$sex == 'Male',], 
                        aes(x = value, after_stat(scaled), 
                        color = type_sub, fill = 'white', group = type_sub),
                        linewidth = 1, alpha = 0.25) +
        geom_point(data = data_session_predictors_syl[data_session_predictors_syl$sex == 'Female',], 
                        aes(x = value, -0.25, 
                        color = type_sub, fill = 'white', group = type_sub),
                        linewidth = 1, alpha = 0.5) +
        geom_density(data = data_session_predictors_syl[data_session_predictors_syl$sex == 'Female',], 
                        aes(x = value, -after_stat(scaled), 
                        color = type_sub, fill = type_sub, group = type_sub),
                        linewidth = 1) +
        scale_x_continuous(name = 'n Frames', expand = c(0,0)) + 
        scale_color_manual(values = col_cs) + 
        scale_fill_manual(values = c(alpha(col_cs, 0.25), 'white')) + 
        facet_wrap(protocol ~ type_sub, nrow = 1) + 
        coord_flip() + 
        ylim(c(-1, 1)) + plot_theme + 
        theme(axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            panel.spacing = unit(0.5, 'cm'),
            strip.text = element_blank())
    ggsave(glue('../plots/FigS4_{m}.pdf'), width = 7.5, height = 2.5)
}


##############################################################################