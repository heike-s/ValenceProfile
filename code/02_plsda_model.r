# PLSDA Model Fitting
# Fits PLSDA models to the keypoint-MoSeq recall day data.
library(mdatools)
library(dplyr)
library(tidyr)
library(magrittr)
library(glue)
library(ggplot2)
library(here)

##############################################################################

setwd(here('..','ValenceProfile','ValenceProfile'))
source('styles.R')
set.seed(2707)

# Load raw kpms data ----
data <- readRDS('data/data_mx_recall.rds')


##############################################################################

### Functions ----
# Extracts model metrics from the model results to compare which mode
# performed best.
extract_model_metrics = function(model_results) { 
    ncomp = model_results$ncomp.selected
    nclass = model_results$nclasses
    r2 = mean(model_results$r2[,ncomp])
    bias = mean(model_results$bias[,ncomp])
    rmse = mean(model_results$rmse[,ncomp])
    fn = model_results$fn[,ncomp][1]
    fp = model_results$fp[,ncomp][1]
    tp = model_results$tp[,ncomp][1]
    tn = model_results$tn[,ncomp][1]
    sensitivity = model_results$sensitivity[1,ncomp]
    specificity = model_results$specificity[1,ncomp]
    misclassified = model_results$misclassified['Total',ncomp]
    accuracy = (tp + tn) / (tp + tn + fp + fn)
    metrics = data.frame('group' = 'total', 'ncomp' = ncomp, 'r2' = r2, 'bias' = bias, 'rmse' = rmse, 
                         'fn' = fn, 'fp' = fp, 'tp' = tp, 'tn' = tn, 
                         'sensitivity' = sensitivity, 'specificity' = specificity, 
                         'misclassified' = misclassified, 'accuracy' = accuracy,
                         row.names = NULL)
    if (nclass > 2) {
        r2 = model_results$r2[,ncomp]
        bias = model_results$bias[,ncomp]
        rmse = model_results$rmse[,ncomp]
        fn = model_results$fn[,ncomp]
        fp = model_results$fp[,ncomp]
        tp = model_results$tp[,ncomp]
        tn = model_results$tn[,ncomp]
        sensitivity = model_results$sensitivity[1:nclass,ncomp]
        specificity = model_results$specificity[1:nclass,ncomp]
        misclassified = model_results$misclassified[1:nclass,ncomp]
        accuracy = (tp + tn) / (tp + tn + fp + fn)
        metrics = rbind(metrics,
                        data.frame('group' = names(r2), 'ncomp' = ncomp, 'r2' = r2, 'bias' = bias, 'rmse' = rmse, 
                                   'fn' = fn, 'fp' = fp, 'tp' = tp, 'tn' = tn, 
                                   'sensitivity' = sensitivity, 'specificity' = specificity, 
                                   'misclassified' = misclassified, 'accuracy' = accuracy,
                                   row.names = NULL))
    }
    return(metrics)
}


##############################################################################

### Average / Summarize data per trial ----
data %>%
    pivot_longer(cols = c(cue_early_5, cue_mid_5, cue_late_5, cue_early_10), 
                 names_to = 'window', values_to = 'include') %>%
    filter(include == 'include') %>%
    group_by(model,id,sex,n,type_sub,
             tone_condition,box,day,window) %>%
    count(syl) %>%
    filter(syl <= 24) %>%
    mutate(syl = paste0('syl', syl)) %>%
    pivot_wider(names_from = c(syl), 
                values_from = nn, values_fill = 0) -> data_syl

data %>%
    pivot_longer(cols = c(cue_early_5, cue_mid_5, cue_late_5, cue_early_10), 
                 names_to = 'window', values_to = 'include') %>%
    filter(include == 'include') %>%
    group_by(model,id,sex,n,type_sub,
             tone_condition,box,day,window) %>%
    summarise(mean_oriented_port = mean(oriented_port), 
        mean_port_dist_snout = mean(port_dist_snout),
        mean_closest_corner_dist_base = 
                mean(closest_corner_dist_base)) -> data_mean

left_join(data_syl, data_mean, 
        by = c('model','id','sex','n','type_sub',
               'tone_condition','box','day','window')) -> data_trial

### Generate dataframes ----
# Generate list with subsets of data to be used for model fitting
data_trial %>%
    pivot_longer(cols = !(c(model,id,n,sex,type_sub,
                            tone_condition,box,day,window))) %>%
    group_by(model,id,sex,type_sub,
             tone_condition,box,day,window,name) %>%
    summarise(value = mean(value)) %>%
    pivot_wider() -> data_session_window

model_data = list()
model_data[['cue early (5s)']] = filter(data_session_window, window == 'cue_early_5')
model_data[['cue early (10s)']] = filter(data_session_window, window == 'cue_early_10')
model_data[['cue mid (5s)']] = filter(data_session_window, window == 'cue_mid_5')
model_data[['cue late (5s)']] = filter(data_session_window, window == 'cue_late_5')

##############################################################################

### Fit model for each window ----
all_model_metrics = data.frame()
for (window in names(model_data)) { 
    # Subset data to window
    # Both Sexes
    window_data = list()
    window_data[['data_triple']] <- model_data[[window]] 
    window_data[['data_triple']] %>%
        filter(type_sub %in% c('CSR', 'CSm')) -> window_data[['data_csrcsm']]
    window_data[['data_triple']] %>%
        filter(type_sub %in% c('CSS', 'CSm')) -> window_data[['data_csscsm']]
    window_data[['data_triple']] %>%
        filter(type_sub %in% c('CSS', 'CSR')) -> window_data[['data_csscsr']]
    
    # Males
    model_data[[window]] %>%
        filter(sex == 'Male') -> window_data[['data_triple_male']]
    window_data[['data_triple']] %>%
        filter(type_sub %in% c('CSR', 'CSm')) -> window_data[['data_csrcsm_male']]
    window_data[['data_triple']] %>%
        filter(type_sub %in% c('CSS', 'CSm')) -> window_data[['data_csscsm_male']]
    window_data[['data_triple']] %>%
        filter(type_sub %in% c('CSS', 'CSR')) -> window_data[['data_csscsr_male']]

    # Females
    model_data[[window]] %>%
        filter(sex == 'Female') -> window_data[['data_triple_female']]
    window_data[['data_triple']] %>%
        filter(type_sub %in% c('CSR', 'CSm')) -> window_data[['data_csrcsm_female']]
    window_data[['data_triple']] %>%
        filter(type_sub %in% c('CSS', 'CSm')) -> window_data[['data_csscsm_female']]
    window_data[['data_triple']] %>%
        filter(type_sub %in% c('CSS', 'CSR')) -> window_data[['data_csscsr_female']]
    
    for (data in names(window_data)) {
        #fit model 
        train_data = filter(window_data[[data]], model == 'train')
        train_x_session = train_data[, !(names(train_data) %in% id_cols)]
        train_x_session = prep.autoscale(data.matrix(train_x_session), center = TRUE, scale = TRUE)
        train_y_session = as.factor(train_data$type_sub)
        model = plsda(train_x_session, train_y_session, 
                      cv = list('rand', 4), cv.scope = 'global')
        model_metrics = extract_model_metrics(model$cvres)
        model_info = data.frame(model = window, data = data, tt = 'train', type = 'cue')
        model_summary = cbind(model_info, model_metrics)

        # save RMSE plots (Fig S1c)
        if(grepl('data_cs', data)) {
            comp_data <- data.frame('RMSE' = c(model$cvres$rmse[1:10], model$res$cal$rmse[1:10]),
                   'data' = c(rep('CV', 10), rep('Train', 10)),
                   'Components' = c(1:10, 1:10))
            ggplot(comp_data, aes(Components, RMSE, color = data)) +
                geom_line(size = 2) +
                geom_point(size = 6) +
                scale_color_manual(values = c('indianred', 'navy')) +
                labs(x = 'Components', y = 'RMSE') +
                plot_theme +
                scale_y_continuous(limits = c(0,1.5)) +    
                scale_x_continuous(breaks = 1:10) +
                theme(legend.position = 'bottom',
                    legend.title = element_blank(),
                    legend.key.size = unit(1, 'line'))
            ggsave(glue('../plots/FigS1_{window}_{data}_rmse.pdf'), width = 6, height = 4)
        }

        # predict
        test_data = filter(window_data[[data]], model == 'test')
        test_x_session = test_data[, !(names(test_data) %in% id_cols)]
        test_x_session = prep.autoscale(data.matrix(test_x_session), center = TRUE, scale = TRUE)
        test_y_session = as.factor(test_data$type_sub)
        test = predict(model, test_x_session, test_y_session)
        test_metrics = extract_model_metrics(test)
        test_info = data.frame(model = window, data = data, tt = 'test', type = 'cue')
        test_summary = cbind(test_info, test_metrics)

        all_model_metrics = rbind(all_model_metrics, model_summary, test_summary)
    }
}

##############################################################################

### Combine model metrics ----
all_model_metrics %<>%
    mutate(data_cue = paste(data, type, group, tt),
           sex = ifelse(grepl('_male',data), 'Male', 
                        ifelse(grepl('_female',data), 'Female', 'Both')),
           data_cue = gsub('_male', '', data_cue),
           data = gsub('_male', '', data),
           data_cue = gsub('_female', '', data_cue),
           data = gsub('_female', '', data),
           model = factor(model, levels = c('cue early (5s)','cue early (10s)',
                          'cue mid (5s)', 'cue late (5s)')))

all_model_metrics %>%
    mutate(model_type = ifelse(data == 'data_triple', 
                               'triple', 'pairwise')) %>%
    group_by(tt, model_type, model, sex) %>%
    summarise(accuracy = mean(accuracy),
              sensitivity = mean(sensitivity),
              specificity = mean(specificity)) %>%
    mutate(data_cue = paste(tt, model_type)) -> all_model_metrics_grouped

bind_rows(all_model_metrics_sub, all_model_metrics_sub_grouped) -> all_model_metrics_sub


### Figure S1 - Model Comparison ----
# Plot accuracy, sensitivity, specificity for all models
ggplot(all_model_metrics_sub, aes(model, data_cue, fill = accuracy)) + 
    geom_tile() + 
    geom_text(aes(label = round(accuracy*100))) +
    scale_fill_viridis_c(option = 'plasma', limits = c(0.5,1), name = '% Accuracy') + 
    scale_y_discrete(expand = c(0,0)) + 
    scale_x_discrete(expand = c(0,0)) + 
    facet_grid(data ~ sex, space = 'free', scales = 'free') + 
    plot_theme + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
          axis.line = element_blank(),
          legend.position = 'bottom', 
          legend.key.size = unit(1, 'line'))
ggsave('../plots/FigS1_model_compsarison_accuracy.pdf', width = 10, height = 12)

ggplot(all_model_metrics_sub, aes(model, data_cue, fill = sensitivity)) + 
    geom_tile() + 
    geom_text(aes(label = round(sensitivity*100))) +
    scale_fill_viridis_c(option = 'plasma', limits = c(0,1)) + 
    scale_y_discrete(expand = c(0,0)) + 
    scale_x_discrete(expand = c(0,0)) + 
    facet_grid(data ~ sex, space = 'free', scales = 'free') + 
    plot_theme + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
          axis.line = element_blank(),
          legend.position = 'bottom', 
          legend.key.size = unit(1, 'line'))
ggsave('../plots/FigS1_model_compsarison_sensitivity.pdf', width = 10, height = 11)

ggplot(all_model_metrics_sub, aes(model, data_cue, fill = specificity)) +
    geom_tile() + 
    geom_text(aes(label = round(specificity*100))) +
    scale_fill_viridis_c(option = 'plasma', limits = c(0,1)) + 
    scale_y_discrete(expand = c(0,0)) + 
    scale_x_discrete(expand = c(0,0)) + 
    facet_grid(data ~ sex, space = 'free', scales = 'free') + 
    plot_theme + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
          axis.line = element_blank(),
          legend.position = 'bottom', 
          legend.key.size = unit(1, 'line'))
ggsave('../plots/FigS1_model_compsarison_specificity.pdf', width = 10, height = 11)


##############################################################################

# Save best model: Pairwise Cue Mid 5s ----
best_data = model_data[['cue mid (5s)']]
best_data %>%
    filter(type_sub %in% c('CSR', 'CSm') & 
           model == 'train') -> best_data_csrcsm
best_data %>%
    filter(type_sub %in% c('CSS', 'CSm') & 
           model == 'train') -> best_data_csscsm
best_data %>%
    filter(day == 'D15' & 
           type_sub %in% c('CSR', 'CSS') & 
           model == 'train') -> best_data_csrcss

train_x_session = best_data_csrcsm[, !(names(best_data_csrcsm) %in% id_cols)]
train_x_session = prep.autoscale(data.matrix(train_x_session), center = TRUE, scale = TRUE)
train_y_session = as.factor(best_data_csrcsm$type_sub)
model_csrcsm = plsda(train_x_session, train_y_session, 
                     cv = list('rand', 4), cv.scope = 'global')
saveRDS(model_csrcsm, 'plsda_models/model_csrcsm.rds')

train_x_session = best_data_csscsm[, !(names(best_data_csscsm) %in% id_cols)]
train_x_session = prep.autoscale(data.matrix(train_x_session), center = TRUE, scale = TRUE)
train_y_session = as.factor(best_data_csscsm$type_sub)
model_csscsm = plsda(train_x_session, train_y_session, 
                     cv = list('rand', 4), cv.scope = 'global')
saveRDS(model_csscsm, 'plsda_models/model_csscsm.rds')

train_x_session = best_data_csrcss[, !(names(best_data_csrcss) %in% id_cols)]
train_x_session = prep.autoscale(data.matrix(train_x_session), center = TRUE, scale = TRUE)
train_y_session = as.factor(best_data_csrcss$type_sub)
model_csrcss = plsda(train_x_session, train_y_session, 
                     cv = list('rand', 4), cv.scope = 'global')
saveRDS(model_csrcss, 'plsda_models/model_csrcss.rds')


##############################################################################