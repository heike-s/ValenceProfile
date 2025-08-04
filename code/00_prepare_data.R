# Process and joing datasets from keypooint_moseq output to generate
# dataframes for further analysis and plotting.
library(dplyr)
library(tidyr)
library(magrittr)
library(glue)
library(here)

setwd(here('..'))

##############################################################################

### Training data - mixed ----
# Load train data
train_folder = 'Train_Data/'
train_master = read.csv(glue('{train_folder}master.csv'))
train_files = list.files(glue('{train_folder}data_preproc/kpms_output/'), 
                         '*.rds', full.names = TRUE)[1:15]

# Read individual kpms files and combine into one dataframe
train = data.frame()
for (file in train_files) {
    print(file)
    train = rbind(readRDS(file), train)
}
train %<>%
    filter(!(grepl('23\\d', id))) %>%  # Filter out IDs with 23 / Error in experiment
    mutate(id = as.numeric(id),
           model = 'train',
           window_all = paste(window_full, window_early, window_late),
           protocol = 'Mixed') %>%
    left_join(train_master, by = join_by('id' == 'ID')) %>%
    dplyr::select(-c(CSDS_Condition,Age))


### Test data - mixed ----
# Load test data
test_folder = 'Test_Data/'
test_master = read.csv(glue('{test_folder}master.csv'))
test_files = list.files(glue('{test_folder}data_preproc/kpms_output/'),
                        '*.rds', full.names = TRUE)[1:15]

# Read individual kpms files and combine into one dataframe
test = data.frame()
for (file in test_files) {
    print(file)
    test = rbind(readRDS(file), test)
}
test %<>%
    mutate(id = as.numeric(id), 
           model = 'test',
           window_all = paste(window_full, window_early, window_late),
           protocol = 'Mixed') %>%
    left_join(test_master, by = join_by('id' == 'ID')) %>%
    dplyr::select(-c(Age,speaker_x,speaker_y))

# Combine train and test data and save
data = rbind(train, test)
saveRDS(data, 'ValenceProfile/ValenceProfile/data/mixed_data_full.rds') ## Includes all variables

# Filter to selected variables
data %>% 
    dplyr::select(c(id, n, type_sub, model, window_all, protocol,
             window_early, day, Sex, Box, Tone_Condition, frame, 
             syl, oriented_port, port_dist_snout, 
             closest_corner_dist_base)) %>%
    rename('sex' = 'Sex', 'box' = 'Box', 
           'tone_condition' = 'Tone_Condition') -> data_mx_select
saveRDS(data_mx_select, 'ValenceProfile/ValenceProfile/data/mixed_data.rds') 


### Single valence data ----
# Load single valence data
single_folder = 'Single_Valence/'
single_master = read.csv(glue('{single_folder}master.csv'))
single_files = list.files(glue('{single_folder}data_preproc/kpms_output/'),
                          '*.rds', full.names = TRUE)[1:16]
single = data.frame()
for (file in single_files) {
    print(file)
    single = rbind(single, readRDS(file))
}
single %<>%
    mutate(id = as.numeric(id),
           model = 'single',
           window_all = paste(window_full, window_early, window_late)) %>%
    left_join(single_master, by = join_by('id' == 'ID')) %>%
    rename(protocol = Condition) %>%
    dplyr::select(-c(Age))

# Save
saveRDS(single, 'ValenceProfile/ValenceProfile/data/single_data_full.rds')

# Filter to selected variables
single %>% 
    dplyr::select(c(id, n, type_sub, model, protocol, window_all, 
             day, Sex, Box, Tone_Condition, frame, 
             syl, oriented_port, port_dist_snout, 
             closest_corner_dist_base)) %>%
    rename('sex' = 'Sex', 'box' = 'Box', 
           'tone_condition' = 'Tone_Condition') -> single_mx_select
saveRDS(single_mx_select, 'ValenceProfile/ValenceProfile/data/single_data.rds') 


##############################################################################

### Fig 2. Fig S1. - Model fitting ----
# 02_plsda_model.r
data_mx_select %>%
    mutate('cue_early_5' = ifelse(window_all == 'cue cue early', 'include', 'exclude'),
           'cue_mid_5' = ifelse(window_all == 'cue cue cue', 'include', 'exclude'),
           'cue_early_10' = ifelse(window_early == 'cue', 'include', 'exclude'),
           'cue_late_5' = ifelse(window_early == 'late', 'include', 'exclude')) %>%
    filter(day == 'D15') -> data_mx_recall

data_mx_recall <- tibble(data_mx_recall[,c('frame','model','id','sex','n','type_sub',
                        'tone_condition','box','day',
                        'cue_early_5','cue_mid_5','cue_early_10','cue_late_5',
                        'syl','oriented_port',
                        'port_dist_snout','closest_corner_dist_base')])

saveRDS(data_mx_recall, 'ValenceProfile/ValenceProfile/data/data_mx_recall.rds')


##############################################################################

### Fig 2. Fig 3. Fig S4. Fig. S5. - Model evaluation & learning ----
# 03_plsda_results.r // 04_learning.r
vars <- c('frame','model','protocol','id','sex','n','type_sub',
          'tone_condition','box','day','syl','oriented_port',
          'port_dist_snout','closest_corner_dist_base')

# Cue data raw
data_mx_select %>%
    filter(window_all == 'cue cue cue') -> data_mx_recall_midcue
data_mx_recall_midcue <- tibble(data_mx_recall_midcue[,vars])

single_mx_select %>%
    filter(window_all == 'cue cue cue') -> data_sg_recall_midcue
data_sg_recall_midcue <- tibble(data_sg_recall_midcue[,vars])

data_mxsg_cue <- bind_rows(data_mx_recall_midcue, data_sg_recall_midcue)
saveRDS(data_mxsg_cue, 'ValenceProfile/ValenceProfile/data/data_sg_mx_cue.rds')


# Pre-cue data raw
data_mx_select %>%
    filter(window_all == 'pre pre pre') %>%
    group_by(model,protocol,id,sex,n,type_sub,
             tone_condition,box,day) %>%
    slice(seq(n(), n()-200)) -> data_mx_recall_pre
data_mx_recall_pre <- tibble(data_mx_recall_pre[,vars])

single_mx_select %>%
    filter(window_all == 'pre pre pre') %>%
    group_by(model,protocol,id,sex,n,type_sub,
             tone_condition,box,day) %>%
    slice(seq(n(), n()-200)) -> data_sg_recall_pre
data_sg_recall_pre <- tibble(data_sg_recall_pre[,vars])

data_mxsg_pre <- bind_rows(data_mx_recall_pre, data_sg_recall_pre)
saveRDS(data_mxsg_pre, 'ValenceProfile/ValenceProfile/data/data_sg_mx_pre.rds')


# Cue data average
data_mxsg_cue %>%
    group_by(model,protocol,id,sex,n,type_sub,
             tone_condition,box,day) %>%
    count(syl) %>%
    filter(syl <= 24) %>%
    mutate(syl = paste0('syl', syl)) %>%
    pivot_wider(names_from = c(syl), 
                values_from = nn, values_fill = 0) -> data_syl_cue

data_mxsg_cue %>%
    group_by(model,protocol,id,sex,n,type_sub,
             tone_condition,box,day) %>%
    summarise(mean_oriented_port = mean(oriented_port), 
        mean_port_dist_snout = mean(port_dist_snout),
        mean_closest_corner_dist_base = 
                    mean(closest_corner_dist_base)) -> data_mean_cue

left_join(data_syl_cue, data_mean_cue, 
            by = c('model','protocol','id','sex','n','type_sub',
                   'tone_condition','box','day')) -> data_trial_cue

data_trial_cue %>%
    pivot_longer(cols = !(c(model,protocol,id,sex,n,type_sub,
                            tone_condition,box,day))) %>%
    group_by(model,protocol,id,sex,type_sub,name,
             tone_condition,box,day) %>%
    summarise(value = mean(value)) %>%
    pivot_wider() -> data_session_cue
saveRDS(data_session_cue, 'ValenceProfile/ValenceProfile/data/data_session_cue.rds')


# Pre-cue data average
data_mxsg_pre %>%
    group_by(model,protocol,id,sex,n,type_sub,
             tone_condition,box,day) %>%
    count(syl) %>%
    filter(syl <= 24) %>%
    mutate(syl = paste0('syl', syl)) %>%
    pivot_wider(names_from = c(syl), 
                values_from = nn, values_fill = 0) -> data_syl_pre

data_mxsg_pre %>%
    group_by(model,protocol,id,sex,n,type_sub,
             tone_condition,box,day) %>%
    summarise(mean_oriented_port = mean(oriented_port), 
        mean_port_dist_snout = mean(port_dist_snout),
        mean_closest_corner_dist_base = 
                    mean(closest_corner_dist_base)) -> data_mean_pre

left_join(data_syl_pre, data_mean_pre, 
            by = c('model','protocol','id','sex','n','type_sub',
                   'tone_condition','box','day')) -> data_trial_pre

data_trial_pre %>%
    pivot_longer(cols = !(c(model,protocol,id,sex,n,type_sub,
                            tone_condition,box,day))) %>%
    group_by(model,protocol,id,sex,type_sub,name,
             tone_condition,box,day) %>%
    summarise(value = mean(value)) %>%
    pivot_wider() -> data_session_pre
saveRDS(data_session_pre, 'ValenceProfile/ValenceProfile/data/data_session_pre.rds')


# Pre-cue data average - not by cue type
data_mxsg_pre %>%
    group_by(model,protocol,id,sex,n,
             tone_condition,box,day) %>%
    count(syl) %>%
    filter(syl <= 24) %>%
    mutate(syl = paste0('syl', syl)) %>%
    pivot_wider(names_from = c(syl), 
                values_from = nn, values_fill = 0) -> data_syl_pre

data_mxsg_pre %>%
    group_by(model,protocol,id,sex,n,
             tone_condition,box,day) %>%
    summarise(mean_oriented_port = mean(oriented_port), 
        mean_port_dist_snout = mean(port_dist_snout),
        mean_closest_corner_dist_base = 
                    mean(closest_corner_dist_base)) -> data_mean_pre

left_join(data_syl_pre, data_mean_pre, 
            by = c('model','protocol','id','sex','n',
                   'tone_condition','box','day')) -> data_trial_pre

data_trial_pre %>%
    pivot_longer(cols = !(c(model,protocol,id,sex,n,
                            tone_condition,box,day))) %>%
    group_by(model,protocol,id,sex,name,
             tone_condition,box,day) %>%
    summarise(value = mean(value)) %>%
    pivot_wider() -> data_session_pre
saveRDS(data_session_pre, 'ValenceProfile/ValenceProfile/data/data_session_pre_nt.rds')


##############################################################################

### Fig 4. - Shock related behaviors early late cue ----
# 05_aversive.r

# Cue data raw
data_mx_select %>%
    filter(window_all %in% c('cue cue early','cue cue cue')) -> data_mx_recall_midcue
data_mx_recall_cue <- tibble(data_mx_recall_midcue[,vars])

single_mx_select %>%
    filter(window_all %in% c('cue cue early','cue cue cue')) -> data_sg_recall_midcue
data_sg_recall_cue <- tibble(data_sg_recall_midcue[,vars])

data_mxsg_cue <- bind_rows(data_mx_recall_midcue, data_sg_recall_midcue)
saveRDS(data_mxsg_cue, 'ValenceProfile/ValenceProfile/data/data_sg_mx_cue_10s.rds')


# Pre-cue data raw
data_mx_select %>%
    filter(window_all == 'pre pre pre') %>%
    group_by(model,protocol,id,sex,n,type_sub,
             tone_condition,box,day) -> data_mx_recall_pre
data_mx_recall_pre <- tibble(data_mx_recall_pre[,vars])

single_mx_select %>%
    filter(window_all == 'pre pre pre') %>%
    group_by(model,protocol,id,sex,n,type_sub,
             tone_condition,box,day) -> data_sg_recall_pre
data_sg_recall_pre <- tibble(data_sg_recall_pre[,vars])

data_mxsg_pre <- bind_rows(data_mx_recall_pre, data_sg_recall_pre)
saveRDS(data_mxsg_pre, 'ValenceProfile/ValenceProfile/data/data_sg_mx_pre_10s.rds')


# Cue data average
data_mxsg_cue %>%
    group_by(model,protocol,id,sex,n,type_sub,
             tone_condition,box,day) %>%
    count(syl) %>%
    filter(syl <= 24) %>%
    mutate(syl = paste0('syl', syl)) %>%
    pivot_wider(names_from = c(syl), 
                values_from = nn, values_fill = 0) -> data_syl_cue

data_mxsg_cue %>%
    group_by(model,protocol,id,sex,n,type_sub,
             tone_condition,box,day) %>%
    summarise(mean_oriented_port = mean(oriented_port), 
        mean_port_dist_snout = mean(port_dist_snout),
        mean_closest_corner_dist_base = 
                    mean(closest_corner_dist_base)) -> data_mean_cue

left_join(data_syl_cue, data_mean_cue, 
            by = c('model','protocol','id','sex','n','type_sub',
                   'tone_condition','box','day')) -> data_trial_cue

data_trial_cue %>%
    filter(day %in% c('D01','D13','D15'),
           protocol %in% c('Aversive', 'Mixed')) %>%
    mutate('All Rear' = syl3 + syl9 + syl15 + syl20 + syl24,
           'All Locomote' = syl16 + syl19 + syl21 + syl22,
           'Explore' = `All Rear` + `All Locomote`,
           protocol = factor(protocol, levels = c('Mixed', 'Aversive'))) %>%
    group_by(model,protocol,id,sex,type_sub,tone_condition,box,day) %>%
    mutate(n_occ_type = row_number(),
           n_cues = max(row_number()),
           window = ifelse(n_occ_type == n_cues | n_occ_type == n_cues -1, 'late', 
                           ifelse(n_occ_type %in% c(1,2), 'early', 'other'))) %>%
    filter(window %in% c('early','late')) %>%
    group_by(model,protocol,id,sex,type_sub,tone_condition,box,day,window) %>%
    summarise('% Pause' = mean(syl0 / 400 * 100)) -> data_el_cue

saveRDS(data_el_cue, 'ValenceProfile/ValenceProfile/data/data_el_cue.rds')


# Pre-cue data average
data_mxsg_pre %>%
    group_by(model,protocol,id,sex,n,type_sub,
             tone_condition,box,day) %>%
    count(syl) %>%
    filter(syl <= 24) %>%
    mutate(syl = paste0('syl', syl)) %>%
    pivot_wider(names_from = c(syl), 
                values_from = nn, values_fill = 0) -> data_syl_pre

data_mxsg_pre %>%
    group_by(model,protocol,id,sex,n,type_sub,
             tone_condition,box,day) %>%
    summarise(mean_oriented_port = mean(oriented_port), 
        mean_port_dist_snout = mean(port_dist_snout),
        mean_closest_corner_dist_base = 
                    mean(closest_corner_dist_base)) -> data_mean_pre

left_join(data_syl_pre, data_mean_pre, 
            by = c('model','protocol','id','sex','n','type_sub',
                   'tone_condition','box','day')) -> data_trial_pre

data_trial_pre %>%
    filter(day %in% c('D01','D13','D15'),
           protocol %in% c('Aversive', 'Mixed')) %>%
    mutate('All Rear' = syl3 + syl9 + syl15 + syl20 + syl24,
           'All Locomote' = syl16 + syl19 + syl21 + syl22,
           'Explore' = `All Rear` + `All Locomote`,
           protocol = factor(protocol, levels = c('Mixed', 'Aversive'))) %>%
    group_by(model,protocol,id,sex,tone_condition,box,day) %>%
    mutate(n_occ_type = row_number(),
           n_cues = max(row_number()),
           window = ifelse(n_occ_type == n_cues | n_occ_type == n_cues -1, 'late', 
                           ifelse(n_occ_type %in% c(1,2), 'early', 'other'))) %>%
    filter(window %in% c('early','late')) %>%
    group_by(model,protocol,id,sex,tone_condition,box,day,window) %>%
    summarise('% Pause' = mean(syl0 / 400 * 100)) -> data_el_pre

saveRDS(data_el_pre, 'ValenceProfile/ValenceProfile/data/data_el_pre.rds')


##############################################################################