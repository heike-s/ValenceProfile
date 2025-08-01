# Standard Readouts Analysis
# This script analyzes and visualizes standard readouts including port- 
# related and freezing behaviors.
library(tidyr)
library(dplyr)
library(forcats)
library(ggplot2)
library(glue)
library(here)
library(lme4)
library(emmeans)

##############################################################################

setwd(here('..','ValenceProfile','ValenceProfile'))
source('styles.R')

### Load Combined Data ----
port <- readRDS('data/port.rds')
freezing <- readRDS('data/freezing.rds')

##############################################################################

### Figure 1 - Standard Readouts ----

# Figure 1bc - Head Entries ----
port %>%
  filter(Metric == 'HE') -> HEs

ggplot(HEs, aes(Session, Mean, color = Type, fill = paste(Sex, Type), 
                group = Type, shape = Protocol)) + 
  geom_hline(yintercept = 0) + 
  stat_summary(fun = mean, geom = 'line', size = 1) + 
  stat_summary(fun.data = mean_se, size = 1, linewidth = 1) + 
  scale_color_manual(values = col_cs) + 
  scale_fill_manual(values = c(col_cs, 'white', 'white', 'white')) + 
  scale_y_continuous(name = 'Head Entries', breaks = c(0,1,2,3), 
                     expand = c(0,0)) +
  scale_x_continuous(breaks =seq(1,15)) + 
  scale_shape_manual(values = c(21,22)) +
  coord_cartesian(ylim = c(0,3)) + 
  facet_wrap(Sex ~ Protocol, scales = 'free') + 
  plot_theme + 
  theme(panel.spacing = unit(0.5, 'cm'),
        strip.text = element_blank())
ggsave('../plots/Fig1bc_HEs_byProtocol_Raw_bySex.pdf', 
       width = 10.5, height = 6.5)

# LME model - Head entries ----
lme_he_cue <- lmer(Mean ~ Type * Sex * Protocol * as.factor(Session) + (1|ID),
                   data = HEs)
mm_hes <- emmeans(lme_he_cue, ~ Type | Session * Sex * Protocol)
tukey_hes <- summary(pairs(mm_hes, adjust = 'tukey'))
hes_contrasts_list <- list("Contrast: Sex | Protocol" = tukey_hes)

export_lmer_summary(lme_he_cue, 'data/lmer/Fig1_HEs.xlsx')
export_emmeans_contrasts(hes_contrasts_list, 
                         "data/lmer/Fig1_HEs_emmeans.xlsx", 
                         one_sheet = TRUE)


# Figure 1de - Freezing (selected parameters) ----
freezing %>%
  filter(Parameters == 'freezing_10_20') -> freezing_sp

ggplot(freezing_sp, aes(Session, Mean, color = Type, fill = paste(Sex, Type), 
                        group = Type, shape = Protocol)) + 
  geom_hline(yintercept = 0) + 
  stat_summary(fun = mean, geom = 'line', size = 1) + 
  stat_summary(fun.data = mean_se,  linewidth = 1, size = 1) + 
  scale_color_manual(values = col_cs) + 
  scale_fill_manual(values = c(col_cs, 'white', 'white', 'white')) + 
  scale_y_continuous(name = '% Freezing', breaks = c(0,25,50,75,100), 
                     expand = c(0,0)) +
  scale_x_continuous(breaks = c(1,4,7,10,13,15)) + 
  scale_shape_manual(values = c(21,23)) +
  coord_cartesian(ylim = c(0,100)) + 
  facet_wrap(Sex ~ Protocol, scales = 'free') + 
  plot_theme + 
  theme(panel.spacing = unit(0.5, 'cm'),
        strip.text = element_blank())
ggsave('../plots/Fig1de_Freezing_byProtocol_Raw_bySex.pdf', 
       width = 6.1, height = 6.5)

# LME model - Freezing (selected parameters) ----
lme_fz_cue <- lmer(Mean ~ Type * Sex * Protocol * as.factor(Session) + (1|ID),
                   data = freezing_sp)
mm_fz <- emmeans(lme_fz_cue, ~ Type | Session * Sex * Protocol)
tukey_fz <- summary(pairs(mm_fz, adjust = 'tukey'))
fz_contrasts_list <- list("Contrast: Sex | Protocol" = tukey_fz)

export_lmer_summary(lme_he_cue, 'data/lmer/Fig1_fz.xlsx')
export_emmeans_contrasts(fz_contrasts_list, 
                         "data/lmer/Fig1_fz_emmeans.xlsx", 
                         one_sheet = TRUE)

##############################################################################


### Figure S2 - Standard Port Readouts ----
port %>%
  filter(Metric != 'HE') -> port

for (me in unique(port$Metric)) {
  # Plot port metric ----
  port %>%
    filter(Metric == me) %>%
    ggplot(., aes(Session, Mean, color = Type, fill = paste(Sex, Type), 
                    group = Type, shape = Protocol)) + 
      geom_hline(yintercept = 0) + 
      stat_summary(fun = mean, geom = 'line', size = 1) + 
      stat_summary(fun.data = mean_se, size = 1, linewidth = 1) + 
      scale_color_manual(values = col_cs) + 
      scale_fill_manual(values = c(col_cs, 'white', 'white', 'white')) + 
      scale_y_continuous(name = 'Head Entries', breaks = c(0,1,2,3), 
                        expand = c(0,0)) +
      scale_x_continuous(breaks =seq(1,15)) + 
      scale_shape_manual(values = c(21,22)) +
      coord_cartesian(ylim = c(0,3)) + 
      facet_wrap(Sex ~ Protocol, scales = 'free') + 
      plot_theme + 
      theme(panel.spacing = unit(0.5, 'cm'),
            strip.text = element_blank())
    ggsave('../plots/FigS2_Port_{me}.pdf', 
          width = 10.5, height = 6.5)

  # LME model - Head entries ----
  port %>%
    filter(Metric == me) %>%
    lmer(Mean ~ Type * Sex * Protocol * as.factor(Session) + (1|ID),
         data = .) -> lme_port_cue
  mm_port <- emmeans(lme_port_cue, ~ Type | Session * Sex * Protocol)
  tukey_port <- summary(pairs(mm_port, adjust = 'tukey'))
  port_contrasts_list <- list("Contrast: Sex | Protocol" = tukey_port)

  export_lmer_summary(lme_port_cue, 'data/lmer/FigS2_port_{me}.xlsx')
  export_emmeans_contrasts(port_contrasts_list, 
                           "data/lmer/FigS2_port_{me}_emmeans.xlsx", 
                           one_sheet = TRUE)
}

##############################################################################


### Figure S3 - Freezing (All Parameters) ----
for (ps in unique(freezing$Parameters)) {
  # Plot Freezing by Parameters ----
  freezing %>%
    filter(Parameters == ps) %>%
    ggplot(., aes(Session, Mean, color = Type, fill = paste(Sex, Type), 
                        group = Type, shape = Protocol)) + 
      geom_hline(yintercept = 0) + 
      stat_summary(fun = mean, geom = 'line', size = 1) + 
      stat_summary(fun.data = mean_se,  linewidth = 1, size = 1) + 
      scale_color_manual(values = col_cs) + 
      scale_fill_manual(values = c(col_cs, 'white', 'white', 'white')) + 
      scale_y_continuous(name = '% Freezing', breaks = c(0,25,50,75,100), 
                        expand = c(0,0)) +
      scale_x_continuous(breaks = c(1,4,7,10,13,15)) + 
      scale_shape_manual(values = c(21,23)) +
      coord_cartesian(ylim = c(0,100)) + 
      facet_wrap(Sex ~ Protocol, scales = 'free') + 
      plot_theme + 
      theme(panel.spacing = unit(0.5, 'cm'),
            strip.text = element_blank())
  ggsave(glue('../plots/FigS3_Freezing_{ps}.pdf'), width = 6, height = 4)

  # LME model - Freezing (selected parameters) ----
  freezing %>%
    filter(Parameters == ps) %>%
    lmer(Mean ~ Type * Sex * Protocol * as.factor(Session) + (1|ID),
                    data = .) -> lme_fz_cue 
  mm_fz <- emmeans(lme_fz_cue, ~ Type | Session * Sex * Protocol)
  tukey_fz <- summary(pairs(mm_fz, adjust = 'tukey'))
  fz_contrasts_list <- list("Contrast: Sex | Protocol" = tukey_fz)
  export_lmer_summary(lme_he_cue, 'data/lmer/FigS3_fz_{ps}.xlsx')
  export_emmeans_contrasts(fz_contrasts_list, 
                          "data/lmer/FigS3_fz_{ps}_emmeans.xlsx", 
                          one_sheet = TRUE)
}

##############################################################################