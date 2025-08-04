# Valence Profile

This is the github repository accompanying the paper: [**Sex-specific exploration accounts for differences in valence learning in male and female mice**](https://www.biorxiv.org/content/10.1101/2024.05.08.593167v1).

This repo contains code and data to reproduce analyses and main and supplementary figures and tables.\
All data can be found [here](https://osf.io/4xfz2/).

------------------------------------------------------------------------

### Overview: Code and data for Figures
| Figures  | Description | Path to Code | Datafiles used | 
|----------|-------------|--------------|----------------|
| | Process kp-MoSeq output into dataframes for downstream analyses. | `./code/00_prepare_data.R` | `Data/KPMS/COHORT/DAY_kpms_df.rds` |
| 1, S2, S3 | Evaluate food port acquired metrics and freezing as metrics of learning. | `./code/01_standard_readouts.R` | `Data/port.rds`, `Data/freezing.rds` |
| | Identify PLSDA model with best fit. | `./code/02_plsda_model.R` | `Data/data_mx_recall.rds` |
| 2, S1, S4 | Evaluate model performance on recall day in different paradigms and identfy behaviors most predictive of each cue type. | `./code/03_plsda_results.R` | `Data/data_session_cue.rds`, `Data/model_csrcsm.rds`, `Data/model_csrcsm.rds`, `Data/model_csrcsm.rds`|
| 3, S5 | Evaluate learning over training; Explore importance of % exploration on learning. | `./code/04_learning.R` | `Data/plsda_predicted.rds`, `Data/data_session_cue.rds`,`Data/data_session_pre_nt.rds` |
| 4 | Explore aversive learning in mixed and aversive-only paradigm. | `./code/05_aversive.R` | `Data/plsda_predicted.rds`, `Data/data_session_cue.rds`,`Data/data_session_pre.rds`, `Data/data_session_pre_nt.rds`, `Data/data_el_cue.rds`,`Data/data_el_pre.rds` |
