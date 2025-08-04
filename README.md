# Valence Profile

This is the github repository accompanying the paper: [**Sex-specific exploration accounts for differences in valence learning in male and female mice**](https://www.biorxiv.org/content/10.1101/2024.05.08.593167v1).

This repo contains code and data to reproduce analyses and main and supplementary figures and tables.\
All data can be found [here](https://osf.io/4xfz2/). Path to datafiles are referring to file structure in OSF repository.

------------------------------------------------------------------------

### Overview: Code and Data
| Figures  | Path to Code | Datafiles used | 
|----------|--------------|----------------|
| | `./code/00_prepare_data.R` | `Data/KPMS/COHORT/DAY_kpms_df.rds` |
| 1, S2, S3 | `./code/01_standard_readouts.R` | `Data/port.rds`, `Data/freezing.rds` |
| | `./code/02_plsda_model.R` | `Data/data_mx_recall.rds` |
| 2, S1, S4 | `Data/data_session_cue.rds`, `Data/model_csrcsm.rds`, `Data/model_csrcsm.rds`, `Data/model_csrcsm.rds`|
| 3, S5 | `./code/04_learning.R` | `Data/plsda_predicted.rds`, `Data/data_session_cue.rds`,`Data/data_session_pre_nt.rds` |
| 4 | `./code/05_aversive.R` | `Data/plsda_predicted.rds`, `Data/data_session_cue.rds`,`Data/data_session_pre.rds`, `Data/data_session_pre_nt.rds`, `Data/data_el_cue.rds`,`Data/data_el_pre.rds` |
