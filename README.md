# Valence Profile

This is the github repository accompanying the paper: [**Sex-specific exploration accounts for differences in valence learning in male and female mice**](https://www.biorxiv.org/content/10.1101/2024.05.08.593167v1).

This repo contains code and data to reproduce analyses and main and supplementary figures and tables.\
All data can be found [here](https://osf.io/4xfz2/).

------------------------------------------------------------------------

### Overview: Code and data for Figures
| Figures  | Description | Path to Code | Datafiles used | 
|----------|-------------|--------------|---------------|
| | Process kp-MoSeq output into dataframes for downstream analyses. | `./code/00_prepare_data.R` | 
| 1, S2, S3 | Evaluate food port acquired metrics and freezing as metrics of learning. | `./code/01_standard_readouts.R` | `Data/port.rds`, `Data/freezing.rds` |
| 2, S1, S4 |  | `./code/02_plsda_model.R`, `./code/03_plsda_results.R` | `Data/data_mx_recall.rds` |
| 3, S5 | `./code/04_plsda_results.R` | `Data/data_mx_recall.rds` |
| 4 | `./code/05_learning.R` |
