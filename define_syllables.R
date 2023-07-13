### define_syllables.R
# 
# Author: Heike Schuler
# 
# Define syllables and identify informative syllables based on 
# last two days of learning and first day of recall as well as 
# modeling data. 


# Prepare environment -----------------------------------------------------

library(rhdf5)
library(dplyr)

path = 'C:/Users/heike/OneDrive - McGill University/PhD/Projects/09_DLC/ValenceProfile/'
setwd(path)


# Load relevant data ------------------------------------------------------

# Read output from learning data (days 13,14,15) of discovery cohort
results = list.files(paste0(path, 'discovery_data/'), '*d*.h5')
learning_data = list()

for (day in days) {
  for (result in results) {
    if (grepl(day, result)) {
      results_file = paste0(path,result)
      files = unique(h5ls(results_file)$group)
      files = files[2:length(files)]
      files = gsub('/', '', files)
      for (file in files) {
        id = paste0(strsplit(file, '_')[[1]][1], '_', 
                    strsplit(file, '_')[[1]][3], '_',
                    gsub('DLC','',strsplit(file, '_')[[1]][4]), '_',
                    'd', day)
        learning_data[[id]] = h5read(results_file, file)
}}}}

# Read output from model training data for syllable characterization
model_data = list()

results_file = paste0(path,result)
files = unique(h5ls(results_file)$group)
files = files[2:length(files)]
files = gsub('/', '', files)
for (file in files) {
  id = paste0(strsplit(file, '_')[[1]][1], '_', 
              strsplit(file, '_')[[1]][3], '_',
              gsub('DLC','',strsplit(file, '_')[[1]][4]), '_',
              'd', day)
  day_data[[id]] = h5read(results_file, file)
  learning_data[[id]] = h5read(results_file, file)
}


# Map reindexed syls  -----------------------------------------------------

syllables = 
  
  
saveRDS(syllables, 'discovery_res/syllables.rds')


# Identify informative syls -----------------------------------------------








### END ###