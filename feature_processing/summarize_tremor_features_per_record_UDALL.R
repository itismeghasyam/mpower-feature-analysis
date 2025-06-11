############################################################################
# UDALL UMN project
# Purpose: Summarize Tremor features [mpower] into median and IQR
# Author: Meghasyam Tummalacherla
# Code modeled after: https://github.com/itismeghasyam/elevateMS_analysis/blob/a13265e6a6f44e37275618d859b93e7aa76942d5/featureExtraction/summarize_tremor_features_mpower_controls_per_record.R
############################################################################
rm(list=ls())
gc()

##############
# Required libraries
##############
library(synapser)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(doMC)
library(jsonlite)
library(parallel)
library(tidyr)
library(lubridate)
library(stringr)
library(sqldf)
library(parsedate)
library(githubr) 
# devtools::install_github("brian-bot/githubr")
library(mhealthtools) 
# devtools::install_github("Sage-Bionetworks/mhealthtools")

#############
# Download data from Synapse
##############
# login to Synapse
synapser::synLogin()

# set system environment to UTC
Sys.setenv(TZ='GMT')

tremor.tbl.id = 'syn33706544' # Tremor Activity-v5
# Select only those healthCodes from the mpower tremor table that are present in the age matched healthcodes
tremor.tbl.syn <- synapser::synTableQuery(paste0("SELECT * FROM ", tremor.tbl.id))
tremor.tbl <- tremor.tbl.syn$asDataFrame()
all.used.ids = tremor.tbl.id

# Get demographics from synapse
demo.tbl.id = 'syn33706622' # Demographics table-v2
demo.tbl.syn <- synapser::synTableQuery(paste0("SELECT * FROM ", demo.tbl.id))
demo.tbl <- demo.tbl.syn$asDataFrame()
metadata.columns <- colnames(demo.tbl)
metadata.columns <- metadata.columns[grepl('metadata', metadata.columns)]
all.used.ids <- c(all.used.ids, demo.tbl.id)

# Get tremor features from synapse and count number of windows available for each hc
ftrs.id = c(handToNose_left = 'syn66643252', handToNose_right = 'syn66643253') # time constraints
all.used.ids = c(all.used.ids, as.character(ftrs.id))

# Load features from synapse
ftrs = purrr::map(ftrs.id, function(id){
  fread(synapser::synGet(id)$path, fill = TRUE) %>%
    unique()
}) %>%
  data.table::rbindlist(idcol = 'Assay') %>%
  dplyr::inner_join(tremor.tbl %>%
                      dplyr::select(recordId, healthCode)) %>%
  dplyr::left_join(demo.tbl %>% # Also rename inferred_diagnosis to PD
                     dplyr::select(healthCode, PD = diagnosis, gender = sex)) %>% 
  dplyr::filter(!is.na(PD), !(is.na(gender))) %>% # remove NAs
  dplyr::mutate(gender = tolower(gender)) %>%
  # dplyr::mutate(PD = 'control') %>% 
  unique() %>%
  droplevels() 

ftrs$energy.tm <- as.numeric(ftrs$energy.tm)

###########################################################
## Summarize features for UDALL 
###########################################################
# Get kinetic tremor features
kinetic.ftr = ftrs %>%
  dplyr::select(-gender) %>% 
  dplyr::mutate(Assay = 'tremor') %>% # no distinction b/2 left and right assays. so making them all tremor
  tidyr::unite(rid, recordId, Assay, sensor, measurementType, axis, Window, sep = '.') %>%
  dplyr::select(-tidyselect::contains('EnergyInBand')) %>%
  dplyr::select(-tidyselect::contains('metadata')) %>% 
  dplyr::select(-tidyselect::contains('error')) %>% 
  tidyr::separate(rid, c('recordId', 'Assay', 'sensor', 'measurementType', 'axis', 'Window'), sep = '\\.') %>%
  dplyr::select(-axis, -Window) %>%
  tidyr::gather(Feature, Value, -Assay, -recordId, -healthCode, -PD, -sensor, -measurementType) %>%
  dplyr::group_by(Feature, Assay, recordId, healthCode, PD, sensor, measurementType) %>%
  dplyr::summarise(iqr = stats::IQR(Value, na.rm = T),
                   md = stats::median(Value, na.rm = T))

# Get kinetic data and covariates seperately
kinetic.cov = kinetic.ftr %>%
  dplyr::ungroup() %>%
  dplyr::select(healthCode, PD) %>%
  unique() %>%
  purrr::map_df(factor) %>%
  as.data.frame()
# rownames(kinetic.cov) = kinetic.cov$healthCode

# Get median of features
kinetic.ftr.md = kinetic.ftr %>%
  dplyr::ungroup() %>%
  dplyr::select(recordId, healthCode, Assay, sensor, PD, measurementType, Feature, md) %>%
  dplyr::mutate(type = 'md') %>%
  tidyr::unite(nFeature, Feature, type, sep = '.') %>%
  tidyr::spread(nFeature, md)

# Get iqr of features
kinetic.ftr.iqr = kinetic.ftr %>%
  dplyr::ungroup() %>%
  dplyr::select(recordId, healthCode, Assay, sensor, PD, measurementType, Feature, iqr) %>%
  dplyr::mutate(type = 'iqr') %>%
  tidyr::unite(nFeature, Feature, type, sep = '.') %>%
  tidyr::spread(nFeature, iqr)

# Combine median and iqr features (all data)
kinetic.ftr = dplyr::inner_join(kinetic.ftr.md, kinetic.ftr.iqr)

# # Remove linearly associated features
# tmp.mat = kinetic.ftr %>%
#   dplyr::select(-healthCode, -sensor, -measurementType)
# lm.combo = caret::findLinearCombos(tmp.mat)

kinetic.ftr.all = kinetic.ftr %>%
  # dplyr::select(-one_of(colnames(tmp.mat)[lm.combo$remove])) %>%
  tidyr::gather(Feature, Value, -Assay, -recordId, -healthCode, -sensor, -measurementType, -PD) %>%
  tidyr::unite(featureName, Feature, measurementType, sensor, sep = '_') %>%
  tidyr::spread(featureName, Value) %>% 
  dplyr::left_join(demo.tbl %>%
                     dplyr::select(recordId, metadata.columns))

#############
# Upload data to Synapse
#############
# upload file to Synapse with provenance
# to learn more about provenance in Synapse, go to http://docs.synapse.org/articles/provenance.html

# name and describe this activity
activityName = "Summarize tremor features for UDALL per record"
activityDescription = "Summarize tremor features for UDALL users into IQR and median"

# upload to Synapse, summary features
synapse.folder.id <- "syn26467775" # synId of folder to upload your file to
OUTPUT_FILE <- "mpowertools_tremor_summarized_record_UDALL.tsv" # name your file
write.table(kinetic.ftr.all, OUTPUT_FILE, sep="\t", row.names=F, quote=F, na="")
synStore(File(OUTPUT_FILE, parentId=synapse.folder.id),
         activityName = activityName,
         activityDescription = activityDescription,
         used = all.used.ids)
unlink(OUTPUT_FILE)