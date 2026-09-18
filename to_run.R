# CRFV TOSCANA
# studio politerapia

# authors: Rosa Gini, Sabrina Giometto, Elena Ferrati

# v 0.1 11 Sep 2026

# skeleton 

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

TEST <- T

dirinput <- ""

####################
# load parameters
source(paste0(thisdir,"/p_parameters/1_parameters_program.R"))
source(paste0(thisdir,"/p_parameters/2_parameters_CDM.R"))
# source(paste0(thisdir,"/p_parameters/3_concept_sets.R"))
source(paste0(thisdir,"/p_parameters/5_variable_lists.R"))
# source(paste0(thisdir,"/p_parameters/6_parameters_study.R"))
# source(paste0(thisdir,"/p_parameters/7_parameters_postprocessing.R"))


#

# ######################################
# # run scripts
# source(paste0(thisdir,"/p_steps/01_T2_10_create_conceptsets.R"))
# source(paste0(thisdir,"/p_steps/01_T2_20_create_spells.R"))
# source(paste0(thisdir,"/p_steps/01_T2_30_create_persons.R"))
source(paste0(thisdir,"/p_steps/05_T4_10_Create_D5_attrition.R"))
source(paste0(thisdir,"/p_steps/06_T5_10_Create_D6_Table_S1.R"))
source(paste0(thisdir,"/p_steps/05_T4_20_Create_D5_Table_1_fasciaeta_2025.R"))
source(paste0(thisdir,"/p_steps/06_T5_20_Create_D6_Table_1.R"))
source(paste0(thisdir,"/p_steps/05_T4_30_Create_D5_Table_2_asl_2025.R"))
source(paste0(thisdir,"/p_steps/06_T5_30_Create_D6_Table_2.R"))


