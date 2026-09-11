###################################################################
# DESCRIBE THE VARIABLES
###################################################################

var_selection <- c("sel_data_incomplete", "sel_no_obs_periods",
                   "sel_obs_period_not_overlapped_study_period", 
                   "sel_no_drug", "sel_no_drug_during_obs_period",
                   "sel_no_adults","sel_no_lookback", "sel_no_ASL", 
                   "is_in_study", "is_first", "is_nofirst", "is_prevalent",
                   "drug_first")

# names of variables

codelists_variable_condition <- c("iperten", "cardioisc", "infart", "arit",
                                  "angi", "ictus", "tia", "scompcard", "dislip",
                                  "diab", "renal")
variables_condition <- paste0("VAR_",codelists_variable_condition)

codelists_variable_medication <- c("cortic", "antiang", "antitromb", "ipolip", 
                                   "antidiab", "bifosf")
variables_medication <- paste0("VAR_",codelists_variable_medication)

variables <- c(variables_condition, variables_medication)

# labels of variables

name_variable <- list()
for (concept in c(codelists_variable_condition, codelists_variable_medication)) {
  name_variable[[paste0("VAR_",concept)]] <- name_codelist[[concept]]
}


# assign the codelists and time spans to those covariates that are computed via codelists

codelists_variable <- list()
for (concept in c(codelists_variable_condition, codelists_variable_medication)) {
  codelists_variable[[paste0("VAR_",concept)]] <- concept
}
timespan <- list()
for (concept in c(codelists_variable_medication)) {
  timespan[[paste0("VAR_",concept)]] <- 730
}