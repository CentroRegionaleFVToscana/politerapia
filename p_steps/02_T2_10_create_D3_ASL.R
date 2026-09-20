# create D3_ASL

# author: Rosa Gini

# v 1.0

# 20 Sep 2026

#########################################

if (TEST){
  testname <- "test_D3_ASL"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirinput
  thisdiroutput <- dirtemp
}

# load data

processing <- fread(file.path(thisdirinput,"MISURE_TD.csv"))

setnames(processing,c("id", "ini_record", "fine_record"),c("person_id", "start_d", "end_d"))

processing[, start_d := ymd(start_d)]
processing[, end_d := ymd(end_d)]

# keep information overlapping the study period

processing <- processing[start_d <= study_end_date & end_d >= study_start_date,]

# asl

processing <- processing[misura == "ASL",]
processing[, ASL := fcase(
  valore == 1, "CE",
  valore == 2, "NO",
  valore == 3, "SE"
)]

# remove records with no ASL

processing <- processing[!is.na(ASL),]
    
# clean and save

tokeep <- c("person_id", "start_d", "end_d", "ASL")

processing <- processing[, ..tokeep]

nameoutputfile <- paste0("D3_ASL.rds")

saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))

