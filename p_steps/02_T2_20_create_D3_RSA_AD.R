# create D3_ASL

# author: Rosa Gini

# v 1.0

# 21 Sep 2026

#########################################

if (TEST){
  testname <- "test_D3_RSA_AD"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirinput
  thisdiroutput <- dirtemp
}

# load data

processing <- fread(file.path(thisdirinput,"MISURE_TD_RSA.csv"))

setnames(processing,c("id", "ini_record", "fine_record"),c("person_id", "start_d", "end_d"))

processing[, start_d := ymd(start_d)]
processing[, end_d := ymd(end_d)]

# keep information overlapping the study period

processing <- processing[start_d <= study_end_date & end_d >= study_start_date,]

# select records with correct measure

processing <- processing[misura == "RSA" | misura == "AD" ,]

# merge spells

processing <- processing[,.(person_id, start_d, end_d)]

processing <- CreateSpells(
  dataset = processing,
  id = "person_id" ,
  start_date = "start_d",
  end_date = "end_d",
  gap_allowed = 1
)
    
setnames(processing, old = c("entry_spell_category","exit_spell_category"), new = c("start_d", "end_d"))

# rsa_adi

processing[,rsa_adi := 1]


# clean and save

tokeep <- c("person_id", "start_d", "end_d", "rsa_adi")

processing <- processing[, ..tokeep]

nameoutputfile <- paste0("D3_RSA_AD.rds")

saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))

