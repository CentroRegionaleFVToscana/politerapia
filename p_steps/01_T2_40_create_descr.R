# author: Rosa Gini

# v 1.0 13 Oct 2026

#########################################
# assign input and output directories

if (TEST){
  # testname <- "test_D3_PERSONS"
  # thisdirinput <- file.path(dirtest,testname)
  # thisdiroutput <- file.path(dirtest,testname,"g_output")
  # dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirinput
  thisdiroutput <- dirtemp
  thisdirexp <- direxp
}

processing <- fread(file.path(thisdirinput,"descr_farmaci.csv"))


#########################################
# save

outputfile <- processing

nameoutput <- "descr_farmaci"
nameoutputext <- paste0(nameoutput,".rds")
assign(nameoutput, outputfile)
saveRDS(outputfile, file = file.path(thisdiroutput, nameoutputext))
fwrite(outputfile, file = file.path(thisdirexp, paste0(nameoutput,".csv")))
