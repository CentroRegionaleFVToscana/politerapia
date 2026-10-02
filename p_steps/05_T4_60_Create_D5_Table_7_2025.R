# author: Sabrina Giometto, Elena Ferrati

# v 0.1 25 Sep 2026

# prima draft

#########################################

if (TEST){
  testname <- "test_D5_Tabella_7_2025"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  thisdirexp <- thisdiroutput
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
  thisdirexp <- direxp
}

year <- year_p

# load data

# dummy data
# data <- read.csv(paste0(thisdirinput, "/D3_coorte_con_caratterizzazione_dummy.csv"), sep = ";")
data <- readRDS(file = paste0(thisdirinput, "/D3_coorte_con_caratterizzazione_", year, ".rds"))
data <- as.data.table(data)

# filter on age
data_new <- data[fasciaeta!="40-64" ,]

toadd <- copy(data_new)[, asl := "Tutte"]
data_new <- rbind(data_new, toadd)


# create D5 with binary covariates
covariates_binary <- c("anticol", "fans", "antipsico",
                       "sulfo", "digo", "sci_insul", "ppi", "anticoag_fans", 
                       "anticol_atleast_3", "ace_fans_diur", "antiagg_fans")


D5_cov <- NULL

for (i in covariates_binary) {
  
  tmp <- data_new[, .(
    N = .N,
    tmp_N = sum(get(i)==1),
    tmp_p = round(sum(get(i)==1)/.N,3)*100),
    .(asl)]
  
  setnames(tmp,"tmp_N",paste0(i, "_N"))
  setnames(tmp,"tmp_p",paste0(i, "_p"))
  
  if (is.null(D5_cov)) {
    
    D5_cov <- tmp
    
  } else {
    
    D5_cov <- merge(D5_cov, tmp, by = c("asl", "N"))
  }
  
}

# save
saveRDS(D5_cov, file = paste0(thisdiroutput, "/D5_Tabella_7_caratterizzazione_",year, ".rds"))
write.csv(D5_cov, file = paste0(thisdirexp, "/D5_Tabella_7_caratterizzazione_",year, ".csv"))

