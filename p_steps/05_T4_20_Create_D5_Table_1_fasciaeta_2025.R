# author: Sabrina Giometto, Elena Ferrati

# v 0.1 11 Sep 2026

# prima draft

#########################################

if (TEST){
  testname <- "test_D5_Tabella_1_fasciaeta_2025"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- dirtemp
}

year <- year_p

# load data

# dummy data
# data <- read.csv(paste0(thisdirinput, "/D3_coorte_con_caratterizzazione_dummy.csv"), sep = ";")
data <- readRDS(file = paste0(thisdirinput, "/D3_coorte_con_caratterizzazione_", year, ".rds"))
data <- as.data.table(data)

atc <- unique(unlist(strsplit(data[,atc_5_almeno_3],"_")))
# data[,(atc):=lapply(atc,function(x) as.integer(grepl(paste0("(^|_)", x, "(_|$)"),data[,atc_5_almeno_3])))]

atc_long <- data[,.(person_id, atc_5_almeno_3)]
atc_long <- atc_long[, .(atc = unlist(strsplit(atc_5_almeno_3, " ", fixed = TRUE))), by = person_id]



# store names of the five drugs (V level) most frequently dispensed
# variable_names_full <- NULL


for (j in fasce_eta) {

  dataj <- copy(data)[fasciaeta == j,]
  toadd <- copy(data)[fasciaeta == j,]
  toadd <- toadd[, asl := "Tutte"]
  dataj <- rbind(dataj, toadd)
  
  # Create D5 with sociodemographic characteristics
  D5_nocov <- dataj[, .(
                N          = .N,
                genere_F_N = sum(genere=="F"),
                genere_F_p = round(sum(genere=="F")/.N,3)*100),
                # aggiungere ulteriori covariate
                .(asl)]
  
  
  # extract 5 most frequent ATC V level in this age band

  temp <- merge(atc_long, dataj[,.(person_id, asl)], by = "person_id", all = F)
  
  temp <- temp[, .N, by = c("atc","asl")]
  setorder(temp, asl, - N)
  temp[, ord := seq(.N), by = asl]
  temp <- temp[ord <= 5, ]
  
  wide <- dcast(temp, 
                asl ~ ord, 
                value.var = c("atc", "N")
                )
  
  setnames(wide, sub("^atc_(\\d+)$", "farmaco_piu_utilizzato_\\1", names(wide)))
  setnames(wide, sub("^N_(\\d+)$", "farmaco_piu_utilizzato_\\1_N", names(wide)))
  

  D5_nocov <- merge(D5_nocov, wide, by = "asl")
  
  for (k in 1:5) {
    
    D5_nocov[, paste0("farmaco_piu_utilizzato_",k, "_p"):=round(get(paste0("farmaco_piu_utilizzato_",k, "_N"))/N,3)]
    
  }

  
  # variable_names <- distr_l_sel[, as.character(variable)]
  

  # create D5 with binary covariates
  covariates_binary <- c("iperpoliterapia", "rsa_adi", "cardiocircolatoria", "reumatologica", "gastroenterologica", "esenzione_qualsiasi")

  # covariates_full <- c(covariates_binary, variable_names)

  D5_cov <- NULL

  for (i in covariates_binary) {

    tmp <- dataj[, .(
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

  # create the final D5 by merging the previous two
  D5 <- merge(D5_nocov, D5_cov, by = c("asl","N"), all = F)

  assign(paste0("D5_",j), D5)
  
  # variable_names_full[[j]] <- variable_names

}

# save
for (j in fasce_eta) {

  saveRDS(get(paste0("D5_", j)), file = paste0(thisdiroutput, "/D5_Tabella_1_caratterizzazione_",year, "_", j, ".rds"))
  write.csv(get(paste0("D5_", j)), file = paste0(thisdiroutput, "/D5_Tabella_1_caratterizzazione_",year,"_", j, ".csv"))

}

