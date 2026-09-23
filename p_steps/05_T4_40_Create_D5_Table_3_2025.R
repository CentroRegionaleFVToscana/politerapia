# author: Sabrina Giometto, Elena Ferrati

# v 0.1 11 Sep 2026

# prima draft

#########################################

if (TEST){
  testname <- "test_D5_Tabella_3_2025"
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

data_new <- data[rsa_adi == 1]

# atc <- unique(unlist(strsplit(data_new[,atc_5_almeno_3],"_")))
# data[,(atc):=lapply(atc,function(x) as.integer(grepl(paste0("(^|_)", x, "(_|$)"),data[,atc_5_almeno_3])))]
# atc_IV_levels <- unique(substring(atc, 1, 5))
# data[,(atc_IV_levels):=lapply(atc_IV_levels, function(x) as.integer(grepl(paste0("(^|_)", x), data[,atc_5_almeno_3])))]


atc_long <- data_new[,.(person_id, atc_5_almeno_3)]
atc_long <- atc_long[, .(atc = unlist(strsplit(atc_5_almeno_3, " ", fixed = TRUE))), by = person_id]

atc_IV_long <- copy(atc_long)
atc_IV_long[, atc_IV:=substring(atc, 1, 5)]
atc_IV_long <- unique(atc_IV_long, by = c("person_id", "atc_IV"))

atc_IV_long[, atc:=NULL]



toadd <- copy(data_new)[, asl := "Tutte"]
data_new <- rbind(data_new, toadd)

# Create D5 with sociodemographic characteristics
D5_nocov <- data_new[, .(
                          N          = .N,
                          genere_F_N = sum(genere=="F"),
                          genere_F_p = round(sum(genere=="F")/.N,3)*100),
                       .(asl)]

for (i in fasce_eta) {
    
    tmp <- data_new[, .(
                        N = .N, 
                        fasciaeta_N = sum(fasciaeta==i),
                        fasciaeta_p = round(sum(fasciaeta==i)/.N, 3)*100),
                      .(asl)] 
    
    setnames(tmp, "fasciaeta_N", paste0("fasciaeta_", i, "_N"))
    setnames(tmp, "fasciaeta_p", paste0("fasciaeta_", i, "_p"))
    
    D5_nocov <- merge(D5_nocov, tmp, by = c("asl", "N"))
    
  }

# extract 5 most frequent ATC V level
temp <- merge(atc_long, data_new[,.(person_id, asl)], by = "person_id", all = F, allow.cartesian = T)

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
  
  D5_nocov[, paste0("farmaco_piu_utilizzato_",k, "_p"):=round(get(paste0("farmaco_piu_utilizzato_",k, "_N"))/N,3)*100]
  
}

# extract 5 most frequent ATC IV level
temp <- merge(atc_IV_long, data_new[,.(person_id, asl)], by = "person_id", all = F, allow.cartesian = T)

temp <- temp[, .N, by = c("atc_IV","asl")]
setorder(temp, asl, - N)
temp[, ord := seq(.N), by = asl]
temp <- temp[ord <= 5, ]

wide <- dcast(temp, 
              asl ~ ord, 
              value.var = c("atc_IV", "N")
)

setnames(wide, sub("^atc_IV_(\\d+)$", "combinazione_piu_utilizzata_\\1", names(wide)))
setnames(wide, sub("^N_(\\d+)$", "combinazione_piu_utilizzata_\\1_N", names(wide)))


D5_nocov <- merge(D5_nocov, wide, by = "asl")

for (k in 1:5) {
  
  D5_nocov[, paste0("combinazione_piu_utilizzata_",k, "_p"):=round(get(paste0("combinazione_piu_utilizzata_",k, "_N"))/N,3)*100]
  
}


# create D5 with binary covariates
covariates_binary <- c("iperpoliterapia", "cardiocircolatoria", "reumatologica", "gastroenterologica", "esenzione_qualsiasi")


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



# create the final D5 by merging the previous two
D5 <- merge(D5_nocov, D5_cov, by = c("asl","N"), all = F)
  

saveRDS(D5, file = paste0(thisdiroutput, "/D5_Tabella_3_caratterizzazione_",year, ".rds"))
write.csv(D5, file = paste0(thisdirexp, "/D5_Tabella_3_caratterizzazione_",year, ".csv"))

