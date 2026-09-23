# author: Sabrina Giometto, Elena Ferrati

# v 0.1 16 Sep 2026 primo draft

#########################################

if (TEST){
  testname <- "test_D5_Tabella_2_asl_2025"
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
data <- readRDS(file = paste0(thisdirinput, "/D3_coorte_con_caratterizzazione_",year,".rds"))
data <- as.data.table(data)

# atc <- unique(unlist(strsplit(data[,atc_5_almeno_3],"_")))
# atc_IV_levels <- unique(substring(atc, 1, 5))
# data[,(atc_IV_levels):=lapply(atc_IV_levels, function(x) as.integer(grepl(paste0("(^|_)", x), data[,atc_5_almeno_3])))]

# variable_names_full <- NULL

atc_long <- data[,.(person_id, atc_5_almeno_3)]
atc_long <- atc_long[, .(atc = unlist(strsplit(atc_5_almeno_3, " ", fixed = TRUE))), by = person_id]

atc_IV_long <- copy(atc_long)
atc_IV_long[, atc_IV:=substring(atc, 1, 5)]
atc_IV_long <- unique(atc_IV_long, by = c("person_id", "atc_IV"))

atc_IV_long[, atc:=NULL]

toadd <- copy(data)[, asl := "Tutte"]
data <- rbind(data, toadd)


for (j in asl) {

  D5 <- data[, .N, asl]
  
  # extract 10 most frequent ATC IV level
  # distr <- data.frame(N=1)
  # 
  # for (k in atc_IV_levels) {
  # 
  #   tmp <- data.frame(sum(data[asl==j, get(k)==1]))
  # 
  #   colnames(tmp) <- k
  # 
  #   distr <- cbind(distr, tmp)
  # 
  # }
  # 
  # distr <- as.data.table(distr)
  # 
  # distr_l <- melt(distr[, N:=NULL],
  #                 measure.vars = names(distr),
  #                 variable.name = "variable",
  #                 value.name = "atc")
  # 
  # distr_l <- setorder(distr_l, -atc)
  # 
  # distr_l_sel <- distr_l[c(1:10)]
  # 
  # variable_names <- distr_l_sel[, as.character(variable)]
  # 
  # variable_names <- variable_names[!is.na(variable_names)]
  
  temp <- merge(atc_IV_long, data[,.(person_id, asl)], by = "person_id", all = F, allow.cartesian = T)
  
  temp <- temp[, .N, by = c("atc_IV","asl")]
  setorder(temp, asl, - N)
  temp[, ord := seq(.N), by = asl]
  temp <- temp[ord <= 10, ]
  
  wide <- dcast(temp, 
                asl ~ ord, 
                value.var = c("atc_IV", "N")
  )
  
  setnames(wide, sub("^atc_IV_(\\d+)$", "combinazione_piu_utilizzata_\\1", names(wide)))
  setnames(wide, sub("^N_(\\d+)$", "combinazione_piu_utilizzata_\\1_N", names(wide)))
  
  
  D5 <- merge(D5, wide, by = "asl")
  
  for (k in 1:10) {
    
    D5[, paste0("combinazione_piu_utilizzata_",k, "_p"):=round(get(paste0("combinazione_piu_utilizzata_",k, "_N"))/N,3)*100]
    
  }

}


saveRDS(D5, file = paste0(thisdiroutput, "/D5_Table_2_combinazioni_farmaci_", year, ".rds"))
write.csv(D5, file = paste0(thisdirexp, "/D5_Table_2_combinazioni_farmaci_", year, ".csv"))


