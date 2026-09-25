# author: Sabrina Giometto, Elena Ferrati

# v 0.2 combinazioni di ATC IV livello create

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

temp <- merge(atc_IV_long, data[,.(person_id, asl)], by = "person_id", all = F, allow.cartesian = T)

# create 10 most frequent combinations of at least 5 different ATC IV level
res <- temp[, {
  trans <- as(split(atc_IV, person_id), "transactions")
  fi <- eclat(trans,
              parameter = list(supp = 0.01, minlen = 5, maxlen = 20),
              control   = list(verbose = FALSE))
  
  if (length(fi) == 0) NULL else {
    top5 <- head(sort(fi, by = "support"), 10)
    .(combo = labels(top5),
      N     = round(quality(top5)$support * length(trans)),
      p  = round(100 * quality(top5)$support, 1))
  }
}, by = asl]

setorder(res, asl, - N)
res[, ord := seq(.N), by = asl]
res <- res[ord <= 10, ]

res[, combo := gsub(",", " ", gsub("[{}]", "", combo))]

# create 10 most frequent combinations of at least 10 ATC IV level
res_10 <- temp[, {
  trans <- as(split(atc_IV, person_id), "transactions")
  fi <- eclat(trans,
              parameter = list(supp = 0.01, minlen = 10, maxlen = 20),
              control   = list(verbose = FALSE))
  
  if (length(fi) == 0) NULL else {
    top5 <- head(sort(fi, by = "support"), 10)
    .(combo = labels(top5),
      N     = round(quality(top5)$support * length(trans)),
      p  = round(100 * quality(top5)$support, 1))
  }
}, by = asl]

setorder(res_10, asl, - N)
res_10[, ord := seq(.N), by = asl]
res_10 <- res_10[ord <= 10, ]

res_10[, combo := gsub(",", " ", gsub("[{}]", "", combo))]


# create D5
D5 <- data[, .N, asl]


wide <- dcast(res, 
              asl ~ ord, 
              value.var = c("combo", "N", "p")
)

setnames(wide, sub("^combo_(\\d+)$", "combinazione_5_piu_utilizzata_\\1", names(wide)))
setnames(wide, sub("^N_(\\d+)$", "combinazione_5_piu_utilizzata_\\1_N", names(wide)))
setnames(wide, sub("^p_(\\d+)$", "combinazione_5_piu_utilizzata_\\1_p", names(wide)))

D5 <- merge(D5, wide, by = "asl")

wide <- dcast(res_10, 
              asl ~ ord, 
              value.var = c("combo", "N", "p")
)

setnames(wide, sub("^combo_(\\d+)$", "combinazione_10_piu_utilizzata_\\1", names(wide)))
setnames(wide, sub("^N_(\\d+)$", "combinazione_10_piu_utilizzata_\\1_N", names(wide)))
setnames(wide, sub("^p_(\\d+)$", "combinazione_10_piu_utilizzata_\\1_p", names(wide)))

D5 <- merge(D5, wide, by = "asl")
  
  

# save
saveRDS(D5, file = paste0(thisdiroutput, "/D5_Table_2_combinazioni_farmaci_", year, ".rds"))
write.csv(D5, file = paste0(thisdirexp, "/D5_Table_2_combinazioni_farmaci_", year, ".csv"))


