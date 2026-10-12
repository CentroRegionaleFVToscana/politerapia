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

# extract most frequent combinations
top_combos <- function(dt, k, top = 10, min_supp) {
  
  # browser()
  
  # dt <- unique(dt[, .(person_id, atc_IV)])
  # n_pers <- uniqueN(dt$person_id)      
  # min_n  <- ceiling(min_supp * n_pers)
  
  denominatori <- dt[
    , .(n_pers = uniqueN(person_id)),
    by = asl
  ]
  denominatori[, min_n := ceiling(min_supp * n_pers)]
  
  freq <- dt[, .N, by = .(atc_IV, asl)]
  
  freq <- merge(freq, denominatori, by = "asl", all = F)
  
  keep <- freq[N >= min_n, .(asl, atc_IV)]
  
  dt <- merge(dt, keep, by = c("asl", "atc_IV"), all=F)
  
  dt <- dt[, if (.N >= k) .SD, by = .(person_id,asl)]
  if (nrow(dt) == 0L) return(NULL)
  
  setorder(dt, person_id, atc_IV)                 
  cmb <- dt[, .(combo = combn(atc_IV, k, paste, collapse = ",")), by = .(person_id, asl)]
  
  temp <- merge(cmb, data[,.(person_id, asl)], by = c("person_id", "asl"), all = F, allow.cartesian = T)
  
  out <- temp[, .N, by = .(asl,combo)]
  
  out <- merge(out, denominatori, by = "asl", all = FALSE)
  out <- out[N >= min_n]
  
  setorder(out, asl, -N, combo)
  out <- out[, head(.SD, top), by = asl]
  out[, p := round(100 * N / n_pers, 1)]
  
  out[, ord := seq(.N), by = asl]
  
}


res <- top_combos(temp, k = 5, min_supp = 0.01)
res_10 <- top_combos(temp, k = 10, min_supp = 0)

# load drug description
descr_farmaci <- readRDS(file = paste0(thisdirinput, "/descr_farmaci.rds"))
descr_farmaci <- as.data.table(descr_farmaci)
atc4 <- unique(descr_farmaci[, .(farmaco_4_atc, farmaco_4_descr)], by = "farmaco_4_atc")


# metto in long gli atc delle combinazioni più frequenti (top 5)
res[, row_id := .I]
long <- res[, .(atc = trimws(unlist(strsplit(combo, ",", fixed = TRUE)))), by = row_id]
long[, pos := seq_len(.N), by = row_id]
# long <- unique(long, by = "atc")
long <- merge(long, atc4,
              by.x = "atc", by.y = "farmaco_4_atc",
              all.x = TRUE)
long[is.na(farmaco_4_descr), farmaco_4_descr := atc]
setorder(long, row_id, pos)

agg <- long[, .(combo_descr = paste(farmaco_4_descr, collapse = " ")), by = row_id]
res <- merge(res, agg, by = "row_id", all.x = TRUE)
setorder(res, row_id)
res[, row_id := NULL]


# metto in long gli atc delle combinazioni più frequenti (top 10)
res_10[, row_id := .I]
long <- res_10[, .(atc = trimws(unlist(strsplit(combo, ",", fixed = TRUE)))), by = row_id]
long[, pos := seq_len(.N), by = row_id]
# long <- unique(long, by = "atc")
long <- merge(long, atc4,
              by.x = "atc", by.y = "farmaco_4_atc",
              all.x = TRUE)
long[is.na(farmaco_4_descr), farmaco_4_descr := atc]
setorder(long, row_id, pos)

agg <- long[, .(combo_descr = paste(farmaco_4_descr, collapse = " ")), by = row_id]
res_10 <- merge(res_10, agg, by = "row_id", all.x = TRUE)
setorder(res_10, row_id)
res_10[, row_id := NULL]


# create D5
D5 <- data[, .N, asl]


wide <- dcast(res, 
              asl ~ ord, 
              value.var = c("combo_descr", "N", "p")
)

setnames(wide, sub("^combo_descr_(\\d+)$", "combinazione_5_piu_utilizzata_\\1", names(wide)))
setnames(wide, sub("^N_(\\d+)$", "combinazione_5_piu_utilizzata_\\1_N", names(wide)))
setnames(wide, sub("^p_(\\d+)$", "combinazione_5_piu_utilizzata_\\1_p", names(wide)))

D5 <- merge(D5, wide, by = "asl")

wide <- dcast(res_10, 
              asl ~ ord, 
              value.var = c("combo_descr", "N", "p")
)

setnames(wide, sub("^combo_descr_(\\d+)$", "combinazione_10_piu_utilizzata_\\1", names(wide)))
setnames(wide, sub("^N_(\\d+)$", "combinazione_10_piu_utilizzata_\\1_N", names(wide)))
setnames(wide, sub("^p_(\\d+)$", "combinazione_10_piu_utilizzata_\\1_p", names(wide)))

D5 <- merge(D5, wide, by = "asl")
  
  

# save
saveRDS(D5, file = paste0(thisdiroutput, "/D5_Table_2_combinazioni_farmaci_", year, ".rds"))
write.csv(D5, file = paste0(thisdirexp, "/D5_Table_2_combinazioni_farmaci_", year, ".csv"))


