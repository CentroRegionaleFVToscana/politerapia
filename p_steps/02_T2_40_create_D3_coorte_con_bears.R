# author: Rosa Gini

# v 1.0 6 Oct 2026

#########################################

if (TEST){
  testname <- "test_D3_coorte_con_bears"
  thisdirinput <- file.path(dirtest,testname)
  thisdirinputcsv <- thisdirinput
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdirinputcsv <- dirinput
  thisdiroutput <- dirtemp
}

i <- year_p

parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D3_coorte_con_bears.xlsx")),1)))

component_variables <- unlist(unique(parameters_this_step[parameter == "component",.(value)]))

#############################
# auxiliary function

# take as input 2 or 3 datasets with data model person_id,DATE
# return the dataset of all pairs (or triplets) that have all records within xxx days the one from the other, default: 15 days

find_close_records <- function(dts, max_days = 15L) {
  stopifnot(is.list(dts), length(dts) %in% 2:3)
  
  # copia pulita: una riga per (person_id, data)
  prep <- lapply(dts, function(d) {
    unique(as.data.table(d)[, .(person_id, DATE = as.IDate(DATE))])
  })
  
  # --- coppia: date2 entro +-max_days da date1 ---
  a <- prep[[1]]
  setnames(a, "DATE", "date1")
  a[, `:=`(lo = date1 - max_days, hi = date1 + max_days)]
  
  b <- prep[[2]]
  setnames(b, "DATE", "date2")
  
  out <- b[a, on = .(person_id, 
                     date2 >= lo, 
                     date2 <= hi),
           .(person_id, 
             date1 = i.date1, 
             date2 = x.date2),
           nomatch = NULL, 
           allow.cartesian = TRUE]
  
  if (length(prep) == 2L) return(out[])
  
  # --- tripletta: date3 deve stare entro max_days da ENTRAMBE le altre ---
  out[, `:=`(lo = pmax(date1, date2) - max_days,
             hi = pmin(date1, date2) + max_days)]
  
  c3 <- prep[[3]]
  setnames(c3, "DATE", "date3")
  
  out <- c3[out, on = .(person_id, 
                        date3 >= lo, 
                        date3 <= hi),
            .(person_id, 
              date1 = i.date1, 
              date2 = i.date2, 
              date3 = x.date3),
            nomatch = NULL, 
            allow.cartesian = TRUE]
  out[]
}


###########################
# load data

processing <- readRDS(file.path(thisdirinput, paste0("D3_coorte_", i, ".rds")))

###########################
# index_date

 index_date <- ymd(paste0(i, "1231"))

 
component <- "ese_cardio"

###########################
# compute all 'simple' variables

for (component in component_variables) {
  processing[, (component) := 0]
  ingredients <- unlist(unique(parameters_this_step[parameter == component,.(value)]))
  for (ingredient in ingredients) {
    temp <- as.data.table(get(load(file.path(thisdirinput, paste0(ingredient,".RData")))[[1]]))
    num <- unlist(unique(parameters_this_step[parameter ==  component & value == ingredient,.(howmany)]))
    winstart <- unlist(unique(parameters_this_step[parameter ==  component & value == ingredient,.(start)]))
    winend <- unlist(unique(parameters_this_step[parameter ==  component & value == ingredient,.(end)]))
    position <- unlist(unique(parameters_this_step[parameter ==  component & value == ingredient,.(position)]))
    setnames(temp, "ID", "person_id")
    temp[, person_id := as.character(person_id)]
    # temp <- temp[DATE >= study_start_date - 730,]
    temp <- merge(processing[,.(person_id, index_date)],temp, by = "person_id", all = F)
    temp <- temp[DATE >= index_date + winstart & DATE <= index_date + winend,]
    # if (!is.na(position) & position == "first") {
    #   temp <- temp[ord == 0 | Table_cdm == "ps",]
    # }
    temp <- unique(temp[,.(person_id, DATE)])
    temp[, n := rowid(person_id)]
    temp <- temp[n == num,]
    temp[, temp := 1]
    tokeep <- c("person_id", "temp")
    temp <- temp[,..tokeep]
    processing <- merge(processing, temp, by = "person_id", all.x = T)
    processing[is.na(temp), temp := 0]
    processing[, (component) := pmax(get(component), temp)]
    processing[, temp := NULL]
  }
  
}

###########################
# sci_insul

processing[, sci_insul := fifelse(ins_rapida == 1 & ins_lunga == 0,1,0)]

###########################
# anticoag_fans	Anticoagulante orale + FANS (e.g. Warfarin/DOAC + FANS + diclofenc)									med_anticoag AND med_fans within 15 days

varname <- "anticoag_fans"

for (ingredient in c("med_anticoag", "med_fans")) {
  temp <- as.data.table(get(load(file.path(thisdirinput, paste0(ingredient,".RData")))[[1]]))
  setnames(temp, "ID", "person_id")
  temp[, person_id := as.character(person_id)]
  temp <- merge(processing[,.(person_id, index_date)],temp, by = "person_id", all = F)
  temp <- temp[DATE >= index_date -365 & DATE <= index_date ,]
  temp <- temp[, .(person_id, DATE)]
  assign(ingredient, temp)
}

med_anticoag[, ref_date := DATE]
med_fans[, start_d := DATE - 15]
med_fans[, end_d := DATE + 15]
med_fans[, DATE := NULL]

temp <- med_fans[
  med_anticoag,
  on = .(
    person_id,
    start_d <= ref_date,
    end_d >= ref_date
  ),
  nomatch = NULL,
  .(person_id,
    start_d  = x.start_d,
    end_d    = x.end_d,
    DATE     = i.DATE,
    ref_date = i.ref_date),
  allow.cartesian = T
]

temp <- unique(temp[, .(person_id)])
temp[, (varname) := 1]

processing <- merge(processing, temp, by = "person_id", all.x = T)
processing[is.na(get(varname)), (varname) := 0]


###########################
# anticol_atleast_3	≥3 farmaci con effetto anticolinergico (i.e. burden anticolinergico cumulativo)									Almeno 3 diversi ATC in med_anticol durante 15 giorni 

varname <- "anticol_atleast_3"

for (ingredient in c("med_anticol")) {
  temp <- as.data.table(get(load(file.path(thisdirinput, paste0(ingredient,".RData")))[[1]]))
  setnames(temp, "ID", "person_id")
  temp[, person_id := as.character(person_id)]
  temp <- merge(processing[,.(person_id, index_date)],temp, by = "person_id", all = F)
  temp <- temp[DATE >= index_date -365 & DATE <= index_date ,]
  temp <- temp[, .(person_id, DATE, codvar)]
  assign(ingredient, temp)
}

temp <- data.table()
med_anticol <- med_anticol[!is.na(codvar),]
atc_anticol <- unique(med_anticol$codvar)
if (length(atc_anticol) >= 3) {
  med <- unique(med_anticol[!is.na(codvar), .(person_id, DATE = as.IDate(DATE), codvar)])
  med[, id := .I]
  med[, `:=`(lo = DATE - 15L, hi = DATE + 15L)]
  
  # coppie di dispensazioni dello stesso soggetto, ATC diversi, a distanza <= 15 gg
  pairs <- med[med, on = .(person_id, DATE >= lo, DATE <= hi),
               .(person_id = i.person_id,
                 id_c = i.id, cod_c = i.codvar, d_c = i.DATE,
                 cod_o = x.codvar, d_o = x.DATE),
               nomatch = NULL, allow.cartesian = TRUE]
  pairs <- pairs[cod_c != cod_o]
  
  # due coppie con la stessa dispensazione "centrale" = tripletta collegata
  p1 <- pairs[, .(person_id, id_c, cod_c, d_c, cod_1 = cod_o, d_1 = d_o)]
  p2 <- pairs[, .(id_c, cod_2 = cod_o, d_2 = d_o)]
  trip <- merge(p1, p2, by = "id_c", allow.cartesian = TRUE)
  trip <- trip[pmax(d_c, d_1, d_2) - pmin(d_c, d_1, d_2) <= 15]
  
  trip <- trip[cod_1 < cod_2]   # i due "laterali" devono essere ATC diversi, ogni coppia una volta sola
  
  # ordina i tre ATC per avere una chiave canonica
  trip[, `:=`(t1 = pmin(cod_c, cod_1, cod_2), t3 = pmax(cod_c, cod_1, cod_2))]
  trip[, t2 := fifelse(cod_c != t1 & cod_c != t3, cod_c,
                       fifelse(cod_1 != t1 & cod_1 != t3, cod_1, cod_2))]
  
  temp <- unique(trip[, .(person_id, t1, t2, t3)])
}

temp <- unique(temp[, .(person_id)])
temp[, (varname) := 1]

processing <- merge(processing, temp, by = "person_id", all.x = T)
processing[is.na(get(varname)), (varname) := 0]

###########################
# for wammy variables: load all the needed datasets

for (ingredient in c("med_acearb","med_fans","med_diur", "med_comb_acearb_diur", "med_comb_acearb_other","med_comb_acearb_bloccal", "med_antiagg")) {
  temp <- as.data.table(get(load(file.path(thisdirinput, paste0(ingredient,".RData")))[[1]]))
  setnames(temp, "ID", "person_id")
  temp[, person_id := as.character(person_id)]
  temp <- merge(processing[,.(person_id, index_date)],temp, by = "person_id", all = F)
  temp <- temp[DATE >= index_date -365 & DATE <= index_date ,]
  temp <- temp[, .(person_id, DATE)]
  assign(ingredient, temp)
}


###########################
# whammy_1	≥3 farmaci, ognuno appartenente a “ACE-inibitore/ARB” e “FANS” e “Diuretici”, dispensati durante un massimo 15 giorni l’uno dall’altro									Almeno una tripletta med_acearb AND med_fans AND med_diur entro 15 giorni

varname <- "whammy_1"
temp <- find_close_records(list(med_acearb, med_fans, med_diur))

temp <- unique(temp[, .(person_id)])
temp[, (varname) := 1]

processing <- merge(processing, temp, by = "person_id", all.x = T)
processing[is.na(get(varname)), (varname) := 0]


###########################
# whammy_2	≥2 farmaci, ognuno appartenente a “ACE-inibitore/ARB + diuretici” e FANS, dispensati durante un massimo 15 giorni l’uno dall’altro									Almeno una coppia med_comb_acearb_diur AND med_fans entro 15 giorni

varname <- "whammy_2"
temp <- find_close_records(list(med_comb_acearb_diur, med_fans))

temp <- unique(temp[, .(person_id)])
temp[, (varname) := 1]

processing <- merge(processing, temp, by = "person_id", all.x = T)
processing[is.na(get(varname)), (varname) := 0]



###########################
# whammy_3	≥2  farmaci, ognuno appartenente "ACE-inibitore/ARB + bloccanti dei canali del calcio  e FANS e Diuretici, dispensati durante un massimo 15 giorni l'uno dall'altro 									Almeno una tripletta med_comb_acearb_bloccal  AND med_fans AND med_diur entro 15 giorni


varname <- "whammy_3"
temp <- find_close_records(list(med_comb_acearb_bloccal, med_fans, med_diur))

temp <- unique(temp[, .(person_id)])
temp[, (varname) := 1]

processing <- merge(processing, temp, by = "person_id", all.x = T)
processing[is.na(get(varname)), (varname) := 0]





###########################
# whammy_4	≥2 farmaci, ognuno appartenente "ACE inibitore/ARB + altre combinazioni"  e "FANS"  dispensati durante un massimo 15 giorni l'uno dall'altro 									Almeno una coppia med_comb_acearb_other AND med_fans entro 15 giorni


varname <- "whammy_4"
temp <- find_close_records(list(med_comb_acearb_other, med_fans))

temp <- unique(temp[, .(person_id)])
temp[, (varname) := 1]

processing <- merge(processing, temp, by = "person_id", all.x = T)
processing[is.na(get(varname)), (varname) := 0]



###########################
# ace_fans_diur	ACE-inibitore/ARB + FANS + diuretico (Triplice whammy)									Whammy_1 + whammy_2 + whammy_3 + whammy_4 >= 0

varname <- "ace_fans_diur"

processing[, (varname) := fifelse((whammy_1 + whammy_2 + whammy_3 + whammy_4) > 0, 1,0)]


###########################
# antiagg_fans	≥1 FANS e un antiaggregante dispensati durante un massimo di 15 giorni l’uno dall’altro	


varname <- "antiagg_fans"
temp <- find_close_records(list(med_antiagg, med_fans))

temp <- unique(temp[, .(person_id)])
temp[, (varname) := 1]

processing <- merge(processing, temp, by = "person_id", all.x = T)
processing[is.na(get(varname)), (varname) := 0]



###########################
# clean and save

# tokeep <- c("person_id", "index_date", "period", "ASL", "age", "ageband", "genere", "met", "antidiabother", "IHD", "AMI", "bypass", "angioplastic", "STROKE", "TIA", "carot", "ateros", "organdamage", "age50plus", "dyslipidemia", "obesity", "hypertension", "smoking", "Cvriskfactors", "RENDIS_Alg1_1", "RENDIS_Alg1_2", "RENDIS_Alg1_3", "RENDIS_Alg1", "RENDIS_Alg2", "CV", "cerebro", "aop", "Cvrisk", "HF", "renal")
# 
# processing <- processing[, ..tokeep]

nameoutputfile <- paste0("D3_coorte_con_bears_", i, ".rds")

saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))



