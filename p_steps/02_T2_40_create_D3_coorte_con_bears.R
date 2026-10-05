# author: Rosa Gini

# v 1.0 5 Oct 2026

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


# load data

processing <- readRDS(file.path(thisdirinput, paste0("D3_coorte_", i, ".rds")))

# index_date

 index_date <- ymd(paste0(i, "1231"))

 
component <- "ese_cardio"
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

# sci_insul

processing[, sci_insul := fifelse(ins_rapida == 1 & ins_lunga == 0,1,0)]


# anticoag_fans	Anticoagulante orale + FANS (e.g. Warfarin/DOAC + FANS + diclofenc)									med_anticoag AND med_fans within 15 days

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
    ref_date = i.ref_date)
]

temp <- unique(temp[, .(person_id)])
temp[, anticoag_fans := 1]

processing <- merge(processing, temp, by = "person_id", all.x = T)
processing[is.na(anticoag_fans), anticoag_fans := 0]

# anticol_atleast_3	≥3 farmaci con effetto anticolinergico (i.e. burden anticolinergico cumulativo)									Almeno 3 diversi ATC in med_anticol durante 15 giorni


# whammy_1	≥3 farmaci, ognuno appartenente a “ACE-inibitore/ARB” e “FANS” e “Diuretici”, dispensati durante un massimo 15 giorni l’uno dall’altro									Almeno una tripletta med_acearb AND med_fans AND med_diur entro 15 giorni


# whammy_2	≥2 farmaci, ognuno appartenente a “ACE-inibitore/ARB + diuretici” e FANS, dispensati durante un massimo 15 giorni l’uno dall’altro									Almeno una coppia med_comb_acearb_diur AND med_fans entro 15 giorni


# whammy_3	≥2  farmaci, ognuno appartenente "ACE-inibitore/ARB + bloccanti dei canali del calcio  e FANS e Diuretici, dispensati durante un massimo 15 giorni l'uno dall'altro 									Almeno una tripletta med_comb_acearb_bloccal  AND med_fans AND med_diur entro 15 giorni


# whammy_4	≥2 farmaci, ognuno appartenente "ACE inibitore/ARB + altre combinazioni"  e "FANS"  dispensati durante un massimo 15 giorni l'uno dall'altro 									Almeno una coppia med_comb_acearb_other AND med_fans entro 15 giorni


# ace_fans_diur	ACE-inibitore/ARB + FANS + diuretico (Triplice whammy)									Whammy_1 + whammy_2 + whammy_3 + whammy_4 >= 0


# antiagg_fans	≥1 FANS e un antiaggregante dispensati durante un massimo di 15 giorni l’uno dall’altro									







# 
# processing[, ref_date := index_date]
# 
# processing <- rsa[
#   processing,
#   on = .(
#     person_id,
#     start_d <= ref_date,
#     end_d >= ref_date
#   )  
# ]



# clean and save

# tokeep <- c("person_id", "index_date", "period", "ASL", "age", "ageband", "genere", "met", "antidiabother", "IHD", "AMI", "bypass", "angioplastic", "STROKE", "TIA", "carot", "ateros", "organdamage", "age50plus", "dyslipidemia", "obesity", "hypertension", "smoking", "Cvriskfactors", "RENDIS_Alg1_1", "RENDIS_Alg1_2", "RENDIS_Alg1_3", "RENDIS_Alg1", "RENDIS_Alg2", "CV", "cerebro", "aop", "Cvrisk", "HF", "renal")
# 
# processing <- processing[, ..tokeep]

nameoutputfile <- paste0("D3_coorte_con_bears_", i, ".rds")

saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))



