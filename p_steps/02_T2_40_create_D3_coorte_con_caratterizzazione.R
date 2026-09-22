# author: Rosa Gini

# v 1.0 22 Sep 2026

#########################################

if (TEST){
  testname <- "test_D3_coorte_con_caratterizzazione"
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

parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D3_coorte_con_caratterizzazione.xlsx")),1)))

component_variables <- unlist(unique(parameters_this_step[parameter == "component",.(value)]))

# persons <- readRDS(file.path(thisdirinput, paste0("D3_PERSONS.rds")))

rsa <- readRDS(file.path(thisdirinput, paste0("D3_RSA_ADI.rds")))

medicines <- rbind(fread(file.path(thisdirinputcsv, paste0("spf",i,".csv"))),
                   fread(file.path(thisdirinputcsv, paste0("fed",i,".csv"))))

listdates <- c("datasped")
medicines[, (listdates) := lapply(.SD, ymd), .SDcols = listdates]
setnames(medicines, c("id","datasped"),c("person_id", "DATE"))


# load data

processing <- readRDS(file.path(thisdirinput, paste0("D3_coorte_", i, ".rds")))

# index_date

 index_date <- ymd(paste0(i, "1231"))

 # select medicines
 
 medenriched <- merge(processing[,.(person_id)], medicines[ DATE >= index_date - 365 & DATE <= index_date,], by = "person_id", all = F)
 
 
# age

processing[, age := age_fast(birth_date, index_date)]

# ageband

processing[, fasciaeta := fcase(
  age >= 40 & age <= 64, "40-64",
  age >= 65 & age <= 84, "65-84",
  age >= 85, "85+"
)
           ]

# genere

setnames(processing, "gender", "genere")

# asl

setnames(processing, "ASL", "asl")

# componenti

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

# # RENDIS_Alg1
# 
# processing[, RENDIS_Alg1 := fifelse(RENDIS_Alg1_1 + RENDIS_Alg1_2 + RENDIS_Alg1_3 == 3 , 1 , 0)]
# 
# # renal
# 
# processing[, renal := fifelse(RENDIS_Alg1 + RENDIS_Alg2 >= 1 , 1 , 0)]

# iperpoliterapia	iperpoliterapia	binary	1 = “10 farmaci ATC IV nello stesso mese per 3 mesi su 12” 0 = altrimenti


temp <- medenriched

temp[, month := month(DATE)]
temp[, atc4 := substr(atc, 1,5)]
temp <- unique(temp[,.(person_id, month, atc4)])
temp <- temp[, .N, by = c("person_id", "month")]
temp[, poly := fifelse(N >= 10, 1, 0)]

temp <- temp[, .(months_poly = sum(poly)), 
             by = c("person_id")
]
temp[, iperpoliterapia := fifelse(months_poly >= 3, 1, 0)]

tokeep <- c("person_id", "iperpoliterapia")

temp <- temp[,..tokeep]

processing <- merge(processing, temp, by = "person_id", all.x = T)

processing[ is.na(iperpoliterapia), iperpoliterapia := 0 ]



# atc_5_almeno_3	lista degli ATC di 5 livello che sono stati dispensati 3+ volte durante l’anno	strings	

temp <- medenriched[, .N, by = c("person_id", "atc")]
temp <- temp[N >= 3,]
temp[, N := NULL]
temp <- temp[!is.na(atc) & atc != "", ]
temp <- temp[, .(atc_5_almeno_3 = paste(sort(unique(atc)), collapse = " ")), by = person_id]

processing <- merge(processing, temp, by = "person_id", all.x = T)

# rsa_adi


processing[, ref_date := index_date]

processing <- rsa[
  processing,
  on = .(
    person_id,
    start_d <= ref_date,
    end_d >= ref_date
  )  
]

# setnames(processing, "RSA", "rsa_adi")

processing[, c("start_d", "end_d") := NULL]  
processing[is.na(rsa_adi), rsa_adi := 0]


# clean and save

# tokeep <- c("person_id", "index_date", "period", "ASL", "age", "ageband", "genere", "met", "antidiabother", "IHD", "AMI", "bypass", "angioplastic", "STROKE", "TIA", "carot", "ateros", "organdamage", "age50plus", "dyslipidemia", "obesity", "hypertension", "smoking", "Cvriskfactors", "RENDIS_Alg1_1", "RENDIS_Alg1_2", "RENDIS_Alg1_3", "RENDIS_Alg1", "RENDIS_Alg2", "CV", "cerebro", "aop", "Cvrisk", "HF", "renal")
# 
# processing <- processing[, ..tokeep]

nameoutputfile <- paste0("D3_coorte_con_caratterizzazione_", i, ".rds")

saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))



