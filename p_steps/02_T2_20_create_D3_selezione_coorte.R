# author: Rosa Gini

# v 1.0 20 Sep 2026

#########################################

if (TEST){
  testname <- "test_D3_selezione_coorte"
  thisdirinput <- file.path(dirtest,testname)
  thisdirinputcsv <- thisdirinput
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdirinputcsv <- dirinput
  thisdiroutput <- dirtemp
}

i <- 2025

index_date <- ymd(paste0(i, "1231"))

# load data

medicines <- rbind(fread(file.path(thisdirinputcsv, paste0("spf",i,".csv"))),
                   fread(file.path(thisdirinputcsv, paste0("fed",i,".csv"))))

listdates <- c("datasped")
medicines[, (listdates) := lapply(.SD, ymd), .SDcols = listdates]
setnames(medicines, c("id","datasped"),c("person_id", "DATE"))

obsperiods <- readRDS(file = file.path(thisdirinput, "D3_OBSPERIODS.rds"))

asl <- readRDS(file = file.path(thisdirinput, "D3_ASL.rds"))

persons <- readRDS(file = file.path(thisdirinput, "D3_PERSONS.rds"))

################################
# start processing

processing <- persons


############################
# selection variables

processing[, sel := 0]

# sel_data_incomplete

thissel <- "sel_data_incomplete" 

processing[ , (thissel) := fifelse( sel == 1 | birth_date_or_gender_invalid == 1, 1, 0) ]

processing[ , sel := fifelse(get(thissel) == 1 , 1, 0) ]

# sel_no_obs_periods, sel_obs_period_not_overlapped_study_period, sel_never18plus_during_study_period

obsenriched <- merge(processing, obsperiods, by = "person_id", all.x = T)

obsenriched[, date_40th_birthday := birth_date %m+% years(40)]

thissel <- "sel_no_obs_periods" 

temp <- unique(obsenriched[is.na(start_op),.(person_id)])

temp[, (thissel) := 1]

processing <- merge(processing, temp, by = "person_id", all.x = T)

processing[ , (thissel) := fifelse( sel == 1 | get(thissel) == 1, 1, 0, 0) ]

processing[ , sel := fifelse(sel == 1 |get(thissel) == 1 , 1, 0) ]

thissel <- "sel_obs_period_not_overlapped_study_period"

# temp <- unique(obsenriched[start_op <= study_end_date & end_op >= study_start_date,.(person_id)])

temp <- unique(obsenriched[start_op <= index_date & end_op >= index_date,.(person_id)])

temp[, (thissel) := 0]

processing <- merge(processing, temp, by = "person_id", all.x = T)

processing[ , (thissel) := fifelse( sel == 1 | is.na(get(thissel)), 1, 0) ]

processing[ , sel := fifelse(sel == 1 | get(thissel) == 1 , 1, 0) ]  

thissel <- "sel_younger_than_40"

temp <- obsenriched[start_op <= index_date & end_op >= index_date & date_40th_birthday <= index_date & date_40th_birthday <= index_date ,]

setorder(temp, person_id, start_op)

temp[, n := rowid(person_id)]

temp <- temp[ n == 1, .(person_id, start_op, end_op)]

setnames(temp, c("start_op", "end_op"), c("start_study_op", "end_study_op"))

temp[, (thissel) := 0]


processing <- merge(processing, temp, by = "person_id", all.x = T)

processing[ , (thissel) := fifelse( sel == 1 | is.na(get(thissel)), 1, 0, 0) ]

processing[ , sel := fifelse(sel == 1 | get(thissel) == 1 , 1, 0) ]  

# sel_no_lookback

thissel <- "sel_no_lookback"

processing[ , (thissel) := fifelse( sel == 1 | index_date < start_study_op + 3*365, 1, 0, 1) ]

processing[ , sel := fifelse(sel == 1 | get(thissel) == 1 , 1, 0) ]


# sel_no_ASL

thissel <- "sel_no_ASL"

asl <- merge(asl, processing[sel == 0,.(person_id)], all = F)

processing[, ref_date := index_date]

processing <- asl[
  processing,
  on = .(
    person_id,
    start_d <= ref_date,
    end_d >= ref_date
  )  
]

processing[, c("start_d", "end_d") := NULL]  

processing[ , (thissel) := fifelse( sel == 1 | is.na(ASL), 1, 0) ]

processing[ , sel := fifelse(sel == 1 | get(thissel) == 1 , 1, 0) ]


# sel_no_polypharmacy

thissel <- "sel_no_polypharmacy"

medenriched <- merge(processing[sel == 0,.(person_id)], medicines[ DATE >= index_date - 365 & DATE <= index_date,], by = "person_id", all = F)

medenriched[, month := month(DATE)]
medenriched[, atc4 := substr(atc, 1,5)]
medenriched <- unique(medenriched[,.(person_id, month, atc4)])
medenriched <- medenriched[, .N, by = c("person_id", "month")]
medenriched[, poly := fifelse(N >= 5, 1, 0)]

medenriched <- medenriched[, .(months_poly = sum(poly)), 
                               by = c("person_id")
                               ]
medenriched[, (thissel) := fifelse(months_poly >= 3, 0, 1)]

tokeep <- c("person_id", thissel)

medenriched <- medenriched[,..tokeep]

processing <- merge(processing, medenriched, by = "person_id", all.x = T)

processing[ , (thissel) := fifelse( sel == 1 | is.na(get(thissel)), 1, get(thissel)) ]

processing[ , sel := fifelse(sel == 1 | get(thissel) == 1 , 1, 0) ]


####################
# final variables


# is_in_study

processing[, is_in_study := fifelse(sel == 0, 1, 0)]

# year

processing[, year := i]

# clean and save

tokeep <- c("person_id", "sel_data_incomplete", "sel_no_obs_periods", "sel_obs_period_not_overlapped_study_period", "sel_younger_than_40", "sel_no_lookback", "sel_no_ASL", "sel_no_polypharmacy", "is_in_study", "ASL", "birth_date", "gender", "year","start_study_op", "end_study_op")

processing <- processing[, ..tokeep]

nameoutputfile <- paste0("D3_selezione_coorte_", i, ".rds")

saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))

# study population

processing <- processing[is_in_study == 1,]

tokeep <- c("person_id", "birth_date", "gender","ASL", "start_study_op", "end_study_op", "year")

processing <- processing[, ..tokeep]

nameoutputfile <- paste0("D3_coorte_", i, ".rds")

saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))

