# create D4_pop_ASL

# author: Rosa Gini

# v 1.0

# 28 Aug 2026

#########################################

if (TEST){
  testname <- "test_D4_pop_ASL"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
}

# load data

processing <- readRDS(file.path(thisdirinput,"D3_ASL.rds"))

processing[, date_40th_birthday := birth_date %m+% years(40)]

# pop_year

for (year in 2025:2025) {
  processing[, in_pop := fifelse(
    start_d <= ymd(paste0(year,"1231")) & 
      end_d >= ymd(paste0(year,"1231")) & 
      date_40th_birthday <= ymd(paste0(year,"1231")) 
    ,1,0)]
  setnames(processing, "in_pop", paste0("pop40_",year))
}

processing <- processing[, lapply(.SD, sum, na.rm = TRUE), .SDcols = patterns("^pop40_"), by = "ASL"]

processing <- melt(processing,
             id.vars = "ASL",
             measure.vars = patterns("^pop40_"),
             variable.name = "year",
             value.name = "pop40"
             )

processing[, year := as.numeric(sub("^pop40_(\\d+)$", "\\1", year))]


# clean and save

tokeep <- c("year", "ASL", "pop40")

processing <- processing[, ..tokeep]

nameoutputfile <- paste0("D4_pop_ASL.rds")

saveRDS(processing, file = file.path(thisdiroutput, nameoutputfile))

