rm(list=ls(all.names=TRUE))

#set the directory where the script is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)

baselinedate <- 20151231

# list of conceptset datasets

dirarchive <- file.path(thisdir,"..","..","p_parameters","archive_parameters/")
parameters_this_step <- as.data.table(unique(readxl::read_excel(file.path(dirarchive,"codebooks",paste0("D3_coorte_con_caratterizzazione.xlsx")),1)))

component_variables <- unlist(unique(parameters_this_step[parameter == "component",.(value)]))

allingredients <- c()
for (component in component_variables) {
  ingredients <- unlist(unique(parameters_this_step[parameter == component,.(value)]))
  allingredients <- unique(c(allingredients, ingredients))
}

# # create empty datset (to be commented)
# 
# for (ingredient in allingredients) {
#   data <- data.table(ID = character(), DATE = character(), Table_cdm = character(), ord  = character())
#   namedataset <- paste0(ingredient, ".xlsx")
#   write_xlsx(data, file.path(thisdir, namedataset))
# }

listdatasetsRData <- unique(c(allingredients))
listdatasetscsv <- c("spf2025", "fed2025")


listdatasets <- c("D3_coorte_2025",  "D3_RSA_ADI", listdatasetsRData, listdatasetscsv)

# dates variables 

listdates <- list()
listdates[["D3_coorte_2025"]] <- c("birth_date", "start_study_op","end_study_op")
for (dataset in allingredients) {
  listdates[[dataset]] <- c("DATE")
}

listdates[["D3_RSA_ADI"]] <- c( "start_d","end_d")


listdates[["D3_PERSONS"]] <- c("birth_date"	,	"death_date")

for (dataset in listdatasetscsv) {
  listdates[[dataset]] <- c("datasped")
  
}


# date baseline

baseline <- vector(mode="list")
for (namedataset in listdatasets) {
  for (datevar in listdates[[namedataset]]) {
    baseline[[namedataset]][[datevar]] <- NA_Date_ # <- as.Date(lubridate::ymd(baselinedate))
  }
}


# load datasets


for (namedataset in listdatasets){
  # data <- fread(paste0(thisdir, "/", namedataset, ".csv") )
  # data[, (listdates[[namedataset]]) := lapply(.SD, lubridate::ymd), .SDcols = listdates[[namedataset]]]
  data <- as.data.table(readxl::read_excel((paste0(thisdir, "/", namedataset, ".xlsx") )))
  for (datevar in listdates[[namedataset]]) {
    if (!is.na(baseline[[namedataset]][[datevar]])){
      #  data[, (datevar) := as.Date(get(datevar), origin = "1970-01-01") + as.numeric(baseline[[namedataset]][[datevar]])]
      data[, (datevar) := as.Date(get(datevar) + baseline[[namedataset]][[datevar]])]
    }else{
      data <- data[, (datevar) := lubridate::ymd(get(datevar))]
      
    }
  }
  
  assign(namedataset,data)
  if (namedataset %in% listdatasetsRData){
    save(data, file = file.path(thisdir, paste0(namedataset,".RData")), list = namedataset)
  }else if (namedataset %in% listdatasetscsv){
    data[, (listdates[[namedataset]]) := lapply(.SD, format, "%Y%m%d"), .SDcols = listdates[[namedataset]]]
    fwrite(data, file.path(thisdir, paste0(namedataset,".csv") ))
  }else{
    saveRDS(data, file = file.path(thisdir, paste0(namedataset,".rds")))
  }
}

