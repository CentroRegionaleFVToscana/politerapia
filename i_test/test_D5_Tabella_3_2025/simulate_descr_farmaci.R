rm(list=ls(all.names=TRUE))

#set the directory where the script is saved as the working directory

if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages

if (!require("data.table")) install.packages("data.table")
library(data.table)


# name of the dataset to be generated
namedataset <- "descr_farmaci"

vocabulary_ATC <- c("A10BK02", "A10BJ03", "A10BX16", "A10BH04", "A10BD24", "A10BD21", "A10BD19","A10BD13", "A10BD11", "A10BD10", "A10BD07", "A10BD08","A10BD09","A10AE56", "A10AE54", "A10BD16", "A10BD15", "A10BD20", "A10BD23")
description_ATC_V <- paste0(letters[1:19], letters[1:19])
description_ATC_IV <- paste0(letters[1:19], letters[1:19])

# create base 
data <- data.table::data.table(farmaco_5_atc = vocabulary_ATC)

# create ATc IV level
data[, farmaco_4_atc:=substring(farmaco_5_atc, 1, 5)]

# descrizioni
data[, farmaco_5_descr:=description_ATC_V]

data[, farmaco_4_descr:= sample(letters[1:5], size = nrow(data), replace = T)]

# save
saveRDS(data, file = paste0(thisdir, "/", namedataset, ".rds"))

