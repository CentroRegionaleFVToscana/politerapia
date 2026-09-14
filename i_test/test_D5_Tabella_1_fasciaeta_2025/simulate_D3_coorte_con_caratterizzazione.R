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
if (!require("truncnorm")) install.packages("truncnorm")
library(truncnorm)



# name of the dataset to be generated
namedataset <- "D3_coorte_con_caratterizzazione"

vocabulary_ATC <- c("A10BK", "A10BJ", "A10BX16", "A10BH", "A10BD24", "A10BD21", "A10BD19","A10BD13", "A10BD11", "A10BD10", "A10BD07", "A10BD08","A10BD09","A10AE56", "A10AE54", "A10BD16", "A10BD15", "A10BD20", "A10BD23")

# set number of persons
Npersons <- 5000
# create base 
data <- data.table::data.table(person_id = 1:Npersons)
# person_id 
data[, person_id := paste0("000000",as.character(seq_len(.N)))]
data[, person_id := paste0("P",substr(person_id, nchar(person_id) - 6, 
                                      nchar(person_id)))]

# ASL
data[, asl:=sample(c("CE", "NO", "SE"), Npersons, replace = TRUE, 
                   prob = c(rep(0.33, 3)))]

# genere
set.seed(1234)
data[, genere := as.character(sample(1:2, Npersons, replace = TRUE, 
                                     prob = c(.5,.5)))]
data[, genere := ifelse(genere == "1","M","F")]

# eta
data[, fasciaeta := as.character(sample(c("40-64", "65-84", "85+"), Npersons, replace = TRUE))]

# atc_5_almeno_3
data[, atc_5_almeno_3:=sapply(sample(3:10, .N, replace=T), function(n) paste(sample(vocabulary_ATC, n, replace = T), collapse = "_") )]

# covariates at t0: binary
covariates_binary <- c("iperpoliterapia", "rsa_adi", "cardiocircolatoria",
                       "reumatologica", "gastroenterologica", 
                       "esenzione_qualsiasi")

for (i in covariates_binary) {

  cov <- seq(0,1)
  probcov = runif(1, min = 0, max = 1)
  totprob = sum(probcov)
  probcov = c(probcov, 1 - totprob)
  data[, cov := sample(cov, Npersons, replace = TRUE, prob = probcov)]
  setnames(data,"cov",i)
}


saveRDS(data, file = paste0(thisdir, "/", namedataset, ".rds"))

