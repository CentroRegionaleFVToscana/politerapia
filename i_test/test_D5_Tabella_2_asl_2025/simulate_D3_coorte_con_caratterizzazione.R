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

vocabulary_ATC <- c("A10BK02", "A10BJ01", "A10BX16", "A10BH02", "A10BD24", 
                    "A10BD21", "A10BD19","A10BD13", "A10BD11", "A10BD10", 
                    "A10BD07", "A10BD08","A10BD09","A10AE56", "A10AE54", 
                    "A10BD16", "A10BD15", "A10BD20", "A10BD23",
                    
                    "B10BK02", "B10BJ01", "B10BX16", "B10BH02", "B10BD24", 
                    "B10BD21", "B10BD19","B10BD13", "B10BD11", "B10BD10", 
                    "B10BD07", "B10BD08","B10BD09","B10BE56", "B10AE54", 
                    "B10BD16", "B10BD15", "B10BD20", "B10BD23",
                    
                    "C10BK02", "C10BJ01", "C10BX16", "C10BH02", "C10BD24", 
                    "C10BD21", "C10BD19","C10BD13", "C10BD11", "C10BD10", 
                    "C10BD07", "C10BD08","C10BD09","C10CE56", "C10CE54", 
                    "C10BD16", "C10BD15", "C10BD20", "C10BD23")

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


saveRDS(data, file = paste0(thisdir, "/", namedataset, "_2025.rds"))

