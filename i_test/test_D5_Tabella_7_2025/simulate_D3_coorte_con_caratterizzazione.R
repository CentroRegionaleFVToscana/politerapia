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
                    "C10BD16", "C10BD15", "C10BD20", "C10BD23",
                    
                    "D10BK02", "D10BJ01", "D10BX16", "D10BH02", "D10BD24", 
                    "D10BD21", "D10BD19","D10BD13", "D10BD11", "D10BD10", 
                    "D10BD07", "D10BD08","D10BD09","D10DE56", "D10DE54", 
                    "D10BD16", "D10BD15", "D10BD20", "D10BD23",
                    
                    "M10BK02", "M10BJ01", "M10BX16", "M10BH02", "M10BD24", 
                    "M10BD21", "M10BD19","M10BD13", "M10BD11", "M10BD10", 
                    "M10BD07", "M10BD08","M10BD09","M10ME56", "M10ME54", 
                    "M10BD16", "M10BD15", "M10BD20", "M10BD23",
                    
                    "J10BK02", "J10BJ01", "J10BX16", "J10BH02", "J10BD24", 
                    "J10BD21", "J10BD19","J10BD13", "J10BD11", "J10BD10", 
                    "J10BD07", "J10BD08","J10BD09","J10JE56", "J10JE54", 
                    "J10BD16", "J10BD15", "J10BD20", "J10BD23")

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
data[, atc_5_almeno_3:=sapply(sample(3:10, .N, replace=T), function(n) paste(sample(vocabulary_ATC, n, replace = T), collapse = " ") )]

# covariates at t0: binary
covariates_binary <- c("iperpoliterapia", "rsa_adi", "cardiocircolatoria",
                       "reumatologica", "gastroenterologica", 
                       "esenzione_qualsiasi", "anticol", "fans", "antipsico",
                       "sulfo", "digo", "sci_insul", "ppi", "anticoag_fans", 
                       "anticol_atleast_3", "ace_fans_diur", "antiagg_fans")

for (i in covariates_binary) {

  cov <- seq(0,1)
  probcov = runif(1, min = 0, max = 1)
  totprob = sum(probcov)
  probcov = c(probcov, 1 - totprob)
  data[, cov := sample(cov, Npersons, replace = TRUE, prob = probcov)]
  setnames(data,"cov",i)
}


saveRDS(data, file = paste0(thisdir, "/", namedataset, "_2025.rds"))

