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
                    
                    "D01BK02", "D02BJ01", "D03BX16", "D04BH02", "D05BD24", 
                    "D06BD21", "D07BD19","D08BD13", "D09BD11", "D10D10", 
                    "D11BD07", "D12BD08","D13BD09","D14CE56", "D15CE54", 
                    "D16BD16", "D17BD15", "D18BD20", "D19BD23",
                    
                    "J01BK02", "J02BJ01", "J03BX16", "J04BH02", "J05BD24", 
                    "J06BD21", "J07BD19","J08BD13", "J09BD11", "J10D10", 
                    "J11BD07", "J12BD08","J13BD09","J14CE56", "J15CE54", 
                    "J16BD16", "J17BD15", "J18BD20", "J19BD23",
                    
                    "N01BK02", "N02BJ01", "N03BX16", "N04BH02", "N05BD24", 
                    "N06BD21", "N07BD19","N08BD13", "N09BD11", "N10D10", 
                    "N11BD07", "N12BD08","N13BD09","N14CE56", "N15CE54", 
                    "N16BD16", "N17BD15", "N18BD20", "N19BD23")

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
data[, atc_5_almeno_3:=sapply(sample(3:20, .N, replace=T), function(n) paste(sample(vocabulary_ATC, n, replace = T), collapse = " ") )]

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

