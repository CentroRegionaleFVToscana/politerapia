# author: Sabrina Giometto, Elena Ferrati

# v 0.1 11 Sep 2026

# prima draft

#########################################

if (TEST){
  testname <- "test_D5_Tabella_1_fasciaeta_2025"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirinput
  thisdiroutput <- dirtemp
}


# load data


data <- readRDS(file = paste0(thisdirinput, "/D3_coorte_con_caratterizzazione.rds"))
data <- as.data.table(data)

#   
# 
# for (j in fasce_eta) {
#   
#   # Create D5 with sociodemographic characteristics
#   D5_nocov <- data[drug==j, .(
#                 N          = .N,
#                 age_median = median(age),
#                 age_q1 = quantile(age, probs = 0.25),
#                 age_q3 = quantile(age, probs = 0.75),
#                 genere_F_N = sum(genere=="F"),
#                 genere_F_p = round(sum(genere=="F")/.N,3)*100),
#                 .(period_first, ASL, user_type)]
#   
#   # create D5 with binary covariates
#   covariates_binary <- c("iperten", "cardioisc", "infart", "arit", "ictus", "tia",
#                          "scompcard", "dislip", "diab", "renal", "cortic", 
#                          "antiang", "antitromb", "ipolip", "antidiab", "bifosf",
#                          "switch_apalu", "switch_enzalu", "switch_darolu", 
#                          "switch_other_oncol")
#   
#   D5_cov <- NULL
#   
#   for (i in covariates_binary) {
#     
#     tmp <- data[drug==j, .(
#                 N = .N,
#                 tmp_N = sum(get(i)==1),
#                 tmp_p = round(sum(get(i)==1)/.N,3)*100),
#                 .(period_first, ASL, user_type)]
#     
#     setnames(tmp,"tmp_N",paste0(i, "_N"))
#     setnames(tmp,"tmp_p",paste0(i, "_p"))
#     
#     if (is.null(D5_cov)) {
#       
#       D5_cov <- tmp
#       
#     } else {
#       
#       D5_cov <- merge(D5_cov, tmp, by = c("period_first", "ASL", "user_type", "N"))
#     }
#     
#   }
#   
#   # create the final D5 by merging the previous two
#   D5 <- merge(D5_nocov, D5_cov, by = c("period_first", "ASL", "user_type","N"), all = F)
#   
#   assign(paste0("D5_",j), D5)
# 
# }
# 
# for (j in drug_names) {
#   
#   saveRDS(get(paste0("D5_", j)), file = paste0(thisdiroutput, "/D5_", j, ".rds"))
#   write.csv(get(paste0("D5_", j)), file = paste0(thisdiroutput, "/D5_", j, ".csv"))
# 
#   # # save
#   # if (TEST & type_data_test=="simulation") {
#   #   
#   #   saveRDS(D5, file = paste0(thisdiroutput, "/D5_from_simulation_", j, ".rds"))
#   #   write.csv(D5, file = paste0(thisdiroutput, "/D5_from_simulation_", j, ".csv"))
#   #   
#   # } else if (TEST & type_data_test=="dummy") {
#   #   
#   #   saveRDS(D5, file = paste0(thisdiroutput, "/D5_from_dummy_data_", j,".rds"))
#   #   write.csv(D5, file = paste0(thisdiroutput, "/D5_from_dummy_data_", j,".csv"))
#   #   
#   # }
#   
# }
