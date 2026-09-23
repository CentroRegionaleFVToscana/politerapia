###################################################################
# DESCRIBE THE CONCEPT SETS
###################################################################
concept_sets_of_our_study_drugs <- c()

# "med_iperten", "med_cardioisc_angi", "med_dislip", "med_diab", "med_RENDIS_Alg1_1", "med_RENDIS_Alg1_2", "med_RENDIS_Alg1_3", "med_RENDIS_Alg2", "med_cortic", "med_antiang", "med_antitromb",  "med_bifos"

concept_sets_of_our_study_diagnosis <- c("ese_cardio","ese_reuma", "ese_gastro", "ese_altra_cronica", "dia_reuma", "dia_gastro", "dia_IHD", "dia_AMI", "dia_STROKE", "dia_TIA", "dia_aop", "dia_ateros", "dia_organdamage", "dia_dyslipidemia", "dia_obesity", "dia_hypertension", "dia_smoking", "dia_HF", "dia_RENDIS")

concept_sets_of_our_study_procedures <- c("proc_bypass", "proc_angioplasty", "proc_carot")


# names of the concept sets

name_codelist <- list()


name_codelist[["med_iperten"]] <- "Medicines contributing to the algorithm on iperten (Table B of the protocol)"
name_codelist[["med_cardioisc_angi"]] <- "Medicines contributing to the algorithm on cardioisc and to the algorithm on angi (Table B of the protocol)"
name_codelist[["med_dislip"]] <- "Medicines contributing to the algorithm on dislip (Table B of the protocol)"
name_codelist[["med_diab"]] <- "Medicines contributing to the algorithm on diab (Table B of the protocol)"
name_codelist[["med_RENDIS_Alg1_1"]] <- "Medicines of type DRUG1 contributing to the algorithm 1 on renal (Table B of the protocol), group 1"
name_codelist[["med_RENDIS_Alg1_2"]] <- "Medicines of type DRUG1 contributing to the algorithm 1 on renal (Table B of the protocol), group 2"
name_codelist[["med_RENDIS_Alg1_3"]] <- "Medicines of type DRUG1 contributing to the algorithm 1 on renal (Table B of the protocol), group 3"
name_codelist[["med_RENDIS_Alg2"]] <- "Medicines of type DRUG2 contributing to the algorithm 2 on renal (Table B of the protocol)"
name_codelist[["med_bifos"]] <- ""
name_codelist[[""]] <- ""
name_codelist[[""]] <- ""
name_codelist[[""]] <- ""
name_codelist[[""]] <- ""




name_codelist[["ese_cardio"]] <- "Esenzione per malattia cardiovascolare"
name_codelist[["ese_reuma"]] <- "Esenzione per malattia reumatologica"
name_codelist[["ese_gastro"]] <- "Esenzione per malarria gastroenterologica"
name_codelist[["ese_altra_cronica"]] <- "Esenzione per altra malattia cronica"
name_codelist[["dia_reuma"]] <- "Diagnosi di malattia reumatologica"
name_codelist[["dia_gastro"]] <- "Diagnosi di malattia gastroenterologica"
name_codelist[["dia_IHD"]] <- "Diagnoses contributing to the algorithm on IHD (Table 3 of the protocol)"
name_codelist[["dia_AMI"]] <- "Diagnoses contributing to the algorithm on AMI (Table 3 of the protocol)"
name_codelist[["dia_STROKE"]] <- "Diagnoses contributing to the algorithm on STROKE (Table 3 of the protocol)"
name_codelist[["dia_TIA"]] <- "Diagnoses contributing to the algorithm on TIA (Table 3 of the protocol)"
name_codelist[["dia_aop"]] <- "Diagnoses contributing to the algorithm on Peripheral arterial disease (Table 3 of the protocol)"
name_codelist[["dia_ateros"]] <- "Diagnoses contributing to the algorithm on atherosclerotic vascular damage (Table 3 of the protocol)"
name_codelist[["dia_organdamage"]] <- "Diagnoses contributing to the algorithm on target-organ damage  (Table 3 of the protocol)"
name_codelist[["dia_dyslipidemia"]] <- "Diagnoses contributing to the algorithm on dyslipidemia (Table 3 of the protocol)" 
name_codelist[["dia_obesity"]] <- "Diagnoses contributing to the algorithm on obesity (Table 3 of the protocol)"
name_codelist[["dia_hypertension"]] <- "Diagnoses contributing to the algorithm on hypertension (Table 3 of the protocol)"
name_codelist[["dia_smoking"]] <- "Diagnoses contributing to the algorithm on smoking (Table 3 of the protocol)"
name_codelist[["dia_HF"]] <- "Diagnoses contributing to the algorithm on heart failure (Table 3 of the protocol)"
name_codelist[["dia_RENDIS"]] <- "Diagnoses contributing to the algorithm on chronic kidney disease (Table 3 of the protocol)"

name_codelist[["proc_bypass"]] <- "Procedures contributing to the algorithm on coronary artery bypass graft (Table 3 of the protocol)"
name_codelist[["proc_angioplasty"]] <- "Procedures contributing to the algorithm on coronary angioplasty (Table 3 of the protocol)"
name_codelist[["proc_carot"]] <- "Procedures contributing to the algorithm on carotid revascularization (Table 3 of the protocol)"


# -concept_set_domains- is a 2-level list encoding for each concept set the corresponding data domain

concept_set_domains <- vector(mode="list")

for (concept_id in concept_sets_of_our_study_drugs) {
  concept_set_domains[[concept_id]] = "Medicines"
}

for (concept_id in concept_sets_of_our_study_diagnosis) {
  concept_set_domains[[concept_id]]="Diagnosis"
}

for (concept_id in concept_sets_of_our_study_procedures) {
  concept_set_domains[[concept_id]]="Procedures"
}


# -concept_set_codes_our_study- is a nested list, with 3 levels: foreach concept set, for each coding system of its data domain, the list of codes is recorded

concept_set_codes_our_study <- vector(mode="list")
concept_set_codes_our_study_excl <- vector(mode="list")

concept_set_codes_our_study[["abira"]][["ATC"]] = c("L02BX03")
concept_set_codes_our_study[["apalu"]][["ATC"]] = c("L02BB05")
concept_set_codes_our_study[["enzalu"]][["ATC"]] = c("L02BB04")
concept_set_codes_our_study[["darolu"]][["ATC"]] = c("L02BB06")

# 
# concept_set_codes_our_study[["med_iperten"]][["ATC"]] <- c("C09", "C02", "C07", "C08C")
# concept_set_codes_our_study[["med_cardioisc_angi"]][["ATC"]] <- "C01DA"
# concept_set_codes_our_study[["med_dislip"]][["ATC"]] <- "C10"
# concept_set_codes_our_study[["med_diab"]][["ATC"]] <- "A10"
# concept_set_codes_our_study[["med_RENDIS_Alg1_1"]][["ATC"]] <- c("C09C")
# concept_set_codes_our_study[["med_RENDIS_Alg1_2"]][["ATC"]] <- c("C09B")
# concept_set_codes_our_study[["med_RENDIS_Alg1_3"]][["ATC"]] <- c("M04AA01")
# concept_set_codes_our_study[["med_RENDIS_Alg2"]][["ATC"]] <- c("B03XA01", "V03AE03", "V03AE02", "V03AE01", "H05BX02", "B03XA02", "H05BX01")
# concept_set_codes_our_study[["med_bifos"]][["ATC"]] <- c()
# concept_set_codes_our_study[[""]][["ATC"]] <- c()
# concept_set_codes_our_study[[""]][["ATC"]] <- c()
# concept_set_codes_our_study[[""]][["ATC"]] <- c()
# 


concept_set_codes_our_study[["ese_cardio"]][["CODESE"]] <- c("0A02", "021", "0A31", "0031")
concept_set_codes_our_study[["ese_reuma"]][["CODESE"]] <- c("006", "045", "054", "028")
concept_set_codes_our_study[["ese_gastro"]][["CODESE"]] <- c("009")
concept_set_codes_our_study[["ese_altra_cronica"]][["CODESE"]] <- c("Q001", "0A02", "0B02", "0C02", "003", "005", "006", "007", "008", "009", "011", "012", "013", "014", "016", "017", "018", "019", "020", "021", "022", "023", "024", "025", "026", "027", "028", "029", "030", "0A31", "0031", "032", "035", "036", "037", "038", "039", " 041", "042", "044", "045", "046", "048", "049", "050", "051", "052", "053", "054", "055", "056", "057", "059", "060", "061", "062", "063", "064", "065", "066", "067")
concept_set_codes_our_study[["dia_reuma"]][["ICD9"]] <- c("714", "710", "711.1", "720.0", "720.1", "720.2", "720.8", "720.9", "696.0", "446", "725", "136.1")
concept_set_codes_our_study[["dia_gastro"]][["ICD9"]] <- c("555","556")

concept_set_codes_our_study[["dia_IHD"]][["ICD9"]] <- c("410","411","412","413","414")
concept_set_codes_our_study[["dia_AMI"]][["ICD9"]] <- c("410")
concept_set_codes_our_study[["dia_STROKE"]][["ICD9"]] <- c("430", "431", "432", "433.01", "433.11", "433.21", "433.31", "433.41", "433.51", "433.61", "433.71", "433.81", "433.91", "434.01", "434.11", "434.21", "434.31", "434.41", "434.51", "434.61", "434.71", "434.81", "434.91", "436")
concept_set_codes_our_study[["dia_TIA"]][["ICD9"]] <- c("435")
concept_set_codes_our_study[["dia_aop"]][["ICD9"]] <- c("440.1", "440.2", "440.3", "440.8", "440.9")
concept_set_codes_our_study[["dia_ateros"]][["ICD9"]] <- c("440.0", "433.00", "433.10", "433.20", "433.30", "433.40", "433.50", "433.60", "433.70", "433.80", "433.90") 
concept_set_codes_our_study[["dia_organdamage"]][["ICD9"]] <- c("250.4", "362.0", "429.3")
concept_set_codes_our_study[["dia_dyslipidemia"]][["ICD9"]] <- c("272.0", "272.1", "272.3", "272.4")
concept_set_codes_our_study[["dia_obesity"]][["ICD9"]] <-c("278.0", "278.1", "278.8", "649.1", "649.2", "V45.86") 
concept_set_codes_our_study[["dia_hypertension"]][["ICD9"]] <- c("401", "402", "403", "404", "405", "362.11", "000")
concept_set_codes_our_study[["dia_smoking"]][["ICD9"]] <- c("305.1")
concept_set_codes_our_study[["dia_HF"]][["ICD9"]] <- c("428", "398.91", "402.01", "402.11", "402.91", "404.01", "404.03", "404.11", "404.13", "404.91", "404.93")
concept_set_codes_our_study[["dia_RENDIS"]][["ICD9"]] <- c("582.0", "582.1", "582.2", "582.3", "582.4", "582.5", "582.6", "582.7", "582.8", "582.9", "581", "753.1", "590.00", "590.01", "589.0", "585", "586")

# codes to be excluded

# aop
codes_excl_dia_aop <- NULL

for (i in 0:9) {
  
  tmp <- paste0(concept_set_codes_our_study[["dia_aop"]][["ICD9"]], i)
  
  codes_excl_dia_aop <- c(codes_excl_dia_aop, tmp)
  
}

concept_set_codes_our_study_excl[["dia_aop"]][["ICD9"]] = codes_excl_dia_aop

# stroke
codes_excl_dia_stroke <- NULL

for (i in 0:9) {
  
  tmp <- paste0(c("430", "431", "432", "436"), ".", i)
  
  codes_excl_dia_stroke <- c(codes_excl_dia_stroke, tmp)
  
}

concept_set_codes_our_study_excl[["dia_STROKE"]][["ICD9"]] = codes_excl_dia_stroke

# danno a organo
codes_excl_dia_organdamage <- paste0("362.0", c(0:9))

concept_set_codes_our_study_excl[["dia_organdamage"]][["ICD9"]] = codes_excl_dia_organdamage



concept_set_codes_our_study[["proc_bypass"]][["ICD9PROC"]] <- c("36.10", "36.11", "36.12", "36.13", "36.14", "36.15", "36.16", "36.17", "36.18", "36.19")
concept_set_codes_our_study[["proc_angioplasty"]][["ICD9PROC"]] <- c("00.66", "36.06", "36.07")
concept_set_codes_our_study[["proc_carot"]][["ICD9PROC"]] <- c("00.61", "00.63")

