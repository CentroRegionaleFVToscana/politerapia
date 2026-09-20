###################################################################
# DESCRIBE THE CONCEPT SETS
###################################################################
concept_sets_of_our_study_drugs <- c()

# "med_iperten", "med_cardioisc_angi", "med_dislip", "med_diab", "med_RENDIS_Alg1_1", "med_RENDIS_Alg1_2", "med_RENDIS_Alg1_3", "med_RENDIS_Alg2", "med_cortic", "med_antiang", "med_antitromb",  "med_bifos"

concept_sets_of_our_study_diagnosis <- c("ese_cardio","ese_reuma", "ese_gastro", "ese_altra_cronica")

# "dia_iperten","dia_cardioisc","dia_angi","dia_dislip","dia_diab","dia_renal", "dia_ictus", "dia_ima", "dia_aritmie", 

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




name_codelist[["dia_iperten"]] <- "Diagnoses contributing to the algorithm on iperten (Table B of the protocol)"
name_codelist[["dia_cardioisc"]] <- "Diagnoses contributing to the algorithm on cardioisc (Table B of the protocol)"
name_codelist[["dia_angi"]] <- "Diagnoses contributing to the algorithm on angi (Table B of the protocol)"
name_codelist[["dia_dislip"]] <- "Diagnoses contributing to the algorithm on dislip (Table B of the protocol)"
name_codelist[["dia_diab"]] <- "Diagnoses contributing to the algorithm on diab (Table B of the protocol)"
name_codelist[["dia_renal"]] <- "Diagnoses contributing to the algorithm on renal (Table B of the protocol)"
name_codelist[["dia_ictus"]] <- "Diagnoses contributing to the algorithm on ictus (Table B of the protocol)"
name_codelist[["ese_cardio"]] <- "Esenzione per malattia cardiovascolare"
name_codelist[["ese_reuma"]] <- "Esenzione per malattia reumatologica"
name_codelist[["ese_gastro"]] <- "Esenzione per malarria gastroenterologica"
name_codelist[["ese_altra_cronica"]] <- "Esenzione per altra malattia cronica"


# -concept_set_domains- is a 2-level list encoding for each concept set the corresponding data domain

concept_set_domains <- vector(mode="list")

for (concept_id in concept_sets_of_our_study_drugs) {
  concept_set_domains[[concept_id]] = "Medicines"
}

for (concept_id in concept_sets_of_our_study_diagnosis) {
  concept_set_domains[[concept_id]]="Diagnosis"
}


# -concept_set_codes_our_study- is a nested list, with 3 levels: foreach concept set, for each coding system of its data domain, the list of codes is recorded

concept_set_codes_our_study <- vector(mode="list")
concept_set_codes_our_study_excl <- vector(mode="list")

concept_set_codes_our_study[["abira"]][["ATC"]] = c("L02BX03")
concept_set_codes_our_study[["apalu"]][["ATC"]] = c("L02BB05")
concept_set_codes_our_study[["enzalu"]][["ATC"]] = c("L02BB04")
concept_set_codes_our_study[["darolu"]][["ATC"]] = c("L02BB06")


concept_set_codes_our_study[["med_iperten"]][["ATC"]] <- c("C09", "C02", "C07", "C08C")
concept_set_codes_our_study[["med_cardioisc_angi"]][["ATC"]] <- "C01DA"
concept_set_codes_our_study[["med_dislip"]][["ATC"]] <- "C10"
concept_set_codes_our_study[["med_diab"]][["ATC"]] <- "A10"
concept_set_codes_our_study[["med_RENDIS_Alg1_1"]][["ATC"]] <- c("C09C")
concept_set_codes_our_study[["med_RENDIS_Alg1_2"]][["ATC"]] <- c("C09B")
concept_set_codes_our_study[["med_RENDIS_Alg1_3"]][["ATC"]] <- c("M04AA01")
concept_set_codes_our_study[["med_RENDIS_Alg2"]][["ATC"]] <- c("B03XA01", "V03AE03", "V03AE02", "V03AE01", "H05BX02", "B03XA02", "H05BX01")
concept_set_codes_our_study[["med_bifos"]][["ATC"]] <- c()
concept_set_codes_our_study[[""]][["ATC"]] <- c()
concept_set_codes_our_study[[""]][["ATC"]] <- c()
concept_set_codes_our_study[[""]][["ATC"]] <- c()


concept_set_codes_our_study[["dia_iperten"]][["ICD9"]] <- "Diagnoses contributing to the algorithm on iperten (Table B of the protocol)"
concept_set_codes_our_study[["dia_cardioisc"]][["ICD9"]] <- "Diagnoses contributing to the algorithm on cardioisc (Table B of the protocol)"
concept_set_codes_our_study[["dia_angi"]][["ICD9"]] <- "Diagnoses contributing to the algorithm on angi (Table B of the protocol)"
concept_set_codes_our_study[["dia_dislip"]][["ICD9"]] <- "Diagnoses contributing to the algorithm on dislip (Table B of the protocol)"
concept_set_codes_our_study[["dia_diab"]][["ICD9"]] <- "Diagnoses contributing to the algorithm on diab (Table B of the protocol)"
concept_set_codes_our_study[["dia_renal"]][["ICD9"]] <- "Diagnoses contributing to the algorithm on renal (Table B of the protocol)"
concept_set_codes_our_study[["dia_ictus"]][["ICD9"]] <- c()
concept_set_codes_our_study[[""]][["ICD9"]] <- c()
concept_set_codes_our_study[[""]][["ICD9"]] <- c()
concept_set_codes_our_study[[""]][["ICD9"]] <- c()


concept_set_codes_our_study[["ese_cardio"]][["CODESE"]] <- c("0A02", "021", "0A31", "0031")
concept_set_codes_our_study[["ese_reuma"]][["CODESE"]] <- c("006", "045", "054")
concept_set_codes_our_study[["ese_gastro"]][["CODESE"]] <- c("009")
concept_set_codes_our_study[["ese_altra_cronica"]][["CODESE"]] <- c("Q001", "0A02", "0B02", "0C02", "003", "005", "006", "007", "008", "009", "011", "012", "013", "014", "016", "017", "018", "019", "020", "021", "022", "023", "024", "025", "026", "027", "028", "029", "030", "0A31", "0031", "032", "035", "036", "037", "038", "039", " 041", "042", "044", "045", "046", "048", "049", "050", "051", "052", "053", "054", "055", "056", "057", "059", "060", "061", "062", "063", "064", "065", "066", "067")

