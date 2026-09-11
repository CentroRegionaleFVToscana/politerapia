###################################################################
# DESCRIBE THE CONCEPT SETS
###################################################################
concept_sets_of_our_study_drugs <- c("abira","apalu","enzalu", "darolu", "med_iperten", "med_cardioisc_angi", "med_dislip", "med_diab", "med_RENDIS_Alg1_1", "med_RENDIS_Alg1_2", "med_RENDIS_Alg1_3", "med_RENDIS_Alg2", "med_cortic", "med_antiang", "med_antitromb",  "med_bifos")

concept_sets_of_our_study_diagnosis <- c("dia_iperten","dia_cardioisc","dia_angi","dia_dislip","dia_diab","dia_renal", "dia_ictus", "dia_ima", "dia_aritmie")

drug_names <- c("abira", "apalu", "enzalu", "darolu")

# names of the concept sets

name_codelist <- list()

name_codelist[["abira"]] = "Abiraterone"
name_codelist[["apalu"]] = "Apalutamide"
name_codelist[["enzalu"]] = "Enzalutamide"
name_codelist[["darolu"]] = "Darolutamide"


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
name_codelist[[""]] <- ""
name_codelist[[""]] <- ""
name_codelist[[""]] <- ""
name_codelist[[""]] <- ""



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
concept_set_codes_our_study[[""]][["ICD9"]] <- c()
