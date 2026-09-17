
#### D6_Table_1 ----


# authors: Sabrina Giometto, Elena Ferrati

# v 0.1 17 Sep 2026 Creation of D6 started



print('CREATE D6_Table_1')


# assign directories

if (TEST){ 
  testname <- "test_D6_Tabella_1_fasciaeta_2025"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- direxp
  thisdiroutput <- direxp
}


# load

for (j in fasce_eta) {

  D5 <- read.csv(paste0(thisdirinput, "D5_Tabella_1_caratterizzazione_2025_", j, ".csv"))

  D5 <- as.data.table(D5)

  assign(paste0("D5_Tabella_1_caratterizzazione_2025_",j), D5)

}




# helpers

add_empty_row <- function(j){

  j <- j + 1
  tab_nice[, cell := ""]
  setnames(tab_nice, "cell", paste0("cell_",j))
  return(j)
}

descriptive_N_perc <- function(j, covar) {

  j <- j + 1
  varN <- paste0(covar,"N")
  varP <- paste0(covar,"p")
  tab_nice[, cell := paste0(
    formatC(get(varN), format = "f", digits = 0, big.mark = ".",
            decimal.mark = ","), " (",
    formatC(get(varP), format = "f", digits = 1, big.mark = ".",
            decimal.mark = ","),"%)"
  )]
  setnames(tab_nice, "cell", paste0("cell_",j))
  return(j)
}

descriptive_median_q1q3 <- function(j, covar) {

  j <- j + 1
  varM <- paste0(covar,"_median")
  varQ1 <- paste0(covar,"_q1")
  varQ3 <- paste0(covar,"_q3")
  tab_nice[, cell := paste0(get(varM), " (", get(varQ1), " - ",
                            get(varQ3), ")")]
  setnames(tab_nice, "cell", paste0("cell_", j))
  return(j)
}



#########################################
# POPULATE ROWS

for (k in fasce_eta) {
  
  tab_nice <- copy(get(paste0("D5_Tabella_1_caratterizzazione_2025_", k)))
  
  # row 0
  row_header_1 <- c()
  j            <- -1

  # row 2
  row_header_1 <- c(row_header_1, "ASL")
  j <- j + 1
  tab_nice[, cell := asl]
  setnames(tab_nice, "cell", paste0("cell_", j))

  # row 4
  row_header_1 <- c(row_header_1, "Totale")
  j <- j + 1
  tab_nice[, cell := as.character(N)]
  setnames(tab_nice, "cell", paste0("cell_", j))

  # row 7
  row_header_1 <- c(row_header_1, "Donne, n (%)")
  j <- descriptive_N_perc(j, "genere_F_")

  # row 9
  row_header_1 <- c(row_header_1, "Pazienti in iper-politerapia (≥10 farmaci ATC IV nello stesso mese per 3 mesi su 12)")
  j <- descriptive_N_perc(j, "iperpoliterapia_")
  
  # row 21
  row_header_1 <- c(row_header_1, "Principi attivi più utilizzati (ATC V livello dispensati ≥ 3 volte in un anno)")
  j <- add_empty_row(j)

  # row 10
  row_header_1 <- c(row_header_1, variable_names_full[[k]][1])
  j <- descriptive_N_perc(j, paste0(variable_names_full[[k]][1], "_"))

  # row 10
  row_header_1 <- c(row_header_1, variable_names_full[[k]][2])
  j <- descriptive_N_perc(j, paste0(variable_names_full[[k]][2], "_"))
  
  # row 10
  row_header_1 <- c(row_header_1, variable_names_full[[k]][3])
  j <- descriptive_N_perc(j, paste0(variable_names_full[[k]][3], "_"))
  
  # row 10
  row_header_1 <- c(row_header_1, variable_names_full[[k]][4])
  j <- descriptive_N_perc(j, paste0(variable_names_full[[k]][4], "_"))
  
  # row 10
  row_header_1 <- c(row_header_1, variable_names_full[[k]][5])
  j <- descriptive_N_perc(j, paste0(variable_names_full[[k]][5], "_"))

  # row 21
  row_header_1 <- c(row_header_1, "Contesto assistenziale")
  j <- add_empty_row(j)

  # row 13
  row_header_1 <- c(row_header_1, "Angina")
  j <- descriptive_N_perc(j, "angi_")

  # row 14
  row_header_1 <- c(row_header_1, "Ictus ischemico o emorragico")
  j <- descriptive_N_perc(j, "ictus_")

  # row 15
  row_header_1 <- c(row_header_1, "Attacco ischemico transitorio (TIA)")
  j <- descriptive_N_perc(j, "tia_")
  
  # row 16
  row_header_1 <- c(row_header_1, "Scompenso cardiaco")
  j <- descriptive_N_perc(j, "scompcard_")
  
  # row 17
  row_header_1 <- c(row_header_1, "Dislipidemia")
  j <- descriptive_N_perc(j, "dislip_")
  
  # row 18
  row_header_1 <- c(row_header_1, "Diabete mellito")
  j <- descriptive_N_perc(j, "diab_")
  
  # row 19
  row_header_1 <- c(row_header_1, "Insufficienza renale cronica")
  j <- descriptive_N_perc(j, "renal_") 
  
  # row 20
  row_header_1 <- c(row_header_1, "Trattamenti concomitanti, media (DS)")
  j <- descriptive_median_q1q3(j, "conc_treat")
  
  # row 21
  row_header_1 <- c(row_header_1, "Farmaci concomitanti")
  j <- add_empty_row(j)
  
  # row 22
  row_header_1 <- c(row_header_1, "Corticosteroidi")
  j <- descriptive_N_perc(j, "cortic_")
  
  # row 20
  row_header_1 <- c(row_header_1, "Antianginosi")
  j <- descriptive_N_perc(j, "antiang_") 
  
  # row 23
  row_header_1 <- c(row_header_1, "Antitrombotici")
  j <- descriptive_N_perc(j, "antitromb_") 
  
  # row 24
  row_header_1 <- c(row_header_1, "Ipolipemizzanti")
  j <- descriptive_N_perc(j, "ipolip_")
  
  # row 25
  row_header_1 <- c(row_header_1, "Antidiabetici")
  j <- descriptive_N_perc(j, "antidiab_")
  
  # row 26
  row_header_1 <- c(row_header_1, "Bifosfonati")
  j <- descriptive_N_perc(j, "bifosf_")
  
  # row 27
  row_header_1 <- c(row_header_1, "Interruzione trattamento a 12 mesi")
  j <- descriptive_N_perc(j, "discont_12_")
  
  # row 27
  row_header_1 <- c(row_header_1, "Interruzione trattamento a 24 mesi")
  j <- descriptive_N_perc(j, "discont_24_")
  
  # row 28
  row_header_1 <- c(row_header_1, "Switch")
  j <- add_empty_row(j)
  
  # row 29
  row_header_1 <- c(row_header_1, "Vs apalutamide a 12 mesi")
  j <- descriptive_N_perc(j, "switch_apalu_12_")
  
  # row 30
  row_header_1 <- c(row_header_1, "Vs enzalutamide a 12 mesi")
  j <- descriptive_N_perc(j, "switch_enzalu_12_")
  
  # row 31
  row_header_1 <- c(row_header_1, "Vs darolutamide a 12 mesi")
  j <- descriptive_N_perc(j, "switch_darolu_12_")
  
  # row 32
  row_header_1 <- c(row_header_1, "Vs altri farmaci oncologici (L01) a 12 mesi")
  j <- descriptive_N_perc(j, "switch_other_oncol_12_")
  
  # row 29
  row_header_1 <- c(row_header_1, "Vs apalutamide a 24 mesi")
  j <- descriptive_N_perc(j, "switch_apalu_24_")
  
  # row 30
  row_header_1 <- c(row_header_1, "Vs enzalutamide a 24 mesi")
  j <- descriptive_N_perc(j, "switch_enzalu_24_")
  
  # row 31
  row_header_1 <- c(row_header_1, "Vs darolutamide a 24 mesi")
  j <- descriptive_N_perc(j, "switch_darolu_24_")
  
  # row 32
  row_header_1 <- c(row_header_1, "Vs altri farmaci oncologici (L01) a 24 mesi")
  j <- descriptive_N_perc(j, "switch_other_oncol_24_")
  
  # row 32
  row_header_1 <- c(row_header_1, "Ri-iniziatori")
  j <- descriptive_N_perc(j, "restarter_")


  #########################################
  # KEEP CELLS

  cell_cols <- grep("^cell_", names(tab_nice), value = TRUE)
  tokeep <- c("period_first", "ASL", "user_type", cell_cols)
  tab_nice <- tab_nice[, ..tokeep]


  #########################################
  # RESHAPE CELLS

  # First reshape tab_nice from wide into long format

  tab_nice <- melt(
    tab_nice,
    id.vars = c("period_first", "ASL", "user_type"),
    measure.vars = patterns(
      cell = "^cell_[0-9]+$"
    ),
    variable.name = "rownum"
  )

  tab_nice[, rownum := as.integer(rownum)]

  setorder(tab_nice, rownum, period_first, ASL, user_type)


  # then reshape from long to wide keeping rownum as the UoO

  tab_nice <- dcast(
    tab_nice,
    rownum ~ period_first + ASL +user_type,
    value.var = "value"
  )


  # Order rows correctly
  setorder(tab_nice, rownum)


  #########################################
  # ADD ROW HEADER
  #

  tab_nice[, row_header := row_header_1]
  tab_nice[, rownum := NULL]

  #########################################
  # FINAL COLUMNS

  data_cols <- setdiff(names(tab_nice), "row_header")
  p1_cols   <- grep("^2016_", data_cols, value = TRUE)
  p2_cols   <- grep("^2020_", data_cols, value = TRUE)
  p3_cols <- setdiff(data_cols, c(p1_cols, p2_cols))
  setcolorder(tab_nice, c("row_header", p1_cols, p2_cols, p3_cols))

  #########################################
  # NAMES

  # newnames <- c(
  #   pre = "Pre- Nota 100 AIFA (1ge2016-25gen2022)",
  #   nota = "Nota 100 AIFA (26gen2022-31lug2025)",
  #   modifica = "Modifica Nota 100 AIFA(1ago2025-31dic2025)"
  # )
  # 
  # 
  # tab_nice[1] <- lapply(tab_nice[1], function(x) {
  # 
  #   idx <- x %in% names(newnames)
  #   x[idx] <- unname(newnames[x[idx]])
  #   x
  # 
  #  })



  #########################################
  # SAVE

  outputfile <- tab_nice
  nameoutput <- "D6_Table_1"
  assign(nameoutput, outputfile)

  # rds
  saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,"_", k,".rds")))
  # csv
  fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,"_", k,".csv")))
  # xls
  write_xlsx(outputfile, file.path(thisdiroutput, paste0(nameoutput,"_", k,".xlsx")))
  # html
  # html_table <- kable(outputfile, format = "html", escape = FALSE) %>% kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
  # writeLines(html_table, file.path(thisdiroutput, paste0(nameoutput,"_", k,".html")))
  # rtf
  doc <- read_docx() %>% body_add_table(outputfile, style = "table_template", header = F) %>% body_end_section_continuous()
  print(doc, target = file.path(thisdiroutput, paste0(nameoutput,"_", k,".docx")))

}
