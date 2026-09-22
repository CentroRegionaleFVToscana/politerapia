
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


descriptive_N_perc_variante <- function(j, covar) {
  
  j <- j + 1
  varN   <- paste0(covar, "N")
  varP   <- paste0(covar, "p")
  varATC <- sub("_[^_]*$", "", covar)  
  
  tab_nice[, cell := paste0(
    get(varATC), " ",
    formatC(get(varN), format = "f", digits = 0, big.mark = ".",
            decimal.mark = ","), " (",
    formatC(get(varP), format = "f", digits = 1, big.mark = ".",
            decimal.mark = ","), "%)"
  )]
  setnames(tab_nice, "cell", paste0("cell_", j))
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

  # row 1
  row_header_1 <- c(row_header_1, "ASL")
  j <- j + 1
  tab_nice[, cell := asl]
  setnames(tab_nice, "cell", paste0("cell_", j))

  # row 2
  row_header_1 <- c(row_header_1, "Totale")
  j <- j + 1
  tab_nice[, cell := as.character(N)]
  setnames(tab_nice, "cell", paste0("cell_", j))

  # row 3
  row_header_1 <- c(row_header_1, "Donne, n (%)")
  j <- descriptive_N_perc(j, "genere_F_")

  # row 4
  row_header_1 <- c(row_header_1, "Pazienti in iper-politerapia (≥10 farmaci ATC IV nello stesso mese per 3 mesi su 12)")
  j <- descriptive_N_perc(j, "iperpoliterapia_")
  
  # row 5
  row_header_1 <- c(row_header_1, "Principi attivi più utilizzati (ATC V livello dispensati ≥ 3 volte in un anno)")
  j <- add_empty_row(j)

  # row 6
  row_header_1 <- c(row_header_1, "1")
  j <- descriptive_N_perc_variante(j, "farmaco_più_utilizzato_1_")

  # row 7
  row_header_1 <- c(row_header_1, variable_names_full[[k]][2])
  j <- descriptive_N_perc(j, paste0(variable_names_full[[k]][2], "_"))
  
  # row 8
  row_header_1 <- c(row_header_1, variable_names_full[[k]][3])
  j <- descriptive_N_perc(j, paste0(variable_names_full[[k]][3], "_"))
  
  # row 9
  row_header_1 <- c(row_header_1, variable_names_full[[k]][4])
  j <- descriptive_N_perc(j, paste0(variable_names_full[[k]][4], "_"))
  
  # row 10
  row_header_1 <- c(row_header_1, variable_names_full[[k]][5])
  j <- descriptive_N_perc(j, paste0(variable_names_full[[k]][5], "_"))

  # row 11
  row_header_1 <- c(row_header_1, "Contesto assistenziale")
  j <- add_empty_row(j)

  # row 12
  row_header_1 <- c(row_header_1, "RSA e/o ADI (≥1 giorno durate l’anno)")
  j <- descriptive_N_perc(j, "rsa_adi_")

  # row 13
  row_header_1 <- c(row_header_1, "Esenzione per patologia cronica")
  j <-  add_empty_row(j)

  # row 14
  row_header_1 <- c(row_header_1, "Cardiocircolatoria")
  j <- descriptive_N_perc(j, "cardiocircolatoria_")
  
  # row 15
  row_header_1 <- c(row_header_1, "Reumatologica")
  j <- descriptive_N_perc(j, "reumatologica_")
  
  # row 16
  row_header_1 <- c(row_header_1, "Gastroenterologica")
  j <- descriptive_N_perc(j, "gastroenterologica_")
  
  # row 17
  row_header_1 <- c(row_header_1, "≥1 esenzione per qualsiasi malattia cronica")
  j <- descriptive_N_perc(j, "esenzione_qualsiasi_")
  
  


  #########################################
  # KEEP CELLS

  cell_cols <- grep("^cell_", names(tab_nice), value = TRUE)
  tokeep <- c("asl", cell_cols)
  tab_nice <- tab_nice[, ..tokeep]


  #########################################
  # RESHAPE CELLS

  # First reshape tab_nice from wide into long format

  tab_nice <- melt(
    tab_nice,
    id.vars = c("asl"),
    measure.vars = patterns(
      cell = "^cell_[0-9]+$"
    ),
    variable.name = "rownum"
  )

  tab_nice[, rownum := as.integer(rownum)]

  setorder(tab_nice, rownum, asl)


  # then reshape from long to wide keeping rownum as the UoO

  tab_nice <- dcast(
    tab_nice,
    rownum ~ asl,
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
