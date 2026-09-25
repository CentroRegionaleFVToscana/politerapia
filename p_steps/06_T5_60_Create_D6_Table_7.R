
#### D6_Table_7 ----


# authors: Sabrina Giometto, Elena Ferrati

# v 0.1 25 Sep 2026 Creation of D6 started



print('CREATE D6_Table_7')


# assign directories

if (TEST){ 
  testname <- "test_D6_Tabella_7_2025"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- direxp
  thisdiroutput <- direxp
}


# load
D5 <- read.csv(paste0(thisdirinput, "/D5_Tabella_7_caratterizzazione_2025.csv"))
D5 <- as.data.table(D5)



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
  varN <- paste0(covar,"N")
  varP <- paste0(covar,"p")
  varATCIV <- sub("_[^_]*$", "", covar)
  
  tab_nice[, cell := paste0(
    get(varATCIV), " ",
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

tab_nice <- copy(D5)

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

# row 8
row_header_1 <- c(row_header_1, "Anziani potenzialmente esposti a prescrizioni di farmaci inappropriati (indipendentemente dalla diagnosi)")
j <-  add_empty_row(j)

# row 8
row_header_1 <- c(row_header_1, "Pazienti politrattati esposti, n (%)")
j <-  add_empty_row(j)

# row 3
row_header_1 <- c(row_header_1, "Anticolinergici (e.g. Idrossizina, clorfeniramina, prometazina")
j <- descriptive_N_perc(j, "anticol_")

# row 4
row_header_1 <- c(row_header_1, "FANS orali (e.g. Ibuprofene, naprossene, diclofenac)")
j <- descriptive_N_perc(j, "fans_")

# row 5
row_header_1 <- c(row_header_1, "Antipsicotici (e.g. Aloperidolo, clorpromazina)")
j <- descriptive_N_perc(j, "antipsico_")

# row 6
row_header_1 <- c(row_header_1, "Sulfoniluree a lunga durata (Glibenclamide)")
j <- descriptive_N_perc(j, "sulfo_")

# row 7
row_header_1 <- c(row_header_1, "Digossina >0,125 mg/die")
j <- descriptive_N_perc(j, "digo_")

# row 8
row_header_1 <- c(row_header_1, "Scivolatori insulinici (Insulina rapida senza basale)")
j <- descriptive_N_perc(j, "sci_insul_")

# row 9
row_header_1 <- c(row_header_1, "Inibitori di pompa protonica (PPI)")
j <- descriptive_N_perc(j, "ppi_")

# row 8
row_header_1 <- c(row_header_1, "Anziani esposti esposti a prescrizioni a rischio di interazioni farmaco-farmaco")
j <-  add_empty_row(j)

# row 8
row_header_1 <- c(row_header_1, "Pazienti politrattati esposti, n (%)")
j <-  add_empty_row(j)

# row 10
row_header_1 <- c(row_header_1, "Anticoagulante orale + FANS (e.g. Warfarin/DOAC
                                 + FANS + diclofenc)")
j <- descriptive_N_perc(j, "anticoag_fans_")

# row 11
row_header_1 <- c(row_header_1, "≥3 farmaci con effetto anticolinergico (i.e. 
                                 burden anticolinergico cumulativo)")
j <- descriptive_N_perc(j, "anticol_atleast_3_")

# row 12
row_header_1 <- c(row_header_1, "ACE-inibitore/ARB + FANS + diuretico (Triplice 
                                 whammy)")
j <- descriptive_N_perc(j, "ace_fans_diur_")

# row 12
row_header_1 <- c(row_header_1, "Antiaggregante + FANS (senza PPI) (e.g. 
                                 Aspirina + diclofenc)")
j <- descriptive_N_perc(j, "antiagg_fans_")


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
  variable.name = "rownum",
)

tab_nice[, rownum := as.integer(sub("cell_", "", rownum))]

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
p1_cols   <- grep("Tutte", data_cols, value = TRUE)
# p2_cols   <- grep("^2020_", data_cols, value = TRUE)
p3_cols <- setdiff(data_cols, p1_cols)
setcolorder(tab_nice, c("row_header", p1_cols, p3_cols))

#########################################
# NAMES

newnames <- c(
  CE = "Centro",
  NO = "Nord-Ovest",
  SE = "Sud-Est"
)


tab_nice[1] <- lapply(tab_nice[1], function(x) {

  idx <- x %in% names(newnames)
  x[idx] <- unname(newnames[x[idx]])
  x

 })



#########################################
# SAVE

outputfile <- tab_nice
nameoutput <- "D6_Table_7"
assign(nameoutput, outputfile)

# rds
saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".rds")))
# csv
fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput,".csv")))
# xls
write_xlsx(outputfile, file.path(thisdiroutput, paste0(nameoutput,".xlsx")))
# html
# html_table <- kable(outputfile, format = "html", escape = FALSE) %>% kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
# writeLines(html_table, file.path(thisdiroutput, paste0(nameoutput,".html")))
# rtf
doc <- read_docx() %>% body_add_table(outputfile, style = "table_template", header = F) %>% body_end_section_continuous()
print(doc, target = file.path(thisdiroutput, paste0(nameoutput,".docx")))



