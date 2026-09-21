# authors: Sabrina Giometto

# v 0.1 18 Set 2026 - D6 created


print('CREATE D6_Table_S1_attrition')

# assign directories

if (TEST){ 
  testname <- "test_D6_Table_S1"
  thisdirinput <- paste0(file.path(dirtest, testname), "/")
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- direxp
  thisdiroutput <- direxp
}

year <- year_p

# load
D5 <- read.csv(paste0(thisdirinput, "D5_Table_S1_attrition_",year,".csv"))
D5 <- as.data.table(D5)
assign("D5_Table_S1_attrition", D5)
  


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

tab_nice <- copy(D5_Table_S1_attrition)

# row 0
row_header_1 <- c()
j            <- -1

# row 1
row_header_1 <- c(row_header_1, "Soggetti con un farmaco tra il 2023 e il 2025 e in anagrafe almeno un giorno tra il 2023 e il 2025 (istanza)")
j <- j + 1
tab_nice[, cell := as.character(N)]
setnames(tab_nice, "cell", paste0("cell_", j))

# row 2
row_header_1 <- c(row_header_1, paste0("Soggetti con data di nascita e sesso compilati e validi"))
j <- descriptive_N_perc(j, "sel_data_incomplete_")

# row 3
row_header_1 <- c(row_header_1, "Soggetti con un periodo di osservazione nel 2025")
j <- descriptive_N_perc(j, "sel_no_obs_periods_")

# row 4
row_header_1 <- c(row_header_1, paste0("Soggetti con un periodo di osservazione che comprende la data indice 31/12/", year))
j <- descriptive_N_perc(j, "sel_obs_period_not_overlapped_study_period_")

# row 5
row_header_1 <- c(row_header_1, "Soggetti di età >= 40 anni alla data indice")
j <- descriptive_N_perc(j, "sel_younger_than_40_")

# row 6
row_header_1 <- c(row_header_1, "Soggetti con almeno 3 anni di osservazione disponibili prima della data indice")
j <- descriptive_N_perc(j, "sel_no_lookback_")


# row 7
row_header_1 <- c(row_header_1, "Soggetti con ASL registrata alla data indice")
j <- descriptive_N_perc(j, "sel_no_ASL_")

# row 8
row_header_1 <- c(row_header_1, "Soggetti politrattati")
j <- descriptive_N_perc(j, "sel_no_polypharmacy_")

# row 9
row_header_1 <- c(row_header_1, "Totale soggetti inclusi nello studio")
j <- descriptive_N_perc(j, "is_in_study_")



#########################################
# KEEP CELLS

cell_cols <- grep("^cell_", names(tab_nice), value = TRUE)
tokeep <- c("N", cell_cols)
tab_nice <- tab_nice[, ..tokeep]


#########################################
# RESHAPE CELLS

# First reshape tab_nice from wide into long format

tab_nice <- melt(
  tab_nice,
  id.vars = "N",
  measure.vars = patterns(
    cell = "^cell_[0-9]+$"
  ),
  variable.name = "rownum"
)

tab_nice[, rownum := as.integer(rownum)]

# setorder(tab_nice, rownum, period, ASL)


# then reshape from long to wide keeping rownum as the UoO

tab_nice <- dcast(
  tab_nice,
  rownum ~ N,
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
setcolorder(tab_nice, c("row_header", data_cols))

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
# })



#########################################
# SAVE

outputfile <- tab_nice
nameoutput <- paste0("D6_Table_S1_attrition_",year)
assign(nameoutput, outputfile)

# rds
saveRDS(outputfile, file = file.path(thisdiroutput, paste0(nameoutput, ".rds")))
# csv
fwrite(outputfile, file = file.path(thisdiroutput, paste0(nameoutput, ".csv")))
# xls
write_xlsx(outputfile, file.path(thisdiroutput, paste0(nameoutput,".xlsx")))
# html
# html_table <- kable(outputfile, format = "html", escape = FALSE) %>% kable_styling(full_width = F, bootstrap_options = c("striped", "hover"))
# writeLines(html_table, file.path(thisdiroutput, paste0(nameoutput,"_", k,".html")))
# rtf
doc <- read_docx() %>% body_add_table(outputfile, style = "table_template", header = F) %>% body_end_section_continuous()
print(doc, target = file.path(thisdiroutput, paste0(nameoutput, ".docx")))


  