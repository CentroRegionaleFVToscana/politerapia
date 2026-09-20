# author: Sabrina Giometto

# v 0.1 18 Set 2026 Creation of D5 drafted


#########################################

if (TEST){
  testname <- "test_D5_Table_S1"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirtemp
  thisdiroutput <- direxp
}


# load data

year <- 2025

# D3_selezione_coorte <- read.csv(file = paste0(thisdirinput, "/D3_selezione_coorte_dummy.csv"), sep = ";")

data <- readRDS(file = paste0(thisdirinput, "/D3_selezione_coorte_",year,".rds"))
data <- as.data.table(data)
  
assign("D3_selezione_coorte", data)
  
# create D5 with binary selection variables

D5 <- NULL

for (i in var_selection) {
  
  if (!i %in% c("is_in_study")) {
    
    tmp <- D3_selezione_coorte[, .(
      N = .N,
      tmp_N = sum(get(i)==0, na.rm = T),
      tmp_p = round(sum(get(i)==0, na.rm = T)/.N,3)*100)]
    
    setnames(tmp,"tmp_N",paste0(i, "_N"))
    setnames(tmp,"tmp_p",paste0(i, "_p"))
    
  } else if (i == "is_in_study") {
    
    tmp <- D3_selezione_coorte[, .(
      N = .N,
      tmp_N = sum(get(i)==1, na.rm = T),
      tmp_p = round(sum(get(i)==1, na.rm = T)/.N,3)*100)]
    
    setnames(tmp,"tmp_N",paste0(i, "_N"))
    setnames(tmp,"tmp_p",paste0(i, "_p"))
    
  } 
  
  if (is.null(D5)) {
    
    D5 <- tmp
    
  } else {
    
    D5 <- merge(D5, tmp)
  }
  
  assign("D5_attrition", D5)
  
}
  

# save
saveRDS(D5_attrition, file = paste0(thisdiroutput, "/D5_Table_S1_attrition_",year,".rds"))
write.csv(D5_attrition, file = paste0(thisdiroutput, "/D5_Table_S1_attrition_",year,".csv"))

