# author: Sabrina Giometto, Elena Ferrati

# v 0.1 16 Sep 2026 primo draft

#########################################

if (TEST){
  testname <- "test_D5_Tabella_2_asl_2025"
  thisdirinput <- file.path(dirtest,testname)
  thisdiroutput <- file.path(dirtest,testname,"g_output")
  dir.create(thisdiroutput, showWarnings = F)
}else{
  thisdirinput <- dirinput
  thisdiroutput <- dirtemp
}


# load data

# dummy data
# data <- read.csv(paste0(thisdirinput, "/D3_coorte_con_caratterizzazione_dummy.csv"), sep = ";")
data <- readRDS(file = paste0(thisdirinput, "/D3_coorte_con_caratterizzazione.rds"))
data <- as.data.table(data)

atc <- unique(unlist(strsplit(data[,atc_5_almeno_3],"_")))
atc_IV_levels <- unique(substring(atc, 1, 5))
data[,(atc_IV_levels):=lapply(atc_IV_levels, function(x) as.integer(grepl(paste0("(^|_)", x), data[,atc_5_almeno_3])))]


for (j in asl) {

  
  # extract 10 most frequent ATC IV level
  distr <- data.frame(N=1)

  for (k in atc_IV_levels) {

    tmp <- data.frame(sum(data[asl==j, get(k)==1]))

    colnames(tmp) <- k

    distr <- cbind(distr, tmp)

  }

  distr <- as.data.table(distr)

  distr_l <- melt(distr[, N:=NULL],
                  measure.vars = names(distr),
                  variable.name = "variable",
                  value.name = "atc")

  distr_l <- setorder(distr_l, -atc)

  distr_l_sel <- distr_l[c(1:10)]

  variable_names <- distr_l_sel[, as.character(variable)]
  
  variable_names <- variable_names[!is.na(variable_names)]


  # create D5 with binary covariates
  covariates_full <- variable_names

  D5_cov <- NULL

  for (i in covariates_full) {

    tmp <- data[asl==j, .(
                N = .N,
                tmp_N = sum(get(i)==1),
                tmp_p = round(sum(get(i)==1)/.N,3)*100)]

    setnames(tmp,"tmp_N",paste0(i, "_N"))
    setnames(tmp,"tmp_p",paste0(i, "_p"))

    if (is.null(D5_cov)) {

      D5_cov <- tmp

    } else {

      D5_cov <- merge(D5_cov, tmp, by = c("N"))
    }

  }

  assign(paste0("D5_",j), D5_cov)

}

# save
for (j in fasce_eta) {

  saveRDS(get(paste0("D5_", j)), file = paste0(thisdiroutput, "/D5_Table_2_combinazioni_farmaci_2025_", j, ".rds"))
  write.csv(get(paste0("D5_", j)), file = paste0(thisdiroutput, "/D5_Table_2_combinazioni_farmaci_2025_", j, ".csv"))

}

