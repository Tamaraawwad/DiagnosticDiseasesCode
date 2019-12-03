#step 1. load in all the data


diag_2019 <- read_excel(file.path(org::PROJ$DATA_RAW, "Diag data" ,"2019.xlsx"))
drugs_2019 <- read_excel(file.path(org::PROJ$DATA_RAW, "Drugs data" ,"2019.xlsx"))



setDT(diag_2019)
diag_2019[,ident_diag:=TRUE]


setDT(drug_2019)
drug_2019[,ident_drug:=TRUE]



diag_2019[,id_within_baradmission:=1:.N,by=BARADMISSIONID]
xtabs(~diag_2019$id_within_baradmission)

drug_2019[,id_within_baradmission:=1:.N,by="BARADMISSIONID"]
xtabs(~drug_2019$id_within_baradmission)

names(drug_2019)

dcast.data.table()
#dcast(DATATABLE, variables that uniquely identify rows ~ the values as your new columns)
#value.var: you have your rows, your columns, and the variable you are shifting: which is this


#step 2. merge
d2019 <- merge(diag_2019, drug_2019, by="BARADMISSIONID")
d2019 <- merge(diag_2019, drug_2019, by=c("BARADMISSIONID","CODE"))


#step 3. check to see you haven't missed/duplicated anyone


nrow(diag_2019)
nrow(drug_2019)
nrow(d2019)
nrow(decripdiag_2019)



d2019[BARADMISSIONID == 2001307447169]






