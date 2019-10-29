#step 1. load in all the data


diag_2019 <- read_excel(file.path(org::PROJ$DATA_RAW, "Diag data" ,"2019.xlsx"))
drugs_2019 <- read_excel(file.path(org::PROJ$DATA_RAW, "Drugs data" ,"2019.xlsx"))

#step 2. merge
d2019 <- merge(diag_2019, drugs_2019, by="BARADMISSIONID")


#step 3. check to see you haven't missed/duplicated anyone
nrow(diag_2019)
nrow(drugs_2019)
nrow(d2019)


d2019 <- merge(diag_2019, obs_2019, by="BARADMISSIONID")



ident_lab <- 
  
ident_diag <- 
  
ident_obs <-
  
ident_drug <-

