

org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  
  HOME = "C:/Users/enasa/OneDrive/Documents/Diagnostic diseases/DiagnosticDiseasesCode",
  DATA_RAW = "C:/Users/enasa/OneDrive/Documents/Diagnostic diseases/data_raw",
  SHARED =  "C:/Users/enasa/OneDrive/Documents/Diagnostic diseases/results"
)

library(readxl)
library(openxlsx)
library(data.table)
library(gmodels)





diag_2017 <- read_excel(file.path(org::PROJ$DATA_RAW, "Diag Data" ,"2017.xlsx"))
diag_2016 <- read_excel(file.path(org::PROJ$DATA_RAW, "Diag Data" ,"2016.xlsx"))



setDT(diag_2017)
diag_2017[,ident_diag:=TRUE]
diag_2017[,year:=2017]

setDT(diag_2016)
diag_2016[,ident_diag:=TRUE]
diag_2016[,year:=2016]


#data cleaning for 2017


diag_2017[,PatientDoB:=as.Date(`Patient DoB`, format="%d-%m-%Y")]
diag_2017[,admissiondate:=as.Date(`Admission Date`, format="%d-%m-%Y")]


diag_2017[,age:= as.numeric(difftime(as.Date(admissiondate), as.Date(PatientDoB), units="days"))/365.25]
diag_2017[, age:= round(age, digits=0)]


diag_2017[,age_cat:=cut(age,
                        breaks = c(0,20,30,40,50,60,100)
                        ,include.lowest=T)]




diag_2017[,id_within_baradmission:=1:.N,by=BARADMISSIONID]
xtabs(~diag_2017$id_within_baradmission)

decripdiag_2017 <- diag_2017[id_within_baradmission==1]
nrow(decripdiag_2017)
setDT(decripdiag_2017)
nrow(diag_2017)


diagnosedm7 <- xtabs(~decripdiag_2017$`Diagnose Name`)
openxlsx :: write.xlsx(diagnosedm7, 
                       file.path(org::PROJ$SHARED_TODAY,"2017diagnosis.xlsx"))



decripdiag_2017 [`Diagnose Name`%in% c("Type 2 diabetes mellitus"	,
                                       "Type 2 diabetes mellitus with circulatory complications",
                                       "Type 2 diabetes mellitus with diabetic arthropathy",
                                       "Type 2 diabetes mellitus with diabetic cataract",
                                       "Type 2 diabetes mellitus with diabetic chronic kidney disease",
                                       "Type 2 diabetes mellitus with diabetic dermatitis",
                                       "Type 2 diabetes mellitus with diabetic nephropathy",
                                       "Type 2 diabetes mellitus with diabetic neuropathic arthropathy",
                                       "Type 2 diabetes mellitus with diabetic neuropathy, unspecified"	,
                                       "Type 2 diabetes mellitus with diabetic peripheral angiopathy with gangrene",
                                       "Type 2 diabetes mellitus with diabetic peripheral angiopathy without gangrene",
                                       "Type 2 diabetes mellitus with diabetic polyneuropathy",
                                       "Type 2 diabetes mellitus with foot ulcer",
                                       "Type 2 diabetes mellitus with hyperglycemia",
                                       "Type 2 diabetes mellitus with hyperosmolarity"	,
                                       "Type 2 diabetes mellitus with hyperosmolarity with coma",
                                       "Type 2 diabetes mellitus with hyperosmolarity without nonketotic hyperglycemic-hyperosmolar coma (NKHHC)"	,
                                       "Type 2 diabetes mellitus with hypoglycemia",
                                       "Type 2 diabetes mellitus with hypoglycemia with coma"	,
                                       "Type 2 diabetes mellitus with hypoglycemia without coma"	,
                                       "Type 2 diabetes mellitus with kidney complications"	,
                                       "Type 2 diabetes mellitus with mild nonproliferative diabetic retinopathy"	,
                                       "Type 2 diabetes mellitus with moderate nonproliferative diabetic retinopathy"	,
                                       "Type 2 diabetes mellitus with neurological complications"	,
                                       "Type 2 diabetes mellitus with ophthalmic complications"	,
                                       "Type 2 diabetes mellitus with other circulatory complications"	,
                                       "Type 2 diabetes mellitus with other diabetic arthropathy"	,
                                       "Type 2 diabetes mellitus with other diabetic kidney complication"	,
                                       "Type 2 diabetes mellitus with other diabetic neurological complication"	,
                                       "Type 2 diabetes mellitus with other diabetic ophthalmic complication"	,
                                       "Type 2 diabetes mellitus with other oral complications"	,
                                       "Type 2 diabetes mellitus with other skin complications"	,
                                       "Type 2 diabetes mellitus with other skin ulcer"	,
                                       "Type 2 diabetes mellitus with other specified complication"	,
                                       "Type 2 diabetes mellitus with other specified complications"	,
                                       "Type 2 diabetes mellitus with proliferative diabetic retinopathy"	,
                                       "Type 2 diabetes mellitus with severe nonproliferative diabetic retinopathy without macular edema"	,
                                       "Type 2 diabetes mellitus with skin complications"	,
                                       "Type 2 diabetes mellitus with unspecified complications"	,
                                       "Type 2 diabetes mellitus with unspecified diabetic retinopathy"	,
                                       "Type 2 diabetes mellitus with unspecified diabetic retinopathy with macular edema"	,
                                       "Type 2 diabetes mellitus with unspecified diabetic retinopathy without macular edema"	,
                                       "Type 2 diabetes mellitus without complications"	)
                 ,DM2:=T]

organdm7 <- xtabs(~decripdiag_2017$Organization)
openxlsx :: write.xlsx(organdm7, 
                       file.path(org::PROJ$SHARED_TODAY,"2017organization.xlsx"))


decripdiag_2017[Organization%in% c("Ramallah PHC",
                                   "Nablus PHC",
                                   "Azzoun Clinic",
                                   "Al Karantina Clinic",
                                   "Tarqumia Clinic",
                                   "Qalqilia PHC",
                                   "مركز محمد بن راشد آل مكتوم / مديرية صحة نابلس",
                                   "مركز الأمراض المزمنة و الجلدية")
                
                ,ident_clinic:=T]

table7 <- decripdiag_2017[DM2==T & ident_clinic==T,
                          .(
                            N=.N
                            
                          ),
                          
                          keyby=
                            .(`Patient Sex`,
                              age_cat,
                              `Marital Status`
                            )]
openxlsx :: write.xlsx(table7, 
                       file.path(org::PROJ$SHARED_TODAY,"2017DM2.xlsx"))

table07 <- decripdiag_2017[DM2==T & ident_clinic==T,
                           .(
                             N=.N
                             
                           ),
                           
                           keyby=Organization
                           ]

openxlsx :: write.xlsx(table07, 
                       file.path(org::PROJ$SHARED_TODAY,"2017DM2perorg.xlsx"))




#data cleaning for 2016

diag_2016[,PatientDoB:=as.Date(`Patient DoB`, format="%d-%m-%Y")]
diag_2016[,admissiondate:=as.Date(`Admission Date`, format="%d-%m-%Y")]


diag_2016[,age:= as.numeric(difftime(as.Date(admissiondate), as.Date(PatientDoB), units="days"))/365.25]
diag_2016[, age:= round(age, digits=0)]


diag_2016[,age_cat:=cut(age,
                        breaks = c(0,20,30,40,50,60,100)
                        ,include.lowest=T)]




diag_2016[,id_within_baradmission:=1:.N,by=BARADMISSIONID]
xtabs(~diag_2016$id_within_baradmission)

decripdiag_2016 <- diag_2016[id_within_baradmission==1]
nrow(decripdiag_2016)
setDT(decripdiag_2016)
nrow(diag_2016)


diagnosedm6 <- xtabs(~decripdiag_2016$`Diagnose Name`)
openxlsx :: write.xlsx(diagnosedm6, 
                       file.path(org::PROJ$SHARED_TODAY,"2016diagnosis.xlsx"))

diagnosedm6 <- xtabs(~diag_2016$`Diagnose Name`)

decripdiag_2016 [`Diagnose Name`%in% c("Type 2 diabetes mellitus"	,
                                       "Type 2 diabetes mellitus with circulatory complications",
                                       "Type 2 diabetes mellitus with diabetic arthropathy",
                                       "Type 2 diabetes mellitus with diabetic cataract",
                                       "Type 2 diabetes mellitus with diabetic chronic kidney disease",
                                       "Type 2 diabetes mellitus with diabetic dermatitis",
                                       "Type 2 diabetes mellitus with diabetic nephropathy",
                                       "Type 2 diabetes mellitus with diabetic neuropathic arthropathy",
                                       "Type 2 diabetes mellitus with diabetic neuropathy, unspecified"	,
                                       "Type 2 diabetes mellitus with diabetic peripheral angiopathy with gangrene",
                                       "Type 2 diabetes mellitus with diabetic peripheral angiopathy without gangrene",
                                       "Type 2 diabetes mellitus with diabetic polyneuropathy",
                                       "Type 2 diabetes mellitus with foot ulcer",
                                       "Type 2 diabetes mellitus with hyperglycemia",
                                       "Type 2 diabetes mellitus with hyperosmolarity"	,
                                       "Type 2 diabetes mellitus with hyperosmolarity with coma",
                                       "Type 2 diabetes mellitus with hyperosmolarity without nonketotic hyperglycemic-hyperosmolar coma (NKHHC)"	,
                                       "Type 2 diabetes mellitus with hypoglycemia",
                                       "Type 2 diabetes mellitus with hypoglycemia with coma"	,
                                       "Type 2 diabetes mellitus with hypoglycemia without coma"	,
                                       "Type 2 diabetes mellitus with kidney complications"	,
                                       "Type 2 diabetes mellitus with mild nonproliferative diabetic retinopathy"	,
                                       "Type 2 diabetes mellitus with moderate nonproliferative diabetic retinopathy"	,
                                       "Type 2 diabetes mellitus with neurological complications"	,
                                       "Type 2 diabetes mellitus with ophthalmic complications"	,
                                       "Type 2 diabetes mellitus with other circulatory complications"	,
                                       "Type 2 diabetes mellitus with other diabetic arthropathy"	,
                                       "Type 2 diabetes mellitus with other diabetic kidney complication"	,
                                       "Type 2 diabetes mellitus with other diabetic neurological complication"	,
                                       "Type 2 diabetes mellitus with other diabetic ophthalmic complication"	,
                                       "Type 2 diabetes mellitus with other oral complications"	,
                                       "Type 2 diabetes mellitus with other skin complications"	,
                                       "Type 2 diabetes mellitus with other skin ulcer"	,
                                       "Type 2 diabetes mellitus with other specified complication"	,
                                       "Type 2 diabetes mellitus with other specified complications"	,
                                       "Type 2 diabetes mellitus with proliferative diabetic retinopathy"	,
                                       "Type 2 diabetes mellitus with severe nonproliferative diabetic retinopathy without macular edema"	,
                                       "Type 2 diabetes mellitus with skin complications"	,
                                       "Type 2 diabetes mellitus with unspecified complications"	,
                                       "Type 2 diabetes mellitus with unspecified diabetic retinopathy"	,
                                       "Type 2 diabetes mellitus with unspecified diabetic retinopathy with macular edema"	,
                                       "Type 2 diabetes mellitus with unspecified diabetic retinopathy without macular edema"	,
                                       "Type 2 diabetes mellitus without complications"	)
                 ,DM2:=T]

organdm6 <- xtabs(~decripdiag_2016$Organization)
openxlsx :: write.xlsx(organdm6, 
                       file.path(org::PROJ$SHARED_TODAY,"2016organization.xlsx"))


decripdiag_2016[Organization%in% c("Ramallah PHC",
                                   "Nablus PHC",
                                   "Azzoun Clinic",
                                   "Al Karantina Clinic",
                                   "Tarqumia Clinic",
                                   "Qalqilia PHC",
                                   "مركز محمد بن راشد آل مكتوم / مديرية صحة نابلس",
                                   "مركز الأمراض المزمنة و الجلدية")
                
                ,ident_clinic:=T]

table6 <- decripdiag_2016[DM2==T & ident_clinic==T,
                          .(
                            N=.N
                            
                          ),
                          
                          keyby=
                            .(`Patient Sex`,
                              age_cat,
                              `Marital Status`
                            )]
openxlsx :: write.xlsx(table6, 
                       file.path(org::PROJ$SHARED_TODAY,"2016DM2.xlsx"))


#for each clinics the size of patients with dm2 in 2019 




table06 <- decripdiag_2016[DM2==T & ident_clinic==T,
                           .(
                             N=.N
                             
                           ),
                           
                           keyby=Organization
                           ]

openxlsx :: write.xlsx(table06, 
                       file.path(org::PROJ$SHARED_TODAY,"2016DM2perorg.xlsx"))

