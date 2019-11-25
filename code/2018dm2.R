
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




diag_2018 <- read_excel(file.path(org::PROJ$DATA_RAW, "Diag Data" ,"2018.xlsx"))



setDT(diag_2018)
diag_2018[,ident_diag:=TRUE]


#some data cleaning &  some decriptive analysis
unique(diag_2018$`Diagnose Name`)



#data cleaning for 2018


diag_2018[,PatientDoB:=as.Date(`Patient DoB`, format="%d-%m-%Y")]
diag_2018[,admissiondate:=as.Date(`Admission Date`, format="%d-%m-%Y")]


diag_2018[,age:= as.numeric(difftime(as.Date(admissiondate), as.Date(PatientDoB), units="days"))/365.25]
diag_2018[, age:= round(age, digits=0)]


diag_2018[,age_cat:=cut(age,
                        breaks = c(0,20,30,40,50,60,100)
                        ,include.lowest=T)]




diag_2018[,id_within_baradmission:=1:.N,by=BARADMISSIONID]
xtabs(~diag_2018$id_within_baradmission)

decripdiag_2018 <- diag_2018[id_within_baradmission==1]
nrow(decripdiag_2018)
setDT(decripdiag_2018)
nrow(diag_2018)


diagnosedm8 <- xtabs(~decripdiag_2018$`Diagnose Name`)
openxlsx :: write.xlsx(diagnosedm8, 
                       file.path(org::PROJ$SHARED_TODAY,"2018diagnosis.xlsx"))



decripdiag_2018 [`Diagnose Name`%in% c("Type 2 diabetes mellitus"	,
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
                 ,DMM2:=T]

organdm8 <- xtabs(~decripdiag_2018$Organization)
openxlsx :: write.xlsx(organdm8, 
                       file.path(org::PROJ$SHARED_TODAY,"2018organization.xlsx"))


decripdiag_2018[Organization%in% c("Ramallah PHC",
                                   "Nablus PHC",
                                   "Azzoun Clinic",
                                   "Al Karantina Clinic",
                                   "Tarqumia Clinic",
                                   "Qalqilia PHC",
                                   " مركز محمد بن راشد آل مكتوم / مديرية صحة نابلس",
                                   "مركز الأمراض المزمنة و الجلدية")
                
                ,ident_clinic:=T]

table8 <- decripdiag_2018[DMM2==T & ident_clinic==T,
                          .(
                            N=.N
                            
                          ),
                          
                          keyby=
                            .(`Patient Sex`,
                              age_cat,
                              `Marital Status`
                            )]
openxlsx :: write.xlsx(table8, 
                       file.path(org::PROJ$SHARED_TODAY,"2018DM2.xlsx"))
#for each clinic in 2018

table8R<- decripdiag_2018[DMM2==T & Organization%in% c("Ramallah PHC"),
                         .(
                           N=.N
                           
                         ),
                         
                         keyby=
                           .(`Patient Sex`,
                             age_cat,
                             `Marital Status`
                           )]
openxlsx :: write.xlsx(table8R, 
                       file.path(org::PROJ$SHARED_TODAY,"2018DM2RAMALLA.xlsx"))
