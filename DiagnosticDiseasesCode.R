
# creating folders, SHARED it is an argument to the function org::InitialiseProject
#shared ....it will create a folder inside with a date

org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  
HOME = "C:/Users/enasa/OneDrive/Documents/Diagnostic diseases/DiagnosticDiseasesCode",
DATA_RAW = "C:/Users/enasa/OneDrive/Documents/Diagnostic diseases/data_raw",
SHARED =  "C:/Users/enasa/OneDrive/Documents/Diagnostic diseases/results"
)

library(readxl)
library(openxlsx)

#creating vectors for data raw
#DIAGNOSTIC DATA

diag_2019 <- read_excel(file.path(org::PROJ$DATA_RAW, "Diag Data" ,"2019.xlsx"))
diag_2018 <- read_excel(file.path(org::PROJ$DATA_RAW, "Diag Data" ,"2018.xlsx"))
diag_2017 <- read_excel(file.path(org::PROJ$DATA_RAW, "Diag Data" ,"2017.xlsx"))
diag_2016 <- read_excel(file.path(org::PROJ$DATA_RAW, "Diag Data" ,"2016.xlsx"))


# Drugs Data
drug_2019 <- read_excel(file.path(org::PROJ$DATA_RAW, "Drugs Data" ,"2019.xlsx"))
drug_2018 <- read_excel(file.path(org::PROJ$DATA_RAW, "Drugs Data" ,"2018.xlsx"))
drug_2017 <- read_excel(file.path(org::PROJ$DATA_RAW, "Drugs Data" ,"2017.xlsx"))
drug_2016 <- read_excel(file.path(org::PROJ$DATA_RAW, "Drugs Data" ,"2016.xlsx"))


#Lab Data
lab_2019 <- read_excel(file.path(org::PROJ$DATA_RAW, "Lab Data" ,"2019.xlsx"))
lab_2018 <- read_excel(file.path(org::PROJ$DATA_RAW, "Lab Data" ,"2018.xlsx"))
lab_2017 <- read_excel(file.path(org::PROJ$DATA_RAW, "Lab Data" ,"2017.xlsx"))
lab_2016 <- read_excel(file.path(org::PROJ$DATA_RAW, "Lab Data" ,"2016.xlsx"))

#Observation Data
obs_2019 <- read_excel(file.path(org::PROJ$DATA_RAW, "observation Data" ,"2019.xlsx"))
obs_2018 <- read_excel(file.path(org::PROJ$DATA_RAW, "observation Data" ,"2018.xlsx"))
obs_2017 <- read_excel(file.path(org::PROJ$DATA_RAW, "observation Data" ,"2017.xlsx"))
obs_2016 <- read_excel(file.path(org::PROJ$DATA_RAW, "observation Data" ,"2016.xlsx"))



View(diag_2019)
names(diag_2019)
dim(diag_2019)
str(diag_2019)


unique(diag_2019$`Diagnose Name`)




tam <- xtabs(~diag_2019$`Medical Order Id`+ diag_2019$'Diagnose Name',addNA = T)

chisq.test(tam,)

tam <- xtabs(~ diag_2019$'Diagnose Name')



openxlsx :: write.xlsx(tam, 
                       file.path("C:/Users/enasa/OneDrive/Documents/Diagnostic diseases/results",
                                 "diagnostic.xlsx"))

barplot(tam, main="Diagnosis Distribution",
        xlab="Number of cases")










counts <- table(diag_2019$'Diagnose Name')

barplot(counts, main="Car Distribution",
        xlab="Number of Gears")



library(gmodels)


tam2 <- CrossTable (diag_2019$`Diagnose Name`,diag_2019$Organization,addNA=T)

openxlsx :: write.xlsx(tam2, 
                       file.path("C:/Users/enasa/OneDrive/Documents/Diagnostic diseases/results",
                                 "diagnostic2.xlsx"))



#library(data.table)
#setDT(diag_2019)
#View(diag_2019)
#diag_2019[,DMDIAG:=F]
#xtabs(~diag_2019$DMDIAG)

#diag_2019['Diagnose Name'%in% c("Addisonian crisis","Tumor lysis syndrome", "Vitamin deficiency, unspecified")                                                                                  

#                             ,DMDIAG:=T]


# #table <- diag_2019[,
#               
#               .(
#                 N=.N,
#                 
#                 DM=sum(`Medical Order Id`,na.rm = T)
#                 ),
#               
#               keyby=
#               .(Organization
#                 
#               )]
