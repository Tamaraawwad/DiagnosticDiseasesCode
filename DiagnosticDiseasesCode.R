
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

#in general if I want to take a quick view on any data, i can work on data frame 
# and do some xtabs and crosstabs, chisquare 
# to create a new variable in ....data.frame:
#   data$variable <- 3
# data.table:
#   data[,variable:=3]

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


# transform to data tabels

library(data.table)

setDT(diag_2019)
diag_2019[,ident_diag:=TRUE]


setDT(drug_2019)
drug_2019[,ident_drug:=TRUE]

setDT(lab_2019)
lab_2019[,ident_lab:=TRUE]

setDT(obs_2019)
obs_2019[,ident_obs:=TRUE]


diag_2019[,id_within_baradmission:=1:.N,by=BARADMISSIONID]
xtabs(~diag_2019$id_within_baradmission)

#d[question, assignment, group/by]

diag_2019[id_within_baraadmission==2001307447169]

d2019 <- merge(diag_2019, drug_2019, by="BARADMISSIONID")


d2019 <- merge(d2019, lab_2019, by="BARADMISSIONID")
d2019 <- merge(d2019, obs_2019, by="BARADMISSIONID")










View(diag_2019)
names(diag_2019)
dim(diag_2019)
str(diag_2019)

# primary analysis 
unique(diag_2019$`Diagnose Name`)


tam <- xtabs(~ diag_2019$'Diagnose Name')

#file.path(org::PROJ$DATA_RAW,"2019.xlsx")
#use SHARED TODAY FOR RESULTS 

openxlsx :: write.xlsx(tam, 
                       file.path(org::PROJ$SHARED_TODAY,"2019.xlsx"))


chisq.test(tam)

barplot(tam, main="Diagnosis Distribution",
        xlab="Number of cases")




counts <- table(diag_2019$'Diagnose Name')




library(gmodels)


tamm <- CrossTable (diag_2019$`Diagnose Name`,diag_2019$Organization,addNA=T)

openxlsx :: write.xlsx(tamm, 
                       file.path(org::PROJ$SHARED_TODAY,"2019cross.xlsx"))


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
