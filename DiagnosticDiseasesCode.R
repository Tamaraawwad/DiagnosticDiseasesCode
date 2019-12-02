
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
library(data.table)
library(gmodels)


#in general if I want to take a quick view on any data, i can work on data frame 
# and do some xtabs and crosstabs, chisquare 
# to create a new variable in ....data.frame:
#   data$variable <- 3
# data.table:
#   data[,variable:=3]

#creating vectors for data raw
#DIAGNOSTIC DATA



diag_2019 <- read_excel(file.path(org::PROJ$DATA_RAW, "Diag Data" ,"2019.xlsx"))
# diag_2018 <- read_excel(file.path(org::PROJ$DATA_RAW, "Diag Data" ,"2018.xlsx"))
# diag_2017 <- read_excel(file.path(org::PROJ$DATA_RAW, "Diag Data" ,"2017.xlsx"))
# diag_2016 <- read_excel(file.path(org::PROJ$DATA_RAW, "Diag Data" ,"2016.xlsx"))


# Drugs Data
drug_2019 <- read_excel(file.path(org::PROJ$DATA_RAW, "Drugs Data" ,"2019.xlsx"))
# drug_2018 <- read_excel(file.path(org::PROJ$DATA_RAW, "Drugs Data" ,"2018.xlsx"))
# drug_2017 <- read_excel(file.path(org::PROJ$DATA_RAW, "Drugs Data" ,"2017.xlsx"))
# drug_2016 <- read_excel(file.path(org::PROJ$DATA_RAW, "Drugs Data" ,"2016.xlsx"))


#Lab Data
lab_2019 <- read_excel(file.path(org::PROJ$DATA_RAW, "Lab Data" ,"2019.xlsx"))
# lab_2018 <- read_excel(file.path(org::PROJ$DATA_RAW, "Lab Data" ,"2018.xlsx"))
# lab_2017 <- read_excel(file.path(org::PROJ$DATA_RAW, "Lab Data" ,"2017.xlsx"))
# lab_2016 <- read_excel(file.path(org::PROJ$DATA_RAW, "Lab Data" ,"2016.xlsx"))

#Observation Data
obs_2019 <- read_excel(file.path(org::PROJ$DATA_RAW, "observation Data" ,"2019.xlsx"))
# obs_2018 <- read_excel(file.path(org::PROJ$DATA_RAW, "observation Data" ,"2018.xlsx"))
# obs_2017 <- read_excel(file.path(org::PROJ$DATA_RAW, "observation Data" ,"2017.xlsx"))
# obs_2016 <- read_excel(file.path(org::PROJ$DATA_RAW, "observation Data" ,"2016.xlsx"))


# transform to data tabels 2019 database and creating new attributes


setDT(diag_2019)
diag_2019[,ident_diag:=TRUE]

setDT(drug_2019)
drug_2019[,ident_drug:=TRUE]

setDT(lab_2019)
lab_2019[,ident_lab:=TRUE]

setDT(obs_2019)
obs_2019[,ident_obs:=TRUE]


#View(diag_2019)
#some data cleaning &  some decriptive analysis

xtabs(~diag_2019$`Patient DoB`)
#how to creat age variable from date of birth
#unique(diag_2019$Patient DoB`
#d[, age := as.numeric(difftime(as.Date("2019-01-01"), as.Date(date_of_birth), units="days"))/365.25]

unique(diag_2019$`Patient DoB`)


diag_2019[,PatientDoB:=as.Date(`Patient DoB`, format="%d-%m-%Y")]
diag_2019[,admissiondate:=as.Date(`Admission Date`, format="%d-%m-%Y")]


diag_2019[,age:= as.numeric(difftime(as.Date(admissiondate), as.Date(PatientDoB), units="days"))/365.25]
diag_2019[, age:= round(age, digits=0)]


diag_2019[,age_cat:=cut(age,
                        breaks = c(0,20,30,40,50,60,100)
                        ,include.lowest=T)]




diag_2019[,id_within_baradmission:=1:.N,by=BARADMISSIONID]
xtabs(~diag_2019$id_within_baradmission)

decripdiag_2019 <- diag_2019[id_within_baradmission==1]
nrow(decripdiag_2019)
setDT(decripdiag_2019)
nrow(diag_2019)

xtabs(~decripdiag_2019$`Patient Sex`)
xtabs(~decripdiag_2019$`Marital Status`)
xtabs(~decripdiag_2019$age_cat)

org<-xtabs(~decripdiag_2019$Organization)

openxlsx :: write.xlsx(org, 
                       file.path(org::PROJ$SHARED_TODAY,"2019organization.xlsx"))


xtabs(~decripdiag_2019$`Admission Status`)
xtabs(~decripdiag_2019$`Admission Hosp/Dept Name`)
departments <- unique(decripdiag_2019$`Admission Hosp/Dept Name`)

openxlsx :: write.xlsx(departments, 
                       file.path(org::PROJ$SHARED_TODAY,"2019depatrment.xlsx"))

xtabs(~decripdiag_2019$`Medical Order Note`)

unique(drug_2019$NAME)
drugs <- xtabs(~drug_2019$NAME)

openxlsx :: write.xlsx(drugs, 
                       file.path(org::PROJ$SHARED_TODAY,"2019drugs.xlsx"))


labtests <- xtabs(~lab_2019$`Labtest Name`)

openxlsx :: write.xlsx(labtests, 
                       file.path(org::PROJ$SHARED_TODAY,"2019labtests.xlsx"))


#creat new variable age in prescdiag2019




View(diag_2019)
names(diag_2019)
dim(diag_2019)
str(diag_2019)

unique(diag_2019$`Diagnose Name`)


tam <- xtabs(~decripdiag_2019$'Diagnose Name')

#file.path(org::PROJ$DATA_RAW,"2019.xlsx")
#use SHARED TODAY FOR RESULTS 

openxlsx :: write.xlsx(tam, 
                       file.path(org::PROJ$SHARED_TODAY,"2019diagnosis.xlsx"))




barplot(tam, main="Diagnosis Distribution",
        xlab="Number of cases")


counts <- table(diag_2019$'Diagnose Name')




library(gmodels)


tamm <- CrossTable (diag_2019$`Diagnose Name`,diag_2019$Organization,addNA=T)

openxlsx :: write.xlsx(tamm, 
                       file.path(org::PROJ$SHARED_TODAY,"2019cross.xlsx"))


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




#merging data sets 2019
#to do check on duplicated cases ...

#d[question, assignment, group/by]

#diag_2019[BARADMISSIONID == 2001307447169]
# we need to merge diag and lab by bara &medical order id 
#then merge with obs by bara,medical, and admition date
#then with drugs by bara  ....more than one drug


xtabs(~diag_2019$id_within_baradmission)

#diag_2019 <- diag_2019[id_within_baradmission==1]
nrow(diag_2019)


labd2019 <- merge(diag_2019, lab_2019, by=c("BARADMISSIONID", "Medical Order Id"))

nrow(labd2019)


obs2019 <- merge(diag_2019, obs_2019, by=c("BARADMISSIONID", "Medical Order Id"))
nrow(obs2019)


diag_2019 <- diag_2019[id_within_baradmission==1]

dru2019 <- merge(diag_2019, drug_2019, by="BARADMISSIONID")
nrow(dru2019)


dru2019[BARADMISSIONID == 2001307447169]






