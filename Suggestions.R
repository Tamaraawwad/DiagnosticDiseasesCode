
# creating folders 

org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  
HOME = "C:/Users/enasa/OneDrive/Documents/Diagnostic diseases/DiagnosticDiseasesCode",
DATA_RAW = "C:/Users/enasa/OneDrive/Documents/Diagnostic diseases/data_raw",
SHARED =  "C:/Users/enasa/OneDrive/Documents/Diagnostic diseases/results"
)


library(readxl)
library(openxlsx)
X2019 <- read_excel(file.path(org::PROJ$DATA_RAW,"2019.xlsx"))

View(X2019)
names(X2019)
dim(X2019)
str(X2019)


unique(X2019$`Diagnose Name`)




tam <- xtabs(~X2019$`Medical Order Id`+ X2019$`Diagnose Name`,addNA = T)

chisq.test(tam,)

tam <- xtabs(~ X2019$`Diagnose Name`)



openxlsx :: write.xlsx(tam, 
                       file.path(org::PROJ$SHARED_TODAY,
                                 "diagnostic.xlsx"))

barplot(tam, main="Diagnosis Distribution",
        xlab="Number of cases")










counts <- table(X2019$'Diagnose Name')

barplot(counts, main="Car Distribution",
        xlab="Number of Gears")



library(gmodels)


tam2 <- CrossTable (X2019$`Diagnose Name`,X2019$Organization,addNA=T)

openxlsx :: write.xlsx(tam2, 
                       file.path("C:/Users/enasa/OneDrive/Documents/Diagnostic diseases/results",
                                 "diagnostic2.xlsx"))



#library(data.table)
#setDT(X2019)
#View(X2019)
#X2019[,DMDIAG:=F]
#xtabs(~X2019$DMDIAG)

#X2019['Diagnose Name'%in% c("Addisonian crisis","Tumor lysis syndrome", "Vitamin deficiency, unspecified")                                                                                  

#                             ,DMDIAG:=T]


# #table <- X2019[,
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
