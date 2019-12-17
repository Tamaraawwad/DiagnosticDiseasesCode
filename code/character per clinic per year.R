

#run 2016,2017,2018,2019 scripts first

d <- rbind(decripdiag_2019,decripdiag_2018, decripdiag_2017, decripdiag_2016, fill=TRUE)
nrow(d)

# error appeared that the class is different between 2 columns 
str(decripdiag_2016)

str(decripdiag_2017)
nrow(decripdiag_2018)
nrow(decripdiag_2016)

d[year=="2018"]
decripdiag_2018[year==2018]
decripdiag_2019[year==2019]
                
                
diag_2019[Organization =="مركز محمد بن راشد آل مكتوم / مديرية صحة نابلس"]
diag_2019[Organization %in% c("مركز محمد بن راشد آل مكتوم / مديرية صحة نابلس")]

decripdiag_2018[Organization =="Ramallah PHC"]
          
diag_2018[BARADMISSIONID==2001021085467]
t <- decripdiag_2018[BARADMISSIONID==2001021085467]
openxlsx :: write.xlsx(t, 
                       file.path(org::PROJ$SHARED_TODAY,"77.xlsx"))


#demographic characters for dm2 in 2019 at ramallah

table9R<- d[DM2==T & Organization %in% c("Ramallah PHC"),
                          .(
                            N=.N
                            
                          ),
                          
                          keyby=
                            .(`Patient Sex`,
                              age_cat,
                              `Marital Status`,
                               year
                            )]
openxlsx :: write.xlsx(table9R, 
                       file.path(org::PROJ$SHARED_TODAY,"DM2ramallah.xlsx"))



#demographic characters for dm2 in 2019 at nablus


table9n<- d[DM2==T & Organization%in% c("Nablus PHC"),
                          .(
                            N=.N
                            
                          ),
                          
                          keyby=
                            .(`Patient Sex`,
                              age_cat,
                              `Marital Status`,
                              year
                            )]
openxlsx :: write.xlsx(table9n, 
                       file.path(org::PROJ$SHARED_TODAY,"DM2nablus.xlsx"))



table9a<- d[DM2==T & Organization%in% c("Azzoun Clinic"),
                          .(
                            N=.N
                            
                          ),
                          
                          keyby=
                            .(`Patient Sex`,
                              age_cat,
                              `Marital Status`,
                              year
                            )]
openxlsx :: write.xlsx(table9a, 
                       file.path(org::PROJ$SHARED_TODAY,"DM2azzon.xlsx"))


table9k<- d[DM2==T & Organization%in% c("Al Karantina Clinic"),
                          .(
                            N=.N
                            
                          ),
                          
                          keyby=
                            .(`Patient Sex`,
                              age_cat,
                              `Marital Status`,
                              year
                            )]
openxlsx :: write.xlsx(table9k, 
                       file.path(org::PROJ$SHARED_TODAY,"DM2karantina.xlsx"))

table9t<- d[DM2==T & Organization%in% c("Tarqumia Clinic"),
                          .(
                            N=.N
                            
                          ),
                          
                          keyby=
                            .(`Patient Sex`,
                              age_cat,
                              `Marital Status`,
                              year
                            )]
openxlsx :: write.xlsx(table9t, 
                       file.path(org::PROJ$SHARED_TODAY,"DM2tarqumia.xlsx"))


table9q<- d[DM2==T & Organization%in% c("Qalqilia PHC"),
                          .(
                            N=.N
                            
                          ),
                          
                          keyby=
                            .(`Patient Sex`,
                              age_cat,
                              `Marital Status`,
                              year
                            )]
openxlsx :: write.xlsx(table9q, 
                       file.path(org::PROJ$SHARED_TODAY,"DM2qalqilia.xlsx"))

table9m<- d[DM2==T & Organization %in% c("مركز محمد بن راشد آل مكتوم / مديرية صحة نابلس"),
                          .(
                            N=.N
                            
                          ),
                          
                          keyby=
                            .(`Patient Sex`,
                              age_cat,
                              `Marital Status`,
                              year
                            )]
openxlsx :: write.xlsx(table9m, 
                       file.path(org::PROJ$SHARED_TODAY,"DM2markezmohammad.xlsx"))

table9ma<- d[DM2==T & Organization%in% c("مركز الأمراض المزمنة و الجلدية"),
                           .(
                             N=.N
                             
                           ),
                           
                           keyby=
                             .(`Patient Sex`,
                               age_cat,
                               `Marital Status`,
                               year
                             )]
openxlsx :: write.xlsx(table9ma, 
                       file.path(org::PROJ$SHARED_TODAY,"DM2markezamrad.xlsx"))



## to do predictive model
tablepredic<- d[DM2==T,
                .(
                  NumberofDiabeticpatient=.N
                  
                ),
                
                keyby=
                  .(
                    year
                  )]
openxlsx :: write.xlsx(tablepredic, 
                       file.path(org::PROJ$SHARED_TODAY,"DM2predict.xlsx"))

#create an aggregated dataset of the number of diabetic patients every year/month



#run a poisson regression, with
#outcome = # diabetic patients
#  exposures = year (continuous) and month (factor)

poissonmodel <- glm(NumberofDiabeticpatient~year, tablepredic, family = "poisson")

summary(poissonmodel)



pred_data <- data.table(year=2015:2025)
predicted <- predict(poissonmodel, pred_data)
pred_data[,predicted:=exp(predicted)]

#by month

d[,month:=lubridate::month(admissiondate)]

xtabs(~d$`Admission Date`)

tablepredicm<- d[DM2==T,
                .(
                  NumberofDiabeticpatient=.N
                  
                ),
                
                keyby=
                  .(
                    year,
                    month
                  )]

tablepredicm[,x:=1:.N]
openxlsx :: write.xlsx(tablepredicm, 
                       file.path(org::PROJ$SHARED_TODAY,"DM2real.xlsx"))

poissonmodelm <- glm(NumberofDiabeticpatient~year + as.factor(month), tablepredicm, family = "poisson")

summary(poissonmodelm)

plot(poissonmodelm)


#as.factor ....because otherwise 'month' is treated as continuous (edited) 
#which means "the difference between january and february is the same as the difference between november and december"
#it means something like "every month the number of patients increases by 1%, until we go december -> january, then it drops by 12%"
#if you put month as a factor, then it models each month individually


# expand.grid means create a data.table with all possible combinations


pred_data <- data.table(expand.grid(year=2015:2025,month=1:12))
predicted <- predict(poissonmodelm, pred_data)
pred_data[,predicted:=round(exp(predicted),digits = 0)]

pred_data[,x:=1:.N]
openxlsx :: write.xlsx(pred_data, 
                       file.path(org::PROJ$SHARED_TODAY,"DM2predictm.xlsx"))


# make a graph
# plotting the observed and predicted data
# x-axis as time, y-axis as number of observation
library(rlang)
library(tidyverse)
# install.packages("devtools") 
# devtools::install_github("r-lib/rlang", build_vignettes = TRUE)



setorder(tablepredicm, year, month)

setorder(pred_data, year, month)

tablepredicm[,x:=1:.N]
pred_data[,x:=1:.N]
p <- ggplot()
p <- p + geom_line(data=pred_data, mapping = aes(x=x, y=predicted, color="Predicted"))
p <- p + geom_line(data=tablepredicm, mapping = aes(x=x, y=NumberofDiabeticpatient, color="Observed"))
p


ggsave(
  filename=file.path(
    org::PROJ$SHARED_TODAY,
    "observed_predicted.png"),
  plot=p, 
  width = 297, 
  height = 210, 
  units = "mm")





       


