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
                
                
diag_2019[Organization ==" مركز محمد بن راشد آل مكتوم / مديرية صحة نابلس"]
diag_2019[Organization %in% c(" مركز محمد بن راشد آل مكتوم / مديرية صحة نابلس")]

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



