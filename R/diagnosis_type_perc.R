
##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
## Diagnosis_Type

diagnosis_type_perc<-function(data){
  diagnosis=unlist(strsplit(paste0(data$Diagnosis_Type),";"))
  Diagnosis_Type=diagnosis[diagnosis!=""]
  as.data.frame(table(Diagnosis_Type))%>%
                                  arrange(desc(Freq))
}
