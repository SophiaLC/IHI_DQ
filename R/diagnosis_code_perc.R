##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
##Diagnosis_Code

diagnosis_code_perc<-function(data){
  diagnosis=unlist(strsplit(paste0(data$Diagnosis_Code),";"))
  Diagnosis_Code=diagnosis[diagnosis!=""]
  as.data.frame(table(Diagnosis_Code))%>%
                              arrange(desc(Freq))

}
