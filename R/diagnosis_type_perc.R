
##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
## Diagnosis_Type

diagnosis_type_perc<-function(data){
  Diagnosis_Type=data%>%
    select(C_BioSense_ID, Diagnosis_Type)%>%
    mutate(Diagnosis_Type=ifelse(is.na(Diagnosis_Type),"NA",Diagnosis_Type))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Diagnosis_Type)%>%
    transmute(Diagnosis_Type,count=n,percentage=round(n/sum(n),3))
  return(
    Diagnosis_Type
  )
}
