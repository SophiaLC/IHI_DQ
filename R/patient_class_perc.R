##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
## C_MFT_Patient_Class

patient_class_perc<-function(data){
    Patient_Class=data%>%
    select(C_BioSense_ID, Patient_Class_Code)%>%
    mutate(Patient_Class_Code=ifelse(is.na(Patient_Class_Code),"NA",Patient_Class_Code))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Patient_Class_Code)%>%
    transmute(Patient_Class_Code,count=n,percentage=round(100*n/sum(n),2))
  return(
    Patient_Class
  )
}
