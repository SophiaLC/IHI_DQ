##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
## C_MFT_Patient_Class

patient_class_perc<-function(data){
    Patient_Class=data%>%
    select(C_BioSense_ID, C_MFT_Patient_Class)%>%
    mutate(C_MFT_Patient_Class=ifelse(is.na(C_MFT_Patient_Class),"NA",C_MFT_Patient_Class))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(C_MFT_Patient_Class)%>%
    transmute(C_MFT_Patient_Class,count=n,percentage=round(n/sum(n),3))
  return(
    Patient_Class
  )
}
