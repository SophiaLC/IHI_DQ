##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
##Diagnosis_Code

diagnosis_code_perc<-function(data){
  Diagnosis_Code=data%>%
    select(C_BioSense_ID, Diagnosis_Code)%>%
    mutate(Diagnosis_Code=ifelse(is.na(Diagnosis_Code),"NA",Diagnosis_Code))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Diagnosis_Code)%>%
    transmute(Diagnosis_Code,count=n,percentage=round(n/sum(n),3))
  return(
    Diagnosis_Code
  )
}
