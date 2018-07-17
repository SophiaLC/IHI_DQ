##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for.

## smoking status description
smoking_status_description_perc<-function(data){
  Smoking_Status_Description=data%>%
    select(C_BioSense_ID, Smoking_Status_Description)%>%
     mutate(Smoking_Status_Description=case_when(is.na(Smoking_Status_Description)==TRUE~"NA"))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Smoking_Status_Description)%>%
    transmute(Smoking_Status_Description,count=n,percentage=round(n/sum(n),3))
  return(
    Smoking_Status_Description
  )
}
