##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for.

## smoking status code
smoking_status_code_perc<-function(data){
  Smoking_Status_Code=data%>%
    select(C_BioSense_ID, Smoking_Status_Code)%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Smoking_Status_Code)%>%
    transmute(Smoking_Status_Code,count=n,percentage=round(n/sum(n),3))
  return(
    Smoking_Status_Code
  )
}
