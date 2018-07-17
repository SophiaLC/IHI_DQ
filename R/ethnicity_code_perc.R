##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 

## Ethnicity_Code
ethnicity_code_perc<-function(data){
  Ethnicity_Code=data%>%
    select(C_BioSense_ID,Ethnicity_Code)%>%
    mutate(Ethnicity_Code=ifelse(is.na(Ethnicity_Code),"NA",Ethnicity_Code))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Ethnicity_Code)%>%
    transmute(Ethnicity_Code,count=n,percentage=round(n/sum(n),3))
  
  return(
    Ethnicity_Code
  )
  
}
