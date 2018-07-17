##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
## for variable Race_Description

race_description_perc<-function(data){
  Race_Description=data%>%
    select(C_BioSense_ID,Race_Description)%>%
    mutate(Race_Description=case_when(is.na(Race_Description)==TRUE~"NA"))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Race_Description)%>%
    transmute(Race_Description,count=n,percentage=round(n/sum(n),3))
  
  return(
    Race_Description
  )
  
}
