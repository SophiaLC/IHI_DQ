##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 

## State
state_perc<-function(data){
  State=data%>%
    select(C_BioSense_ID, Patient_State)%>%
    mutate(Patient_State=case_when(is.na(Patient_State)==TRUE~"NA"))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Patient_State)%>%
    transmute(Patient_State,count=n,percentage=round(n/sum(n),3))
  return(
    State
  )
}
