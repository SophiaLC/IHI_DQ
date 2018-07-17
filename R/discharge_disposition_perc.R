##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 

## discharge disposition
discharge_disposition_perc<-function(data){
  Discharge_Disposition=data%>%
    select(C_BioSense_ID, Discharge_Disposition)%>%
    mutate(Discharge_Disposition=ifelse(is.na(Discharge_Disposition),"NA",Discharge_Disposition))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Discharge_Disposition)%>%
    transmute(Discharge_Disposition,count=n,percentage=round(n/sum(n),3))
  return(
    Discharge_Disposition
  )
}
