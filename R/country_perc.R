##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 

##country
country_perc<-function(data){
  Country=data%>%
    select(C_BioSense_ID, Patient_Country)%>%
    mutate(Patient_Country=ifelse(is.na(Patient_Country),"NA",Patient_Country))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Patient_Country)%>%
    transmute(Patient_Country,count=n,percentage=round(n/sum(n),3))
  return(
    Country
  )
}
