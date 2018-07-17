##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 

## Insurance_Company_ID
insurance_company_id_perc<-function(data){
  Insurance_Company_ID=data%>%
    select(C_BioSense_ID, Insurance_Company_ID)%>%
     mutate(Insurance_Company_ID=ifelse(is.na(Insurance_Company_ID),"NA",Insurance_Company_ID))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Insurance_Company_ID)%>%
    transmute(Insurance_Company_ID,count=n,percentage=round(n/sum(n),3))
  return(
    Insurance_Company_ID
  )
}
