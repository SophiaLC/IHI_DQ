##Frequency of occurrence in data - what is actually occurring in the data and
##how often each distinct entry is occurring and percentage of all visits each entry
##accounts for. 
##Facility_Type_Description

facility_type_description_perc<-function(data){
  Facility_Type_Description=data%>%
    select(C_BioSense_ID, Facility_Type_Description)%>%
    mutate(Facility_Type_Description=toupper(Facility_Type_Description),
           Facility_Type_Description=ifelse(is.na(Facility_Type_Description),"NA",Facility_Type_Description))%>%
    distinct(C_BioSense_ID,.keep_all=TRUE)%>%
    count(Facility_Type_Description)%>%
    transmute(Facility_Type_Description,count=n,percentage=round(n/sum(n),3))
  return(
    Facility_Type_Description
  )
}
