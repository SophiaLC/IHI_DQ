admit_reason_description_count<-function(data){
  admit_reason_description=data%>%
    select(Admit_Reason_Description,C_BioSense_ID)%>%
    filter(is.na(Admit_Reason_Description)==FALSE)%>%
    group_by(C_BioSense_ID)%>%
    count(Admit_Reason_Description)%>%
    arrange(Admit_Reason_Description)%>%
    select(Admit_Reason_Description,n,C_BioSense_ID)
    
    return(admit_reason_description)
}
