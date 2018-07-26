clinical_impression_count<-function(data){
  clinical_impression=data%>%
    select(Clinical_Impression,C_BioSense_ID)%>%
    filter(is.na(Clinical_Impression)==FALSE)%>%
    group_by(C_BioSense_ID)%>%
    count(Clinical_Impression)%>%
    arrange(Clinical_Impression)%>%
    select(Clinical_Impression,n,C_BioSense_ID)
  
  return(clinical_impression)
}
