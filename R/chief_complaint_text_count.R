chief_complaint_text_count<-function(data){
  chief_complaint_text=data%>%
    select(Chief_Complaint_Text,C_BioSense_ID)%>%
    filter(is.na(Chief_Complaint_Text)==FALSE)%>%
    group_by(C_BioSense_ID)%>%
    count(Chief_Complaint_Text)%>%
    arrange(Chief_Complaint_Text)%>%
    select(Chief_Complaint_Text,n,C_BioSense_ID)
    
  
  return(chief_complaint_text)
}
